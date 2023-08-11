{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Eval.NormalOrder (
  RedexWithContext (RExpr, RType),
  findRedex,
  foldMapExpr,
  FMExpr (..),
  -- Exported for testing
  singletonCxt,
) where

import Foreword hiding (hoistAccum)
import Foreword qualified

import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.Accum (
  Accum,
  AccumT,
  add,
  evalAccumT,
  execAccum,
  look,
  readerToAccumT,
 )
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Map qualified as M
import Primer.Core (
  Expr,
  Expr' (
    APP,
    App,
    Case,
    Hole,
    Let,
    LetType,
    Letrec
  ),
  HasID,
  LocalName (unLocalName),
  TyVarName,
  Type,
  Type' (
    TLet
  ),
  getID,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.Redex (
  Cxt (..),
  Dir (..),
  EvalLog,
  Redex,
  RedexType,
  viewRedex,
  viewRedexType, ViewRedexOptions,
 )
import Primer.Log (ConvertLogMessage)
import Primer.Name (Name)
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  ExprZ,
  IsZipper,
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  TypeZ,
  down,
  focus,
  focusType,
  getBoundHere',
  letBindingName,
  right,
  target,
 )
import Primer.Zipper.Type (
  LetTypeBinding' (LetTypeBind),
  getBoundHereTy',
 )

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

data ViewLet = ViewLet
  { bindingVL :: LetBinding
  -- ^ the binding itself
  , bodyVL :: Accum Cxt (Dir, ExprZ)
  -- ^ the body (i.e. after the `in`)
  , typeChildrenVL :: [Accum Cxt TypeZ]
  -- ^ any non-body type children
  , termChildrenVL :: [Accum Cxt (Dir, ExprZ)]
  -- ^ any non-body term children (i.e. rhs of the binding)
  }
viewLet :: (Dir, ExprZ) -> Maybe ViewLet
viewLet dez@(_, ez) = case (target ez, exprChildren dez) of
  (Let _ x e _b, [ez', bz]) -> Just (ViewLet (LetBind x e) bz [] [ez'])
  (Letrec _ x e ty _b, [ez', bz]) -> Just (ViewLet (LetrecBind x e ty) bz tz [ez'])
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (ViewLet (LetTyBind $ LetTypeBind a ty) bz tz [])
  _ -> Nothing
  where
    tz :: [Accum Cxt TypeZ]
    -- as with focusType', we don't need to bind anything here
    tz = maybe [] ((: []) . pure) $ focusType ez

-- | This is similar to 'foldMap', with a few differences:
-- - 'Expr' is not foldable
-- - We target every subexpression and also every (nested) subtype (e.g. in an annotation)
-- - We keep track of context and directionality (necessitating an extra 'Dir' argument, for "what directional context this whole expression is in")
-- - We handle @let@s specially, since we need to handle them differently when finding the normal order redex.
--   (When we hit the body of a @let@ (of any flavor), we use the provided 'subst' or 'substTy' argument, if provided, and do no further recursion.
--   If the corresponding argument is 'Nothing', then we recurse as normal.)
-- - We accumulate in some 'f a' for 'MonadPlus f' rather than an arbitrary monoid
--
-- The fold progresses in something similar to normal order, (modulo the special handling for @let@s).
-- Sepcifically, we find the root-most redex, and recurse into type children first, followed by term children left-to-right.
-- Since stuck terms (ignoring lets, because they have special handling) are stuck because of
-- their type annotation, or their first child, this will unblock progress at the root before doing too much work on its children.
-- However, we may reduce type children to normal form more eagerly than necessary,
-- both reducing type annotations more than needed, and reducing type applications when not needed.
-- Since computation in types is strongly normalising, this will not cause us to fail to find any normal forms.
foldMapExpr :: forall f a. MonadPlus f => FMExpr (f a) -> Dir -> Expr -> f a
foldMapExpr extract topDir = flip evalAccumT mempty . go . (topDir,) . focus
  where
    go :: (Dir, ExprZ) -> AccumT Cxt f a
    go dez@(d, ez) =
      readerToAccumT (ReaderT $ extract.expr ez d)
        <|> case (extract.subst, viewLet dez) of
          (Just goSubst, Just (ViewLet{bindingVL, bodyVL})) -> (readerToAccumT . ReaderT . (\(d', b) -> goSubst bindingVL b d')) =<< hoistAccum bodyVL
          -- Prefer to compute inside the body of a let, but otherwise compute in the binding
          (Nothing, Just (ViewLet{bodyVL, typeChildrenVL, termChildrenVL})) ->
            msum $
              (go =<< hoistAccum bodyVL)
                : map (goType <=< hoistAccum) typeChildrenVL
                  <> map (go <=< hoistAccum) termChildrenVL
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          _ ->
            msum $
              (goType =<< focusType' ez)
                : map (go <=< hoistAccum) (exprChildren dez)
    goType :: TypeZ -> AccumT Cxt f a
    goType tz =
      readerToAccumT (ReaderT $ extract.ty tz)
        <|> case (extract.substTy, target tz) of
          (Just goSubstTy, TLet _ a t _body)
            | [_, bz] <- typeChildren tz -> (readerToAccumT . ReaderT . goSubstTy a t) =<< hoistAccum bz
          (Nothing, TLet _ _ _t _body)
            -- Prefer to compute inside the body of a let, but otherwise compute in the binding
            | [tz', bz] <- typeChildren tz -> (goType =<< hoistAccum bz) <|> (goType =<< hoistAccum tz')
          _ -> msum $ map (goType <=< hoistAccum) $ typeChildren tz

data FMExpr m = FMExpr
  { expr :: ExprZ -> Dir -> Cxt -> m
  , ty :: TypeZ -> Cxt -> m
  , subst :: Maybe (LetBinding -> ExprZ {- The body of the let-} -> Dir -> Cxt -> m)
  , substTy :: Maybe (TyVarName -> Type -> TypeZ -> Cxt -> m)
  }

focusType' :: MonadPlus m => ExprZ -> AccumT Cxt m TypeZ
-- Note that nothing in Expr binds a variable which scopes over a type child
-- so we don't need to 'add' anything
focusType' = maybe empty pure . focusType

hoistAccum :: Monad m => Accum Cxt b -> AccumT Cxt m b
hoistAccum = Foreword.hoistAccum generalize

-- We find the normal-order redex.
findRedex ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  ViewRedexOptions ->
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  MaybeT m RedexWithContext
findRedex opts tydefs globals =
  foldMapExpr
    ( FMExpr
        { expr = \ez d -> runReaderT (RExpr ez <$> viewRedex opts tydefs globals d (target ez))
        , ty = \tz -> hoistMaybe . runReader (RType tz <<$>> viewRedexType opts (target tz))
        , subst = Nothing
        , substTy = Nothing
        }
    )

children' :: IsZipper za a => za -> [za]
children' z = case down z of
  Nothing -> mempty
  Just z' -> z' : unfoldr (fmap (\x -> (x, x)) . right) z'

exprChildren :: (Dir, ExprZ) -> [Accum Cxt (Dir, ExprZ)]
exprChildren (d, ez) =
  children' ez <&> \c -> do
    let bs = getBoundHere' (target ez) (Just $ target c)
    let d' = case target ez of
          App _ f _ | f == target c -> Syn
          APP _ f _ | f == target c -> Syn
          Case _ scrut _ _ | scrut == target c -> Syn
          Hole _ _ -> Syn
          -- bodies of lets are the same direction as
          -- the let themselves
          Let _ _ e _
            | e == target c -> Syn
            | otherwise -> d
          LetType{} -> d
          Letrec _ _ e _ _
            | e == target c -> Chk
            | otherwise -> d
          _ -> Chk
    addBinds ez bs
    pure (d', c)

typeChildren :: TypeZ -> [Accum Cxt TypeZ]
typeChildren tz =
  children' tz <&> \c -> do
    let bs = getBoundHereTy' (target tz) (Just $ target c)
    addBinds tz $ bimap unLocalName LetTyBind <$> bs
    pure c

addBinds :: HasID i => i -> [Either Name LetBinding] -> Accum Cxt ()
addBinds i' bs = do
  let i = getID i'
  cxt <- look
  add $
    Cxt $
      M.fromList $
        bs <&> \case
          Left n -> (n, (Nothing, i, cxt))
          Right l -> (letBindingName l, (Just l, i, cxt))

singletonCxt :: HasID i => i -> LetBinding -> Cxt
singletonCxt i l = addBinds i [Right l] `execAccum` mempty
