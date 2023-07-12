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

import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Accum (
  Accum,
  add,
  execAccum,
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
  TyVarName,
  Type,
  Type' (
    TLet
  ),
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
  ViewRedexOptions,
  cxtAddLet,
  viewRedex,
  viewRedexType,
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
  letBindingName,
  right,
  target,
 )
import Primer.Zipper.Type (
  LetTypeBinding' (LetTypeBind),
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
  , bodyVL :: (Dir, ExprZ)
  -- ^ the body (i.e. after the `in`)
  , typeChildrenVL :: [TypeZ]
  -- ^ any non-body type children
  , termChildrenVL :: [(Dir, ExprZ)]
  -- ^ any non-body term children (i.e. rhs of the binding)
  }
viewLet :: (Dir, ExprZ) -> Maybe ViewLet
viewLet dez@(_, ez) = case (target ez, exprChildren dez) of
  (Let _ x e _b, [ez', bz]) -> Just (ViewLet (LetBind x e) bz [] [ez'])
  (Letrec _ x e ty _b, [ez', bz]) -> Just (ViewLet (LetrecBind x e ty) bz tz [ez'])
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (ViewLet (LetTyBind $ LetTypeBind a ty) bz tz [])
  _ -> Nothing
  where
    tz :: [TypeZ]
    tz = maybeToList $ focusType ez

-- | This is similar to 'foldMap', with a few differences:
-- - 'Expr' is not foldable
-- - We target every subexpression and also every (nested) subtype (e.g. in an annotation)
-- - We keep track of context and directionality (necessitating an extra 'Dir' argument, for "what directional context this whole expression is in")
--   (the "context" is just of the immediately-enclosing lets, which are the only ones that may "cross a binder")
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
foldMapExpr extract topDir = go mempty . (topDir,) . focus
  where
    go :: Cxt -> (Dir, ExprZ) -> f a
    go lets dez@(d, ez) =
      extract.expr ez d lets
        <|> case (extract.subst, viewLet dez) of
          (Just goSubst, Just (ViewLet{bindingVL, bodyVL = (d', b)})) -> goSubst bindingVL b d' $ cxtAddLet bindingVL lets
          -- Prefer to compute inside the body of a let, but otherwise compute in the binding
          -- NB: we never push lets into lets, so the Cxt is reset for non-body children
          (Nothing, Just (ViewLet{bindingVL, bodyVL, typeChildrenVL, termChildrenVL})) ->
            msum $
              go (cxtAddLet bindingVL lets) bodyVL
                : map (goType mempty) typeChildrenVL
                  <> map (go mempty) termChildrenVL
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          -- Since this node is not a let, the context is reset
          _ ->
            msum $
              (goType mempty =<< focusType' ez)
                : map (go mempty) (exprChildren dez)
    goType :: Cxt -> TypeZ -> f a
    goType lets tz =
      extract.ty tz lets
        <|> case (extract.substTy, target tz) of
          (Just goSubstTy, TLet _ a t _body)
            | [_, bz] <- typeChildren tz -> goSubstTy a t bz lets
          (Nothing, TLet _ a t _body)
            -- Prefer to compute inside the body of a let, but otherwise compute in the binding
            | [tz', bz] <- typeChildren tz -> goType (cxtAddLet (LetTyBind $ LetTypeBind a t) lets) bz <|> goType mempty tz'
          _ -> msum $ map (goType mempty) $ typeChildren tz

data FMExpr m = FMExpr
  { expr :: ExprZ -> Dir -> Cxt -> m
  , ty :: TypeZ -> Cxt -> m
  , subst :: Maybe (LetBinding -> ExprZ {- The body of the let-} -> Dir -> Cxt -> m)
  , substTy :: Maybe (TyVarName -> Type -> TypeZ -> Cxt -> m)
  }

focusType' :: MonadPlus m => ExprZ -> m TypeZ
focusType' = maybe empty pure . focusType

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

exprChildren :: (Dir, ExprZ) -> [(Dir, ExprZ)]
exprChildren (d, ez) =
  children' ez <&> \c -> do
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
    (d', c)

typeChildren :: TypeZ -> [TypeZ]
typeChildren = children'

addBinds :: i -> [Either Name LetBinding] -> Accum Cxt ()
addBinds _ bs = do
  add $
    Cxt $
      M.fromList $
        bs <&> \case
          Left n -> (n, Nothing)
          Right l -> (letBindingName l, Just l)

singletonCxt :: i -> LetBinding -> Cxt
singletonCxt i l = addBinds i [Right l] `execAccum` mempty
