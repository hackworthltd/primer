{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
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
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Optics (
  elemOf,
  folded,
  getting,
  to,
  (%),
  _2,
 )
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
  bindName,
  getID,
 )
import Primer.Core.Utils (
  _freeVarsTy,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.Redex (
  Cxt (..),
  Dir (..),
  EvalLog,
  Redex (
    ElideLet,
    InlineLet,
    InlineLetrec,
    RenameBindingsCase,
    RenameBindingsLAM,
    RenameBindingsLam,
    RenameSelfLet,
    RenameSelfLetType,
    branches,
    letBinding,
    tyvar,
    var
  ),
  RedexType (
    ElideLetInType,
    InlineLetInType,
    RenameForall,
    RenameSelfLetInType,
    letBinding,
    origBinder,
    var
  ),
  viewRedex,
  viewRedexType,
  _freeVarsLetBinding,
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
  focusConTypes,
  focusType,
  getBoundHere,
  getBoundHere',
  getBoundHereTy,
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

viewLet :: (Dir, ExprZ) -> Maybe (LetBinding, Accum Cxt (Dir, ExprZ))
viewLet dez@(_, ez) = case (target ez, exprChildren dez) of
  (Let _ x e _b, [_, bz]) -> Just (LetBind x e, bz)
  (Letrec _ x e ty _b, [_, bz]) -> Just (LetrecBind x e ty, bz)
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (LetTyBind $ LetTypeBind a ty, bz)
  _ -> Nothing

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
          (Just goSubst, Just (l, bz)) -> (readerToAccumT . ReaderT . (\(d', b) -> goSubst l b d')) =<< hoistAccum bz
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          _ ->
            msum $
              (goType =<< focusType' ez)
                : ((goType =<<) <$> focusConTypes' ez)
                  <> map (go <=< hoistAccum) (exprChildren dez)
    goType :: TypeZ -> AccumT Cxt f a
    goType tz =
      readerToAccumT (ReaderT $ extract.ty tz)
        <|> case (extract.substTy, target tz) of
          (Just goSubstTy, TLet _ a t _body)
            | [_, bz] <- typeChildren tz -> (readerToAccumT . ReaderT . goSubstTy a t) =<< hoistAccum bz
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

focusConTypes' :: Monad m => ExprZ -> [AccumT Cxt m TypeZ]
-- Note that nothing in Expr binds a variable which scopes over a type child
-- so we don't need to 'add' anything
focusConTypes' = maybe empty (fmap pure) . focusConTypes

hoistAccum :: Monad m => Accum Cxt b -> AccumT Cxt m b
hoistAccum = Foreword.hoistAccum generalize

-- We find the normal-order redex.
-- Annoyingly this is not quite leftmost-outermost wrt our Expr type, as we
-- are using 'let's to encode something similar to explicit substitution, and
-- reduce them by substituting one occurrance at a time, removing the 'let'
-- when there are no more substitutions to be done. Note that the 'let' itself
-- doesn't get "pushed down" in the tree.
-- example:
--   lettype a = Bool -> Bool in (λx.not x : a) True
-- reduces to
--   lettype a = Bool -> Bool in (λx.not x : Bool -> Bool) True
-- and then to
--   (λx.not x : Bool -> Bool) True
-- This can be seen as "leftmost-outermost" if you consider the location of the
-- "expand a" redex to be the 'lettype' rather than the variable occurrance.
findRedex ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  MaybeT m RedexWithContext
findRedex tydefs globals =
  foldMapExpr
    ( FMExpr
        { expr = \ez d -> runReaderT (RExpr ez <$> viewRedex tydefs globals d (target ez))
        , ty = \tz -> hoistMaybe . runReader (RType tz <<$>> viewRedexType (target tz))
        , subst = Just (\l -> fmap evalAccumT . goSubst l)
        , substTy = Just (\v t -> fmap hoistMaybe . evalAccumT . goSubstTy v t)
        }
    )
  where
    goSubst :: LetBinding -> ExprZ -> Dir -> AccumT Cxt (MaybeT m) RedexWithContext
    goSubst l ez d = do
      let here =
            readerToAccumT (viewRedex tydefs globals d $ target ez) >>= \case
              -- We should inline such 'v' (note that we will not go under any 'v' binders)
              r@(InlineLet{var}) | letBindingName l == unLocalName var -> pure $ RExpr ez r
              r@(InlineLetrec{var}) | letBindingName l == unLocalName var -> pure $ RExpr ez r
              -- Elide a let only if it blocks the reduction
              r@(ElideLet{letBinding}) | elemOf _freeVarsLetBinding (letBindingName letBinding) l -> pure $ RExpr ez r
              -- Rename a binder only if it blocks the reduction
              r@(RenameBindingsLam{var}) | elemOf _freeVarsLetBinding (unLocalName var) l -> pure $ RExpr ez r
              r@(RenameBindingsLAM{tyvar}) | elemOf _freeVarsLetBinding (unLocalName tyvar) l -> pure $ RExpr ez r
              r@(RenameBindingsCase{branches})
                | not $ S.disjoint (setOf _freeVarsLetBinding l) (setOf (folded % #_CaseBranch % _2 % folded % to bindName % to unLocalName) branches) ->
                    pure $ RExpr ez r
              r@(RenameSelfLet{var}) | elemOf _freeVarsLetBinding (unLocalName var) l -> pure $ RExpr ez r
              r@(RenameSelfLetType{tyvar}) | elemOf _freeVarsLetBinding (unLocalName tyvar) l -> pure $ RExpr ez r
              _ -> mzero
          -- Switch to an inner let if substituting under it would cause capture
          innerLet = case viewLet (d, ez) of
            Just (l', bz)
              | letBindingName l' /= letBindingName l
              , elemOf _freeVarsLetBinding (letBindingName l') l ->
                  (\(d', b) -> goSubst l' b d') =<< hoistAccum bz
            _ -> mzero
          dive =
            let substChild (d', c) = do
                  -- We should not go under 'v' binders, but otherwise substitute in each child
                  guard $ S.notMember (letBindingName l) $ getBoundHere (target ez) (Just $ target c)
                  goSubst l c d'
                substTyChild c = case l of
                  LetTyBind (LetTypeBind v t) -> goSubstTy v t c
                  _ -> mzero
             in msum @[] $
                  Foreword.hoistAccum hoistMaybe (substTyChild =<< focusType' ez)
                    : (Foreword.hoistAccum hoistMaybe . (substTyChild =<<) <$> focusConTypes' ez)
                      <> map (substChild <=< hoistAccum) (exprChildren (d, ez))
       in here <|> innerLet <|> dive
    goSubstTy :: TyVarName -> Type -> TypeZ -> AccumT Cxt Maybe RedexWithContext
    goSubstTy v t tz =
      let isFreeIn = elemOf (getting _freeVarsTy % _2)
       in do
            hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
              -- We should inline such 'v' (note that we will not go under any 'v' binders)
              Just r@(InlineLetInType{var}) | var == v -> pure $ RType tz r
              -- Elide a let only if it blocks the reduction
              Just r@(ElideLetInType{letBinding = (LetTypeBind w _)}) | w `isFreeIn` t -> pure $ RType tz r
              -- Rename a binder only if it blocks the reduction
              Just r@(RenameSelfLetInType{letBinding = (LetTypeBind w _)}) | w `isFreeIn` t -> pure $ RType tz r
              Just r@(RenameForall{origBinder}) | origBinder `isFreeIn` t -> pure $ RType tz r
              -- We switch to an inner let if substituting under it would cause capture
              Nothing
                | TLet _ w s _ <- target tz
                , [_, bz] <- typeChildren tz
                , v /= w
                , w `isFreeIn` t ->
                    goSubstTy w s =<< hoistAccum bz
              -- We should not go under 'v' binders, but otherwise substitute in each child
              _ ->
                let substChild c = do
                      guard $
                        S.notMember (unLocalName v) $
                          S.map unLocalName $
                            getBoundHereTy (target tz) (Just $ target c)
                      goSubstTy v t c
                 in msum $ map (substChild <=< hoistAccum) (typeChildren tz)

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
          Case _ scrut _ | scrut == target c -> Syn
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
