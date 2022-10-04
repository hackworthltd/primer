{-# LANGUAGE GADTs #-}

module Primer.Eval.NormalOrder (
  RedexWithContext(RExpr,RType),
  findRedex,
) where

-- TODO: share code with Primer.Eval
-- I hope to reuse this code in Eval - the current implementation does some weird things with annotations and metadata
-- but that will come later
-- TODO/REVIEW: delete this comment!

-- TODO: ensure do sane things to metadata
-- (Perhaps we should just run a TC pass after each step?)
-- See https://github.com/hackworthltd/primer/issues/6

import Foreword hiding (hoistAccum)
import Foreword qualified

import Data.Map qualified as M
import Data.Set qualified as S
import Optics (getting, (%), _2, elemOf)
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
  LocalName (unLocalName),
  LocalNameKind (ATyVar),
  TyVarName,
  Type,
  Type' (
    TLet
  ), HasID, getID,
 )
import Primer.Core.Utils (
  _freeVarsTy,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Name (Name)
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  ExprZ,
  TypeZ,
  down,
  focus,
  focusType,
  getBoundHere,
  right,
  target,
  up, getBoundHereTy, IsZipper,
 )
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.Accum (readerToAccumT, Accum, look, add, evalAccumT, AccumT)
import Primer.Eval.Redex (Redex (InlineLet, InlineLetrec, ElideLet, RenameBindingsLam, RenameBindingsLAM, RenameSelfLet, RenameSelfLetType), RedexType (InlineLetInType, ElideLetInType, RenameSelfLetInType, RenameForall), Dir (Syn, Chk), SomeLocal (LSome), Cxt(Cxt),
                              _freeVarsLocal,
                              Local (LLetType, LLet, LLetrec), viewRedex, viewRedexType, localName)

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

-- TODO: push into find?
-- What is the direction from the context?
-- i.e. are we in the head of an elimination (or inside a hole)?
focusDir :: Dir -> ExprZ -> Dir
focusDir dirIfTop ez = case up ez of
  Nothing -> dirIfTop
  Just z -> case target z of
    App _ f _ | f == target ez -> Syn
    APP _ f _ | f == target ez -> Syn
    Case _ scrut _ | scrut == target ez -> Syn
    Hole _ _ -> Syn
    _ -> Chk

viewLet :: ExprZ -> Maybe (SomeLocal, Accum Cxt ExprZ)
viewLet ez = case (target ez, exprChildren ez) of
  (Let _ x e _b, [_,bz]) -> Just (LSome $ LLet x e, bz)
  (Letrec _ x e ty _b, [_,bz]) -> Just (LSome $ LLetrec x e ty,bz)
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (LSome $ LLetType a ty,bz)
  _ -> Nothing

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
--
-- This is unfortunately annoying to implement, because our Zipper doesn't mesh
-- well here (movements are Maybe, I know what should happen, but cannot
-- express the moves nicely...)
--
-- TODO: update docs ^
findRedex ::
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  Maybe RedexWithContext
findRedex tydefs globals dir = flip evalAccumT mempty . go . focus
  where
    focusType' :: ExprZ -> AccumT Cxt Maybe TypeZ
    -- Note that nothing in Expr binds a variable which scopes over a type child
    -- so we don't need to 'add' anything
    focusType' = lift .  focusType
    hoistAccum :: Accum Cxt a -> AccumT Cxt Maybe a
    hoistAccum = Foreword.hoistAccum generalize
    go :: ExprZ -> AccumT Cxt Maybe RedexWithContext
    go ez = do
     hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir dir ez) (target ez)) >>= \case
      Just r -> pure $ RExpr ez r
      Nothing | Just (LSome l, bz) <- viewLet ez -> goSubst l =<< hoistAccum bz
              -- Since stuck things other than lets are stuck on the first child or
              -- its type annotation, we can handle them all uniformly
              | otherwise -> msum $  (goType =<< focusType' ez)
                                  : (map (go <=< hoistAccum) $ exprChildren ez)
                --(_ >>= goType) <|> msum (map (go <=< lift) $ exprChildren ez)
    goType :: TypeZ -> AccumT Cxt Maybe RedexWithContext
    goType tz = do
     hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
      Just r -> pure $ RType tz r
      Nothing | TLet _ a t _body <- target tz
              , [_,bz] <- typeChildren tz -> goSubstTy a t =<< hoistAccum bz
              | otherwise -> msum $ map (goType <=< hoistAccum) $ typeChildren tz
    goSubst :: Local k -> ExprZ -> AccumT Cxt Maybe RedexWithContext
    goSubst l ez = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir dir ez) $ target ez) >>= \case
        -- We should inline such 'v' (note that we will not go under any 'v' binders)
        Just r@(InlineLet w e) | localName l == unLocalName w -> pure $ RExpr ez r
        Just r@(InlineLetrec w e t) | localName l == unLocalName w -> pure $ RExpr ez r
        -- Elide a let only if it blocks the reduction
        Just r@(ElideLet (LSome w) _) | elemOf _freeVarsLocal (localName w) l -> pure $ RExpr ez r
        -- Rename a binder only if it blocks the reduction
        Just r@(RenameBindingsLam _ w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameBindingsLAM _ w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
--        Redex r@(RenameBindingsCase _ e brs avoid)
--          | not $ S.disjoint (setOf freeVarsLocal l) (setOf (folded % #_CaseBranch % _2 % folded % to bindName % to unLocalName) brs) ->
--              pure $ RExpr ez r
        Just r@(RenameSelfLet w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameSelfLetType w _ _) | elemOf _freeVarsLocal (unLocalName w) l -> pure $ RExpr ez r
        -- Switch to an inner let if substituting under it would cause capture
        Nothing | Just (LSome l',bz) <- viewLet ez
                , localName l' /= localName l
                , elemOf _freeVarsLocal (localName l') l -> goSubst l' =<< hoistAccum bz
        -- We should not go under 'v' binders, but otherwise substitute in each child
        _ ->
          let substChild c = do
                guard $ S.notMember (localName l) $ getBoundHere (target ez) (Just $ target c)
                goSubst l c
              substTyChild c = case l of
                LLetType v t -> goSubstTy v t c
                _ -> mzero
          in msum @[] $ (substTyChild =<< focusType' ez) : map (substChild <=< hoistAccum) (exprChildren ez)
    goSubstTy :: TyVarName -> Type -> TypeZ -> AccumT Cxt Maybe RedexWithContext
    goSubstTy v t tz = let isFreeIn = elemOf (getting _freeVarsTy % _2)
                       in do
     hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
      -- We should inline such 'v' (note that we will not go under any 'v' binders)
      Just r@(InlineLetInType w _) | w == v -> pure $ RType tz r
      -- Elide a let only if it blocks the reduction
      Just r@(ElideLetInType (LLetType w _) _)        | w `isFreeIn` t -> pure $ RType tz r
      -- Rename a binder only if it blocks the reduction
      Just r@(RenameSelfLetInType w _ _) | w `isFreeIn` t -> pure $ RType tz r
      Just r@(RenameForall _ w _ _ _) | w `isFreeIn` t -> pure $ RType tz r
      -- We switch to an inner let if substituting under it would cause capture
      Nothing
        | TLet _ w s _ <- target tz
        , [_,bz] <- typeChildren tz
        , v /= w
        , w `isFreeIn` t -> goSubstTy w s =<< hoistAccum bz
      -- We should not go under 'v' binders, but otherwise substitute in each child
      _ ->
        let substChild c = do
              guard $ S.notMember (unLocalName v)
                $ S.map unLocalName $ getBoundHereTy (target tz) (Just $ target c)
              goSubstTy v t c
         in msum $ map (substChild <=< hoistAccum) (typeChildren tz)

children' :: IsZipper za a => za -> [za]
children' z = case down z of
      Nothing -> mempty
      Just z' -> z' : unfoldr (fmap (\x -> (x, x)) . right) z'

exprChildren :: ExprZ -> [Accum Cxt ExprZ]
exprChildren ez = children' ez <&> \c -> do
                            let bs = getBoundHere' (target ez) (Just $ target c)
                            addBinds ez bs
                            pure c

typeChildren :: TypeZ -> [Accum Cxt TypeZ]
typeChildren tz = children' tz <&> \c -> do
                            let bs = getBoundHereTy' (target tz) (Just $ target c)
                            addBinds tz bs
                            pure c


-- TODO: Yuck, is there another way?
getBoundHere' :: Expr -> Maybe Expr -> [Either Name SomeLocal]
getBoundHere' (Let _ x e1 e2) boundIn | boundIn == Just e2 = [Right $ LSome $ LLet x e1]
getBoundHere' (LetType _ a t e) boundIn = [ Right $ LSome $ LLetType a t]
getBoundHere' (Letrec _ x e1 t e2) boundIn = [ Right $ LSome $ LLetrec x e1 t]
getBoundHere' boundAt boundIn = map Left $ S.toList $ getBoundHere boundAt boundIn

getBoundHereTy' :: Type -> Maybe Type -> [Either Name SomeLocal]
getBoundHereTy' = fmap (bimap unLocalName LSome) <<$>> getBoundHereTy''
 where getBoundHereTy'' :: Type -> Maybe Type -> [Either TyVarName (Local 'ATyVar)]
       getBoundHereTy'' (TLet _ x t1 t2) boundIn | boundIn == Just t2 = [Right $ LLetType x t1]
       getBoundHereTy'' boundAt boundIn = map Left $ S.toList $ getBoundHereTy boundAt boundIn
  
addBinds :: HasID i => i -> [Either Name SomeLocal] -> Accum Cxt ()
addBinds i' bs = do
  let i = getID i'
  cxt <- look
  add $ Cxt $ M.fromList $ bs <&> \case
    Left n -> (n,(Nothing,i,cxt))
    Right ls@(LSome l) -> (localName l,(Just ls,i,cxt))
