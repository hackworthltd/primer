{-# LANGUAGE ViewPatterns #-}

module Primer.Core.Utils (
  freshLocalName,
  freshLocalName',
  exprIDs,
  typeIDs,
  generateTypeIDs,
  regenerateTypeIDs,
  regenerateKindIDs,
  generateKindIDs,
  forgetTypeMetadata,
  forgetKindMetadata,
  generateIDs,
  regenerateExprIDs,
  forgetMetadata,
  noHoles,
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  freeVars,
  _freeVarsTy,
  freeVarsTy,
  boundVarsTy,
  freeGlobalVars,
  alphaEqTy,
  concreteTy,
  alphaEq,
  freshen,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (firstM)
import Optics (
  Fold,
  Traversal,
  Traversal',
  adjoin,
  getting,
  set,
  summing,
  to,
  traversalVL,
  traverseOf,
  (%),
  _2,
  _Left,
  _Right,
 )

import Primer.Core (
  CaseBranch' (..),
  CaseFallback' (CaseExhaustive, CaseFallback),
  Expr,
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  LVarName,
  LocalName (LocalName, unLocalName),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyVarName,
  Type' (..),
  bindName,
  traverseFallback,
  trivialMeta,
  trivialMetaUnit,
  _exprKindMeta,
  _exprMeta,
  _exprTypeMeta,
 )
import Primer.Core.Fresh (freshLocalName, freshLocalName')
import Primer.Core.Type.Utils (
  alphaEqTy,
  alphaEqTy',
  boundVarsTy,
  concreteTy,
  forgetKindMetadata,
  forgetTypeMetadata,
  freeVarsTy,
  generateKindIDs,
  generateTypeIDs,
  noHoles,
  regenerateKindIDs,
  regenerateTypeIDs,
  traverseFreeVarsTy,
  typeIDs,
  _freeVarsTy,
 )
import Primer.Name (Name (unName), unsafeMkName)

-- | Regenerate all IDs (including in types and kinds), not changing any other metadata
regenerateExprIDs :: (HasID a, HasID b, HasID c, MonadFresh ID m) => Expr' a b c -> m (Expr' a b c)
regenerateExprIDs = regenerateExprIDs' (set _id) (set _id) (set _id)

regenerateExprIDs' ::
  MonadFresh ID m =>
  (ID -> a -> a') ->
  (ID -> b -> b') ->
  (ID -> c -> c') ->
  Expr' a b c ->
  m (Expr' a' b' c')
regenerateExprIDs' se st sk =
  traverseOf _exprMeta (\a -> flip se a <$> fresh)
    >=> traverseOf _exprTypeMeta (\a -> flip st a <$> fresh)
    >=> traverseOf _exprKindMeta (\a -> flip sk a <$> fresh)

-- | Like 'generateTypeIDs', but for expressions
generateIDs :: MonadFresh ID m => Expr' () () () -> m Expr
generateIDs = regenerateExprIDs' (const . trivialMeta) (const . trivialMeta) (const . trivialMetaUnit)

-- | Like 'forgetTypeMetadata', but for expressions
forgetMetadata :: Expr' a b c -> Expr' () () ()
forgetMetadata = set _exprKindMeta () . set _exprTypeMeta () . set _exprMeta ()

-- Both term and type vars, but not constructors or global variables.
-- This is because constructor names and global variables are never
-- captured by lambda bindings etc (since they are looked up in a different
-- namespace)
freeVars :: Expr' a b c -> Set Name
freeVars = setOf $ _freeVars % (_Left % _2 % to unLocalName `summing` _Right % _2 % to unLocalName)

-- We can't offer a traversal, as we can't enforce replacing term vars with
-- terms and type vars with types. Use _freeTmVars and _freeTyVars for
-- traversals.
_freeVars :: Fold (Expr' a b c) (Either (a, LVarName) (b, TyVarName))
_freeVars = getting _freeTmVars % to Left `summing` getting _freeTyVars % to Right

_freeTmVars :: Traversal (Expr' a b c) (Expr' a b c) (a, LVarName) (Expr' a b c)
_freeTmVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set LVarName -> ((a, LVarName) -> f (Expr' a b c)) -> Expr' a b c -> f (Expr' a b c)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> pure ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> pure ty
      Con m c tms -> Con m c <$> traverse (go bound f) tms
      Lam m v e -> Lam m v <$> go (S.insert v bound) f e
      LAM m tv e ->
        -- A well scoped term will not refer to tv as a term
        -- variable, so we do not need to add it to the bound set
        LAM m tv <$> go bound f e
      t@(Var m v)
        | LocalVarRef n <- v
        , not $ S.member n bound ->
            curry f m n
        | otherwise -> pure t
      Let m v e b -> Let m v <$> go bound f e <*> go (S.insert v bound) f b
      Letrec m v e t b -> Letrec m v <$> go (S.insert v bound) f e <*> pure t <*> go (S.insert v bound) f b
      LetType m tv ty e ->
        -- A well scoped term will not refer to tv as a term
        -- variable, so we do not need to add it to the bound set
        LetType m tv ty <$> go bound f e
      Case m e bs fb -> Case m <$> go bound f e <*> traverse freeVarsBr bs <*> traverseFallback (go bound f) fb
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound $ S.fromList $ map bindName binds) f e

_freeTyVars :: Traversal (Expr' a b c) (Expr' a b c) (b, TyVarName) (Type' b c)
_freeTyVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set TyVarName -> ((b, TyVarName) -> f (Type' b c)) -> Expr' a b c -> f (Expr' a b c)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      Con m c tms -> Con m c <$> traverse (go bound f) tms
      Lam m v e ->
        -- A well scoped term will not refer to v as a type
        -- variable, so we do not need to add it to the bound set
        Lam m v <$> go bound f e
      LAM m tv e -> LAM m tv <$> go (S.insert tv bound) f e
      t@Var{} -> pure t -- These are always term variables, so not a target
      Let m v e b ->
        -- A well scoped term will not refer to v as a type
        -- variable, so we do not need to add it to the bound set
        Let m v <$> go bound f e <*> go bound f b
      Letrec m v e ty b ->
        -- A well scoped term will not refer to v as a type
        -- variable, so we do not need to add it to the bound set
        Letrec m v <$> go bound f e <*> traverseFreeVarsTy bound f ty <*> go bound f b
      LetType m v ty e -> LetType m v <$> traverseFreeVarsTy bound f ty <*> go (S.insert v bound) f e
      Case m e bs fb -> Case m <$> go bound f e <*> traverse freeVarsBr bs <*> traverseFallback (go bound f) fb
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables

freeGlobalVars :: (Data a, Data b, Data c) => Expr' a b c -> Set GVarName
freeGlobalVars e = S.fromList [v | Var _ (GlobalVarRef v) <- universe e]

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b, HasID c) => Traversal' (Expr' a b c) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id) `adjoin` (_exprKindMeta % _id)

-- Check two terms for alpha equality
--
-- it makes usage easier if this is pure
-- i.e. we don't want to need a fresh name supply
-- We assume both inputs are both from the same context
--
-- Note that we do not expand let bindings, they must be structurally
-- the same (perhaps with a different named binding)
alphaEq :: Expr' () () () -> Expr' () () () -> Bool
alphaEq = go (0, mempty, mempty)
  where
    go bs (Hole _ t1) (Hole _ t2) = go bs t1 t2
    go _ (EmptyHole _) (EmptyHole _) = True
    go bs (Ann _ t1 ty1) (Ann _ t2 ty2) = go bs t1 t2 && alphaEqTy' (extractTypeEnv bs) ty1 ty2
    go bs (App _ f1 t1) (App _ f2 t2) = go bs f1 f2 && go bs t1 t2
    go bs (APP _ e1 ty1) (APP _ e2 ty2) = go bs e1 e2 && alphaEqTy' (extractTypeEnv bs) ty1 ty2
    go bs (Con _ c1 as1) (Con _ c2 as2) = c1 == c2 && length as1 == length as2 && and (zipWith (go bs) as1 as2)
    go bs (Lam _ v1 t1) (Lam _ v2 t2) = go (newTm bs v1 v2) t1 t2
    go bs (LAM _ v1 t1) (LAM _ v2 t2) = go (newTy bs v1 v2) t1 t2
    go (_, bs1, bs2) (Var _ (LocalVarRef v1)) (Var _ (LocalVarRef v2)) = bs1 ! Left v1 == bs2 ! Left v2
    go _ (Var _ (GlobalVarRef v1)) (Var _ (GlobalVarRef v2)) = v1 == v2
    go bs (Let _ v1 s1 t1) (Let _ v2 s2 t2) = go bs s1 s2 && go (newTm bs v1 v2) t1 t2
    go bs (LetType _ v1 ty1 t1) (LetType _ v2 ty2 t2) = alphaEqTy' (extractTypeEnv bs) ty1 ty2 && go (newTy bs v1 v2) t1 t2
    go bs (Letrec _ v1 t1 ty1 e1) (Letrec _ v2 t2 ty2 e2) =
      go (newTm bs v1 v2) t1 t2
        && alphaEqTy' (extractTypeEnv bs) ty1 ty2
        && go (newTm bs v1 v2) e1 e2
    go bs (Case _ e1 brs1 fb1) (Case _ e2 brs2 fb2) =
      go bs e1 e2
        && and
          ( zipWith
              ( \(CaseBranch c1 (fmap bindName -> vs1) t1)
                 (CaseBranch c2 (fmap bindName -> vs2) t2) ->
                    c1 == c2
                      && length vs1 == length vs2
                      && go (foldl' (uncurry . newTm) bs $ zip vs1 vs2) t1 t2
              )
              brs1
              brs2
          )
        && case (fb1, fb2) of
          (CaseExhaustive, CaseExhaustive) -> True
          (CaseFallback f1, CaseFallback f2) -> go bs f1 f2
          _ -> False
    go _ (PrimCon _ c1) (PrimCon _ c2) = c1 == c2
    go _ _ _ = False
    p ! n = case p M.!? n of
      Nothing -> Left n -- free vars: compare by name
      Just i -> Right i -- bound vars: up to alpha
      -- Note that the maps 'p' and 'q' map names to "which forall
      -- they came from", in some sense.  The @c@ value is how many
      -- binders we have gone under, and is thus the next value free
      -- in the map.
    new (c, bs1, bs2) n m = (c + 1 :: Int, M.insert n c bs1, M.insert m c bs2)
    newTm bs v1 v2 = new bs (Left v1) (Left v2)
    newTy bs v1 v2 = new bs (Right v1) (Right v2)
    extractTypeEnv (c, bs1, bs2) = let f = M.fromList . mapMaybe (firstM rightToMaybe) . M.assocs in (c, f bs1, f bs2)

freshen :: Set Name -> LocalName k -> LocalName k
freshen fvs n = go (0 :: Int)
  where
    go i =
      let suffix = if i > 0 then "_" <> show i else ""
          m = LocalName $ unsafeMkName $ unName (unLocalName n) <> suffix
       in if unLocalName m `elem` fvs
            then go (i + 1)
            else m
