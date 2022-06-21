module Primer.Core.Utils (
  freshLocalName,
  freshLocalName',
  exprIDs,
  typeIDs,
  generateTypeIDs,
  regenerateTypeIDs,
  forgetTypeIDs,
  generateIDs,
  regenerateExprIDs,
  forgetIDs,
  nextID,
  noHoles,
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  freeVars,
  _freeVarsTy,
  freeVarsTy,
  alphaEqTy,
  concreteTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set.Optics (setOf)
import Optics (
  Fold,
  Traversal,
  Traversal',
  adjoin,
  foldlOf',
  getting,
  hasn't,
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
  ASTDef (..),
  CaseBranch' (..),
  Def (..),
  Expr,
  Expr' (..),
  HasID (_id),
  ID,
  Kind (KHole),
  LVarName,
  LocalName (LocalName, unLocalName),
  PrimDef (..),
  TmVarRef (LocalVarRef),
  TyVarName,
  Type,
  Type' (..),
  bindName,
  trivialMeta,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Name (Name, NameCounter, freshName)

-- | Helper, wrapping 'freshName'
freshLocalName :: MonadFresh NameCounter m => S.Set (LocalName k) -> m (LocalName k)
freshLocalName = freshLocalName' . S.map unLocalName

-- | Helper, wrapping 'freshName'
freshLocalName' :: MonadFresh NameCounter m => S.Set Name -> m (LocalName k)
freshLocalName' = fmap LocalName . freshName

-- | Regenerate all IDs, not changing any other metadata
regenerateTypeIDs :: (HasID a, MonadFresh ID m) => Type' a -> m (Type' a)
regenerateTypeIDs = regenerateTypeIDs' (set _id)

regenerateTypeIDs' :: MonadFresh ID m => (ID -> a -> b) -> Type' a -> m (Type' b)
regenerateTypeIDs' s = traverseOf _typeMeta (\a -> flip s a <$> fresh)

-- | Adds 'ID's and trivial metadata
generateTypeIDs :: MonadFresh ID m => Type' () -> m Type
generateTypeIDs = regenerateTypeIDs' $ const . trivialMeta

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeIDs :: Type' a -> Type' ()
forgetTypeIDs = set _typeMeta ()

-- | Regenerate all IDs, not changing any other metadata
regenerateExprIDs :: (HasID a, HasID b, MonadFresh ID m) => Expr' a b -> m (Expr' a b)
regenerateExprIDs = regenerateExprIDs' (set _id) (set _id)

regenerateExprIDs' :: MonadFresh ID m => (ID -> a -> a') -> (ID -> b -> b') -> Expr' a b -> m (Expr' a' b')
regenerateExprIDs' se st =
  traverseOf _exprMeta (\a -> flip se a <$> fresh)
    >=> traverseOf _exprTypeMeta (\a -> flip st a <$> fresh)

-- | Like 'generateTypeIDs', but for expressions
generateIDs :: MonadFresh ID m => Expr' () () -> m Expr
generateIDs = regenerateExprIDs' (const . trivialMeta) (const . trivialMeta)

-- | Like 'forgetTypeIDs', but for expressions
forgetIDs :: Expr' a b -> Expr' () ()
forgetIDs = set _exprTypeMeta () . set _exprMeta ()

-- | Test whether an type contains any holes
-- (empty or non-empty, or inside a kind)
noHoles :: Data a => Type' a -> Bool
noHoles t = flip all (universe t) $ \case
  THole{} -> False
  TEmptyHole{} -> False
  TForall _ _ k _ -> flip all (universe k) $ \case
    KHole -> False
    _ -> True
  _ -> True

freeVarsTy :: Type' a -> Set TyVarName
freeVarsTy = setOf (getting _freeVarsTy % _2)

_freeVarsTy :: Traversal (Type' a) (Type' a) (a, TyVarName) (Type' a)
_freeVarsTy = traversalVL $ traverseFreeVarsTy mempty

-- Helper for _freeVarsTy and _freeTyVars
-- Takes a set of considered-to-be-bound variables
traverseFreeVarsTy :: Applicative f => Set TyVarName -> ((a, TyVarName) -> f (Type' a)) -> Type' a -> f (Type' a)
traverseFreeVarsTy = go
  where
    go bound f = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go bound f t
      t@TCon{} -> pure t
      TFun m s t -> TFun m <$> go bound f s <*> go bound f t
      v@(TVar m a)
        | S.member a bound -> pure v
        | otherwise -> curry f m a
      TApp m s t -> TApp m <$> go bound f s <*> go bound f t
      TForall m a k s -> TForall m a k <$> go (S.insert a bound) f s

-- Check two types for alpha equality
--
-- it makes usage easier if this is pure
-- i.e. we don't want to need a fresh name supply
-- We assume both inputs are both from the same context
alphaEqTy :: Type' () -> Type' () -> Bool
alphaEqTy = go (0, mempty, mempty)
  where
    go _ (TEmptyHole _) (TEmptyHole _) = True
    go bs (THole _ s) (THole _ t) = go bs s t
    go _ (TCon _ n) (TCon _ m) = n == m
    go bs (TFun _ a b) (TFun _ c d) = go bs a c && go bs b d
    go (_, p, q) (TVar _ n) (TVar _ m) = p ! n == q ! m
    go bs (TApp _ a b) (TApp _ c d) = go bs a c && go bs b d
    go bs (TForall _ n k s) (TForall _ m l t) = k == l && go (new bs n m) s t
    go _ _ _ = False
    p ! n = case p M.!? n of
      Nothing -> Left n -- free vars: compare by name
      Just i -> Right i -- bound vars: up to alpha
      -- Note that the maps 'p' and 'q' map names to "which forall
      -- they came from", in some sense.  The @c@ value is how many
      -- binders we have gone under, and is thus the next value free
      -- in the map.
    new (c, p, q) n m = (c + 1 :: Int, M.insert n c p, M.insert m c q)

-- Both term and type vars, but not constructors or global variables.
-- This is because constructor names and global variables are never
-- captured by lambda bindings etc (since they are looked up in a different
-- namespace)
freeVars :: Expr' a b -> Set Name
freeVars = setOf $ _freeVars % (_Left % _2 % to unLocalName `summing` _Right % _2 % to unLocalName)

-- We can't offer a traversal, as we can't enforce replacing term vars with
-- terms and type vars with types. Use _freeTmVars and _freeTyVars for
-- traversals.
_freeVars :: Fold (Expr' a b) (Either (a, LVarName) (b, TyVarName))
_freeVars = getting _freeTmVars % to Left `summing` getting _freeTyVars % to Right

_freeTmVars :: Traversal (Expr' a b) (Expr' a b) (a, LVarName) (Expr' a b)
_freeTmVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set LVarName -> ((a, LVarName) -> f (Expr' a b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> pure ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> pure ty
      t@Con{} -> pure t
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
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound . S.fromList $ fmap bindName binds) f e

_freeTyVars :: Traversal (Expr' a b) (Expr' a b) (b, TyVarName) (Type' b)
_freeTyVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set TyVarName -> ((b, TyVarName) -> f (Type' b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      t@Con{} -> pure t
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
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables

concreteTy :: Data b => Type' b -> Bool
concreteTy ty = hasn't (getting _freeVarsTy) ty && noHoles ty

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b) => Traversal' (Expr' a b) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)

-- | Traverse the 'ID's in a 'Type''.
typeIDs :: HasID a => Traversal' (Type' a) ID
typeIDs = _typeMeta % _id

-- | Given a 'Def', return its next 'ID'.
--
-- Note: do not rely on the implementation of this function, as it may
-- change in the future.
nextID :: Def -> ID
nextID (DefAST (ASTDef e t)) =
  let eid = foldlOf' exprIDs max minBound e
      tid = foldlOf' typeIDs max minBound t
   in succ $ max eid tid
nextID (DefPrim (PrimDef t)) = succ $ foldlOf' typeIDs max minBound t
{-# INLINE nextID #-}
