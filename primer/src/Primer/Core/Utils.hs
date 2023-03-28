module Primer.Core.Utils (
  freshLocalName,
  freshLocalName',
  exprIDs,
  typeIDs,
  generateTypeIDs,
  regenerateTypeIDs,
  forgetTypeMetadata,
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
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Data.Set qualified as S
import Data.Set.Optics (setOf)
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
  Expr,
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  LVarName,
  LocalName (unLocalName),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyVarName,
  Type' (..),
  bindName,
  trivialMeta,
  _exprMeta,
  _exprTypeMeta,
 )
import Primer.Core.Fresh (freshLocalName, freshLocalName')
import Primer.Core.Type.Utils (
  alphaEqTy,
  boundVarsTy,
  concreteTy,
  forgetTypeMetadata,
  freeVarsTy,
  generateTypeIDs,
  noHoles,
  regenerateTypeIDs,
  traverseFreeVarsTy,
  typeIDs,
  _freeVarsTy,
 )
import Primer.Name (Name)

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

-- | Like 'forgetTypeMetadata', but for expressions
forgetMetadata :: Expr' a b -> Expr' () ()
forgetMetadata = set _exprTypeMeta () . set _exprMeta ()

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
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound $ S.fromList $ map bindName binds) f e

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
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      t@PrimCon{} -> pure t
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables

freeGlobalVars :: (Data a, Data b) => Expr' a b -> Set GVarName
freeGlobalVars e = S.fromList [v | Var _ (GlobalVarRef v) <- universe e]

-- | Traverse the 'ID's in an 'Expr''.
exprIDs :: (HasID a, HasID b) => Traversal' (Expr' a b) ID
exprIDs = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)
