module Primer.Subst (
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  freeVars,
  _freeVarsTy,
  freeVarsTy,
  alphaEqTy,
  substTy,
  substTys,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set.Optics (setOf)
import Optics (Fold, Traversal, getting, summing, to, traversalVL, (%), _2)
import Primer.Core (CaseBranch' (..), Expr' (..), Type' (..), bindName)
import Primer.Name (Name, NameCounter, freshName)

-- | Simple and inefficient capture-avoiding substitution.
-- @substTy n a t@  is t[a/n]
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTy :: MonadFresh NameCounter m => Name -> Type' () -> Type' () -> m (Type' ())
substTy n a = go
  where
    avoid = freeVarsTy a
    go = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go t
      t@TCon{} -> pure t
      TFun _ s t -> TFun () <$> go s <*> go t
      t@(TVar _ m)
        | n == m -> pure a
        | otherwise -> pure t
      TApp _ s t -> TApp () <$> go s <*> go t
      t@(TForall _ m k s)
        | m == n -> pure t
        -- these names will not enter the user's program, so we don't need to worry about shadowing, only variable capture
        | m `elem` avoid -> freshName (avoid <> freeVarsTy s) >>= \m' -> substTy m (TVar () m') s >>= fmap (TForall () m' k) . go
        | otherwise -> TForall () m k <$> go s

substTys :: MonadFresh NameCounter m => [(Name, Type' ())] -> Type' () -> m (Type' ())
substTys sb t = foldrM (uncurry substTy) t sb

freeVarsTy :: Type' a -> Set Name
freeVarsTy = setOf (getting _freeVarsTy % _2)

_freeVarsTy :: Traversal (Type' a) (Type' a) (a, Name) (Type' a)
_freeVarsTy = traversalVL $ traverseFreeVarsTy mempty

-- Helper for _freeVarsTy and _freeTyVars
-- Takes a set of considered-to-be-bound variables
traverseFreeVarsTy :: Applicative f => Set Name -> ((a, Name) -> f (Type' a)) -> Type' a -> f (Type' a)
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
alphaEqTy = go mempty mempty
  where
    go _ _ (TEmptyHole _) (TEmptyHole _) = True
    go p q (THole _ s) (THole _ t) = go p q s t
    go _ _ (TCon _ n) (TCon _ m) = n == m
    go p q (TFun _ a b) (TFun _ c d) = go p q a c && go p q b d
    go p q (TVar _ n) (TVar _ m) = p ! n == q ! m
    go p q (TApp _ a b) (TApp _ c d) = go p q a c && go p q b d
    go p q (TForall _ n k s) (TForall _ m l t) = k == l && go (new p n) (new q m) s t
    go _ _ _ _ = False
    p ! n = case p M.!? n of
      Nothing -> Left n -- free vars: compare by name
      Just i -> Right i -- bound vars: up to alpha
      -- Note that the maps 'p' and 'q' in 'go' are always the same size, and map
      -- names to "which forall they came from", in some sense.
      -- We just require 'new p n' and 'new q m' to associate "the same" value to
      -- the new key.
      -- Instead of threading a "how many binders seen" parameter, we use this
      -- trick: values are an initial segment of the natural numbers, so the next
      -- value to use is just the size of the map
    new p n = M.insert n (M.size p) p

-- Both term and type vars, but not constructors or global variables.
-- This is because constructor names and global variables are never
-- captured by lambda bindings etc (since they are looked up in a different
-- namespace)
freeVars :: Expr' a b -> Set Name
freeVars = setOf $ _freeVars % _2

-- We can't offer a traversal, as we can't enforce replacing term vars with
-- terms and type vars with types. Use _freeTmVars and _freeTyVars for
-- traversals.
_freeVars :: Fold (Expr' a b) (Either a b, Name)
_freeVars = getting _freeTmVars % to (first Left) `summing` getting _freeTyVars % to (first Right)

_freeTmVars :: Traversal (Expr' a b) (Expr' a b) (a, Name) (Expr' a b)
_freeTmVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set Name -> ((a, Name) -> f (Expr' a b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> pure ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> pure ty
      t@Con{} -> pure t
      Lam m v e -> Lam m v <$> go (S.insert v bound) f e
      LAM m tv e -> LAM m tv <$> go bound f e
      t@(Var m v)
        | S.member v bound -> pure t
        | otherwise -> curry f m v
      t@GlobalVar{} -> pure t
      Let m v e b -> Let m v <$> go bound f e <*> go (S.insert v bound) f b
      Letrec m v e t b -> Letrec m v <$> go (S.insert v bound) f e <*> pure t <*> go (S.insert v bound) f b
      LetType m v ty e -> LetType m v ty <$> go bound f e
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go (S.union bound $ S.fromList $ map bindName binds) f e

_freeTyVars :: Traversal (Expr' a b) (Expr' a b) (b, Name) (Type' b)
_freeTyVars = traversalVL $ go mempty
  where
    go :: Applicative f => Set Name -> ((b, Name) -> f (Type' b)) -> Expr' a b -> f (Expr' a b)
    go bound f = \case
      Hole m e -> Hole m <$> go bound f e
      t@EmptyHole{} -> pure t
      Ann m e ty -> Ann m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      App m e s -> App m <$> go bound f e <*> go bound f s
      APP m e ty -> APP m <$> go bound f e <*> traverseFreeVarsTy bound f ty
      t@Con{} -> pure t
      Lam m v e -> Lam m v <$> go bound f e
      LAM m tv e -> LAM m tv <$> go (S.insert tv bound) f e
      t@Var{} -> pure t
      t@GlobalVar{} -> pure t
      Let m v e b -> Let m v <$> go bound f e <*> go bound f b
      Letrec m v e ty b -> Letrec m v <$> go bound f e <*> traverseFreeVarsTy bound f ty <*> go bound f b
      LetType m v ty e -> LetType m v <$> traverseFreeVarsTy bound f ty <*> go (S.insert v bound) f e
      Case m e bs -> Case m <$> go bound f e <*> traverse freeVarsBr bs
      where
        freeVarsBr (CaseBranch c binds e) = CaseBranch c binds <$> go bound f e -- case branches only bind term variables
