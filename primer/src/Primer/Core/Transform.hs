module Primer.Core.Transform (
  renameVar,
  renameTyVar,
  renameTyVarExpr,
  unfoldApp,
  unfoldAPP,
  removeAnn,
) where

import Data.Bifunctor (second)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Primer.Core (CaseBranch' (..), Expr' (..), Type' (..), bindName)
import Primer.Name (Name)

-- AST transformations.
-- This module contains global transformations on expressions and types, in
-- contrast to the focused, local transformations provided by the zipper.

-- | Attempt to replace all free ocurrences of @x@ in @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameVar :: (Data a, Data b) => Name -> Name -> Expr' a b -> Maybe (Expr' a b)
renameVar x y = \case
  Lam m v e
    | v == x -> pure $ Lam m v e
    | v == y -> Nothing
    | otherwise -> Lam m v <$> renameVar x y e
  Let m v e1 e2
    | v == x -> pure $ Let m v e1 e2
    | v == y -> Nothing
    | otherwise -> Let m v <$> renameVar x y e1 <*> renameVar x y e2
  Case m scrut branches -> Case m <$> renameVar x y scrut <*> mapM renameBranch branches
    where
      renameBranch b@(CaseBranch con termargs rhs)
        | x `elem` bindingNames b = pure b
        | y `elem` bindingNames b = Nothing
        | otherwise = CaseBranch con termargs <$> renameVar x y rhs
      bindingNames (CaseBranch _ bs _) = map bindName bs
  Var m v
    | v == x -> pure $ Var m y
    | v == y -> Nothing
    | otherwise -> pure $ Var m v
  e -> descendM (renameVar x y) e

-- | Attempt to replace all free ocurrences of @x@ in @t@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVar :: Data a => Name -> Name -> Type' a -> Maybe (Type' a)
-- We cannot use substTy to implement renaming, as that restricts to b~(), so as to not
-- duplicate metadata. But for renaming, we know that will not happen.
renameTyVar x y = \case
  TForall m v k t
    | v == x -> pure $ TForall m v k t
    | v == y -> Nothing
    | otherwise -> TForall m v k <$> renameTyVar x y t
  TVar m v
    | v == x -> pure $ TVar m y
    | v == y -> Nothing
    | otherwise -> pure $ TVar m v
  t -> descendM (renameTyVar x y) t

-- | Attempt to replace all free ocurrences of @x@ in some type inside @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVarExpr :: (Data a, Data b) => Name -> Name -> Expr' a b -> Maybe (Expr' a b)
renameTyVarExpr x y = \case
  LAM m v e
    | v == x -> pure $ LAM m v e
    | v == y -> Nothing
    | otherwise -> LAM m v <$> renameTyVarExpr x y e
  Ann m e t -> Ann m e <$> renameTyVar x y t
  APP m e t -> APP m e <$> renameTyVar x y t
  e -> descendM (renameTyVarExpr x y) e

-- | Unfold a nested term application into the application head and a list of arguments.
unfoldApp :: Expr' a b -> (Expr' a b, [Expr' a b])
unfoldApp = second reverse . go
  where
    go (App _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Unfold a nested term-level type application into the application head and a list of arguments.
unfoldAPP :: Expr' a b -> (Expr' a b, [Type' b])
unfoldAPP = second reverse . go
  where
    go (APP _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Remove any outer annotations from an expression
removeAnn :: Expr' a b -> Expr' a b
removeAnn (Ann _ e _) = removeAnn e
removeAnn e = e
