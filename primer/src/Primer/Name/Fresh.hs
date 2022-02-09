module Primer.Name.Fresh (
  mkFreshName,
  mkAvoidForFreshName,
  mkFreshNameTy,
  mkAvoidForFreshNameTy,
  isFresh,
  isFreshTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Set as S
import Primer.Core (Expr, Type)
import Primer.Core.Utils (freeVars, freeVarsTy)
import Primer.Name (Name, NameCounter, freshName)
import qualified Primer.Typecheck as TC
import Primer.Zipper (
  ExprZ,
  TypeZ,
  bindersAbove,
  bindersAboveTypeZ,
  bindersBelow,
  bindersBelowTy,
  focusOnlyType,
 )

-- Check that a name is fresh for an expression. I.e. it does not
-- occur as a free variables, and thus binding it will not capture
-- a variable.
-- However, it may shadow a binding in more global scope that happens not to
-- be used in the expression, or a binding in the expression may shadow the
-- name.
isFresh :: Name -> Expr -> Bool
isFresh v e = v `S.notMember` freeVars e

isFreshTy :: Name -> Type -> Bool
isFreshTy v t = v `S.notMember` freeVarsTy t

-- We make a fresh name that is appropriate for binding here (i.e. wrapping the
-- target of the zipper).
-- To avoid variable capture we must avoid any name free in the focussed expr;
-- this is important for correctness.
-- To avoid shadowing any other variable we should avoid any more-globally bound
-- name (i.e. "up" in the zipper); this is not a correctness concern, but a
-- usability concern: we don't want automatically generated names inducing
-- shadowing.
-- To avoid being shadowed we should avoid any names bound in the focussed
-- expr; this is also a usability concern only.
--
-- NB: the free names of the target are a subset of the more-globally bound
-- names, so we don't need to explicitly worry about them.
--
-- Because of implementation details, locally bound variables are in a
-- different namespace than top-level definitions and from term/type
-- constructors. However, for the sake of non-confusingness, we don't care
-- about that here. Thus when we avoid more-globally bound names, we will also
-- include globally-scoped things.
mkFreshName :: (MonadFresh NameCounter m, MonadReader TC.Cxt m) => ExprZ -> m Name
mkFreshName e = freshName =<< mkAvoidForFreshName e

mkAvoidForFreshNameTy :: MonadReader TC.Cxt m => TypeZ -> m (S.Set Name)
mkAvoidForFreshNameTy t = do
  let moreGlobal = bindersAboveTypeZ t
      moreLocal = bindersBelowTy $ focusOnlyType t
  globals <- TC.getGlobalNames
  pure $ S.unions [moreGlobal, moreLocal, globals]

mkFreshNameTy :: (MonadFresh NameCounter m, MonadReader TC.Cxt m) => TypeZ -> m Name
mkFreshNameTy t = freshName =<< mkAvoidForFreshNameTy t

mkAvoidForFreshName :: MonadReader TC.Cxt m => ExprZ -> m (S.Set Name)
mkAvoidForFreshName e = do
  let moreGlobal = bindersAbove e
      moreLocal = bindersBelow e
  globals <- TC.getGlobalNames
  pure $ S.unions [moreGlobal, moreLocal, globals]
