-- | Helpers for building expressions involving built-in types.
module Primer.Builtins.DSL (
  bool_,
  nat,
  maybe_,
  list_,
  listSat_,
  listOf,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Numeric.Natural (Natural)
import Primer.Builtins (
  cCons,
  cFalse,
  cJust,
  cNil,
  cNothing,
  cSucc,
  cTrue,
  cZero,
  tList,
 )
import Primer.Core (
  Expr,
  ID,
  TyConName,
  Type,
 )
import Primer.Core.DSL (
  aPP,
  app,
  con,
  conSat,
  tapp,
  tcon,
 )

-- These functions rely on particular types being in scope.
bool_ :: MonadFresh ID m => Bool -> m Expr
bool_ b = con $ if b then cTrue else cFalse

nat :: MonadFresh ID m => Natural -> m Expr
nat = \case
  0 -> con cZero
  n -> app (con cSucc) $ nat (n - 1)

maybe_ :: MonadFresh ID m => m Type -> (a -> m Expr) -> Maybe a -> m Expr
maybe_ t f = \case
  Nothing -> con cNothing `aPP` t
  Just x -> con cJust `aPP` t `app` f x

list_ :: MonadFresh ID m => TyConName -> [m Expr] -> m Expr
list_ t =
  foldr
    ( \a b ->
        con cCons
          `aPP` tcon t
          `app` a
          `app` b
    )
    (con cNil `aPP` tcon t)

listSat_ :: MonadFresh ID m => TyConName -> [m Expr] -> m Expr
listSat_ t =
  foldr
    ( \a b ->
        conSat cCons [tcon t] [a,b]
    )
    (conSat cNil [tcon t] [])

listOf :: MonadFresh ID m => m Type -> m Type
listOf = tapp (tcon tList)
