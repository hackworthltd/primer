-- | Helpers for building expressions involving built-in types.
module Primer.Builtins.DSL (
  bool_,
  boolAnn,
  nat,
  maybe_,
  maybeAnn,
  list_,
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
  tBool,
  tList,
  tMaybe,
 )
import Primer.Core (
  Expr,
  ID,
  TyConName,
  Type,
 )
import Primer.Core.DSL (
  ann,
  con,
  con0,
  con1,
  tapp,
  tcon,
 )

-- These functions rely on particular types being in scope.
bool_ :: MonadFresh ID m => Bool -> m Expr
bool_ b = con0 (if b then cTrue else cFalse)

boolAnn :: MonadFresh ID m => Bool -> m Expr
boolAnn b = bool_ b `ann` tcon tBool

nat :: MonadFresh ID m => Natural -> m Expr
nat = \case
  0 -> con0 cZero
  n -> con1 cSucc $ nat (n - 1)

maybe_ :: MonadFresh ID m => m Type -> (a -> m Expr) -> Maybe a -> m Expr
maybe_ t f = \case
  Nothing -> con cNothing [t] []
  Just x -> con cJust [t] [f x]

maybeAnn :: MonadFresh ID m => m Type -> (a -> m Expr) -> Maybe a -> m Expr
maybeAnn t f x = maybe_ t f x `ann` (tcon tMaybe `tapp` t)

list_ :: MonadFresh ID m => TyConName -> [m Expr] -> m Expr
list_ t =
  foldr
    ( \a b ->
        con cCons [tcon t] [a, b]
    )
    (con cNil [tcon t] [])

listOf :: MonadFresh ID m => m Type -> m Type
listOf = tapp (tcon tList)
