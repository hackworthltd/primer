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

maybe_ :: MonadFresh ID m => (a -> m Expr) -> Maybe a -> m Expr
maybe_ f = \case
  Nothing -> con cNothing []
  Just x -> con cJust [f x]

maybeAnn :: MonadFresh ID m => m Type -> (a -> m Expr) -> Maybe a -> m Expr
maybeAnn t f x = maybe_ f x `ann` (tcon tMaybe `tapp` t)

list_ :: MonadFresh ID m => [m Expr] -> m Expr
list_ =
  foldr
    ( \a b ->
        con cCons [a, b]
    )
    (con cNil [])

listOf :: MonadFresh ID m => m Type -> m Type
listOf = tapp (tcon tList)
