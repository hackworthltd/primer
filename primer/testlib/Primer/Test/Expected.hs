module Primer.Test.Expected (
  Expected (..),
  mapEven,
) where

import Foreword

import Data.Data (Data)
import Data.Map qualified as M
import Primer.Builtins (
  cFalse,
  cSucc,
  cTrue,
  cZero,
  tBool,
  tList,
  tNat,
 )
import Primer.Builtins.DSL (
  list_,
 )
import Primer.Core (
  Expr,
  ID,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  con,
  con,
  create,
  gvar,
  tapp,
  tcon, con0,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Examples qualified as Examples

data Expected = Expected
  { defMap :: DefMap
  , expr :: Expr
  , maxID :: ID
  , expectedResult :: Expr
  }
  deriving stock (Eq, Show, Data, Generic)
  deriving anyclass (NFData)

-- | Implements @map even [0, 1, ..., n]@ in Primer, where each @n@ is
-- a @Nat@. Normal-order evaluation of the Primer expression is O(n^2)
-- because @even@ is defined recursively on the @Nat@s.
mapEven :: Int -> Expected
mapEven n =
  let modName = mkSimpleModuleName "TestModule"
      ((globals, e, expected), id) = create $ do
        (mapName, mapDef) <- Examples.map modName
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let lst = list_ tNat $ take n $ iterate (con cSucc [] . (: [])) (con0 cZero)
        expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
        let globs = M.fromList [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
        expect <- list_ tBool (take n $ cycle [con0 cTrue, con0 cFalse]) `ann` (tcon tList `tapp` tcon tBool)
        pure (globs, expr, expect)
   in Expected globals e id expected
