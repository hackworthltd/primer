module Primer.Test.Expr (
  mapEven,
) where

import Foreword

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
  create,
  gvar,
  tapp,
  tcon,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Examples qualified as Examples

mapEven :: Int -> (DefMap, Expr, Expr, ID)
mapEven n =
  let modName = mkSimpleModuleName "TestModule"
      ((globals, e, expected), maxID) = create $ do
        (mapName, mapDef) <- Examples.map modName
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let lst = list_ tNat $ take n $ iterate (con cSucc `app`) (con cZero)
        expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
        let globs = M.fromList [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
        expect <- list_ tBool (take n $ cycle [con cTrue, con cFalse]) `ann` (tcon tList `tapp` tcon tBool)
        pure (globs, expr, expect)
   in (globals, e, expected, maxID)
