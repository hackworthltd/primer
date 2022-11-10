{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Criterion.Main (
  bench,
  bgroup,
  defaultMain,
  nfIO,
 )
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
import Primer.EvalFull
import Primer.Examples qualified as Examples
import Primer.Test.Util

mapEven :: TerminationBound -> Int -> IO (Either EvalFullError Expr)
mapEven maxEvals n =
  let modName = mkSimpleModuleName "TestModule"
      ((globals, e), maxID) = create $ do
        (mapName, mapDef) <- Examples.map modName
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let lst = list_ tNat $ take n $ iterate (con cSucc `app`) (con cZero)
        expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
        let globs = [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
        pure (globs, expr)
   in evalFullTest maxID builtinTypes (M.fromList globals) maxEvals Syn e

main :: IO ()
main =
  defaultMain
    [ bgroup
        "EvalFull"
        [ bench "mapEven 100 1" $ nfIO $ mapEven 100 1
        , bench "mapEven 1000 10" $ nfIO $ mapEven 1000 10
        , bench "mapEven 10000 100" $ nfIO $ mapEven 10000 100
        ]
    ]
