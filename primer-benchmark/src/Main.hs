{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Criterion.Main (
  bench,
  bgroup,
  defaultMain,
  env,
  nfIO,
 )
import Primer.Eval (Dir (Syn))
import Primer.Test.Expr (
  mapEven,
 )
import Primer.Test.Util

main :: IO ()
main =
  defaultMain
    [ bgroup
        "EvalFull"
        [ env (mapEvenEnv 1) $ \ ~(glob, expr, _, maxID) -> bench "mapEven 1" $ nfIO $ evalFull maxID glob 10 expr
        , env (mapEvenEnv 10) $ \ ~(glob, expr, _, maxID) -> bench "mapEven 10" $ nfIO $ evalFull maxID glob 100 expr
        , env (mapEvenEnv 100) $ \ ~(glob, expr, _, maxID) -> bench "mapEven 100" $ nfIO $ evalFull maxID glob 1000 expr
        ]
    ]
  where
    evalFull maxID globals maxEvals = evalFullTest maxID builtinTypes globals maxEvals Syn
    mapEvenEnv n = pure $ mapEven n
