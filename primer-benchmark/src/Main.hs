{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Criterion.Main (
  bench,
  bgroup,
  defaultMain,
  env,
  nf,
 )
import Primer.EvalFull (
  Dir (Syn),
  EvalLog,
  evalFull,
 )
import Primer.Log (
  runPureLogT,
 )
import Primer.Module (
  builtinTypes,
 )
import Primer.Test.Expected (
  Expected (defMap, expr, maxID),
  mapEven,
 )
import Primer.Test.TestM (
  evalTestM,
 )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "evalTestM"
        [ bgroup
            "pure logs"
            [ env (mapEvenEnv 1) $ \e -> bench "mapEven 1" $ nf (evalTestMPureLogs e) 100
            , env (mapEvenEnv 10) $ \e -> bench "mapEven 10" $ nf (evalTestMPureLogs e) 1000
            , env (mapEvenEnv 100) $ \e -> bench "mapEven 100" $ nf (evalTestMPureLogs e) 10000
            ]
        ]
    ]
  where
    evalTestMPureLogs e maxEvals =
      fst
        <$> evalTestM (maxID e)
        $ runPureLogT
        $ evalFull @EvalLog builtinTypes (defMap e) maxEvals Syn (expr e)
    mapEvenEnv n = pure $ mapEven n
