{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Foreword

import Control.Monad.Log (
  Severity (..),
  WithSeverity (..),
 )
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
  runDiscardLogT,
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

-- Orphans for 'NFData' instances.
deriving instance Generic (WithSeverity a)
deriving instance Generic Severity
deriving instance NFData Severity
instance (NFData a) => NFData (WithSeverity a)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "evalTestM"
        [ bgroup
            "pure logs"
            [ env (mapEvenEnv 1) $ \e -> bench "mapEven 1" $ nf (evalTestMPureLogs e) 100
            , env (mapEvenEnv 10) $ \e -> bench "mapEven 10" $ nf (evalTestMPureLogs e) 1000
            -- This benchmark is too slow to be practical for CI.
            -- , env (mapEvenEnv 100) $ \e -> bench "mapEven 100" $ nf (evalTestMPureLogs e) 10000
            ]
        , bgroup
            "discard logs"
            [ env (mapEvenEnv 1) $ \e -> bench "mapEven 1" $ nf (evalTestMDiscardLogs e) 100
            , env (mapEvenEnv 10) $ \e -> bench "mapEven 10" $ nf (evalTestMDiscardLogs e) 1000
            -- This benchmark is too slow to be practical for CI.
            -- , env (mapEvenEnv 100) $ \e -> bench "mapEven 100" $ nf (evalTestMDiscardLogs e) 10000
            ]
        ]
    ]
  where
    evalTestMPureLogs e maxEvals =
      evalTestM (maxID e) $
        runPureLogT $
          evalFull @EvalLog builtinTypes (defMap e) maxEvals Syn (expr e)
    evalTestMDiscardLogs e maxEvals =
      evalTestM (maxID e) $
        runDiscardLogT $
          evalFull @EvalLog builtinTypes (defMap e) maxEvals Syn (expr e)
    mapEvenEnv n = pure $ mapEven n
