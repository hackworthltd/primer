{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Benchmarks (benchmarks, runBenchmarks, runTests) where

import Foreword

import Control.Monad.Log (
  Severity (..),
  WithSeverity (..),
 )
import Criterion (
  bench,
  bgroup,
  env,
  nf,
 )
import Criterion qualified as C
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
  Expected (defMap, expectedResult, expr, maxID),
  mapEven,
 )
import Primer.Test.TestM (
  evalTestM,
 )
import Primer.Test.Util (zeroIDs)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

-- Orphans for 'NFData' instances.
deriving instance Generic (WithSeverity a)
deriving instance Generic Severity
deriving instance NFData Severity
instance (NFData a) => NFData (WithSeverity a)

-- | An abstraction over @criterion@ and @tasty@: a tree whose nodes are
-- both a benchmark, and a test on the return value of the benchmarked action.
data Benchmark where
  -- Because of the differing signatures of 'env' and 'withResource',
  -- it is difficult to split this into two constructors @Bench@ and @Env@
  EnvBench :: NFData env => IO env -> Text -> (env -> Benchmarkable) -> Benchmark
  Group :: Text -> [Benchmark] -> Benchmark

-- | The constructors of @Benchmarkable@ correspond to (a subset of)
-- @criterion@'s exports for running benchmarks, with an extra final argument
-- that is a @tasty-hunit@ test for correctness of the output of the benchmark.
data Benchmarkable where
  NF :: NFData b => (a -> b) -> a -> IO (b -> Assertion) -> Benchmarkable

benchmarks :: [Benchmark]
benchmarks =
  [ Group
      "evalTestM"
      [ Group
          "pure logs"
          [ benchExpectedPureLogs (mapEvenEnv 1) "mapEven 1" 100
          , benchExpectedPureLogs (mapEvenEnv 10) "mapEven 10" 1000
          -- This benchmark is too slow to be practical for CI.
          -- , benchExpectedPureLogs (mapEvenEnv 100) "mapEven 100" 10000
          ]
      , Group
          "discard logs"
          [ benchExpectedDiscardLogs (mapEvenEnv 1) "mapEven 1" 100
          , benchExpectedDiscardLogs (mapEvenEnv 10) "mapEven 10" 1000
          -- This benchmark is too slow to be practical for CI.
          -- , benchExpectedDiscardLogs (mapEvenEnv 100) "mapEven 100" 10000
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
    benchExpected f g e n b = EnvBench e n $ \e' ->
      NF
        (f e')
        b
        (pure $ (@?= Right (zeroIDs $ expectedResult e')) . fmap zeroIDs . g)
    benchExpectedPureLogs = benchExpected evalTestMPureLogs fst
    benchExpectedDiscardLogs = benchExpected evalTestMDiscardLogs identity
    mapEvenEnv n = pure $ mapEven n

runBenchmarks :: [Benchmark] -> [C.Benchmark]
runBenchmarks = map go
  where
    go (EnvBench act n b) = env act $ \e -> bench (toS n) $ runBenchmarkable $ b e
    go (Group n bs) = bgroup (toS n) $ runBenchmarks bs

    runBenchmarkable (NF f x _) = nf f x

runTests :: [Benchmark] -> TestTree
runTests = testGroup "Benchmark result tests" . map go
  where
    go (EnvBench act n b) = withResource act (const $ pure ()) $ \e ->
      testCase (toS n) $
        e >>= testBenchmarkable . b
    go (Group n bs) = testGroup (toS n) $ map go bs

    testBenchmarkable (NF f x test) = test >>= ($ f x)