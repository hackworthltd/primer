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
import Primer.App (
  tcWholeProgWithImports,
 )
import Primer.App.Utils (forgetProgTypecache)
import Primer.Eval (
  NormalOrderOptions (UnderBinders),
  RunRedexOptions (RunRedexOptions, pushAndElide),
  ViewRedexOptions (ViewRedexOptions, aggressiveElision, groupedLets),
 )
import Primer.EvalFull (
  Dir (Syn),
  EvalLog,
  evalFull,
 )
import Primer.Examples (
  mapOddPrimProg,
  mapOddProg,
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
import Primer.Typecheck (TypeError)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

-- Orphans for 'NFData' instances.
deriving stock instance Generic (WithSeverity a)
deriving stock instance Generic Severity
deriving anyclass instance NFData Severity
deriving anyclass instance (NFData a) => NFData (WithSeverity a)

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
  , Group
      "typecheck"
      [ benchTC (mapOddProgEnv 1) "mapOdd 1"
      , benchTC (mapOddProgEnv 10) "mapOdd 10"
      , benchTC (mapOddProgEnv 100) "mapOdd 100"
      , benchTC (mapOddPrimProgEnv 1) "mapOddPrim 1"
      , benchTC (mapOddPrimProgEnv 10) "mapOddPrim 10"
      , benchTC (mapOddPrimProgEnv 100) "mapOddPrim 100"
      ]
  ]
  where
    evalOptionsN = UnderBinders
    evalOptionsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True}
    evalOptionsR = RunRedexOptions{pushAndElide = True}
    evalTestMPureLogs e maxEvals =
      evalTestM (maxID e)
        $ runPureLogT
        $ evalFull @EvalLog evalOptionsN evalOptionsV evalOptionsR builtinTypes (defMap e) maxEvals Syn (expr e)
    evalTestMDiscardLogs e maxEvals =
      evalTestM (maxID e)
        $ runDiscardLogT
        $ evalFull @EvalLog evalOptionsN evalOptionsV evalOptionsR builtinTypes (defMap e) maxEvals Syn (expr e)

    benchExpected f g e n b = EnvBench e n $ \e' ->
      NF
        (f e')
        b
        (pure $ (@?= Right (zeroIDs $ expectedResult e')) . fmap zeroIDs . g)

    benchExpectedPureLogs = benchExpected evalTestMPureLogs fst
    benchExpectedDiscardLogs = benchExpected evalTestMDiscardLogs identity

    tcTest id = evalTestM id . runExceptT @TypeError . tcWholeProgWithImports

    benchTC e n = EnvBench e n $ \(prog, maxId, _) -> NF (tcTest maxId) prog
      $ pure
      $ \case
        Left err -> assertFailure $ "Failed to typecheck: " <> show err
        Right p -> assertBool "Unexpected smarthole changes" $ forgetProgTypecache p == forgetProgTypecache prog

    mapEvenEnv n = pure $ mapEven n
    mapOddProgEnv = pure . mapOddProg
    mapOddPrimProgEnv = pure . mapOddPrimProg

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
      testCase (toS n)
        $ e
        >>= testBenchmarkable
        . b
    go (Group n bs) = testGroup (toS n) $ map go bs

    testBenchmarkable (NF f x test) = test >>= ($ f x)
