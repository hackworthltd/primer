{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Benchmarks (hashProg, benchmarks, runBenchmarks, runTests) where

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
import Data.Aeson (encode)
import Data.Text qualified as T
import Primer.API (viewProg)
import Primer.App (
  App,
  MutationRequest,
  Prog,
  ProgError,
  appProg,
  handleMutationRequest,
  newApp,
  runEditAppM,
  tcWholeProgWithImports,
 )
import Primer.App.Utils (forgetProgTypecache)
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
  runDiscardLog,
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
import Prelude (error)

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
  , Group
      "edits"
      [runFixture "sayHello"]
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

    tcTest id = evalTestM id . runExceptT @TypeError . tcWholeProgWithImports

    benchTC e n = EnvBench e n $ \(prog, maxId, _) -> NF (tcTest maxId) prog $
      pure $ \case
        Left err -> assertFailure $ "Failed to typecheck: " <> show err
        Right p -> assertBool "Unexpected smarthole changes" $ forgetProgTypecache p == forgetProgTypecache prog

    mapEvenEnv n = pure $ mapEven n
    mapOddProgEnv = pure . mapOddProg
    mapOddPrimProgEnv = pure . mapOddPrimProg

runFixture :: Text -> Benchmark
runFixture fixture =
  Group
    fixture
    [ benchFixture None
    , benchFixture App
    , benchFixture Prog
    , benchFixture ProgJSON
    ]
  where
    parseEdits :: IO ([MutationRequest], Maybe Int)
    parseEdits = do
      cts <- fmap lines . readFile . toS $ "fixtures/" <> fixture <> ".edits"
      case cts of
        (l : ls)
          | Just h <- T.stripPrefix "expected result hash: " l ->
              pure (fmap unsafeRead ls, Just $ unsafeRead h)
        _ -> pure (fmap unsafeRead cts, Nothing)

    info = \case
      None -> "(no forcing)"
      App -> "(force app)"
      Prog -> "(force prog)"
      ProgJSON -> "(force progjson)"

    benchFixture f = EnvBench parseEdits (fixture <> " " <> info f) $
      \(edits, expected) ->
        NF
          (benchEdits f)
          edits
          ( pure $ \p ->
              let ph = hashProg (appProg p)
               in case expected of
                    Nothing -> assertFailure $ "No expected result hash given. Actual result hash was " <> show ph
                    Just h -> ph @?= h
          )

    -- This may throw an exception, but that will show as a failing test
    unsafeRead :: Read a => Text -> a
    unsafeRead = fromMaybe (error "failed to parse fixture file") . readMaybe

-- | We sanity check our benchmarks result by comparing the hash of
-- the actual result with an expected hash. This is because the actual
-- result may be somewhat large, so we wish to avoid storing the whole
-- thing in the repository).  The only properties we care about are
-- that the hash is small, and checking hash-equality gives a good
-- change of catching any changes.  NB: this hash should capture
-- every detail of the 'Prog', including metadata and IDs.
hashProg :: Prog -> Int
-- To avoid having to define a 'Hashable' instance just for this we
-- use this hack.
hashProg = hash @Text . show

-- Various forcings
data Force = None | App | Prog | ProgJSON
force' :: Force -> App -> App
force' None a = a
force' App a = force a
force' Prog a = viewProg (appProg a) `deepseq` a
force' ProgJSON a = encode (viewProg $ appProg a) `deepseq` a

-- run actions, forcing intermediate results as dictated by Force argument
benchEdits :: Force -> [MutationRequest] -> App
benchEdits forceIntermediates actions =
  case foldlM runAction newApp actions of
    Left err -> error $ "Running actions failed with: " <> show err
    Right a -> a
  where
    runAction :: App -> MutationRequest -> Either ProgError App
    runAction a m = (\(e, a') -> e $> force' forceIntermediates a') $ runDiscardLog (runEditAppM (handleMutationRequest m) a)

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
