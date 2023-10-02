-- | Utilities useful across several types of tests.
module Primer.API.Test.Util (
  runAPI,
) where

import Foreword

import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Control.Monad.Log (
  WithSeverity,
 )
import Primer.API (
  Env (..),
  PrimerM,
  runPrimerM,
 )
import Primer.Database (
  ServiceCfg (..),
  runNullDb',
  serve,
 )
import Primer.Log (PureLogT, runPureLogT)
import Primer.Test.Util (
  LogMsg,
  assertNoSevereLogs,
 )
import StmContainers.Map qualified as StmMap

-- Run 2 threads: one that serves a 'NullDb', and one that runs Primer
-- API actions. This allows us to simulate a database and API service.
--
-- We run the database thread on the fork, because it will need to run
-- until it's terminated. The Primer API action will run on the main
-- thread and terminate the database thread when the API action runs
-- to completion or throws.
runAPI :: PrimerM (PureLogT (WithSeverity LogMsg) IO) a -> IO a
runAPI action = do
  -- This is completely arbitrary and just for testing. In production,
  -- this value will be provided by the production environment and
  -- will likely come from the git rev used to build the production
  -- service.
  let version = "git123"
  dbOpQueue <- newTBQueueIO 1
  initialSessions <- StmMap.newIO
  (r, logs) <-
    withAsync (runNullDb' $ serve (ServiceCfg dbOpQueue version))
      $ const
      $ runPureLogT
      . runPrimerM action
      $ Env initialSessions dbOpQueue version
  assertNoSevereLogs logs
  pure r
