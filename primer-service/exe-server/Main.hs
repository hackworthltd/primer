{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword hiding (
  catchJust,
 )

import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Control.Monad.Catch (catchJust)
import Control.Monad.Fail (fail)
import Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Data.String (String)
import Hasql.Connection
import Numeric.Natural (Natural)
import Options.Applicative (
  Parser,
  argument,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  str,
  value,
 )
import Primer.Database (Version)
import qualified Primer.Database as Db (
  ServiceCfg (..),
  serve,
 )
import Primer.Database.Rel8 (
  Rel8DbException,
  runRel8DbT,
 )
import Primer.Server (
  serve,
 )
import qualified StmContainers.Map as StmMap
import System.Directory (withCurrentDirectory)
import System.Environment (lookupEnv)

{- HLINT ignore GlobalOptions "Use newtype instead of data" -}
data GlobalOptions = GlobalOptions
  { cmd :: !Command
  }

newtype Database = PostgreSQL BS.ByteString

parseDatabase :: Parser Database
parseDatabase = PostgreSQL <$> option auto (long "pgsql-url")

data Command
  = Serve FilePath Version (Maybe Database) Int Natural

serveCmd :: Parser Command
serveCmd =
  Serve <$> argument str (metavar "PATH")
    <*> argument str (metavar "VERSION")
    <*> optional parseDatabase
    <*> option auto (long "port" <> value 8081)
    <*> option auto (long "db-op-queue-size" <> value 128)

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions
    <$> hsubparser
      ( command
          "serve"
          (info serveCmd (progDesc "Run the server"))
      )

pgUrlEnvVar :: String
pgUrlEnvVar = "DATABASE_URL"

-- | When no database flag is provided on the command line, we try
-- first to lookup and parse the magic @DATABASE_URL@ environment
-- variable. If that's not present, we fail.
defaultDb :: IO Database
defaultDb = do
  envVar <- lookupEnv pgUrlEnvVar
  case envVar of
    Nothing -> fail "You must provide a PostgreSQL connection URL either via the command line, or by setting the DATABASE_URL environment variable."
    Just uri -> pure $ PostgreSQL $ fromString uri

runDb :: Db.ServiceCfg -> Database -> IO ()
runDb cfg =
  \case
    PostgreSQL uri ->
      bracket (connect uri) release restart
  where
    -- Connect to the database.
    connect u = acquire u >>= either (fail . show) return

    -- Catch exceptions for which we can restart the database service.
    -- For the time being, this is any 'Rel8DbException', but later we
    -- should be more selective about this. See:
    -- https://github.com/hackworthltd/primer/issues/381
    restartableException :: Rel8DbException -> Maybe Rel8DbException
    restartableException = Just

    -- The database computation server.
    go = runRel8DbT $ Db.serve cfg

    -- This action should log exceptions somewhere useful. For now, it
    -- only outputs the exception to 'stderr'. See
    -- https://github.com/hackworthltd/primer/issues/179.
    logDbException e = putErrLn (show e :: Text)

    -- The database computation exception handler. If an exception
    -- occurs and it's restartable, we make a note of it and restart.
    --
    -- Note that forward progress in this implementation is only
    -- guaranteed if the database computation server has removed the
    -- operation that caused the original exception from the database
    -- operation queue. This is, in fact, how the 'Db.serve' server is
    -- implemented, but this note is left as a caveat, regardless.
    restart conn =
      catchJust
        restartableException
        (go conn)
        (\e -> logDbException e >> restart conn)

-- The choice of which process to fork and which to run in the "main"
-- thread doesn't particularly matter for our purposes, as far as I
-- can determine. We do need some proper exception handling, however,
-- and we'll also need to shut down properly on SIGINT or similar once
-- we have an actual persistent database, so we may need to reconsider
-- what to do on the main thread when we implement those additional
-- process-related features.
--
-- For now, we run the Servant service in the forked thread, because
-- exceptions in the web service are already reasonably well-handled
-- and reported (to the student, via HTTP error codes) by Servant.
run :: GlobalOptions -> IO ()
run opts = case cmd opts of
  Serve root ver dbFlag port qsz -> do
    dbOpQueue <- newTBQueueIO qsz
    initialSessions <- StmMap.newIO
    _ <- forkIO $ withCurrentDirectory root (serve initialSessions dbOpQueue ver port)
    db <- maybe defaultDb pure dbFlag
    runDb (Db.ServiceCfg dbOpQueue ver) db

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (helper <*> cmds)
        ( fullDesc <> progDesc "A web service for Primer."
            <> header
              "primer-service - A web service for Primer."
        )
