{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( newTBQueueIO,
  )
import Control.Exception (bracket)
import Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Database.Selda.Backend (runSeldaT)
import Database.Selda.PostgreSQL
  ( pgOpen',
    seldaClose,
  )
import Database.Selda.SQLite (withSQLite)
import Numeric.Natural (Natural)
import Options.Applicative
  ( Parser,
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
    optional,
    progDesc,
    str,
    value,
    (<|>),
  )
import Primer.Database (Version)
import qualified Primer.Database as Db
  ( ServiceCfg (..),
    serve,
  )
import qualified Primer.Database.Selda as Db (initialize)
import Primer.Server
  ( serve,
  )
import qualified StmContainers.Map as StmMap
import System.Directory (canonicalizePath, withCurrentDirectory)
import System.Environment (lookupEnv)

{- HLINT ignore "Use newtype instead of data" -}
data GlobalOptions = GlobalOptions
  { cmd :: !Command
  }

data Database
  = SQLite FilePath
  | PostgreSQL BS.ByteString

parseDatabase :: Parser Database
parseDatabase =
  (SQLite <$> option str (long "sqlite-db"))
    <|> (PostgreSQL <$> option auto (long "pgsql-url"))

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

defaultSQLiteDB :: FilePath
defaultSQLiteDB = "primer.sqlite"

pgUrlEnvVar :: String
pgUrlEnvVar = "DATABASE_URL"

-- | When no database flag is provided on the command line, we try
-- first to lookup and parse the magic @DATABASE_URL@ environment
-- variable. If that's not present, we fall back on a local SQLite
-- database.
--
-- Note: this is less than ideal for production, where we should fail
-- when no valid PostgreSQL configuration exists. See
-- <https://github.com/hackworthltd/primer/issues/1>
defaultDb :: IO Database
defaultDb = do
  envVar <- lookupEnv pgUrlEnvVar
  case envVar of
    Nothing -> pure $ SQLite defaultSQLiteDB
    Just uri -> pure $ PostgreSQL $ fromString uri

runDb :: Db.ServiceCfg -> Database -> IO ()
runDb cfg =
  \case
    SQLite path -> do
      dbPath <- canonicalizePath path
      withSQLite dbPath dbGo
    PostgreSQL uri ->
      -- We don't use @withPostgreSQL@ because we have a raw URI, and
      -- we don't want to parse it.
      bracket (pgOpen' Nothing uri) seldaClose $ runSeldaT dbGo
  where
    dbGo = do
      -- For convenience, in case it doesn't already exist, we
      -- initialize the database. If it does already exist, this
      -- action does nothing.
      --
      -- TODO: better privilege separation for PostgreSQL databases
      -- operations. See
      -- <https://github.com/hackworthltd/primer/issues/2>
      Db.initialize
      Db.serve cfg

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
