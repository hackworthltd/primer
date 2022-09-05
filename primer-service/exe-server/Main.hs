{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Control.Monad.Fail (fail)
import Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Data.String (String)
import Hasql.Pool (
  acquire,
  release,
 )
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
  switch,
  value,
 )
import Primer.API (
  Env (..),
  addSession,
  runPrimerIO,
 )
import Primer.App (
  App,
 )
import Primer.Database (Version)
import Primer.Database qualified as Db (
  ServiceCfg (..),
  discardOp,
  serve,
 )
import Primer.Database.Rel8 (
  Rel8DbException (..),
  runRel8DbT,
 )
import Primer.Examples (
  even3App,
 )
import Primer.Server (
  serve,
 )
import StmContainers.Map qualified as StmMap
import System.Environment (lookupEnv)
import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
 )

{- HLINT ignore GlobalOptions "Use newtype instead of data" -}
data GlobalOptions = GlobalOptions
  { cmd :: !Command
  }

newtype Database = PostgreSQL BS.ByteString

parseDatabase :: Parser Database
parseDatabase = PostgreSQL <$> option auto (long "pgsql-url")

data Command
  = Serve Version (Maybe Database) Int Natural Bool

serveCmd :: Parser Command
serveCmd =
  Serve
    <$> argument str (metavar "VERSION")
    <*> optional parseDatabase
    <*> option auto (long "port" <> value 8081)
    <*> option auto (long "db-op-queue-size" <> value 128)
    <*> switch (long "seed-db")

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
      bracket (acquire poolSize timeout uri) release start
  where
    -- Note: pool size must be 1 in order to guarantee
    -- read-after-write and write-after-write semantics for individual
    -- sessions. See:
    --
    -- https://github.com/hackworthltd/primer/issues/640#issuecomment-1217290598
    poolSize = 1

    -- 1 second, which is pretty arbitrary.
    timeout = Just $ 1 * 1000000

    justRel8DbException :: Rel8DbException -> Maybe Rel8DbException
    justRel8DbException = Just

    -- The database computation server.
    go = runRel8DbT $ Db.serve cfg

    -- This action should log exceptions somewhere useful. For now, it
    -- only outputs the exception to 'stderr'. See
    -- https://github.com/hackworthltd/primer/issues/179.
    logDbException e = putErrLn (show e :: Text)

    -- The database computation exception handler.
    start pool =
      catchJust
        justRel8DbException
        (go pool)
        $ \e -> do
          logDbException e
          case e of
            -- Retry the same operation until it succeeds.
            -- Note: we need some backoff here. See:
            --
            -- https://github.com/hackworthltd/primer/issues/678
            ConnectionFailed _ -> start pool
            -- Retry the same operation until it succeeds.
            TimeoutError -> start pool
            -- The operation will probably fail if we try it again,
            -- but other operations might be fine, so discard the
            -- failed op from the queue and continue serving
            -- subsequent ops.
            --
            -- Note that we should be more selective than this: some
            -- exceptions may indicate a serious problem with the
            -- database, in which case we may not want to restart.
            -- See:
            --
            -- https://github.com/hackworthltd/primer/issues/381
            _ -> do
              Db.discardOp (Db.opQueue cfg)
              start pool

-- A list of 'App's used to seed the database.
seedApps :: [(Text, App)]
seedApps =
  [ ("even3", even3App)
  ]

banner :: Text
banner =
  "                      ███                                    \n\
  \                     ░░░                                     \n\
  \ ████████  ████████  ████  █████████████    ██████  ████████ \n\
  \░░███░░███░░███░░███░░███ ░░███░░███░░███  ███░░███░░███░░███\n\
  \ ░███ ░███ ░███ ░░░  ░███  ░███ ░███ ░███ ░███████  ░███ ░░░ \n\
  \ ░███ ░███ ░███      ░███  ░███ ░███ ░███ ░███░░░   ░███     \n\
  \ ░███████  █████     █████ █████░███ █████░░██████  █████    \n\
  \ ░███░░░  ░░░░░     ░░░░░ ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░     \n\
  \ ░███                                                        \n\
  \ █████                                                       \n\
  \░░░░░                                                        "

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
  Serve ver dbFlag port qsz seedDb -> do
    dbOpQueue <- newTBQueueIO qsz
    initialSessions <- StmMap.newIO
    putText banner
    putText $ "primer-server version " <> ver
    _ <- forkIO $ do
      when seedDb $ do
        let env = Env initialSessions dbOpQueue ver
        flip runPrimerIO env $
          forM_ seedApps $
            uncurry addSession
      serve initialSessions dbOpQueue ver port
    db <- maybe defaultDb pure dbFlag
    runDb (Db.ServiceCfg dbOpQueue ver) db

main :: IO ()
main = do
  -- It's common in Linux containers to log to stdout, so let's ensure
  -- it's line-buffered, as we can't guarantee what the GHC runtime
  -- will do by default.
  hSetBuffering stdout LineBuffering
  execParser opts >>= run
  where
    opts =
      info
        (helper <*> cmds)
        ( fullDesc
            <> progDesc "A web service for Primer."
            <> header
              "primer-service - A web service for Primer."
        )
