{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword hiding (Handler)

import Control.Concurrent.Async (
  concurrently_,
 )
import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Control.Monad.Fail (fail)
import Control.Monad.Log (
  Handler,
  MonadLog,
  WithSeverity (..),
  defaultBatchingOptions,
  logError,
  logInfo,
  logNotice,
  runLoggingT,
  withBatchedHandler,
 )
import Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Data.String (String)
import Hasql.Pool (
  Pool,
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
  value,
 )
import Primer.Database (Version)
import Primer.Database qualified as Db
import Primer.Database.Rel8 (
  Rel8DbException (..),
  Rel8DbLogMessage (..),
  runRel8DbT,
 )
import Primer.Server qualified as Server
import StmContainers.Map qualified as StmMap
import System.Environment (lookupEnv)
import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
 )

{- HLINT ignore GlobalOptions "Use newtype instead of data" -}
data GlobalOptions = GlobalOptions !Command

newtype Database = PostgreSQL BS.ByteString

parseDatabase :: Parser Database
parseDatabase = PostgreSQL <$> option auto (long "pgsql-url")

data Command
  = Serve Version (Maybe Database) Int Natural

serveCmd :: Parser Command
serveCmd =
  Serve
    <$> argument str (metavar "VERSION")
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

runDb :: (MonadCatch m, MonadIO m, MonadLog (WithSeverity Rel8DbLogMessage) m) => Db.ServiceCfg -> Pool -> m ()
runDb cfg = start
  where
    justRel8DbException :: Rel8DbException -> Maybe Rel8DbException
    justRel8DbException = Just

    logDbException = logError . LogRel8DbException

    -- The database computation exception handler.
    start pool =
      catchJust
        justRel8DbException
        (flip runRel8DbT pool $ Db.serve cfg)
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

banner :: [Text]
banner =
  [ "                      ███                                    "
  , "                     ░░░                                     "
  , " ████████  ████████  ████  █████████████    ██████  ████████ "
  , "░░███░░███░░███░░███░░███ ░░███░░███░░███  ███░░███░░███░░███"
  , " ░███ ░███ ░███ ░░░  ░███  ░███ ░███ ░███ ░███████  ░███ ░░░ "
  , " ░███ ░███ ░███      ░███  ░███ ░███ ░███ ░███░░░   ░███     "
  , " ░███████  █████     █████ █████░███ █████░░██████  █████    "
  , " ░███░░░  ░░░░░     ░░░░░ ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░     "
  , " ░███                                                        "
  , " █████                                                       "
  , "░░░░░                                                        "
  ]

serve ::
  Database ->
  Version ->
  Int ->
  Natural ->
  Handler IO (WithSeverity Text) ->
  IO ()
serve (PostgreSQL uri) ver port qsz logger =
  bracket (acquire poolSize timeout uri) release $ \pool -> do
    dbOpQueue <- newTBQueueIO qsz
    initialSessions <- StmMap.newIO
    flip runLoggingT logger $ do
      logInfo $ unlines banner
      logNotice $ "primer-server version " <> ver
    concurrently_
      (Server.serve initialSessions dbOpQueue ver port)
      (flip runLoggingT (\(WithSeverity s t) -> logger $ WithSeverity s (show t)) $ runDb (Db.ServiceCfg dbOpQueue ver) pool)
  where
    -- Note: pool size must be 1 in order to guarantee
    -- read-after-write and write-after-write semantics for individual
    -- sessions. See:
    --
    -- https://github.com/hackworthltd/primer/issues/640#issuecomment-1217290598
    poolSize = 1

    -- 1 second, which is pretty arbitrary.
    timeout = Just $ 1 * 1000000

main :: IO ()
main = do
  -- It's common in Linux containers to log to stdout, so let's ensure
  -- it's line-buffered, as we can't guarantee what the GHC runtime
  -- will do by default.
  hSetBuffering stdout LineBuffering
  withBatchedHandler defaultBatchingOptions flush $ \(logToStdout :: Handler IO (WithSeverity Text)) ->
    handleAll (bye logToStdout) $ do
      args <- execParser opts
      case args of
        GlobalOptions (Serve ver dbFlag port qsz) -> do
          db <- maybe defaultDb pure dbFlag
          serve db ver port qsz logToStdout
  where
    opts =
      info
        (helper <*> cmds)
        ( fullDesc
            <> progDesc "A web service for Primer."
            <> header
              "primer-service - A web service for Primer."
        )
    bye :: HasCallStack => Handler IO (WithSeverity Text) -> SomeException -> IO ()
    bye logger e = do
      flip runLoggingT logger $ do
        logError $ "Fatal exception: " <> show e
        logError $ toS $ prettyCallStack callStack
      exitFailure
    flush :: (Foldable t, MonadIO m, Show a) => t a -> m ()
    flush messages = forM_ messages print
