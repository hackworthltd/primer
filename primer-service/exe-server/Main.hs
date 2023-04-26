{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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
  Severity (Informational),
  WithSeverity (..),
  defaultBatchingOptions,
  runLoggingT,
  withBatchedHandler,
 )
import Data.ByteString as BS
import Data.ByteString.UTF8 (fromString)
import Data.String (String)
import Data.Text qualified as Text
import Data.Time.Clock (
  secondsToDiffTime,
 )
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
  eitherReader,
  execParser,
  flag,
  fullDesc,
  header,
  help,
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
import Primer.API (APILog, PrimerErr (..))
import Primer.Database (Version)
import Primer.Database qualified as Db
import Primer.Database.Rel8 (
  MonadRel8Db,
  Rel8DbException,
  Rel8DbLogMessage (..),
  runRel8DbT,
 )
import Primer.Database.Rel8 qualified as Rel8Db
import Primer.Database.Selda ()
import Primer.Database.Selda as SeldaDb
import Primer.Database.Selda.SQLite (
  MonadSeldaSQLiteDb,
  runSeldaSQLiteDbT,
 )
import Primer.Eval (EvalLog)
import Primer.Log (
  ConvertLogMessage (..),
  logCritical,
  logError,
  logInfo,
  logNotice,
  textWithSeverity,
 )
import Primer.Server (
  ConvertServerLogs,
  CorsAllowedOrigins (..),
  ServantLog,
  parseCorsAllowedOrigins,
  prettyPrintCorsAllowedOrigins,
 )
import Primer.Server qualified as Server
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import StmContainers.Map qualified as StmMap
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.IO (
  BufferMode (LineBuffering),
  hSetBuffering,
 )

{- HLINT ignore GlobalOptions "Use newtype instead of data" -}
data GlobalOptions = GlobalOptions !Command

data Database
  = SQLite FilePath
  | PostgreSQL BS.ByteString

parseDatabase :: Parser Database
parseDatabase =
  (SQLite <$> option str (long "sqlite-db"))
    <|> (PostgreSQL <$> option auto (long "pgsql-url"))

data Logger = Standard | Replay

data Command
  = Serve Version (Maybe Database) Int Natural Logger CorsAllowedOrigins

parseOrigins :: Parser CorsAllowedOrigins
parseOrigins =
  option
    parser
    ( long
        "cors-allow-origin"
        <> value AllowAnyOrigin
        <> metavar "ORIGIN,ORIGIN,..."
        <> help "Comma-separated list of RFC 6454-formatted allowed CORS origins"
    )
  where
    parser = eitherReader $ \arg ->
      case Text.strip <$> Text.splitOn "," (toS arg) of
        [] -> Left "Expected a list of origins, separated by commas"
        ts ->
          case parseCorsAllowedOrigins ts of
            Left err -> Left (toS err)
            Right corsAllowedOrigins -> Right corsAllowedOrigins

serveCmd :: Parser Command
serveCmd =
  Serve
    <$> argument str (metavar "VERSION")
    <*> optional parseDatabase
    <*> option auto (long "port" <> value 8081)
    <*> option auto (long "db-op-queue-size" <> value 128)
    <*> flag Standard Replay (long "record-replay" <> help "Change the log format to capture enough information so one can replay sessions")
    <*> parseOrigins

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
    Nothing -> fail "No database argument was given, and the DATABASE_URL environment variable is not set. Exiting."
    Just uri -> pure $ PostgreSQL $ fromString uri

runRel8Db :: MonadRel8Db m l => Db.ServiceCfg -> Pool -> m Void
runRel8Db cfg = start
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
            Rel8Db.ConnectionFailed _ -> start pool
            -- Retry the same operation until it succeeds.
            Rel8Db.TimeoutError -> start pool
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

runSqliteDb :: MonadSeldaSQLiteDb m l => Db.ServiceCfg -> FilePath -> m Void
runSqliteDb cfg = start
  where
    justSeldaDbException :: SeldaDbException -> Maybe SeldaDbException
    justSeldaDbException = Just

    logSeldaDbException = logError . LogSeldaDbException

    -- The database computation exception handler.
    start dbPath =
      catchJust
        justSeldaDbException
        (runSeldaSQLiteDbT dbPath $ Db.serve cfg)
        $ \e -> do
          logSeldaDbException e
          case e of
            SeldaDb.ConnectionFailed _ -> do
              -- This means the file doesn't exist, it's corrupt, we
              -- don't have the proper permissions, there's an IO
              -- error, etc. This is fatal, so we rethrow.
              throwM e
            _ -> do
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
              Db.discardOp (Db.opQueue cfg)
              start dbPath

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
  ( ConvertLogMessage Rel8DbLogMessage l
  , ConvertLogMessage SeldaDbLogMessage l
  , ConvertLogMessage Text l
  , ConvertLogMessage BS.ByteString l
  , ConvertLogMessage FilePath l
  , ConvertLogMessage PrimerErr l
  , ConvertServerLogs l
  , ConvertLogMessage ServantLog l
  ) =>
  Database ->
  Version ->
  Int ->
  Natural ->
  CorsAllowedOrigins ->
  -- | NB: this logging handler will be called concurrently in multiple threads.
  -- It is expected that we use and pass around one global 'withBatchedHandler',
  -- which is thread-safe (in the sense that messages will be logged atomically:
  -- they will all appear, and will not interleave like
  -- @concurrently_ (putStrLn s1) (putStrLn s2)@ can.)
  Handler IO (WithSeverity l) ->
  IO ()
serve (PostgreSQL uri) ver port qsz origins logger =
  bracket (acquire poolSize timeout maxLifetime uri) release $ \pool -> do
    dbOpQueue <- newTBQueueIO qsz
    initialSessions <- StmMap.newIO
    flip runLoggingT logger $ do
      forM_ banner logInfo
      logNotice $ "primer-server version " <> ver
      logNotice ("Listening on port " <> show port :: Text)
      logNotice $ "CORS allowed origins: " <> prettyPrintCorsAllowedOrigins origins
      logNotice $ "PostgreSQL database: " <> uri
    concurrently_
      (Server.serve initialSessions dbOpQueue ver port origins logger)
      (flip runLoggingT logger $ runRel8Db (Db.ServiceCfg dbOpQueue ver) pool)
  where
    -- Note: pool size must be 1 in order to guarantee
    -- read-after-write and write-after-write semantics for individual
    -- sessions. See:
    --
    -- https://github.com/hackworthltd/primer/issues/640#issuecomment-1217290598
    poolSize = 1
    -- 10 second connection timeout (arbitrary)
    timeout = secondsToDiffTime 10
    -- 30 min max connection lifetime (arbitrary)
    maxLifetime = secondsToDiffTime $ 60 * 30
serve (SQLite path) ver port qsz origins logger = do
  dbPath <- canonicalizePath path
  dbOpQueue <- newTBQueueIO qsz
  initialSessions <- StmMap.newIO
  flip runLoggingT logger $ do
    forM_ banner logInfo
    logNotice $ "primer-server version " <> ver
    logNotice ("Listening on port " <> show port :: Text)
    logNotice $ "CORS allowed origins: " <> prettyPrintCorsAllowedOrigins origins
    logNotice $ "SQLite database: " <> dbPath
  concurrently_
    (Server.serve initialSessions dbOpQueue ver port origins logger)
    (flip runLoggingT logger $ runSqliteDb (Db.ServiceCfg dbOpQueue ver) dbPath)

main :: IO ()
main = do
  -- Register GHC metrics with Prometheus. This needs to be done very
  -- early in the program.
  void $ P.register P.ghcMetrics

  -- It's common in Linux containers to log to stdout, so let's ensure
  -- it's line-buffered, as we can't guarantee what the GHC runtime
  -- will do by default.
  hSetBuffering stdout LineBuffering
  withBatchedHandler defaultBatchingOptions flush $ \logToStdout ->
    handleAll (bye (logToStdout . logMsgWithSeverity)) $ do
      args <- execParser opts
      case args of
        GlobalOptions (Serve ver dbFlag port qsz logger origins) -> do
          db <- maybe defaultDb pure dbFlag
          case logger of
            Standard -> serve db ver port qsz origins (logToStdout . logMsgWithSeverity)
            Replay -> serve db ver port qsz origins (logToStdout . logReplay)
  where
    opts =
      info
        (helper <*> cmds)
        ( fullDesc
            <> progDesc "A web service for Primer."
            <> header
              "primer-service - A web service for Primer."
        )
    bye :: Handler IO (WithSeverity LogMsg) -> SomeException -> IO ()
    bye logger e = do
      flip runLoggingT logger $ logCritical e
      exitFailure
    flush :: (Foldable t, MonadIO m, Print a) => t a -> m ()
    flush messages = forM_ messages putStrLn

-- | Avoid orphan instances.
newtype LogMsg = LogMsg {unLogMsg :: Text}

logMsgWithSeverity :: WithSeverity LogMsg -> Text
logMsgWithSeverity (WithSeverity s m) = textWithSeverity $ WithSeverity s (unLogMsg m)

instance ConvertLogMessage Text LogMsg where
  convert = LogMsg

instance ConvertLogMessage BS.ByteString LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage FilePath LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage Rel8DbLogMessage LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage SeldaDbLogMessage LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage SomeException LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage PrimerErr LogMsg where
  convert (DatabaseErr e) = LogMsg e
  convert (UnknownDef e) = LogMsg $ show e
  convert (UnknownTypeDef e) = LogMsg $ show e
  convert (UnexpectedPrimDef e) = LogMsg $ show e
  convert (UnexpectedPrimTypeDef e) = LogMsg $ show e
  convert (AddDefError m n e) = LogMsg $ show (m, n, e)
  convert (AddTypeDefError tc vcs e) = LogMsg $ show (tc, vcs, e)
  convert (ActionOptionsNoID e) = LogMsg $ show e
  convert (ToProgActionError a e) = LogMsg $ show (a, e)
  convert (ApplyActionError as e) = LogMsg $ show (as, e)
  convert (UndoError e) = LogMsg $ show e
  convert (RedoError e) = LogMsg $ show e

instance ConvertLogMessage APILog LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage EvalLog LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage ServantLog LogMsg where
  convert = LogMsg . show

data LogReplay
  = API APILog
  | Servant ServantLog
  | Other LogMsg

instance ConvertLogMessage LogReplay LogMsg where
  convert (API m) = convert m
  convert (Servant m) = convert m
  convert (Other m) = m

instance ConvertLogMessage Text LogReplay where
  convert = Other . LogMsg

instance ConvertLogMessage BS.ByteString LogReplay where
  convert = Other . convert

instance ConvertLogMessage FilePath LogReplay where
  convert = Other . convert

instance ConvertLogMessage Rel8DbLogMessage LogReplay where
  convert = Other . convert

instance ConvertLogMessage SeldaDbLogMessage LogReplay where
  convert = Other . convert

instance ConvertLogMessage SomeException LogReplay where
  convert = Other . convert

instance ConvertLogMessage PrimerErr LogReplay where
  convert = Other . convert

instance ConvertLogMessage APILog LogReplay where
  convert = API

instance ConvertLogMessage EvalLog LogReplay where
  convert = Other . convert

instance ConvertLogMessage ServantLog LogReplay where
  convert = Servant

-- | Logger whose output is designed to be able to be "replayed".
-- It is assumed that all "replay-relevant" messages are at the 'Informational' level.
logReplay :: WithSeverity LogReplay -> Text
-- NB: primer-benchmark/mkfixture and primer-service/exe-replay
-- depend on this format, so should be updated in sync with format
-- changes here.
logReplay (WithSeverity Informational (API l)) = "[REPLAY] " <> show l
logReplay (WithSeverity Informational (Servant l)) = "[REPLAY] " <> show l
logReplay (WithSeverity s m) = logMsgWithSeverity (WithSeverity s $ convert m)
