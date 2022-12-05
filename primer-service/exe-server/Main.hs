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
  WithSeverity (..),
  defaultBatchingOptions,
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
import Primer.API (APILog, PrimerErr (..))
import Primer.Database (Version)
import Primer.Database qualified as Db
import Primer.Database.Rel8 (
  MonadRel8Db,
  Rel8DbException (..),
  Rel8DbLogMessage (..),
  runRel8DbT,
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
import Primer.Server (ConvertServerLogs)
import Primer.Server qualified as Server
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
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

runDb :: MonadRel8Db m l => Db.ServiceCfg -> Pool -> m Void
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
  ( ConvertLogMessage Rel8DbLogMessage l
  , ConvertLogMessage Text l
  , ConvertLogMessage PrimerErr l
  , ConvertServerLogs l
  ) =>
  Database ->
  Version ->
  Int ->
  Natural ->
  -- | NB: this logging handler will be called concurrently in multiple threads.
  -- It is expected that we use and pass around one global 'withBatchedHandler',
  -- which is thread-safe (in the sense that messages will be logged atomically:
  -- they will all appear, and will not interleave like
  -- @concurrently_ (putStrLn s1) (putStrLn s2)@ can.)
  Handler IO (WithSeverity l) ->
  IO ()
serve (PostgreSQL uri) ver port qsz logger =
  bracket (acquire poolSize timeout uri) release $ \pool -> do
    dbOpQueue <- newTBQueueIO qsz
    initialSessions <- StmMap.newIO
    flip runLoggingT logger $ do
      forM_ banner logInfo
      logNotice $ "primer-server version " <> ver
      logNotice ("Listening on port " <> show port :: Text)
    concurrently_
      (Server.serve initialSessions dbOpQueue ver port logger)
      (flip runLoggingT logger $ runDb (Db.ServiceCfg dbOpQueue ver) pool)
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
        GlobalOptions (Serve ver dbFlag port qsz) -> do
          db <- maybe defaultDb pure dbFlag
          serve db ver port qsz (logToStdout . logMsgWithSeverity)
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

instance ConvertLogMessage Rel8DbLogMessage LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage SomeException LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage PrimerErr LogMsg where
  convert (DatabaseErr e) = LogMsg e
  convert (UnknownDef e) = LogMsg $ show e
  convert (UnexpectedPrimDef e) = LogMsg $ show e
  convert (AddDefError m n e) = LogMsg $ show (m, n, e)
  convert (ActionOptionsNoID e) = LogMsg $ show e
  convert (ToProgActionError a e) = LogMsg $ show (a, e)
  convert (ApplyActionError as e) = LogMsg $ show (as, e)

instance ConvertLogMessage APILog LogMsg where
  convert = LogMsg . show

instance ConvertLogMessage EvalLog LogMsg where
  convert = LogMsg . show
