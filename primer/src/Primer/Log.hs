module Primer.Log (
  ConvertLogMessage (..),
  logCritical,
  logDebug,
  logError,
  logEmergency,
  logInfo,
  logNotice,
  logWarning,
  textWithSeverity,
  PureLogT,
  runPureLogT,
  PureLog,
  runPureLog,
  DiscardLogT,
  runDiscardLogT,
  DiscardLog,
  runDiscardLog,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (
  DiscardLoggingT,
  LoggingT,
  MonadLog,
  PureLoggingT,
  Severity (..),
  WithSeverity (..),
  discardLogging,
  logMessage,
  runLoggingT,
  runPureLoggingT,
 )
import Control.Monad.Log qualified as Log (
  logCritical,
  logDebug,
  logEmergency,
  logError,
  logInfo,
  logNotice,
  logWarning,
 )
import Control.Monad.Trans (MonadTrans)

logSeverity :: Severity -> Text
logSeverity Debug = "[DEBUG]     "
logSeverity Informational = "[INFO]      "
logSeverity Notice = "[NOTICE]    "
logSeverity Warning = "[WARNING]   "
logSeverity Error = "[ERROR]     "
logSeverity Critical = "[CRITICAL]  "
logSeverity Alert = "[ALERT]     "
logSeverity Emergency = "[EMERGENCY] "

class ConvertLogMessage source target where
  convert :: source -> target

logCritical :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logCritical = Log.logCritical . convert

logDebug :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logDebug = Log.logDebug . convert

logEmergency :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logEmergency = Log.logEmergency . convert

logError :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logError = Log.logError . convert

logInfo :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logInfo = Log.logInfo . convert

logNotice :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logNotice = Log.logNotice . convert

logWarning :: (ConvertLogMessage source target, MonadLog (WithSeverity target) m) => source -> m ()
logWarning = Log.logWarning . convert

textWithSeverity :: WithSeverity Text -> Text
textWithSeverity (WithSeverity s m) = logSeverity s <> m

-- | Convenient for discarding logging.
instance ConvertLogMessage a () where
  convert = pure ()

instance ConvertLogMessage a a where
  convert = identity

-- | Purely collect log messages in a 'Seq'
newtype PureLogT l m a = PureLogs (LoggingT l (PureLoggingT (Seq l) m) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog l
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadFresh i
    )

instance MonadTrans (PureLogT l) where
  lift = PureLogs . lift . lift

-- | Purely accumulate log messages in a 'Seq'.
-- Note that this may cause a large amount of memory to be retained if you
-- use this with an action that logs excessively.
-- Note that the logs are accumulated strictly, i.e. each element in the
-- resulting 'Seq' will be in WHNF.
runPureLogT :: Monad m => PureLogT l m a -> m (a, Seq l)
runPureLogT (PureLogs m) = runPureLoggingT $ runLoggingT m $ \l ->
  let !l' = l in logMessage $ pure l'

type PureLog l = PureLogT l Identity

runPureLog :: PureLog l a -> (a, Seq l)
runPureLog = runIdentity . runPureLogT

-- | Discard log messages.
newtype DiscardLogT l m a = DiscardLogs (DiscardLoggingT l m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog l
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadFresh i
    )

instance MonadTrans (DiscardLogT l) where
  lift = DiscardLogs . lift

runDiscardLogT :: DiscardLogT l m a -> m a
runDiscardLogT (DiscardLogs m) = discardLogging m

type DiscardLog l = DiscardLogT l Identity

runDiscardLog :: DiscardLog l a -> a
runDiscardLog = runIdentity . runDiscardLogT
