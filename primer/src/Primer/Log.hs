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
) where

import Foreword

import Control.Monad.Log (
  MonadLog,
  Severity (..),
  WithSeverity (..),
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
