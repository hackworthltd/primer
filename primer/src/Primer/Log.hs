{-# LANGUAGE DefaultSignatures #-}

module Primer.Log (
  LogMessage (..),
  textWithSeverity,
) where

import Foreword

import Control.Monad.Log (
  Severity (..),
  WithSeverity (..),
 )

brackets :: Text -> Text
brackets t = "[" <> t <> "] "

logSeverity :: Severity -> Text
logSeverity Debug = "debug"
logSeverity Informational = "info"
logSeverity Notice = "notice"
logSeverity Warning = "warning"
logSeverity Error = "error"
logSeverity Critical = "critical"
logSeverity Alert = "alert"
logSeverity Emergency = "emergency"

class LogMessage a where
  toText :: a -> Text
  default toText :: (Show a) => a -> Text
  toText = show

instance LogMessage Text

textWithSeverity :: (LogMessage a) => WithSeverity a -> Text
textWithSeverity msg = brackets (logSeverity $ msgSeverity msg) <> toText (discardSeverity msg)
