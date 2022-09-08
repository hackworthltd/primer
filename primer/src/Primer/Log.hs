{-# LANGUAGE DefaultSignatures #-}

module Primer.Log (
  LogMessage (..),
) where

import Foreword

class LogMessage a where
  toText :: a -> Text
  default toText :: (Show a) => a -> Text
  toText = show
