module Primer.Typecheck.SmartHoles (SmartHoles (..)) where

import Foreword

import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

data SmartHoles = SmartHoles | NoSmartHoles
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON SmartHoles
  deriving anyclass (NFData)
