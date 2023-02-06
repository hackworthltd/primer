module Primer.Action.Movement (Movement (..)) where

import Foreword

import Primer.Core.Meta (ValConName)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

-- | Core movements
data Movement = Child1 | Child2 | Parent | Branch ValConName
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Movement
