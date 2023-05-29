module Primer.Action.Movement (BranchMove (..), Movement (..)) where

import Foreword

import Primer.Core.Meta (ValConName)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

{- HLINT ignore BranchMove "Use newtype instead of data" -}
data BranchMove
  = Pattern ValConName
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON BranchMove
  deriving anyclass (NFData)

-- | Core movements
data Movement
  = Child1
  | Child2
  | Parent
  | Branch BranchMove
  | -- | Move into a term argument of a saturated constructor (zero-indexed)
    ConChild Int
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Movement
  deriving anyclass (NFData)
