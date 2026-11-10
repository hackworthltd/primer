module Primer.Action.Movement (BranchMove (..), Movement (..)) where

import Foreword

import Primer.Core.Meta (Pattern)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

data BranchMove
  = Pattern Pattern
  | Fallback
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
  | -- | Move to the binding introduced by a λ, Λ, let, letrec, let type, ∀ or type-level let.
    -- (Case branches can bind several variables, so their bindings must instead be focused on by ID.)
    Binding
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Movement
  deriving anyclass (NFData)
