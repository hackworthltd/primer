module Primer.Action.Movement (Movement (..)) where

import Foreword

import Primer.Core.Meta (ValConName)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

-- | Core movements
data Movement
  = Child1
  | Child2
  | Parent
  | Branch ValConName
  | -- | Move into a term argument of a saturated constructor (zero-indexed)
    -- TODO (saturated constructors) the current, temporary, situation is that
    -- constructors have both type arguments (indices) and term arguments. To move
    -- into a term argument, one uses this @Movement@ type. To move into a *type*
    -- argument, use the special 'EnterConTypeArgument' action (it cannot be a
    -- movement because it changes from focusing on an expression to focusing on a
    -- type).
    ConChild Int
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Movement
  deriving anyclass (NFData)
