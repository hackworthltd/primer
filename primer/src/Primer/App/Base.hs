-- | Definitions needed to build the app.
-- These are not part of the core language, but we may want to use them in dependencies of 'Primer.App'.
module Primer.App.Base (
  Level (..),
  Editable (..),
  NodeType (..),
) where

import Protolude

import Data.Data (Data)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )

-- | The current programming "level". This setting determines which
-- actions are displayed to the student, the labels on UI elements,
-- etc.
data Level
  = -- | Bare minimum features to define sum types, and functions on
    -- those types using simple pattern matching.
    Beginner
  | -- | Function application & monomorphic HoF. (Support for the latter
    -- should probably be split into a separate level.)
    Intermediate
  | -- | All features.
    Expert
  deriving stock (Eq, Read, Show, Enum, Bounded, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Level

data Editable = Editable | NonEditable
  deriving stock (Bounded, Enum, Show)

data NodeType = BodyNode | SigNode
  deriving stock (Eq, Show, Read, Bounded, Enum, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON NodeType
  deriving anyclass (NFData)
