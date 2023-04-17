{-# LANGUAGE OverloadedLabels #-}

-- | Definitions needed to build the app.
-- These are not part of the core language, but we may want to use them in dependencies of 'Primer.App'.
module Primer.App.Base (
  Level (..),
  Editable (..),
  NodeType (..),
  Selection (..),
  NodeSelection (..),
) where

import Protolude

import Data.Data (Data)
import Optics
import Primer.Core (
  ExprMeta,
  GVarName,
  HasID (..),
  TypeMeta,
  getID,
 )
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

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: GVarName
  -- ^ the ID of some ASTDef
  , selectedNode :: Maybe NodeSelection
  }
  deriving stock (Eq, Show, Read, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON Selection
  deriving anyclass (NFData)

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , meta :: Either ExprMeta TypeMeta
  }
  deriving stock (Eq, Show, Read, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON NodeSelection
  deriving anyclass (NFData)

instance HasID NodeSelection where
  _id =
    lens
      (either getID getID . meta)
      (flip $ \id -> over #meta $ bimap (set _id id) (set _id id))
