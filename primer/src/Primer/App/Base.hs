{-# LANGUAGE OverloadedLabels #-}

-- | Definitions needed to build the app.
-- These are not part of the core language, but we may want to use them in dependencies of 'Primer.App'.
module Primer.App.Base (
  Level (..),
  Editable (..),
  NodeType (..),
  Selection,
  Selection' (..),
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
type Selection = Selection' (Either ExprMeta TypeMeta)

data Selection' a = Selection
  { selectedDef :: GVarName
  -- ^ the ID of some ASTDef
  , selectedNode :: Maybe (NodeSelection a)
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (Selection' a)
  deriving anyclass (NFData)

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection a = NodeSelection
  { nodeType :: NodeType
  , meta :: a
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (NodeSelection a)
  deriving anyclass (NFData)

instance HasID a => HasID (NodeSelection a) where
  _id = lens (getID . meta) (flip $ over #meta . set _id)
