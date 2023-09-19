{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Definitions needed to build the app.
-- These are not part of the core language, but we may want to use them in dependencies of 'Primer.App'.
module Primer.App.Base (
  Level (..),
  Editable (..),
  NodeType (..),
  Selection,
  Selection' (..),
  TypeDefSelection (..),
  TypeDefNodeSelection (..),
  TypeDefParamSelection (..),
  TypeDefConsSelection (..),
  TypeDefConsFieldSelection (..),
  DefSelection (..),
  NodeSelection (..),
  getTypeDefConFieldType,
) where

import Protolude

import Data.Data (Data)
import Optics
import Primer.Core (
  ExprMeta,
  GVarName,
  HasID (..),
  KindMeta,
  TyConName,
  TyVarName,
  Type',
  TypeMeta,
  ValConName,
  getID,
 )
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.TypeDef (ASTTypeDef, ValCon (..), astTypeDefConstructors)

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

-- | Describes which element of a (type or term) definition the student has selected.
-- We have the following invariant: when this contains a `NodeSelection` with @nodeType = SigNode@,
-- or any `TypeDefConsFieldSelection`, then they will always have @meta = Right _@.
type Selection = Selection' (Either ExprMeta (Either TypeMeta KindMeta))

data Selection' a
  = SelectionDef (DefSelection a)
  | SelectionTypeDef (TypeDefSelection a)
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (Selection' a)
  deriving anyclass (NFData)

-- | Some element of a type definition.
data TypeDefSelection a = TypeDefSelection
  { def :: TyConName
  , node :: Maybe (TypeDefNodeSelection a)
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDefSelection a)
  deriving anyclass (NFData)

-- | Some element in a type definition, other than simply the definition itself.
data TypeDefNodeSelection a
  = TypeDefParamNodeSelection (TypeDefParamSelection a)
  | TypeDefConsNodeSelection (TypeDefConsSelection a)
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDefNodeSelection a)
  deriving anyclass (NFData)

-- | Some element of a definition of a type parameter.
data TypeDefParamSelection a = TypeDefParamSelection
  { param :: TyVarName
  , kindMeta :: Maybe a
  -- ^ `Nothing` indicates that the parameter name is selected, `Just` means a node in its kind.
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDefParamSelection a)
  deriving anyclass (NFData)

-- | Some element of a definition of a constructor.
data TypeDefConsSelection a = TypeDefConsSelection
  { con :: ValConName
  , field :: Maybe (TypeDefConsFieldSelection a)
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDefConsSelection a)
  deriving anyclass (NFData)

-- | Some element of a field in the definition of a constructor.
data TypeDefConsFieldSelection a = TypeDefConsFieldSelection
  { index :: Int
  , meta :: a
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDefConsFieldSelection a)
  deriving anyclass (NFData)

-- | Some element of a term definition.
data DefSelection a = DefSelection
  { def :: GVarName
  , node :: Maybe (NodeSelection a)
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (DefSelection a)
  deriving anyclass (NFData)

-- | Some element of a node, in the body or type signature of a term definition.
data NodeSelection a = NodeSelection
  { nodeType :: NodeType
  , meta :: a
  }
  deriving stock (Eq, Show, Read, Functor, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON (NodeSelection a)
  deriving anyclass (NFData)

instance HasID a => HasID (NodeSelection a) where
  _id = lens (getID . (.meta)) (flip $ over #meta . set _id)

getTypeDefConFieldType :: ASTTypeDef a b -> ValConName -> Int -> Maybe (Type' a b)
getTypeDefConFieldType def con index =
  flip atMay index . valConArgs
    =<< find ((== con) . valConName) (astTypeDefConstructors def)
