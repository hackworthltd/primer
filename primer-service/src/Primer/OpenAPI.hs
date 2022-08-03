{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)
import Data.OpenApi.Internal.Schema (GToSchema)
import Deriving.Aeson (AesonOptions (aesonOptions))
import Primer.API (Def, Module, NodeBody, NodeFlavor, Prog, Tree)
import Primer.Core (GlobalName, ID (..), LVarName, ModuleName)
import Primer.Database (Session, SessionName)
import Primer.JSON (CustomJSON)
import Primer.Name (Name)

import Foreword

-- $orphanInstances
--
-- We define some OpenApi orphan instances in primer-service, to avoid
-- pulling in the openapi3 dependency into primer core. This is necessary to
-- build primer with ghcjs, because openapi3 transitively depends on network,
-- which ghcjs currently cannot build.

-- Suitable for deriving via, when the ToJSON instance is via PrimerJSON
instance
  (Typeable a, Generic a, GToSchema (Rep a), Typeable os, Typeable ks, AesonOptions os) =>
  ToSchema (CustomJSON (os :: ks) a)
  where
  declareNamedSchema _ = genericDeclareNamedSchema (fromAesonOptions (aesonOptions @os)) (Proxy @a)

instance ToSchema SessionName
instance ToSchema Session

-- We need to GND the ID instance to match its To/FromJSON instances
deriving newtype instance ToSchema ID

-- We can't GND derive for Name as it is an opaque type
-- But the JSON instance is done by GND, so we must match here...
-- This instance works because the parameter has a phantom role!
deriving via Text instance (ToSchema Name)

-- For GlobalName and LVarName, we must derive ToSchema via Name,
-- as that is how the To/FromJSON instances are derived
deriving via Name instance Typeable k => ToSchema (GlobalName k)
deriving via Name instance (ToSchema LVarName)
instance ToSchema Tree
instance ToSchema NodeBody
instance ToSchema NodeFlavor
instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
instance ToSchema Module
instance ToSchema Prog
