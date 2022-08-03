{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Data.OpenApi (SchemaOptions, fromAesonOptions)
import Data.OpenApi.Internal.Schema
import Deriving.Aeson (AesonOptions (aesonOptions))
import Foreword
import Primer.API (Def, Module, NodeBody, NodeFlavor, Prog, Tree)
import Primer.Core (GlobalName, ID (..), LVarName, ModuleName)
import Primer.Database (Session, SessionName)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (Name)

-- $orphanInstances
--
-- We define some OpenApi orphan instances in primer-service, to avoid
-- pulling in the openapi3 dependency into primer core. This is necessary to
-- build primer with ghcjs, because openapi3 transitively depends on network,
-- which ghcjs currently cannot build.

-- TODO: this compiles, but is it correct?

-- Suitable for deriving via, when ToJSON is via the custom json
-- instance (AesonOptions os, ToSchema a, Typeable os, Typeable ks) => ToSchema (CustomJSON (os :: ks) a) where
--  declareNamedSchema = genericDeclareNamedSchemaNewtype opts (declareNamedSchema @a)
instance
  (Typeable a, Generic a, GToSchema (Rep a), Typeable os, Typeable ks, AesonOptions os) =>
  ToSchema (CustomJSON (os :: ks) a)
  where
  declareNamedSchema _ = genericDeclareNamedSchema opts (Proxy @a)
    where
      opts :: SchemaOptions
      opts = fromAesonOptions (aesonOptions @os)

-- Technically should GND this, but cannot because of exports...
-- instance ToSchema SessionName
deriving via Text instance ToSchema SessionName -- see comments on instance ToSchema Name
deriving via PrimerJSON Session instance ToSchema Session

-- We need to GND the ID instance to match its To/FromJSON instances
deriving newtype instance ToSchema ID

-- We can't GND derive for Name as it is an opaque type
-- But the JSON instance is done by GND, so we must match here...
-- This instance works because the parameter has a phantom role!
deriving via Text instance (ToSchema Name)

-- For GlobalName and LVarName, we must derive ToSchema via Name,
-- as that is how the To/FromJSON instances are derived
-- deriving via Name instance Typeable k => ToSchema (GlobalName k)
instance Typeable k => ToSchema (GlobalName k) -- actually, just plain ToJSON here...
deriving via Name instance (ToSchema LVarName)

-- These are just plain ToJSON
instance ToSchema Tree
instance ToSchema NodeBody
instance ToSchema NodeFlavor
instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
instance ToSchema Module
instance ToSchema Prog
