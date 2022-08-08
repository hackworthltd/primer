{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Data.OpenApi (ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)
import Data.OpenApi.Internal.Schema (GToSchema, rename)
import Deriving.Aeson (AesonOptions (aesonOptions))
import Primer.API (Def, Module, NodeBody, NodeFlavor, Prog, Tree)
import Primer.Core (
  GlobalName,
  GlobalNameKind (ADefName, ATyCon, AValCon),
  ID (..),
  LVarName,
  ModuleName,
 )
import Primer.Database (Session, SessionName)
import Primer.JSON (CustomJSON, PrimerJSON)
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

-- For GlobalNames, we know the tag is just phantom type information
-- and they all serialise in the same way. We collapse the distinction
-- at the openapi level, so api consumers do not have to deal with
-- three identical types. Note that our openapi interface is a
-- simplified view, so this collapse is in the correct spirit.
instance ToSchema (GlobalName 'ADefName) where
  declareNamedSchema _ = rename (Just "GlobalName") <$> declareNamedSchema (Proxy @(PrimerJSON (GlobalName 'ADefName)))
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'ATyCon)
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'AValCon)

deriving via Name instance (ToSchema LVarName)
instance ToSchema Tree
instance ToSchema NodeBody
instance ToSchema NodeFlavor
instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
instance ToSchema Module
instance ToSchema Prog
