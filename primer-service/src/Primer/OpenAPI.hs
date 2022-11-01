{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Foreword

import Data.Aeson (
  toJSON,
 )
import Data.OpenApi (
  ToParamSchema (toParamSchema),
  ToSchema (declareNamedSchema),
  fromAesonOptions,
  genericDeclareNamedSchema,
 )
import Data.OpenApi.Internal.ParamSchema (
  timeParamSchema,
 )
import Data.OpenApi.Internal.Schema (
  GToSchema,
  named,
  rename,
  timeSchema,
 )
import Data.Time (
  UTCTime (..),
  fromGregorian,
 )
import Deriving.Aeson (AesonOptions (aesonOptions))
import Optics (
  (?~),
 )
import Primer.API (ApplyActionBody, Def, ExprTreeOpts, Module, NodeBody, NodeFlavor, NodeSelection (..), Prog, Selection (..), Tree)
import Primer.Action.Available (
  ActionOption,
  ActionOptions,
  InputAction (..),
  Level (..),
  NoInputAction (..),
  OfferedAction,
 )
import Primer.Core (
  GlobalName,
  GlobalNameKind (ADefName, ATyCon, AValCon),
  ID (..),
  LVarName,
  ModuleName,
  NodeType,
 )
import Primer.Database (
  LastModified,
  Session,
  SessionName,
 )
import Primer.JSON (CustomJSON, PrimerJSON)
import Primer.Name (Name)
import Servant.API (FromHttpApiData (parseQueryParam))

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
deriving via PrimerJSON Session instance ToSchema Session

-- | The whole point of this newtype is to get the correct
-- 'toParamSchema' for our UTC timestamps.
--
-- Cribbed from:
-- https://hackage.haskell.org/package/openapi3-3.2.2/docs/src/Data.OpenApi.Internal.Schema.html#line-662
instance ToSchema LastModified where
  declareNamedSchema _ =
    pure $
      named "LastModified" $
        timeSchema "date-time"
          & #example ?~ toJSON (UTCTime (fromGregorian 2022 10 20) 0)

instance ToParamSchema LastModified where
  toParamSchema _ = timeParamSchema "date-time"

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

-- TODO check for unnecessary instances
deriving via Name instance (ToSchema LVarName)
deriving via PrimerJSON Tree instance ToSchema Tree
deriving via PrimerJSON NodeBody instance ToSchema NodeBody
deriving via PrimerJSON NodeFlavor instance ToSchema NodeFlavor
deriving via PrimerJSON Def instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
deriving via PrimerJSON Module instance ToSchema Module
deriving via PrimerJSON Prog instance ToSchema Prog
deriving via PrimerJSON ExprTreeOpts instance ToSchema ExprTreeOpts
deriving via PrimerJSON NoInputAction instance ToSchema NoInputAction
deriving via PrimerJSON InputAction instance ToSchema InputAction
deriving via PrimerJSON ActionOption instance ToSchema ActionOption
deriving via PrimerJSON ActionOptions instance ToSchema ActionOptions
deriving via PrimerJSON OfferedAction instance ToSchema OfferedAction
deriving via PrimerJSON Selection instance ToSchema Selection
deriving via PrimerJSON ApplyActionBody instance ToSchema ApplyActionBody
deriving via PrimerJSON NodeSelection instance ToSchema NodeSelection
deriving via PrimerJSON NodeType instance ToSchema NodeType
deriving via PrimerJSON Level instance ToSchema Level

-- TODO can we derive these (and thus remove the `Read` instances)?
deriving instance ToParamSchema NoInputAction
instance FromHttpApiData NoInputAction where
  parseQueryParam t = maybeToEither ("unknown NoInputAction: " <> t) $ readMaybe t
deriving instance ToParamSchema InputAction
instance FromHttpApiData InputAction where
  parseQueryParam t = maybeToEither ("unknown InputAction: " <> t) $ readMaybe t
deriving instance ToParamSchema Level
instance FromHttpApiData Level where
  parseQueryParam t = maybeToEither ("unknown level: " <> t) $ readMaybe t
