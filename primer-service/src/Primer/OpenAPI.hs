{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Foreword

import Data.Aeson (
  FromJSON,
  GFromJSON,
  GToEncoding,
  GToJSON,
  ToJSON,
  Zero,
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
import Data.Text qualified as T
import Data.Time (
  UTCTime (..),
  fromGregorian,
 )
import Deriving.Aeson (AesonOptions (aesonOptions))
import Optics (
  (?~),
 )
import Primer.API (
  ApplyActionBody,
  Def,
  EvalFullResp,
  Module,
  NewSessionReq,
  NodeBody,
  Prog,
  Selection,
  Tree,
  TypeDef,
  ValCon,
 )
import Primer.API qualified as API
import Primer.API.NodeFlavor (
  NodeFlavorBoxBody,
  NodeFlavorNoBody,
  NodeFlavorPrimBody,
  NodeFlavorTextBody,
 )
import Primer.API.RecordPair (RecordPair)
import Primer.Action.Available qualified as Available
import Primer.App (NodeSelection, NodeType)
import Primer.App.Base (Level)
import Primer.Core (
  GlobalName,
  GlobalNameKind (ADefName, ATyCon, AValCon),
  ID (..),
  LVarName,
  ModuleName,
  PrimCon,
  TyVarName,
 )
import Primer.Database (
  LastModified,
  Session,
  SessionName,
 )
import Primer.JSON (CustomJSON (CustomJSON), PrimerJSON)
import Primer.Name (Name)
import Servant.API (FromHttpApiData (parseQueryParam), ToHttpApiData (toQueryParam))

newtype PrimerJSONNamed (s :: Symbol) a = PrimerJSONNamed a
deriving via PrimerJSON a instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (PrimerJSONNamed s a)
deriving via PrimerJSON a instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (PrimerJSONNamed s a)

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

instance
  (Typeable a, Generic a, GToSchema (Rep a), KnownSymbol s) =>
  ToSchema (PrimerJSONNamed s a)
  where
  declareNamedSchema _ = rename (Just $ T.pack $ symbolVal $ Proxy @s) <$> declareNamedSchema (Proxy @(PrimerJSON a))

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
deriving via PrimerJSONNamed "GlobalName" (GlobalName 'ADefName) instance ToSchema (GlobalName 'ADefName)
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'ATyCon)
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'AValCon)

deriving via Name instance (ToSchema LVarName)
deriving via Name instance (ToSchema TyVarName)
deriving via PrimerJSON (RecordPair a b) instance (ToSchema a, ToSchema b) => ToSchema (RecordPair a b)
deriving via PrimerJSON Tree instance ToSchema Tree
deriving via PrimerJSON API.Name instance ToSchema API.Name
deriving via PrimerJSON PrimCon instance ToSchema PrimCon
deriving via PrimerJSON NodeBody instance ToSchema NodeBody
deriving via PrimerJSON NodeFlavorTextBody instance ToSchema NodeFlavorTextBody
deriving via PrimerJSON NodeFlavorPrimBody instance ToSchema NodeFlavorPrimBody
deriving via PrimerJSON NodeFlavorBoxBody instance ToSchema NodeFlavorBoxBody
deriving via PrimerJSON NodeFlavorNoBody instance ToSchema NodeFlavorNoBody
deriving via PrimerJSON TypeDef instance ToSchema TypeDef
deriving via PrimerJSON ValCon instance ToSchema ValCon
deriving via PrimerJSON Def instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
deriving via PrimerJSON Module instance ToSchema Module
deriving via PrimerJSON Prog instance ToSchema Prog
deriving via PrimerJSON Available.NoInputAction instance ToSchema Available.NoInputAction
deriving via PrimerJSON Available.InputAction instance ToSchema Available.InputAction
deriving via PrimerJSON Available.Option instance ToSchema Available.Option
deriving via PrimerJSON Available.FreeInput instance ToSchema Available.FreeInput
deriving via PrimerJSON Available.Options instance ToSchema Available.Options
deriving via PrimerJSON Available.Action instance ToSchema Available.Action
deriving via PrimerJSON ApplyActionBody instance ToSchema ApplyActionBody
deriving via PrimerJSONNamed "Selection" Selection instance ToSchema Selection
deriving via PrimerJSONNamed "NodeSelection" (NodeSelection ID) instance ToSchema (NodeSelection ID)
deriving via PrimerJSON NodeType instance ToSchema NodeType
deriving via PrimerJSON Level instance ToSchema Level
deriving via PrimerJSON NewSessionReq instance ToSchema NewSessionReq

deriving anyclass instance ToParamSchema Available.NoInputAction
instance FromHttpApiData Available.NoInputAction where
  parseQueryParam = parseQueryParamRead "action"
instance ToHttpApiData Available.NoInputAction where
  toQueryParam = show
deriving anyclass instance ToParamSchema Available.InputAction
instance FromHttpApiData Available.InputAction where
  parseQueryParam = parseQueryParamRead "action"
instance ToHttpApiData Available.InputAction where
  toQueryParam = show
deriving anyclass instance ToParamSchema Level
instance FromHttpApiData Level where
  parseQueryParam = parseQueryParamRead "level"
instance ToHttpApiData Level where
  toQueryParam = show
parseQueryParamRead :: Read a => Text -> Text -> Either Text a
parseQueryParamRead m t = maybeToEither ("unknown " <> m <> ": " <> t) $ readMaybe t

deriving via PrimerJSON EvalFullResp instance ToSchema EvalFullResp
