{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Foreword

import Data.OpenApi (ToParamSchema, ToSchema (declareNamedSchema), fromAesonOptions, genericDeclareNamedSchema)
import Data.OpenApi.Internal.Schema (GToSchema, rename)
import Data.Text qualified as T
import Deriving.Aeson (AesonOptions (aesonOptions))
import Primer.API (Def, ExprTreeOpts, Module, NodeBody, NodeFlavor, OfferedAction, Prog, Tree)
import Primer.Action (ActionName, ActionType, Level (..))
import Primer.App (Editable (..))
import Primer.Core (
  GlobalName,
  GlobalNameKind (ADefName, ATyCon, AValCon),
  ID (..),
  LVarName,
  ModuleName,
 )
import Primer.Database (Session, SessionName)
import Primer.JSON (CustomJSON, PrimerJSON)
import Primer.Name (Name (..))
import Servant (FromHttpApiData)
import Servant.API (FromHttpApiData (parseUrlPiece))

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

-- TODO vias don't much instance - weird?
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'ATyCon)
deriving via GlobalName 'ADefName instance ToSchema (GlobalName 'AValCon)

-- deriving via Name instance Typeable a => (ToSchema (GlobalName a))

deriving via Name instance (ToSchema LVarName)
deriving via PrimerJSON Tree instance ToSchema Tree
deriving via PrimerJSON NodeBody instance ToSchema NodeBody
deriving via PrimerJSON NodeFlavor instance ToSchema NodeFlavor
deriving via PrimerJSON Def instance ToSchema Def
deriving via NonEmpty Name instance ToSchema ModuleName
deriving via PrimerJSON Module instance ToSchema Module
deriving via PrimerJSON Prog instance ToSchema Prog
deriving via PrimerJSON ExprTreeOpts instance ToSchema ExprTreeOpts
deriving via PrimerJSON OfferedAction instance ToSchema OfferedAction
deriving via PrimerJSON ActionName instance ToSchema ActionName
deriving via PrimerJSON ActionType instance ToSchema ActionType
deriving via PrimerJSON Level instance ToSchema Level
deriving via PrimerJSON Editable instance ToSchema Editable

deriving anyclass instance ToParamSchema Level

-- deriving anyclass instance FromHttpApiData Level
deriving anyclass instance ToParamSchema Editable
deriving newtype instance ToParamSchema ID
deriving via Text instance ToParamSchema Name

-- TODO this class should be derivable for enums at least - https://github.com/haskell-servant/servant/issues/1014
instance FromHttpApiData Editable where
  parseUrlPiece = maybeToEither "no read" . readMaybe . T.unpack
deriving instance Read Editable
instance FromHttpApiData Level where
  parseUrlPiece = maybeToEither "no read" . readMaybe . T.unpack
deriving instance Read Level

deriving newtype instance FromHttpApiData Name
deriving newtype instance FromHttpApiData ID

-- deriving via Text instance FromHttpApiData Name

-- deriving via PrimerJSON (OfferedAction a) instance ToSchema a => ToSchema (OfferedAction a)

-- deriving via PrimerJSON ProgAction instance ToSchema ProgAction
-- deriving via PrimerJSON APIProgAction instance ToSchema APIProgAction

-- instance ToSchema (Type' ()) where
--   declareNamedSchema = undefined
-- instance ToSchema ASTTypeDef where
--   declareNamedSchema = undefined
-- instance ToSchema SmartHoles where
--   declareNamedSchema = undefined

-- instance ToSchema (Off) where
--   declareNamedSchema = undefined
