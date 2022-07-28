module Primer.JSON (
  CustomJSON (..),
  PrimerJSON,
  PrimerJSONPrefix,
  ToJSON,
  FromJSON,
  ToJSONKey,
  FromJSONKey,
) where

import Foreword

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List (stripPrefix)
import Deriving.Aeson (
  CustomJSON (..),
  FieldLabelModifier,
  FromJSON,
  StringModifier (..),
  ToJSON,
 )
import Deriving.Aeson.Stock (
  Vanilla,
 )

-- | A type for 'Primer' style JSON encoding.
type PrimerJSON a = Vanilla a

-- | Like 'PrimerJSON', but strips the given prefix from field names.
--
-- This type should only be used for records whose fields all begin with
-- the given prefix, followed by a capital letter.
type PrimerJSONPrefix prefix a = CustomJSON '[FieldLabelModifier (StripPrefixAndStartLowercase prefix)] a

data StripPrefixAndStartLowercase prefix

instance KnownSymbol prefix => StringModifier (StripPrefixAndStartLowercase prefix) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @prefix)) s of
    Just "" -> ""
    Just (c : s') -> toLower c : s'
    Nothing -> s
