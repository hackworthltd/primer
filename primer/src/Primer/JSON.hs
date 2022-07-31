module Primer.JSON (CustomJSON (..), PrimerJSON, PrimerJSONPrefix, ToJSON, FromJSON, ToJSONKey, FromJSONKey) where

import Foreword

import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List (stripPrefix)
import Deriving.Aeson

-- | A type for 'Primer' style JSON encoding.
-- All our types use this, so we generate consistent encodings for the whole
-- API.
-- This is designed to be compatible with purescript-foreign-generic.
type PrimerJSON a = CustomJSON '[NoAllNullaryToStringTag] a

-- | Like 'PrimerJSON' but strips the given prefix from field names.
-- Useful if your Haskell fields are fooA, fooB and you want the corresponding purescript
-- record to use a, b.
-- This should only be used for records whose fields all begin with the given prefix, followed by a
-- capital letter.
type PrimerJSONPrefix prefix a = CustomJSON '[NoAllNullaryToStringTag, FieldLabelModifier (StripPrefixAndStartLowercase prefix)] a

data StripPrefixAndStartLowercase prefix

instance KnownSymbol prefix => StringModifier (StripPrefixAndStartLowercase prefix) where
  getStringModifier s = case stripPrefix (symbolVal (Proxy @prefix)) s of
    Just "" -> ""
    Just (c : s') -> toLower c : s'
    Nothing -> s
