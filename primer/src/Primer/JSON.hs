module Primer.JSON (
  CustomJSON (..),
  PrimerJSON,
  ToJSON,
  FromJSON,
  ToJSONKey,
  FromJSONKey,
) where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Deriving.Aeson

-- | A type for 'Primer' style JSON encoding.
-- All our types use this, so we generate consistent encodings for the whole
-- API.
-- This is designed to be compatible with purescript-foreign-generic.
type PrimerJSON a = CustomJSON '[NoAllNullaryToStringTag] a
