module Primer.JSON (
  CustomJSON (..),
  PrimerJSON,
  ToJSON,
  FromJSON,
  ToJSONKey,
  FromJSONKey,
) where

import Data.Aeson (
  FromJSON,
  FromJSONKey,
  ToJSON,
  ToJSONKey,
 )
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Vanilla)

-- | A type for Primer API JSON encodings.
--
-- Note: at the moment, this makes no changes to the default
-- 'FromJSON' and 'ToJSON' encodings. We keep it a) to get the more
-- efficient generic encodings generated by @deriving-aeson@, and b)
-- to provide an escape hatch if we need to add custom encodings in
-- the future.
type PrimerJSON a = Vanilla a
