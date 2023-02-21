module Primer.API.RecordPair (
  RecordPair (..),
) where

import Foreword

import Primer.JSON (CustomJSON (CustomJSON), PrimerJSON, ToJSON)

-- | Isomorphic to `(a, b)`, but serialized as an object.
--
-- This avoids a limitation of OpenAPI 3.0, which can't handle Aeson's tuple encoding.
-- See https://github.com/biocad/openapi3/issues/31 for details.
--
-- Note that when this limitation is hopefully relaxed, we may not wish to replace all use cases with
-- `(a, b)`. In the case of constructors with a single field `C (RecordPair a b)`, we'd want the
-- simpler change to `C a b` rather than `C (a, b)`.
data RecordPair a b = RecordPair {fst :: a, snd :: b}
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON) via PrimerJSON (RecordPair a b)
