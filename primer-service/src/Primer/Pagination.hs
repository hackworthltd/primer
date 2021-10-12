{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Primer.Pagination (
  PaginationParams,
  Pagination (..),
  -- 'Positive' is abstract. Do not export its constructor.
  Positive (getPositive),
  mkPositive,
) where

import Foreword

import Data.OpenApi (ToParamSchema, toParamSchema)
import Numeric.Natural (Natural)
import Optics ((?~))
import Servant (
  DefaultErrorFormatters,
  ErrorFormatters,
  FromHttpApiData (parseUrlPiece),
  HasContextEntry,
  HasServer (hoistServerWithContext, route),
  QueryParam,
  ServerT,
  (:>),
  type (.++),
 )
import Servant.OpenApi (HasOpenApi (toOpenApi))

-- | Guarantees its contents is strictly positive.
-- @getPositive x > 0@ is always true (because the only way to create one is
-- via the 'mkPositive' smart constructor.
newtype Positive = Pos {getPositive :: Natural}

mkPositive :: Natural -> Maybe Positive
mkPositive a = if a > 0 then Just (Pos a) else Nothing

instance FromHttpApiData Positive where
  parseUrlPiece x = parseUrlPiece x >>= maybeToEither ("Non-positive value: " <> x) . mkPositive

instance ToParamSchema Positive where
  toParamSchema _ = toParamSchema (Proxy @Natural) & #exclusiveMinimum ?~ True

data PaginationParams

-- How PaginationParams are implemented under the hood
type PP api =
  QueryParam "page" Positive
    :> QueryParam "pageSize" Positive
    :> api

data Pagination = Pagination
  { -- | Defaults to @1@ if not given in the query parameters
    page :: Positive
  , -- | Does not default, since there is no default that would work for all cases
    size :: Maybe Positive
  }

instance
  ( HasServer api context
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasServer (PaginationParams :> api) context
  where
  type ServerT (PaginationParams :> api) m = Pagination -> ServerT api m
  route _ cxt subserver = route (Proxy @(PP api)) cxt $ fmap f subserver
    where
      f handler mpage msize = handler $ Pagination{page = fromMaybe (Pos 1) mpage, size = msize}
  hoistServerWithContext _ proxyCxt nt handler = hoistServerWithContext (Proxy @api) proxyCxt nt . handler

instance HasOpenApi (PP api) => HasOpenApi (PaginationParams :> api) where
  toOpenApi _ = toOpenApi (Proxy @(PP api))
