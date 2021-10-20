{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Primer.Pagination (
  PaginationParams,
  Pagination (..),
  Paginated,
  -- 'Positive' is abstract. Do not export its constructor.
  Positive (getPositive),
  mkPositive,
  pagedDefaultClamp,
  -- the following are exposed for testing
  meta,
  totalItems,
  pageSize,
  firstPage,
  prevPage,
  thisPage,
  nextPage,
  lastPage,
  getNonNeg,
  items,
) where

import Foreword

import Data.Aeson (ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema, declareNamedSchema, toParamSchema)
import Data.OpenApi.Internal.Schema (plain)
import Optics ((?~))
import Primer.Database (
  OffsetLimit (OL, limit, offset),
  Page (Page, pageContents, total),
 )
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
newtype Positive = Pos {getPositive :: Int}
  deriving (Eq, Ord)
  deriving newtype (ToJSON)

mkPositive :: Int -> Maybe Positive
mkPositive a = if a > 0 then Just (Pos a) else Nothing

instance FromHttpApiData Positive where
  parseUrlPiece x = parseUrlPiece x >>= maybeToEither ("Non-positive value: " <> x) . mkPositive

instance ToParamSchema Positive where
  toParamSchema _ =
    toParamSchema (Proxy @Int)
      & #minimum ?~ 0
      & #exclusiveMinimum ?~ True

instance ToSchema Positive where
  declareNamedSchema = plain . toParamSchema

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

-- | An endpoint returning paginated data will also return some metadata.
-- 'Paginated' wraps up both the data and metadata.
data Paginated a = Paginated
  { meta :: PaginatedMeta
  , items :: [a]
  }
  deriving (Generic)

instance ToJSON a => ToJSON (Paginated a)
instance ToSchema a => ToSchema (Paginated a)

-- Used solely for nice bounds in schema
newtype NonNeg = NonNeg Int
  deriving newtype (ToJSON)
instance ToParamSchema NonNeg where
  toParamSchema _ = toParamSchema (Proxy @Int) & #minimum ?~ 0
instance ToSchema NonNeg where
  declareNamedSchema = plain . toParamSchema

getNonNeg :: NonNeg -> Int
getNonNeg (NonNeg i) = i

data PaginatedMeta = PM
  { totalItems :: NonNeg
  , pageSize :: Positive
  , firstPage :: Positive
  , prevPage :: Maybe Positive
  , thisPage :: Positive
  , nextPage :: Maybe Positive
  , lastPage :: Positive
  }
  deriving (Generic)
instance ToJSON PaginatedMeta
instance ToSchema PaginatedMeta

-- | Run a paginated query, where we default an unspecified pageSize to the
-- given 'Int' (or @1@ if non-positive), similarly we clamp a given pageSize to
-- be at most the given 'Int'.
pagedDefaultClamp ::
  Functor m =>
  Int ->
  Pagination ->
  (OffsetLimit -> m (Page a)) ->
  m (Paginated a)
pagedDefaultClamp def' pg f =
  let def = fromMaybe (Pos 1) $ mkPositive def'
      sz = fromMaybe def $ guarded (<= def) =<< size pg
      sz' = getPositive sz
      pageIdx = getPositive (page pg)
      off = sz' * (pageIdx - 1)
      thePage = f $ OL{offset = off, limit = Just sz'}
      info Page{total, pageContents} =
        let (full, leftover) = total `quotRem` sz'
            lastPage = full + if leftover == 0 then 0 else 1
         in Paginated
              { meta =
                  PM
                    { totalItems = NonNeg (max 0 total)
                    , pageSize = sz
                    , firstPage = Pos 1
                    , prevPage = mkPositive =<< guarded (>= 1) (pageIdx - 1)
                    , thisPage = page pg
                    , nextPage = mkPositive =<< guarded (<= lastPage) (pageIdx + 1)
                    , -- if there are no items, we report firstPage = lastPage = 1
                      lastPage = fromMaybe (Pos 1) $ mkPositive lastPage
                    }
              , items = pageContents
              }
   in info <$> thePage
