{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Primer.Pagination (
  PaginationParams,
  Pagination (..),
  -- Constructor and field accessors of Pagination exported for testing
  Paginated (..),
  -- 'Positive' is abstract. Do not export its constructor.
  Positive (getPositive),
  mkPositive,
  pagedDefaultClamp,
  -- the following are exposed for testing
  PaginatedMeta (PM),
  totalItems,
  pageSize,
  firstPage,
  prevPage,
  thisPage,
  nextPage,
  lastPage,
  getNonNeg,
  NonNeg,
  mkNonNeg,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema, declareNamedSchema, toParamSchema)
import Data.OpenApi.Internal.Schema (plain)
import Optics ((?~))
import Primer.Database (
  OffsetLimit (OL, limit, offset),
  Page (Page, pageContents, total),
  Session,
 )
import Primer.OpenAPI ()
import Servant (
  DefaultErrorFormatters,
  ErrorFormatters,
  FromHttpApiData (parseUrlPiece),
  HasContextEntry,
  HasServer (hoistServerWithContext, route),
  QueryParam,
  ServerT,
  ToHttpApiData (toUrlPiece),
  (:>),
  type (.++),
 )
import Servant.Client.Core (
  HasClient (
    Client,
    clientWithRoute,
    hoistClientMonad
  ),
 )
import Servant.OpenApi (HasOpenApi (toOpenApi))

-- | Guarantees its contents is strictly positive.
-- @getPositive x > 0@ is always true (because the only way to create one is
-- via the 'mkPositive' smart constructor.
newtype Positive = Pos {getPositive :: Int}
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

mkPositive :: Int -> Maybe Positive
mkPositive a = if a > 0 then Just (Pos a) else Nothing

instance FromHttpApiData Positive where
  parseUrlPiece x = parseUrlPiece x >>= maybeToEither ("Non-positive value: " <> x) . mkPositive

instance ToHttpApiData Positive where
  toUrlPiece = toUrlPiece . getPositive

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
  { page :: Positive
  -- ^ Defaults to @1@ if not given in the query parameters
  , size :: Maybe Positive
  -- ^ Does not default, since there is no default that would work for all cases
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

instance
  HasClient m api =>
  HasClient m (PaginationParams :> api)
  where
  type
    Client m (PaginationParams :> api) =
      Pagination -> Client m api
  clientWithRoute pm _ req pag = clientWithRoute pm (Proxy @(PP api)) req mpage msize
    where
      mpage = Just $ page pag
      msize = size pag
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy @api) f . cl

instance HasOpenApi (PP api) => HasOpenApi (PaginationParams :> api) where
  toOpenApi _ = toOpenApi (Proxy @(PP api))

-- | An endpoint returning paginated data will also return some metadata.
-- 'Paginated' wraps up both the data and metadata.
data Paginated a = Paginated
  { meta :: PaginatedMeta
  , items :: [a]
  }
  deriving (Generic, Show)

-- We may well need more instances than just Paginated Session in the future.
-- However, giving polymorphic `instance To... (Paginated a)` can generate
-- a schema inconsistent with the ToJSON for some 'a'.
-- This happens because aeson and openapi3 differ in their special handling
-- for lists (e.g. to serialise strings as strings rather than arrays of
-- characters). In particular the instance for 'Paginated Char' is broken.
-- See https://github.com/biocad/openapi3/issues/58
-- We prefer to explicitly list the particular instances we need, rather
-- than having a known broken polymorphic instance, even if we expect to
-- never hit the broken case.
instance ToJSON (Paginated Session)
instance FromJSON (Paginated Session)
instance ToSchema (Paginated Session)

-- Used solely for nice bounds in schema
newtype NonNeg = NonNeg Int
  deriving newtype (FromJSON, ToJSON, Show)
instance ToParamSchema NonNeg where
  toParamSchema _ = toParamSchema (Proxy @Int) & #minimum ?~ 0
instance ToSchema NonNeg where
  declareNamedSchema = plain . toParamSchema

getNonNeg :: NonNeg -> Int
getNonNeg (NonNeg i) = i

-- For testing purposes
mkNonNeg :: Int -> Maybe NonNeg
mkNonNeg a = if a >= 0 then Just (NonNeg a) else Nothing

data PaginatedMeta = PM
  { totalItems :: NonNeg
  , pageSize :: Positive
  , firstPage :: Positive
  , prevPage :: Maybe Positive
  , thisPage :: Positive
  , nextPage :: Maybe Positive
  , lastPage :: Positive
  }
  deriving (Generic, Show)
instance FromJSON PaginatedMeta
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
