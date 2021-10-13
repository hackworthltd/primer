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
) where

import Foreword

import qualified Control.Monad.Catch.Pure as CatchPure
import Data.Aeson (ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema, toParamSchema)
import Data.Pagination (mkPagination, paginate, paginatedItems)
import qualified Data.Pagination as P
import Numeric.Natural (Natural)
import Optics ((?~))
import Primer.API (OffsetLimit (OL, limit, offset), extract, total)
import qualified Primer.API as API
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
  parseUrlPiece x = do
    i :: Integer <- parseUrlPiece x
    if i > 0
      then Right $ Pos (fromInteger i)
      else Left $ "Non-positive value: " <> x

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

-- | An endpoint returning paginated data will also return some metadata.
-- 'Paginated' wraps up both the data and metadata.
data Paginated a = Paginated
  { meta :: PaginatedMeta
  , items :: [a]
  }
  deriving (Generic)

instance ToJSON a => ToJSON (Paginated a)
instance ToSchema a => ToSchema (Paginated a)

-- Many of the fields of PaginatedMeta "should" be Positive,
-- but that is not exposed by the pagination package.
data PaginatedMeta = PM
  { totalItems, pageSize, firstPage :: Natural
  , prevPage :: Maybe Natural
  , thisPage :: Natural
  , nextPage :: Maybe Natural
  , lastPage :: Natural
  }
  deriving (Generic)
instance ToJSON PaginatedMeta
instance ToSchema PaginatedMeta

-- | Take arguments @pageSize@ and @pageIndex@. Wraps 'mkPagination' in a
-- exception-free interface when the inputs are known to be positive.
mkPosPagination :: Positive -> Positive -> P.Pagination
mkPosPagination sz idx =
  -- It is awkward that mkPagination throws errors since we know they will not be
  -- thrown in this case.
  let pg' = mkPagination (getPositive sz) (getPositive idx)
      pg'' =
        pg' `CatchPure.catch` \case
          P.ZeroPageSize -> error "Positive Natural cannot be zero"
          P.ZeroPageIndex -> error "Positive Natural cannot be zero"
   in case CatchPure.runCatch pg'' of
        Left _ -> error "mkPagination should only throw PaginationException, and will not for positive inputs"
        Right pg -> pg

-- | Extract a page, defaulting and clamping the page size to the given
-- argument (or @1@, if the clamp is @0@).
pagedDefaultClamp ::
  Functor m =>
  Natural ->
  Pagination ->
  API.Paginated m a ->
  m (Paginated a)
pagedDefaultClamp def pOpts pg =
  let total' = fromIntegral $ total pg
      sz =
        fromMaybe (Pos 1) $
          (guarded ((<= def) . getPositive) =<< size pOpts)
            <|> mkPositive def
      pOpts' = mkPosPagination sz (page pOpts)
      getElts' offset limit = extract pg $ OL{offset, limit}
   in do
        paged <- paginate pOpts' total' getElts'
        pure $
          Paginated
            { items = paginatedItems paged
            , meta =
                let i = P.pageIndex $ P.paginatedPagination paged
                 in PM
                      { totalItems = P.paginatedItemsTotal paged
                      , pageSize = P.pageSize $ P.paginatedPagination paged
                      , firstPage = 1
                      , prevPage = if P.hasPrevPage paged then Just $ i - 1 else Nothing
                      , thisPage = i
                      , nextPage = if P.hasNextPage paged then Just $ i + 1 else Nothing
                      , lastPage = P.paginatedPagesTotal paged
                      }
            }
