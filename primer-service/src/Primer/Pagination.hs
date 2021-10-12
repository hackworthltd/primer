{-# LANGUAGE NamedFieldPuns #-}
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
  pagedDefaultAll,
) where

import Foreword

import qualified Control.Monad.Catch.Pure as CatchPure
import Data.OpenApi (ToParamSchema, toParamSchema)
import Data.Pagination (mkPagination, paginate, paginatedItems)
import qualified Data.Pagination as P
import Numeric.Natural (Natural)
import Optics ((?~))
import Primer.API (OffsetLimit (OL, limit, offset), Paginated (extract, total))
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

-- | Extract a page, defaulting the page size to infinity (technically, the
-- total number of entries in the backing list).
pagedDefaultAll :: Functor m => Pagination -> Paginated m a -> m [a]
pagedDefaultAll pOpts pg =
  let total' = fromIntegral $ total pg
      sz = fromMaybe (Pos 1) $ size pOpts <|> mkPositive total'
      pOpts' = mkPosPagination sz (page pOpts)
      getElts' offset limit = extract pg $ OL{offset, limit}
   in paginatedItems <$> paginate pOpts' total' getElts'
