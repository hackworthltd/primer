{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Primer.Finite (Finite, getFinite, packFinite) where

import Foreword

import Data.Either.Extra (eitherToMaybe)
import Data.OpenApi (ToParamSchema (toParamSchema))
import GHC.TypeLits (type (<=))
import Numeric.Natural (Natural)
import Optics ((?~))
import Refined (FromTo, Refined, refine, unrefine)
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

newtype Finite l u = Finite {unFinite :: Refined (FromTo l u) Natural}

getFinite :: Finite l u -> Natural
getFinite = unrefine . unFinite

packFinite :: (KnownNat l, KnownNat u, l <= u) => Natural -> Maybe (Finite l u)
packFinite = fmap Finite . eitherToMaybe . refine

instance (KnownNat l, KnownNat u) => ToParamSchema (Finite l u) where
  toParamSchema _ =
    toParamSchema (Proxy @Natural)
      & #minimum ?~ fromIntegral (natVal $ Proxy @l)
      & #exclusiveMinimum ?~ False
      & #maximum ?~ fromIntegral (natVal $ Proxy @u)
      & #exclusiveMaximum ?~ False

instance (KnownNat l, KnownNat u, l <= u) => FromHttpApiData (Finite l u) where
  parseUrlPiece x = do
    y <- parseUrlPiece x
    let z = packFinite y
    let r = "[" <> show (natVal $ Proxy @l) <> "," <> show (natVal $ Proxy @u) <> "]"
    maybeToEither ("Value outside the range " <> r <> ": " <> x) z

instance ToHttpApiData (Finite l u) where
  toUrlPiece = toUrlPiece . getFinite
