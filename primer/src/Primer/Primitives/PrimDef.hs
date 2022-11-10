{-# LANGUAGE DeriveAnyClass #-}

module Primer.Primitives.PrimDef (PrimDef (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642

import Foreword

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Data)
import Primer.JSON (CustomJSON (..), PrimerJSON)

-- | A primitive, built-in definition.
-- For each of these, we should have a test that the evaluator produces expected results.
data PrimDef
  = ToUpper
  | IsSpace
  | HexToNat
  | NatToHex
  | EqChar
  | IntAdd
  | IntMinus
  | IntMul
  | IntQuotient
  | IntRemainder
  | IntQuot
  | IntRem
  | IntLT
  | IntLTE
  | IntGT
  | IntGTE
  | IntEq
  | IntNeq
  | IntToNat
  | IntFromNat
  deriving (Eq, Show, Enum, Bounded, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PrimDef
  deriving anyclass (NFData)
