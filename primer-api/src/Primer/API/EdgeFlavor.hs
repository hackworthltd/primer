-- | An indication of the meaning of an edge, which frontend may use for labelling, colour etc.
module Primer.API.EdgeFlavor (
  EdgeFlavor (..),
) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642

import Foreword

import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

data EdgeFlavor
  = Hole
  | AnnTerm
  | Ann
  | AppFun
  | AppArg
  | ConField
  | Lam
  | LetEqual
  | LetIn
  | MatchInput
  | Pattern
  | MatchOutput
  | FunIn
  | FunOut
  | ForallKind
  | Forall
  | Bind
  deriving stock (Show, Read, Eq, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EdgeFlavor
  deriving anyclass (NFData)
