module Primer.API.NodeFlavor (NodeFlavor (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions because this depends on Expr

import Foreword

import Primer.JSON (CustomJSON (..), PrimerJSON, ToJSON)

-- | An indication of the meaning of a node, which frontend may use for labelling, colour etc.
-- These mostly correspond to constructors of `Expr'` or `Type'`.
data NodeFlavor
  = FlavorHole
  | FlavorEmptyHole
  | FlavorAnn
  | FlavorApp
  | FlavorAPP
  | FlavorCon
  | FlavorLam
  | FlavorLAM
  | FlavorGlobalVar
  | FlavorLocalVar
  | FlavorLet
  | FlavorLetType
  | FlavorLetrec
  | FlavorCase
  | FlavorCaseWith
  | FlavorPrimCon
  | FlavorTEmptyHole
  | FlavorTHole
  | FlavorTCon
  | FlavorTFun
  | FlavorTVar
  | FlavorTApp
  | FlavorTForall
  | FlavorPattern
  | FlavorPatternCon
  | FlavorPatternBind
  | FlavorPatternApp
  deriving (Show, Eq, Generic, Enum, Bounded)
  deriving (ToJSON) via PrimerJSON NodeFlavor
