-- | An indication of the meaning of a node, which frontend may use for labelling, colour etc.
-- These mostly correspond to constructors of `Expr'` or `Type'`.
module Primer.API.NodeFlavor (
  NodeFlavorTextBody (..),
  NodeFlavorPrimBody (..),
  NodeFlavorBoxBody (..),
  NodeFlavorNoBody (..),
) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions because this depends on Expr

import Foreword

import Deriving.Aeson (TagSingleConstructors)
import Primer.JSON (CustomJSON (..), PrimerJSON, ToJSON)

data NodeFlavorTextBody
  = FlavorCon
  | FlavorLam
  | FlavorLAM
  | FlavorLet
  | FlavorLetType
  | FlavorLetrec
  | FlavorPatternBind
  | FlavorPatternCon
  | FlavorTCon
  | FlavorTVar
  | FlavorTForall
  | FlavorTLet
  | FlavorGlobalVar
  | FlavorLocalVar
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving (ToJSON) via PrimerJSON NodeFlavorTextBody

data NodeFlavorPrimBody
  = FlavorPrimCon
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving (ToJSON) via CustomJSON '[TagSingleConstructors] NodeFlavorPrimBody

data NodeFlavorBoxBody
  = FlavorPattern
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving (ToJSON) via CustomJSON '[TagSingleConstructors] NodeFlavorBoxBody

data NodeFlavorNoBody
  = FlavorHole
  | FlavorEmptyHole
  | FlavorAnn
  | FlavorApp
  | FlavorAPP
  | FlavorCase
  | FlavorCaseWith
  | FlavorPatternApp
  | FlavorTEmptyHole
  | FlavorTHole
  | FlavorTFun
  | FlavorTApp
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving (ToJSON) via PrimerJSON NodeFlavorNoBody
