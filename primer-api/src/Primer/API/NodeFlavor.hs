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

import Foreword

import Deriving.Aeson (TagSingleConstructors)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

data NodeFlavorTextBody
  = Con
  | PatternCon
  | TCon
  | TVar
  | GlobalVar
  | LocalVar
  | VarBind
  | TVarBind
  deriving stock (Show, Read, Eq, Generic, Enum, Bounded)
  deriving (ToJSON, FromJSON) via PrimerJSON NodeFlavorTextBody
  deriving anyclass (NFData)

data NodeFlavorPrimBody
  = PrimCon
  | PrimPattern
  deriving stock (Show, Read, Eq, Generic, Enum, Bounded)
  deriving (ToJSON, FromJSON) via CustomJSON '[TagSingleConstructors] NodeFlavorPrimBody
  deriving anyclass (NFData)

data NodeFlavorBoxBody
  = Pattern
  deriving stock (Show, Read, Eq, Generic, Enum, Bounded)
  deriving (ToJSON, FromJSON) via CustomJSON '[TagSingleConstructors] NodeFlavorBoxBody
  deriving anyclass (NFData)

data NodeFlavorNoBody
  = Hole
  | EmptyHole
  | Ann
  | App
  | APP
  | Case
  | CaseWith
  | TEmptyHole
  | THole
  | TFun
  | TApp
  | PatternWildcard
  | KType
  | KHole
  | KFun
  | Lam
  | LAM
  | Let
  | LetType
  | Letrec
  | TLet
  | TForall
  deriving stock (Show, Read, Eq, Generic, Enum, Bounded)
  deriving (ToJSON, FromJSON) via PrimerJSON NodeFlavorNoBody
  deriving anyclass (NFData)
