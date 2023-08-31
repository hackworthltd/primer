{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Let (
  LetRemovalDetail (..),
) where

import Foreword

import Primer.Core (
  ID,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a removal of a let binding.
-- This can be any of: a term-level non-recursive let, a
-- term-level recursive let, a term-level let binding a type
-- or a type-level let.
-- If term-level: t ~ Expr; if type-level: t ~ Type
data LetRemovalDetail t = LetRemovalDetail
  { before :: t
  -- ^ the let expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , bindingNames :: NonEmpty Name
  -- ^ the names of the unused bound variables (either term or type variable)
  , letIDs :: NonEmpty ID
  -- ^ the dropped let expressions
  , bodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (LetRemovalDetail t)
