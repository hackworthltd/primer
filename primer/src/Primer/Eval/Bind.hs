{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Bind (BindRenameDetail (..)) where

import Foreword

import Primer.Core (
  ID,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a renaming of a binding.
-- If term-level: t ~ Expr; if type-level: t ~ Type
data BindRenameDetail t = BindRenameDetail
  { before :: t
  -- ^ the expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , bindingNamesOld :: [Name]
  -- ^ the old names of the bound variables
  , bindingNamesNew :: [Name]
  -- ^ the new names of the bound variables
  , bindersOld :: [ID]
  -- ^ the old binders
  , bindersNew :: [ID]
  -- ^ the new binders
  , bindingOccurrences :: [ID]
  -- ^ where the old name occurred inside the bound expression
  , bodyID :: ID
  -- ^ the right hand side of the binders
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (BindRenameDetail t)
