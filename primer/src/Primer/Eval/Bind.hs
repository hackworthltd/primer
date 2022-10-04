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
  -- ^ the let expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , bindingNameOld :: [Name]
  -- ^ the old names of the bound variables
  , bindingNameNew :: [Name]
  -- ^ the new names of the bound variables
  , binderID :: [ID]
  -- ^ the full binding expression
  , renameLetID :: [ID]
  -- ^ the inserted (just under the binderID) @let@s which implement the renaming
  , bodyID :: ID
  -- ^ the right hand side of the binders
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (BindRenameDetail t)
