{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Bind (BindRenameDetail (..)) where

import Foreword

import Primer.Core (
  ID,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a renaming of a binding.
-- (Possibly of multiple bindings in one branch of a case expression.)
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
  -- ^ where the old name occurred inside the bound expression, for
  -- renaming "self-capturing (non-recursive) lets"
  -- (e.g. @let x = x+1 in x+x ~> let y = x+1 in let x = y in x + x@,
  -- it will contain the @x@ inside @x+1@).
  -- For renaming other binders (e.g. lambdas), this list will be empty.
  , renamingLets :: [ID]
  -- ^ the newly-inserted (just under the binder) @let@
  -- (note that renamings are small-step: @x.t[x] ~> y.let x = y in t[x]@)
  , bodyID :: ID
  -- ^ the right hand side of the binders
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (BindRenameDetail t)
