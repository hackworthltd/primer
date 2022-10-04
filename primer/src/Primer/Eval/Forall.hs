{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Forall (ForallRenameDetail (..)) where

import Foreword

import Control.Arrow ((***))
import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as Set
import Optics (filtered, getting, notElemOf, to, (%), (^.), (^..), _1, _2)
import Primer.Core (
  Expr,
  Expr' (Let, LetType, Letrec),
  ID,
  LocalName (unLocalName),
  getID,
  _id, Type,
 )
import Primer.Core.DSL (letType, let_)
import Primer.Core.Utils (freeVars, freeVarsTy, _freeTmVars, _freeTyVars, _freeVars, _freeVarsTy)
import Primer.Eval.Utils (makeSafeLetBinding, makeSafeLetTypeBinding)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a renaming of a forall binding.
data ForallRenameDetail = ForallRenameDetail
  { before :: Type
  -- ^ the forall before reduction
  , after :: Type
  -- ^ the resulting type after reduction
  , bindingNameOld :: Name
  -- ^ the old name of the bound variable
  , bindingNameNew :: Name
  -- ^ the new name of the bound variable
  , forallID :: ID
  -- ^ the full forall expression
  , bindingOccurrences :: [ID]
  -- ^ where the old name occurred inside the bound expression
  , bodyID :: ID
  -- ^ the right hand side of the forall
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (ForallRenameDetail)
