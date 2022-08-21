{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Let (LetRemovalDetail (..), LetRenameDetail (..)) where

import Foreword

import Primer.Core (
  Expr,
  ID,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

data LetRemovalDetail = LetRemovalDetail
  { before :: Expr
  -- ^ the let expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , bindingName :: Name
  -- ^ the name of the unused bound variable (either term or type variable)
  , letID :: ID
  -- ^ the full let expression
  , bodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON LetRemovalDetail

data LetRenameDetail = LetRenameDetail
  { before :: Expr
  -- ^ the let expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , bindingNameOld :: Name
  -- ^ the old name of the let-bound variable
  , bindingNameNew :: Name
  -- ^ the new name of the let-bound variable
  , letID :: ID
  -- ^ the full let expression
  , bindingOccurrences :: [ID]
  -- ^ where the old name occurred inside the bound expression
  , bodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON LetRenameDetail
