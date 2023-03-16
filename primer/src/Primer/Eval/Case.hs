{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Case (CaseReductionDetail (..)) where

import Foreword

import Primer.Core (
  Expr,
  ID,
  ValConName,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data CaseReductionDetail = CaseReductionDetail
  { before :: Expr
  -- ^ the case expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , targetID :: ID
  -- ^ the ID of the target (scrutinee)
  , targetCtorID :: ID
  -- ^ the ID of the constructor node in the target
  -- TODO (saturated constructors) these may or may not be the same, depending on annotations. This needs clarity and documenting
  , ctorName :: ValConName
  -- ^ the name of the matching constructor
  , targetArgIDs :: [ID]
  -- ^ the arguments to the constructor in the target
  , branchBindingIDs :: [ID]
  -- ^ the bindings in the case branch (one for each arg above)
  , branchRhsID :: ID
  -- ^ the right hand side of the selected case branch
  , letIDs :: [ID]
  -- ^ the let expressions binding each argument in the result
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON CaseReductionDetail
