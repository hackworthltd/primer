{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Case (
  CaseReductionTrivialDetail (..),
  CaseReductionDetail (..),
) where

import Foreword

import Primer.Core (
  Expr,
  ID,
 )
import Primer.Core.Meta (Pattern)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data CaseReductionTrivialDetail = CaseReductionTrivialDetail
  { before :: Expr
  -- ^ the case expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , targetID :: ID
  -- ^ the ID of the target (the whole scrutinee)
  , branchRhsID :: ID
  -- ^ the right hand side of the fallback case branch
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON CaseReductionTrivialDetail

data CaseReductionDetail = CaseReductionDetail
  { before :: Expr
  -- ^ the case expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , targetID :: ID
  -- ^ the ID of the target (the whole scrutinee)
  , targetCtorID :: ID
  -- ^ the ID of the constructor node in the target
  -- (nb: this is likely to be different from 'targetID'. if the
  -- scrutinee was @Succ n : Nat@, then the @targetID@ is the root of
  -- this subtree (the annotation node), but the @targetCtorID@ is the
  -- @Succ@ node)
  , ctorName :: Pattern
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
