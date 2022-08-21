{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Push (PushAppIntoLetrecDetail (..)) where

import Foreword

import Primer.Core (
  Expr,
  ID,
  LVarName,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data PushAppIntoLetrecDetail = PushAppIntoLetrecDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the expression after reduction
  , argID :: ID
  -- ^ the ID of the argument to the application
  , letrecID :: ID
  -- ^ the ID of the letrec
  , lamID :: ID
  -- ^ the ID of the lambda
  , letBindingName :: LVarName
  -- ^ The name of the variable bound by the letrec
  , isTypeApplication :: Bool
  -- ^ If 'True', the application is of a big lambda to a type.
  -- Otherwise it is of a small lambda to a term.
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PushAppIntoLetrecDetail
