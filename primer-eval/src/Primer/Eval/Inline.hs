{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Inline (
  LocalVarInlineDetail (..),
  GlobalVarInlineDetail (..),
) where

import Foreword

import Primer.Core (
  Expr,
  ID,
  LocalName,
 )
import Primer.Def (ASTDef)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data LocalVarInlineDetail k = LocalVarInlineDetail
  { letID :: ID
  -- ^ ID of the let expression that binds this variable
  , varID :: ID
  -- ^ ID of the variable being replaced
  , bindingName :: LocalName k
  -- ^ Name of the variable
  , valueID :: ID
  -- ^ ID of the expression or type that the variable is bound to
  , replacementID :: ID
  -- ^ ID of the expression or type that has replaced the variable in the result
  , isTypeVar :: Bool
  -- ^ If 'True', the variable being inlined is a type variable.
  -- Otherwise it is a term variable.
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (LocalVarInlineDetail k)

data GlobalVarInlineDetail = GlobalVarInlineDetail
  { def :: ASTDef
  -- ^ The definition that the variable refers to
  , var :: Expr
  -- ^ The variable being replaced
  , after :: Expr
  -- ^ The result of the reduction
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON GlobalVarInlineDetail
