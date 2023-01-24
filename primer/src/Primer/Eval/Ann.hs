{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Ann (RemoveAnnDetail (..)) where

import Foreword

import Primer.Core (
  Expr,
  ID,
 )
import Primer.JSON (CustomJSON (CustomJSON), PrimerJSON, ToJSON)

data RemoveAnnDetail = RemoveAnnDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , typeID :: ID
  -- ^ the ID of the type annotation
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON) via PrimerJSON RemoveAnnDetail
