{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Push (PushLetDetail (..)) where

import Foreword

import Primer.Core.Meta ( ID )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about pushing a group of 'let's down the tree
-- This each can be any of: a term-level non-recursive let, a
-- term-level recursive let, a term-level let binding a type
-- or a type-level let.
-- If term-level: t ~ Expr; if type-level: t ~ Type
data PushLetDetail t = PushLetDetail
  { before :: t
  -- ^ the expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , letIDs :: [ID]
  -- ^ the 'ID' of each original let
  , letBindingNames :: [Name]
  -- ^ the names of the variables bound by the @let@s
  , intoID :: ID
  -- ^ the 'ID' of the term we push into
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (PushLetDetail t)
