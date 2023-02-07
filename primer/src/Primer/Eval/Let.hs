{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Let (
  LetRemovalDetail (..),
  findFreeOccurrencesExpr,
  findFreeOccurrencesType,
) where

import Foreword

import Control.Arrow ((***))
import Optics (filtered, getting, to, (%), (^..), _1)
import Primer.Core (
  Expr,
  ID,
  LocalName (unLocalName),
  TyVarName,
  Type,
  getID,
 )
import Primer.Core.Utils (_freeVars, _freeVarsTy)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

-- | Detailed information about a removal of a let binding.
-- This can be any of: a term-level non-recursive let, a
-- term-level recursive let, a term-level let binding a type
-- or a type-level let.
-- If term-level: t ~ Expr; if type-level: t ~ Type
data LetRemovalDetail t = LetRemovalDetail
  { before :: t
  -- ^ the let expression before reduction
  , after :: t
  -- ^ the resulting expression after reduction
  , bindingName :: Name
  -- ^ the name of the unused bound variable (either term or type variable)
  , letID :: ID
  -- ^ the full let expression
  , bodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (LetRemovalDetail t)

findFreeOccurrencesExpr :: LocalName k -> Expr -> [ID]
findFreeOccurrencesExpr x e = e ^.. _freeVars % to idName % filtered ((== unLocalName x) . snd) % _1
  where
    idName = either (getID *** unLocalName) (getID *** unLocalName)

findFreeOccurrencesType :: TyVarName -> Type -> [ID]
findFreeOccurrencesType x ty = ty ^.. getting _freeVarsTy % to (first getID) % filtered ((== x) . snd) % _1
