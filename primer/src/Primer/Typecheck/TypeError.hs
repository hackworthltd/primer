module Primer.Typecheck.TypeError (TypeError (..)) where

import Foreword

import Primer.Core (Expr, Kind)
import Primer.Core.Meta (TmVarRef, TyConName, TyVarName, ValConName)
import Primer.Core.Type (Type')
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

data TypeError
  = InternalError Text
  | UnknownVariable TmVarRef
  | UnknownTypeVariable TyVarName
  | WrongSortVariable Name -- type var instead of term var or vice versa
  | UnknownConstructor ValConName
  | UnknownTypeConstructor TyConName
  | -- | Cannot use a PrimCon when either no type of the appropriate name is
    -- in scope, or it is a user-defined type
    PrimitiveTypeNotInScope TyConName
  | CannotSynthesiseType Expr
  | InconsistentTypes (Type' ()) (Type' ())
  | TypeDoesNotMatchArrow (Type' ())
  | TypeDoesNotMatchForall (Type' ())
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT (Type' ())
  | CannotCaseNonSaturatedADT (Type' ())
  | -- | Either wrong number, wrong constructors or wrong order. The fields are @name of the ADT@, @branches given@
    WrongCaseBranches TyConName [ValConName]
  | CaseBranchWrongNumberPatterns
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeError
