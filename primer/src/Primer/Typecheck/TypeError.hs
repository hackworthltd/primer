module Primer.Typecheck.TypeError (TypeError (..)) where

import Foreword

import Primer.Core (Expr, Pattern)
import Primer.Core.Meta (LVarName, TmVarRef, TyConName, ValConName)
import Primer.Core.Type (Type')
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Typecheck.KindError (KindError)

data TypeError
  = InternalError Text
  | UnknownVariable TmVarRef
  | TmVarWrongSort Name -- type var instead of term var
  | -- | Constructors (term-level) only inhabit fully-applied ADTs
    -- i.e. @Maybe a@, but not @Maybe@, @Maybe a b@, @Nat -> Bool@ or holes
    ConstructorNotFullAppADT (Type' () ()) ValConName
  | -- | This ADT does not have a constructor of that name
    ConstructorWrongADT TyConName ValConName
  | UnknownConstructor ValConName
  | -- | Constructors (term-level) must be saturated.
    -- This error catches both under- and over-saturation.
    UnsaturatedConstructor ValConName
  | -- | Cannot use a PrimCon when either no type of the appropriate name is
    -- in scope, or it is a student-defined type
    PrimitiveTypeNotInScope TyConName
  | CannotSynthesiseType Expr
  | InconsistentTypes (Type' () ()) (Type' () ())
  | TypeDoesNotMatchArrow (Type' () ())
  | TypeDoesNotMatchForall (Type' () ())
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT (Type' () ())
  | CannotCaseNonSaturatedADT (Type' () ())
  | -- | Either wrong number, wrong constructors or wrong order. The fields are @name of the ADT@, @branches given@, @wildcard/fallback branch given@
    WrongCaseBranches TyConName [Pattern] Bool
  | CaseBranchWrongNumberPatterns
  | -- | One AST node binds the same name twice
    -- (currently this can only happen in a case branch)
    DuplicateBinders [LVarName]
  | KindError KindError
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeError
  deriving anyclass (NFData)
