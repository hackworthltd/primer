module Primer.Typecheck.KindError (KindError (..)) where

import Foreword

import Primer.Core.Meta (TyConName, TyVarName)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)
import Primer.Typecheck.Cxt (Kind)

data KindError
  = UnknownTypeVariable TyVarName
  | TyVarWrongSort Name -- term var instead of type var
  | UnknownTypeConstructor TyConName
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  | -- | We currently cannot typecheck a let inside a type,
    -- they should only transiently appear in evaluation, as explicit substitutions.
    TLetUnsupported
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON KindError
  deriving anyclass (NFData)
