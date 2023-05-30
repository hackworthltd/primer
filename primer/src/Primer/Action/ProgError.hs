module Primer.Action.ProgError (ProgError (..)) where

import Foreword

import Data.Aeson (FromJSON (..), ToJSON (..))
import Primer.Action.Errors (ActionError)
import Primer.Core.Meta (GVarName, ModuleName, TyConName, TyVarName, ValConName)
import Primer.Eval.EvalError (EvalError)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (Name)

data ProgError
  = NoDefSelected
  | NoTypeDefSelected
  | DefNotFound GVarName
  | DefAlreadyExists GVarName
  | DefInUse GVarName
  | TypeDefIsPrim TyConName
  | TypeDefNotFound TyConName
  | TypeDefAlreadyExists TyConName
  | ConNotFound ValConName
  | ConAlreadyExists ValConName
  | -- | We expected to see more arguments to a constructor than actually existed
    -- (this should never happen in a well-typed program)
    ConNotSaturated ValConName
  | ParamNotFound TyVarName
  | ParamAlreadyExists TyVarName
  | TyConParamClash Name
  | ValConParamClash Name
  | ActionError ActionError
  | EvalError EvalError
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  | -- | Currently one can only add a typedef by a form in the frontend,
    --   which does its own error checking. Thus this is an "internal error"
    --   that should never happen!
    --   (However, this is not entirely true currently, see
    --    https://github.com/hackworthltd/primer/issues/3)
    TypeDefError Text
  | IndexOutOfRange Int
  | -- | Cannot rename a module to the same name as some other module
    RenameModuleNameClash
  | ModuleNotFound ModuleName
  | -- | Cannot edit an imported module
    ModuleReadonly ModuleName
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ProgError
