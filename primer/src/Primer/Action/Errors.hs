module Primer.Action.Errors (ActionError (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions because this depends on Expr

import Foreword

import Data.Aeson (FromJSON (..), ToJSON (..))
import Primer.Action.Actions (Action)
import Primer.Action.Available qualified as Available
import Primer.Action.Movement (Movement)
import Primer.Core (Expr, GVarName, ID, LVarName, ModuleName, Pattern, TyConName, Type', ValConName)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Typecheck.TypeError (TypeError)
import Primer.Zipper (SomeNode)

-- | Errors that may arise when applying an action
-- TODO: convert all CustomFailures to individual constructors
-- https://github.com/hackworthltd/primer/issues/8
data ActionError
  = CustomFailure
      -- | action that caused the error
      Action
      -- | the error message
      Text
  | InternalFailure Text
  | IDNotFound ID
  | MovementFailed (ID, Movement)
  | UnknownDef GVarName
  | NeedEmptyHole Action Expr
  | NeedNonEmptyHole Action Expr
  | NeedAnn Action Expr
  | TypeError TypeError
  | -- | Both actual and potential, eg renaming the lambda x to y in any of
    -- λx.y     the binder captures the existing y
    -- λx.λy.x  occurance gets captured by the inner binder
    -- λx.λy.y  this would be ok, but we are paranoid and bail out
    NameCapture
  | CaseBindsClash LVarName [LVarName]
  | CaseAlreadyExhaustive
  | CaseBranchAlreadyExists Pattern
  | CaseBranchNotExist Pattern
  | -- | Attempted to add a branch for an unexpected ctor
    CaseBranchNotCon Pattern (Type' () ())
  | -- TODO: semantic errors.
    -- https://github.com/hackworthltd/primer/issues/8
    SaturatedApplicationError (Either Text TypeError)
  | -- | @RefineError@ should never happen unless we use the API wrong or have
    -- a bug. It does not get thrown for "no valid refinement found"
    -- - see Note [No valid refinement]
    RefineError (Either Text TypeError)
  | -- | Cannot import modules whose names clash with previously-imported things
    -- (or with each other)
    ImportNameClash [ModuleName]
  | -- | Importing some modules failed.
    -- This should be impossible as long as the requested modules are well-typed
    -- and all of their dependencies are already imported
    -- The extra unit is to avoid having two constructors with a single
    -- TypeError field, breaking our MonadNestedError machinery...
    ImportFailed () TypeError
  | NeedTFun (Type' () ())
  | NeedType SomeNode
  | NeedGlobal Available.Option
  | NeedLocal Available.Option
  | NeedInt Available.Option
  | NeedChar Available.Option
  | NeedTermDef
  | NeedTypeDef
  | NeedTermDefSelection
  | NeedTypeDefSelection
  | NeedTypeDefNodeSelection
  | NeedTypeDefConsSelection
  | NeedTypeDefConsFieldSelection
  | NeedTypeDefParamSelection
  | NeedTypeDefParamKindSelection
  | NoNodeSelection
  | ValConNotFound TyConName ValConName
  | FieldIndexOutOfBounds ValConName Int
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionError
