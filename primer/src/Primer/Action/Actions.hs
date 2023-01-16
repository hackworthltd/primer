module Primer.Action.Actions (
  Action (..),
  Movement (..),
  QualifiedText,
) where

import Foreword

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Primer.Action.Movement (Movement (..))
import Primer.Core (PrimCon)
import Primer.Core.Meta (ID, TmVarRef)
import Primer.JSON (CustomJSON (..), PrimerJSON)

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642

type QualifiedText = (NonEmpty Text, Text)

-- | Core actions.
--  These describe edits to the core AST.
--  Some of them take Text arguments rather than Name because they represent
--  untrusted input from the frontend.
data Action
  = -- | Do nothing
    NoOp
  | -- | Move the cursor to the expression with this ID
    SetCursor ID
  | -- | Move one step in some direction
    Move Movement
  | -- | Delete the expression under the cursor
    Delete
  | -- | Set metadata under the cursor
    SetMetadata Value
  | -- | Enter the hole under the cursor.
    -- You can only enter an empty hole, changing it into a non-empty hole in the process.
    -- To enter a non-empty hole, use 'Move Child1'
    EnterHole
  | -- | Replace a non-empty hole with its contents
    FinishHole
  | -- | Construct a variable in an empty hole
    ConstructVar TmVarRef
  | -- | Insert a variable, with a saturated spine of term/type applications in an empty hole
    InsertSaturatedVar TmVarRef
  | -- | Insert a variable, with an infered spine of term/type applications in an empty hole
    InsertRefinedVar TmVarRef
  | -- | Apply the expression under the cursor
    ConstructApp
  | -- | Apply the expression under the cursor to a type
    ConstructAPP
  | -- | Annotate the expression under the cursor (with a type hole)
    ConstructAnn
  | -- | Remove an annotation, leaving a now non-annotated term
    RemoveAnn
  | -- | Construct a lambda
    ConstructLam (Maybe Text)
  | -- | Construct a type abstraction "big-lambda"
    ConstructLAM (Maybe Text)
  | -- | Put a constructor in an empty hole
    ConstructCon QualifiedText
  | -- | Put a literal in an empty hole
    ConstructPrim PrimCon
  | -- | Put a constructor applied to a saturated spine in an empty hole
    ConstructSaturatedCon QualifiedText
  | -- | Put a constructor in an empty hole, and infer what it should be applied to
    ConstructRefinedCon QualifiedText
  | -- | Put a let expression in an empty hole
    ConstructLet (Maybe Text)
  | -- | Put a letrec expression in an empty hole
    ConstructLetrec (Maybe Text)
  | -- | Convert a let to a letrec
    ConvertLetToLetrec
  | -- | Scrutinise the expression under the cursor with a @case@
    ConstructCase
  | -- | Rename a lambda binding
    RenameLam Text
  | -- | Rename a big lambda binding
    RenameLAM Text
  | -- | Rename a let or letrec binding
    RenameLet Text
  | -- | Move from an annotation to its type
    EnterType
  | -- | Move from a type up into the surrounding annotation
    ExitType
  | -- | Construct a function type around the type under the cursor.
    -- The type under the cursor is placed in the domain (left) position.
    ConstructArrowL
  | -- | Construct a function type around the type under the cursor.
    -- The type under the cursor is placed in the range (right) position.
    ConstructArrowR
  | -- | Put a type constructor in a type hole
    ConstructTCon QualifiedText
  | -- | Construct a type variable in an empty type hole
    ConstructTVar Text
  | -- | Construct a forall type (only at kind KType for now)
    ConstructTForall (Maybe Text)
  | -- | Construct an application in types
    ConstructTApp
  | -- | Rename a forall binding
    RenameForall Text
  | -- | Rename a case binding
    RenameCaseBinding Text
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Action
  deriving anyclass (NFData)
