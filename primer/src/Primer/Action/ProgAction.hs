module Primer.Action.ProgAction (ProgAction (..)) where

-- We split this module to increase parallelism in our build.
-- This module does not depend on much, but takes a long time
-- to build because of
-- https://gitlab.haskell.org/ghc/ghc/-/issues/5642
--
-- It is split from Actions and ActionError
-- because this depends on ASTTypeDef

import Foreword

import Data.Aeson (FromJSON (..), ToJSON (..))
import Primer.Action.Actions (Action)
import Primer.Core.Meta (GVarName, ID, ModuleName, TyConName, TyVarName, ValConName)
import Primer.Core.Type (Kind', Type')
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.TypeDef (ASTTypeDef)
import Primer.Typecheck.SmartHoles (SmartHoles)

-- | High level actions
-- These actions move around the whole program or modify definitions
data ProgAction
  = -- | Move the cursor to the definition with the given Name
    MoveToDef GVarName
  | -- | Rename the definition with the given (base) Name
    RenameDef GVarName Text
  | -- | Create a new definition
    CreateDef ModuleName (Maybe Text)
  | -- | Delete a new definition
    DeleteDef GVarName
  | -- | Move the cursor to the type definition with the given name
    MoveToTypeDef TyConName
  | -- | Move the cursor to the type definition parameter with the given name
    MoveToTypeDefParam TyConName TyVarName
  | -- | Move the cursor to the constructor definition with the given name
    MoveToTypeDefCon TyConName ValConName
  | -- | Add a new type definition
    AddTypeDef TyConName (ASTTypeDef () ())
  | -- | Delete a type definition
    DeleteTypeDef TyConName
  | -- | Rename the type definition with the given name, and its type constructor
    RenameType TyConName Text
  | -- | Rename the value constructor with the given name, in the given type
    RenameCon TyConName ValConName Text
  | -- | Rename the type parameter with the given name, in the given type
    RenameTypeParam TyConName TyVarName Text
  | -- | Add a value constructor at the given position, in the given type
    AddCon TyConName Int Text
  | -- | Delete the value constructor with the given name, from the given type
    DeleteCon TyConName ValConName
  | -- | Add a new field, at the given index, to the given constructor
    AddConField TyConName ValConName Int (Type' () ())
  | -- | Delete the field at the given index of the given value constructor, in the given type
    DeleteConField TyConName ValConName Int
  | -- | Add a parameter at the given position, with the given name and kind, in the given type
    AddTypeParam TyConName Int Text (Kind' ())
  | -- | Remove the parameter with the given name, from the given type
    DeleteTypeParam TyConName TyVarName
  | -- | Execute a sequence of actions on the body of the definition
    BodyAction [Action]
  | -- | Execute a sequence of actions on the type annotation of the definition
    SigAction [Action]
  | -- | Execute a sequence of actions on the type of a field of a constructor in a typedef
    ConFieldAction TyConName ValConName Int [Action]
  | -- | Execute a sequence of actions on the kind of a parameter in a typedef
    ParamKindAction TyConName TyVarName [Action]
  | SetSmartHoles SmartHoles
  | -- | CopyPaste (d,i) as
    --   remembers the tree in def d, node i
    --   runs actions as (in the currently selected def), which should end up in a hole
    --   and then tries to paste the remembered subtree
    --   This rather complex setup enables encoding 'raise' operations,
    --     f s ~> f
    --   where we remember f, then delete f s, then paste f back
    --   as well as allowing cross-definition copy+paste
    --   whilst letting the backend avoid remembering the 'copied' thing in some state.
    --   The cursor is left on the root of the inserted subtree, which may or may not be inside a hole and/or annotation.
    --   At the start of the actions, the cursor starts at the root of the definition's type/expression
    CopyPasteSig (GVarName, ID) [Action]
  | CopyPasteBody (GVarName, ID) [Action]
  | -- | Renames an editable module (will return an error if asked to rename an imported module)
    RenameModule ModuleName (NonEmpty Text)
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ProgAction
  deriving anyclass (NFData)
