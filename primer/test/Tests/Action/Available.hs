module Tests.Action.Available where

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Hedgehog (
  PropertyT,
  annotate,
  annotateShow,
  collect,
  discard,
  failure,
  label,
  success,
  (===),
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllWithT)
import Optics (toListOf, (%), (^..))
import Primer.Action (
  ActionError (CaseBindsClash, NameCapture),
 )
import Primer.Action.Available (
  OfferedAction (..),
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
 )
import Primer.App (
  App,
  EditAppM,
  Editable (Editable, NonEditable),
  ProgError (ActionError, DefAlreadyExists),
  QueryAppM,
  allTyConNames,
  allValConNames,
  appProg,
  checkAppWellFormed,
  handleEditRequest,
  handleQuestion,
  progAllDefs,
  progAllTypeDefs,
  runEditAppM,
  runQueryAppM,
 )
import Primer.Core (
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  LocalName (unLocalName),
  ModuleName (ModuleName, unModuleName),
  TmVarRef (GlobalVarRef, LocalVarRef),
  mkSimpleModuleName,
  moduleNamePretty,
  qualifyName,
  _typeMeta,
 )
import Primer.Core.DSL (
  create',
  emptyHole,
  gvar,
  tEmptyHole,
 )
import Primer.Core.Utils (
  exprIDs,
  typeIDs,
 )
import Primer.Def (
  ASTDef (..),
  Def (DefAST, DefPrim),
  defAST,
 )
import Primer.Examples (comprehensiveWellTyped)
import Primer.Gen.App (genApp)
import Primer.Gen.Core.Raw (genName)
import Primer.Gen.Core.Typed (WT, forAllT, propertyWT)
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (Module, moduleDefs),
  builtinModule,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Questions (variablesInScopeExpr, variablesInScopeTy)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles, SmartHoles),
  buildTypingContextFromModules,
  checkEverything,
 )
import Primer.Zipper (focusOn, focusOnTy, locToEither)
import System.FilePath ((</>))
import Tasty (Property, withDiscards, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))
import TestUtils (testNoSevereLogs)
import Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha), runTypecheckTestMIn)
import Text.Pretty.Simple (pShowNoColor)
