module Tests.Undo where

import Foreword

import Primer.API (
  Prog,
  undoAvailable,
  viewProg,
 )
import Primer.App (
  App,
  ProgAction (CreateDef),
  appIdCounter,
  appProg,
  handleEditRequest,
  progModules,
 )
import Primer.App qualified as App
import Primer.Core (
  ModuleName,
 )
import Primer.Module (
  moduleName,
 )
import Primer.Test.App (
  comprehensive,
 )
import Test.Tasty.HUnit (
  Assertion,
  assertBool,
 )
import Tests.Action.Prog (runAppTestM)
import Prelude (error)

mainModuleName :: App.Prog -> ModuleName
mainModuleName p = case progModules p of
  [m] -> moduleName m
  _ -> error "expected exactly one module in given program"

getProg :: App -> Prog
getProg = viewProg . appProg

unit_no_undo_available :: Assertion
unit_no_undo_available =
  assertBool "Expected no undo available" $ not $ undoAvailable $ getProg comprehensive

unit_undo_available :: Assertion
unit_undo_available =
  let app = comprehensive
      scope = mainModuleName $ appProg app
      action = handleEditRequest [CreateDef scope $ Just "newDef"]
   in do
        (result, newApp) <- runAppTestM (appIdCounter app) app action
        case result of
          Left _ -> error "expected successful action"
          Right _ -> assertBool "Expected undo to be available" $ undoAvailable $ getProg newApp
