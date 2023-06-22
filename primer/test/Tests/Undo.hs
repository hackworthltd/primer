{-# LANGUAGE OverloadedLabels #-}

module Tests.Undo where

import Foreword

import Optics (
  (.~),
 )
import Primer.API (
  Prog,
  redoAvailable,
  undoAvailable,
  viewProg,
 )
import Primer.App (
  App,
  Log (..),
  ProgAction (CreateDef),
  ProgError,
  appIdCounter,
  appProg,
  defaultLog,
  handleEditRequest,
  handleMutationRequest,
  progModules,
  redoLog,
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
  assertFailure,
  (@?=),
 )
import Tests.Action.Prog (AppTestM, runAppTestM)
import Prelude (error)

mainModuleName :: App.Prog -> ModuleName
mainModuleName p = case progModules p of
  [m] -> moduleName m
  _ -> error "expected exactly one module in given program"

getProg :: App -> Prog
getProg = viewProg . appProg

actionFailed :: ProgError -> Assertion
actionFailed e = assertFailure $ "Expected successful action, but got " <> show e

undoFailed :: ProgError -> Assertion
undoFailed e = assertFailure $ "Expected successful undo, but got " <> show e

redoFailed :: ProgError -> Assertion
redoFailed e = assertFailure $ "Expected successful redo, but got " <> show e

expectSuccess :: (Either ProgError a, b) -> IO (a, b)
expectSuccess = \case
  (Left e, _) -> assertFailure $ "action unexpectedly failed: " <> show e
  (Right x, y) -> pure (x, y)

run :: App -> AppTestM a -> IO (Either ProgError a, App)
run app = runAppTestM (appIdCounter app) app

compareAfterUndo :: App.Prog -> App.Prog -> App.Log -> Assertion
compareAfterUndo orig undone expectedRedoLog =
  (orig & #redoLog .~ expectedRedoLog) @?= undone

unit_no_undo_available :: Assertion
unit_no_undo_available =
  assertBool "Expected no undo available" $ not $ undoAvailable $ getProg comprehensive

unit_undo_available :: Assertion
unit_undo_available =
  let app = comprehensive
      scope = mainModuleName $ appProg app
      action = handleEditRequest [CreateDef scope $ Just "newDef"]
   in do
        (result, newApp) <- run app action
        case result of
          Left e -> actionFailed e
          Right _ -> assertBool "Expected undo to be available" $ undoAvailable $ getProg newApp

unit_undo_test1 :: Assertion
unit_undo_test1 =
  let originalApp = comprehensive
      scope = mainModuleName $ appProg originalApp
      action = [CreateDef scope $ Just "newDef"]
      edit = handleEditRequest action
      undo = handleMutationRequest App.Undo
   in do
        (_, newApp) <- expectSuccess =<< run originalApp edit
        (result, undoneApp) <- run newApp undo
        case result of
          Left e -> undoFailed e
          Right _ -> do
            compareAfterUndo (appProg originalApp) (appProg undoneApp) $ Log [action]
            assertBool "Expected no undo available" $ not $ undoAvailable $ getProg undoneApp
            assertBool "Expected redo available" $ redoAvailable $ getProg undoneApp

-- Test that [edit, undo, edit] leaves no redo log.
unit_undo_test2 :: Assertion
unit_undo_test2 =
  let originalApp = comprehensive
      scope = mainModuleName $ appProg originalApp
      action = [CreateDef scope $ Just "newDef"]
      edit = handleEditRequest action
      undo = handleMutationRequest App.Undo
   in do
        (_, newApp1) <- expectSuccess =<< run originalApp edit
        (_, undoneApp) <- expectSuccess =<< run newApp1 undo
        (result, newApp2) <- run undoneApp edit
        case result of
          Left e -> undoFailed e
          Right _ -> do
            -- Don't compare the programs, since the new edit will
            -- have a different ID.
            assertBool "Expected undo available" $ undoAvailable $ getProg newApp2
            assertBool "Expected no redo available" $ not $ redoAvailable $ getProg newApp2

unit_no_redo_available :: Assertion
unit_no_redo_available =
  assertBool "Expected no redo available" $ not $ redoAvailable $ getProg comprehensive

unit_no_redo_available_after_edit :: Assertion
unit_no_redo_available_after_edit =
  let app = comprehensive
      scope = mainModuleName $ appProg app
      action = handleEditRequest [CreateDef scope $ Just "newDef"]
   in do
        (result, newApp) <- run app action
        case result of
          Left e -> actionFailed e
          Right _ -> assertBool "Expected no redo available" $ not $ redoAvailable $ getProg newApp

unit_redo_test1 :: Assertion
unit_redo_test1 =
  let originalApp = comprehensive
      scope = mainModuleName $ appProg originalApp
      action = [CreateDef scope $ Just "newDef"]
      edit = handleEditRequest action
      undo = handleMutationRequest App.Undo
      redo = handleMutationRequest App.Redo
   in do
        (_, newApp) <- expectSuccess =<< run originalApp edit
        (_, undoneApp) <- expectSuccess =<< run newApp undo
        (result, redoneApp) <- run undoneApp redo
        case result of
          Left e -> redoFailed e
          Right _ -> do
            compareAfterUndo (appProg newApp) (appProg redoneApp) defaultLog
            assertBool "Expected undo available" $ undoAvailable $ getProg redoneApp
            assertBool "Expected no redo available" $ not $ redoAvailable $ getProg redoneApp
