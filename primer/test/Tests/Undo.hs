module Tests.Undo where

import Foreword

import Data.Map qualified as M
import Primer.Action (Action (ConstructPrim, InsertSaturatedVar, SetCursor))
import Primer.App (
  ProgAction (BodyAction, MoveToDef),
  ProgError,
  appProg,
  handleEditRequest,
  handleEvalFullRequest,
  handleMutationRequest,
  progModules,
 )
import Primer.App qualified as App
import Primer.Core (
  Expr' (App, Var),
  Meta (Meta),
  ModuleName,
  PrimCon (PrimInt),
  TmVarRef (GlobalVarRef),
  getID,
  qualifyName,
 )
import Primer.Def (astDefExpr, defAST)
import Primer.Eval (Dir (Syn))
import Primer.Module (
  moduleDefsQualified,
  moduleName,
 )
import Primer.Prelude.Integer qualified as Integer
import Primer.Test.App (
  runAppTestM,
 )
import Test.Tasty.HUnit (
  Assertion,
  assertFailure,
  (@?=),
 )
import Tests.Action.Prog (readerToState)
import Prelude (error)

mainModuleName :: App.Prog -> ModuleName
mainModuleName p = case progModules p of
  [m] -> moduleName m
  _ -> error "expected exactly one module in given program"

expectSuccess :: (Either ProgError a, b) -> IO (a, b)
expectSuccess = \case
  (Left e, _) -> assertFailure $ "action unexpectedly failed: " <> show e
  (Right x, y) -> pure (x, y)

unit_redo_eval :: Assertion
unit_redo_eval =
  let originalApp = App.newApp
      scope = mainModuleName $ appProg originalApp
      action1 =
        [ MoveToDef $ qualifyName scope "main"
        , BodyAction [InsertSaturatedVar $ GlobalVarRef Integer.even]
        ]
      action2 i =
        [ MoveToDef $ qualifyName scope "main"
        , BodyAction
            [ SetCursor i
            , ConstructPrim $ PrimInt 4
            ]
        ]
      eval =
        readerToState
          $ handleEvalFullRequest
            App.EvalFullReq
              { App.evalFullReqExpr = Var (Meta 0 Nothing Nothing) (GlobalVarRef $ qualifyName scope "main")
              , App.evalFullCxtDir = Syn
              , App.evalFullMaxSteps = 10
              }
      edit1 = handleEditRequest action1
      edit2 = handleEditRequest . action2
      undo = handleMutationRequest App.Undo
      redo = handleMutationRequest App.Redo
      run' act app = fmap snd . expectSuccess =<< runAppTestM app act
   in do
        originalApp' <- run' eval originalApp
        newApp1 <- run' edit1 originalApp'
        i <- case fmap astDefExpr . defAST =<< foldMap' moduleDefsQualified (progModules $ appProg newApp1) M.!? qualifyName scope "main" of
          Just (App _ _ e) -> pure $ getID e
          _ -> liftIO $ assertFailure "unexpected form of main"
        newApp <- run' (edit2 i) newApp1
        a3 <- run' undo newApp
        a4 <- run' redo a3
        a5 <- run' undo a4
        a6 <- run' undo a5
        a7 <- run' redo a6
        a8 <- run' redo a7
        finalApp <- run' eval a8
        finalApp @?= newApp
