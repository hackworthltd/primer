module Tests.API where

import Foreword

import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as M
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Hedgehog hiding (Property, Var, property)
import Optics ((.~))
import Primer.API (
  NewSessionReq (..),
  OkOrMismatch (Mismatch, Ok, expected, got),
  PrimerErr,
  Tree,
  TypeOrKind (Kind, Type),
  addSession,
  copySession,
  deleteSession,
  edit,
  evalFull',
  findSessions,
  flushSessions,
  getApp,
  getProgram,
  getSelectionTypeOrKind,
  getSessionName,
  getVersion,
  listSessions,
  newSession,
  redo,
  renameSession,
  treeIds,
  undo,
  viewTreeExpr,
  viewTreeKind,
  viewTreeType,
 )
import Primer.API.Test.Util (runAPI)
import Primer.Action (
  Action (
    ConstructApp,
    ConstructPrim,
    ConstructTApp,
    ConstructVar,
    EnterHole,
    InsertSaturatedVar,
    Move,
    SetCursor
  ),
  Movement (Child1, Child2, Parent),
 )
import Primer.App (
  DefSelection (DefSelection),
  MutationRequest (Edit),
  NodeSelection (NodeSelection),
  NodeType (BodyNode, SigNode),
  Prog (progModules),
  ProgAction (BodyAction, CreateDef, MoveToDef, SigAction),
  Selection' (SelectionDef),
  newApp,
 )
import Primer.Builtins (cTrue, cZero, tBool, tList, tMaybe, tNat)
import Primer.Core
import Primer.Core.DSL hiding (app)
import Primer.Core.Utils (forgetMetadata, forgetTypeMetadata)
import Primer.Database (
  OffsetLimit (OL, limit, offset),
  Page (pageContents, total),
  Session (..),
  defaultSessionName,
  fromSessionName,
 )
import Primer.Def (astDefExpr, astDefType, defAST)
import Primer.Eval (NormalOrderOptions (UnderBinders))
import Primer.Examples (
  comprehensive,
  even3App,
 )
import Primer.Gen.Core.Raw (evalExprGen, genExpr, genType)
import Primer.Module (moduleDefsQualified)
import Primer.Name (unName)
import Primer.Prelude.Integer qualified as Integer
import Primer.Prelude.Logic qualified as PL
import Primer.Test.Util (
  ExceptionPredicate,
  assertException,
  constructSaturatedCon,
  constructTCon,
  (@?=),
 )
import Primer.UUIDv4 (nextRandom)
import Protolude.Unsafe (unsafeFromJust)
import Tasty (
  Property,
  property,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit hiding ((@?=))
import Text.Pretty.Simple (pShowNoColor)

tasty_viewTreeExpr_injective :: Property
tasty_viewTreeExpr_injective = property $ do
  e1 <- forAll $ evalExprGen 0 genExpr
  e2 <- forAll $ evalExprGen 0 genExpr
  when (e1 == e2) discard
  viewTreeExpr e1 /== viewTreeExpr e2

tasty_viewTreeType_injective :: Property
tasty_viewTreeType_injective = property $ do
  t1 <- forAll $ evalExprGen 0 genType
  t2 <- forAll $ evalExprGen 0 genType
  when (t1 == t2) discard
  viewTreeType t1 /== viewTreeType t2

test_golden :: TestTree
test_golden =
  testGroup
    "golden"
    [ goldenVsString "expr" "test/outputs/APITree/Expr" $ do
        pure . BSL.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor . viewTreeExpr $ astDefExpr def
    , goldenVsString "type" "test/outputs/APITree/Type" $ do
        pure . BSL.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor . viewTreeType $ astDefType def
    ]
  where
    def = unsafeFromJust . defAST . snd . create' . comprehensive $ mkSimpleModuleName "M"

-- regression tests to check we encode names into the tree

unit_viewTreeExpr_injective_con :: Assertion
unit_viewTreeExpr_injective_con =
  distinctTreeExpr (con0' ["M"] "C") (con0' ["M"] "D")

unit_viewTreeExpr_injective_lam :: Assertion
unit_viewTreeExpr_injective_lam =
  distinctTreeExpr (lam "x" emptyHole) (lam "y" emptyHole)

unit_viewTreeExpr_injective_LAM :: Assertion
unit_viewTreeExpr_injective_LAM =
  distinctTreeExpr (lAM "x" emptyHole) (lAM "y" emptyHole)

unit_viewTreeExpr_injective_var :: Assertion
unit_viewTreeExpr_injective_var =
  distinctTreeExpr (lvar "x") (lvar "y")

unit_viewTreeExpr_injective_globalvar :: Assertion
unit_viewTreeExpr_injective_globalvar =
  distinctTreeExpr (gvar' ["M"] "0") (gvar' ["M"] "1")

-- When we changed how references were handled so 'Expr' had one constructor
-- that handled both local and global variable references, there was a
-- regression where they both rendered identically.
-- This is a regression test for said issue (which occurred before
-- global variables had a qualified name).
unit_viewTreeExpr_injective_locglobvar :: Assertion
unit_viewTreeExpr_injective_locglobvar =
  distinctTreeExpr (lvar "x") (gvar' ["M"] "x")

unit_viewTreeExpr_injective_let :: Assertion
unit_viewTreeExpr_injective_let =
  distinctTreeExpr (let_ "x" emptyHole emptyHole) (let_ "y" emptyHole emptyHole)

unit_viewTreeExpr_injective_lettype :: Assertion
unit_viewTreeExpr_injective_lettype =
  distinctTreeExpr (letType "x" tEmptyHole emptyHole) (letType "y" tEmptyHole emptyHole)

unit_viewTreeExpr_injective_letrec :: Assertion
unit_viewTreeExpr_injective_letrec =
  distinctTreeExpr (letrec "x" emptyHole tEmptyHole emptyHole) (letrec "y" emptyHole tEmptyHole emptyHole)

unit_viewTreeExpr_injective_case_conName :: Assertion
unit_viewTreeExpr_injective_case_conName =
  distinctTreeExpr (case_ emptyHole [branch' (["M"], "C") [("x", Nothing)] emptyHole]) (case_ emptyHole [branch' (["M"], "D") [("x", Nothing)] emptyHole])

unit_viewTreeExpr_injective_case_paramName :: Assertion
unit_viewTreeExpr_injective_case_paramName =
  distinctTreeExpr (case_ emptyHole [branch' (["M"], "C") [("x", Nothing)] emptyHole]) (case_ emptyHole [branch' (["M"], "C") [("y", Nothing)] emptyHole])

unit_viewTreeType_injective_con :: Assertion
unit_viewTreeType_injective_con =
  distinctTreeType (tcon' ["M"] "T") (tcon' ["M"] "S")

unit_viewTreeType_injective_var :: Assertion
unit_viewTreeType_injective_var =
  distinctTreeType (tvar "a") (tvar "b")

unit_viewTreeType_injective_forall_param :: Assertion
unit_viewTreeType_injective_forall_param =
  distinctTreeType (tforall "a" ktype tEmptyHole) (tforall "b" ktype tEmptyHole)

unit_viewTreeType_injective_forall_kind :: Assertion
unit_viewTreeType_injective_forall_kind =
  distinctTreeType (tforall "a" ktype tEmptyHole) (tforall "a" khole tEmptyHole)

distinctTreeExpr :: S Expr -> S Expr -> Assertion
distinctTreeExpr e1 e2 =
  let t1 = viewTreeExpr $ create' e1
      t2 = viewTreeExpr $ create' e2
   in assertBool ("non-injective viewTreeExpr: " ++ show t1) (t1 /= t2)

distinctTreeType :: S Type -> S Type -> Assertion
distinctTreeType e1 e2 =
  let t1 = viewTreeType $ create' e1
      t2 = viewTreeType $ create' e2
   in assertBool ("non-injective viewTreeType: " ++ show t1) (t1 /= t2)

-- API tests for portions of the API that don't deal with programs.

test_newSession_roundtrip :: TestTree
test_newSession_roundtrip =
  testCaseSteps "newSession database round-tripping" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Create a new session"
      sid <- newSession $ NewSessionReq "new session" True
      step "Get its name"
      name <- getSessionName sid
      name @?= "new session"
      step "Get its app"
      app <- getApp sid
      app @?= newApp
      step "Clear the in-memory database"
      flushSessions
      step "Get the session name again"
      name' <- getSessionName sid
      name' @?= name

test_newSession_duplicate_names :: TestTree
test_newSession_duplicate_names =
  testCaseSteps "newSession with duplicate names" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      let sessionName = "my new session"
      let newSessionReq = NewSessionReq sessionName True
      step "Create a new session"
      sid <- newSession newSessionReq
      step "Get its name"
      name <- getSessionName sid
      name @?= sessionName
      step "Create a new session with the same name as the first"
      sid2 <- newSession newSessionReq
      step "Get its name"
      name2 <- getSessionName sid2
      name2 @?= sessionName

test_newSession_invalid_names :: TestTree
test_newSession_invalid_names =
  testCaseSteps "newSession with invalid names" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Create a new session with an empty name"
      sid <- newSession $ NewSessionReq "" True
      step "Get its actual name"
      name <- getSessionName sid
      name @?= fromSessionName defaultSessionName
      step "Create a new session with a name full of whitespace"
      sid2 <- newSession $ NewSessionReq "      \t  \n" True
      step "Get its actual name"
      name2 <- getSessionName sid2
      name2 @?= fromSessionName defaultSessionName

test_newSession_modified_names :: TestTree
test_newSession_modified_names =
  testCaseSteps "newSession with names that are modified by the API" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Create a new session with a name with leading whitespace"
      sid <- newSession $ NewSessionReq "    this is a session name" True
      step "Get its actual name"
      name <- getSessionName sid
      name @?= "this is a session name"
      step "Create a new session with a name with an embedded newline"
      sid2 <- newSession $ NewSessionReq "this is\na session name" True
      step "Get its actual name"
      name2 <- getSessionName sid2
      name2 @?= "this is"
      let tooLong = toS . concat $ replicate 7 ['0' .. '9']
      step "Create a new session with a name that is too long"
      sid3 <- newSession $ NewSessionReq tooLong True
      step "Get its actual name"
      name3 <- getSessionName sid3
      name3 @?= Text.take 64 tooLong

test_addSession_roundtrip :: TestTree
test_addSession_roundtrip =
  testCaseSteps "addSession database round-tripping" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Add a new session"
      sid <- addSession "even3App" even3App
      step "Get its name"
      name <- getSessionName sid
      name @?= "even3App"
      step "Get its app"
      app <- getApp sid
      app @?= even3App

-- Note: we don't bother testing paging here, because it's not very
-- interesting to test. 'listSessions' doesn't do any of the paging,
-- that's all handled by the database implementation.
test_listSessions :: TestTree
test_listSessions =
  testCaseSteps "listSessions" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "List session on an empty database"
      s0 <- listSessions $ OL{offset = 0, limit = Nothing}
      total s0 @?= 0
      let m :: Int = 107
      step $ "Create " <> show m <> " sessions"
      ss <- forM ([1 .. m] :: [Int]) $ const $ newSession $ NewSessionReq "new session" True
      step "List all the sessions"
      ss' <- listSessions $ OL{offset = 0, limit = Nothing}
      total ss' @?= m
      -- Sort by session ID, because 'listSessions' sorts by name but
      -- all the new session names are the same by default.
      --
      -- Note: hlint is confused by our use of 'id' here. It's because
      -- Protolude renames the Prelude's 'id' to 'identity' and we use
      -- 'id' as one of the field names in the 'Session' type.
      --
      -- See:
      -- https://github.com/hackworthltd/primer/issues/545
      {- HLINT ignore test_listSessions "Functor law" -}
      sort (id <$> pageContents ss') @?= sort ss

-- Note: we don't bother testing paging here, because it's not very
-- interesting to test. 'findSessions' doesn't do any of the paging,
-- that's all handled by the database implementation.
test_findSessions :: TestTree
test_findSessions =
  testCaseSteps "findSessions" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Find session on an empty database"
      s0 <- findSessions "nope" $ OL{offset = 0, limit = Nothing}
      total s0 @?= 0
      let m :: Int = 100
      step $ "Create " <> show m <> " sessions"
      forM_ ([1 .. m] :: [Int]) $ \n -> newSession $ NewSessionReq ("new session " <> show n) True
      step "Find all the sessions whose names contain '1'"
      ss' <- findSessions "1" $ OL{offset = 0, limit = Nothing}
      total ss' @?= 20
      step "Find all the sessions whose names contain '99'"
      ss'' <- findSessions "99" $ OL{offset = 0, limit = Nothing}
      total ss'' @?= 1
      step "Find all the sessions whose names contain '101'"
      ss''' <- findSessions "101" $ OL{offset = 0, limit = Nothing}
      total ss''' @?= 0

test_copySession :: TestTree
test_copySession =
  testCaseSteps "copySession" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Add a session"
      sid <- addSession "foo" even3App
      step "Change its name"
      name <- renameSession sid "original session"
      step "Copy it to a new session"
      sid' <- copySession sid
      step "Ensure the session IDs are distinct"
      liftIO $ assertBool "copied session ID is not different than original" $ sid' /= sid
      step "Check the copied session's name"
      name' <- getSessionName sid'
      name' @?= name
      step "Check the copied session's app"
      app <- getApp sid'
      app @?= even3App
      step "Rename the original sesision"
      name'' <- renameSession sid "still the original"
      liftIO $ assertBool "copied session name was changed" $ name' /= name''
      step "Rename the copied session"
      name''' <- renameSession sid' "new copy name"
      liftIO $ assertBool "original session name was changed" $ name''' /= name''

test_copySession_failure :: TestTree
test_copySession_failure =
  testCaseSteps "copySession failure" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "copy a nonexistent session"
      id_ <- liftIO nextRandom
      assertException "copySession" (const True :: ExceptionPredicate PrimerErr) $ copySession id_

test_deleteSession :: TestTree
test_deleteSession =
  testCaseSteps "deleteSession" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Add a session"
      sid <- addSession "foo" even3App
      step "Add a second session with the same name as the first"
      sid' <- addSession "foo" even3App
      step "Delete the first session"
      deleteSession sid
      step "Ensure the first session is deleted"
      assertException "deleteSession" (const True :: ExceptionPredicate PrimerErr) $ copySession sid
      step "Ensure the second session hasn't been deleted"
      name' <- getSessionName sid'
      name' @?= "foo"
      step "Delete the second session"
      deleteSession sid'
      step "Ensure the second session is deleted"
      assertException "deleteSession" (const True :: ExceptionPredicate PrimerErr) $ copySession sid'

test_deleteSession_failure :: TestTree
test_deleteSession_failure =
  testCaseSteps "deleteSession failure" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "delete a nonexistent session"
      id_ <- liftIO nextRandom
      assertException "deleteSession" (const True :: ExceptionPredicate PrimerErr) $ deleteSession id_

test_getSessionName_failure :: TestTree
test_getSessionName_failure =
  testCaseSteps "getSessionName failure" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "get a nonexistent session"
      id_ <- liftIO nextRandom
      assertException "getSessionName" (const True :: ExceptionPredicate PrimerErr) $ getSessionName id_

test_getVersion :: TestTree
test_getVersion =
  testCaseSteps "getVersion" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Get the version"
      version <- getVersion
      version @?= "git123"

test_renameSession :: TestTree
test_renameSession =
  testCaseSteps "renameSession" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "Create a session"
      sid <- newSession $ NewSessionReq "testing" True
      step "Change its name"
      name <- renameSession sid "new name"
      step "Get the session's name"
      name' <- getSessionName sid
      name' @?= name

test_renameSession_failure :: TestTree
test_renameSession_failure =
  testCaseSteps "renameSession failure" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "rename a nonexistent session"
      id_ <- liftIO nextRandom
      assertException "renameSession" (const True :: ExceptionPredicate PrimerErr) $ renameSession id_ "new name"

test_renameSession_invalid_name :: TestTree
test_renameSession_invalid_name =
  testCaseSteps "renameSession invalid name" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      step "rename a session"
      sid <- newSession $ NewSessionReq "xyz" True
      void $ renameSession sid "abcd"
      step "rename it again with an invalid name"
      name <- renameSession sid ""
      step "it should be the default session name"
      name @?= fromSessionName defaultSessionName

test_renameSession_too_long :: TestTree
test_renameSession_too_long =
  testCaseSteps "renameSession with a too long name" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      sid <- newSession $ NewSessionReq "a new session" True
      -- Note: we cut off session names rather arbitrarily at 64 characters.
      step "rename a session with a name longer than 64 characters"
      name <- renameSession sid $ toS $ replicate 65 'a'
      step "it should be truncated at 64 characters"
      name @?= toS (replicate 64 'a')

test_eval_undo :: TestTree
test_eval_undo =
  testCaseSteps "eval plays nicely with undo/redo" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      let expectSuccess m =
            m >>= \case
              Left err -> liftIO $ assertFailure $ show err
              Right x -> pure x
      step "create session"
      sid <- newSession $ NewSessionReq "a new session" True
      let scope = mkSimpleModuleName "Main"
      step "eval"
      void $ evalFull' sid (Just 100) (Just UnderBinders) $ qualifyName scope "main"
      step "insert λ"
      let getMain = do
            p <- getProgram sid
            pure $ fmap astDefExpr . defAST =<< foldMap' moduleDefsQualified (progModules p) M.!? qualifyName scope "main"
      i1 <-
        getMain >>= \case
          Just e@EmptyHole{} -> pure $ getID e
          _ -> liftIO $ assertFailure "unexpected form of main"
      _ <-
        expectSuccess
          $ edit sid
          $ Edit
            [ MoveToDef $ qualifyName scope "main"
            , BodyAction
                [ SetCursor i1
                , InsertSaturatedVar $ GlobalVarRef Integer.even
                ]
            ]
      step "insert 4"
      i2 <-
        getMain >>= \case
          Just (App _ _ e) -> pure $ getID e
          _ -> liftIO $ assertFailure "unexpected form of main"
      _ <-
        expectSuccess
          $ edit sid
          $ Edit
            [ MoveToDef $ qualifyName scope "main"
            , BodyAction
                [ SetCursor i2
                , ConstructPrim $ PrimInt 4
                ]
            ]
      step "get edited App"
      app0 <- getApp sid
      step "undo"
      _ <- undo sid
      step "redo"
      _ <- redo sid
      step "undo *2"
      _ <- undo sid >> undo sid
      step "redo"
      _ <- redo sid
      step "redo"
      _ <- redo sid
      step "get final App"
      app1 <- getApp sid
      step "edited and redone progAllModules identical"
      app1 @?= app0

test_selectioninfo :: TestTree
test_selectioninfo =
  testCaseSteps "selection info on mismatches" $ \step' -> do
    runAPI $ do
      let step = liftIO . step'
      let assertFailure' = liftIO . assertFailure
      let expectSuccess m =
            m >>= \case
              Left err -> liftIO $ assertFailure $ show err
              Right x -> pure x
      step "create session"
      sid <- newSession $ NewSessionReq "a new session" True
      let scope = mkSimpleModuleName "Main"
      let getDef d = do
            p <- getProgram sid
            pure $ defAST =<< foldMap' moduleDefsQualified (progModules p) M.!? qualifyName scope d
      let getExpr d = astDefExpr <<$>> getDef d
      let getType d = astDefType <<$>> getDef d
      let mkExpr d as = do
            _ <- expectSuccess $ edit sid $ Edit [CreateDef scope $ Just $ unName d]
            i <-
              getExpr d >>= \case
                Just e@EmptyHole{} -> pure $ getID e
                _ -> assertFailure' $ "unexpected form of " <> toS (unName d)
            _ <-
              expectSuccess
                $ edit sid
                $ Edit
                  [ MoveToDef $ qualifyName scope d
                  , BodyAction $ SetCursor i : as
                  ]
            pure ()
      let mkType d as = do
            _ <- expectSuccess $ edit sid $ Edit [CreateDef scope $ Just $ unName d]
            i <-
              getType d >>= \case
                Just e@TEmptyHole{} -> pure $ getID e
                _ -> assertFailure' $ "unexpected form of " <> toS (unName d)
            _ <-
              expectSuccess
                $ edit sid
                $ Edit
                  [ MoveToDef $ qualifyName scope d
                  , SigAction $ SetCursor i : as
                  ]
            pure ()

      step "tm1 :: ? = not {? Zero ?}"
      mkExpr
        "tm1"
        [ ConstructApp
        , Move Child1
        , ConstructVar $ GlobalVarRef PL.not
        , Move Parent
        , Move Child2
        , constructSaturatedCon cZero
        ]
      htm1 <-
        getExpr "tm1" >>= \case
          Just (App _ v (Hole m c)) -> do
            forgetMetadata v @?= forgetMetadata (create' $ gvar PL.not)
            forgetMetadata c @?= forgetMetadata (create' $ con0 cZero)
            pure $ getID m
          e -> assertFailure' $ "unexpected form of tm1: " <> show e
      step "tm1 mismatch info"
      tm1tk <-
        getSelectionTypeOrKind sid
          $ SelectionDef
          $ DefSelection (qualifyName scope "tm1")
          $ Just
          $ NodeSelection BodyNode htm1
      zeroTKIds tm1tk
        @?= zeroTKIds
          ( Type
              $ Mismatch
                { got = viewTreeType $ create' $ tcon tNat
                , expected = viewTreeType $ create' $ tcon tBool
                }
          )

      step "tm2 :: ? = {? Zero ?} True"
      mkExpr
        "tm2"
        [ ConstructApp
        , Move Child1
        , EnterHole
        , constructSaturatedCon cZero
        , Move Parent
        , Move Parent
        , Move Child2
        , constructSaturatedCon cTrue
        ]
      htm2 <-
        getExpr "tm2" >>= \case
          Just (App _ (Hole m c1) c2) -> do
            forgetMetadata c1 @?= forgetMetadata (create' $ con0 cZero)
            forgetMetadata c2 @?= forgetMetadata (create' $ con0 cTrue)
            pure $ getID m
          e -> assertFailure' $ "unexpected form of tm2: " <> show e
      step "tm2 mismatch info"
      tm2tk <-
        getSelectionTypeOrKind sid
          $ SelectionDef
          $ DefSelection (qualifyName scope "tm2")
          $ Just
          $ NodeSelection BodyNode htm2
      zeroTKIds tm2tk
        @?= zeroTKIds
          ( Type
              $ Mismatch
                { got = viewTreeType $ create' $ tcon tNat
                , -- We require @expected@ to be an empty hole, matching
                  -- the behaviour of @? True@
                  -- Arguably we should change both this and the empty hole case to
                  -- expose that we expect @Bool -> Maybe Nat@, see
                  -- https://github.com/hackworthltd/primer/issues/81
                  expected = viewTreeType $ create' tEmptyHole
                }
          )

      step "ty1 :: List {? Maybe ?} = ?"
      mkType
        "ty1"
        [ ConstructTApp
        , Move Child1
        , constructTCon tList
        , Move Parent
        , Move Child2
        , constructTCon tMaybe
        ]
      hty1 <-
        getType "ty1" >>= \case
          Just (TApp _ c1 (THole m c2)) -> do
            forgetTypeMetadata c1 @?= forgetTypeMetadata (create' $ tcon tList)
            forgetTypeMetadata c2 @?= forgetTypeMetadata (create' $ tcon tMaybe)
            pure $ getID m
          e -> assertFailure' $ "unexpected form of ty1: " <> show e
      step "ty1 mismatch info"
      ty1tk <-
        getSelectionTypeOrKind sid
          $ SelectionDef
          $ DefSelection (qualifyName scope "ty1")
          $ Just
          $ NodeSelection SigNode hty1
      zeroTKIds ty1tk
        @?= zeroTKIds
          ( Kind
              $ Mismatch
                { got = viewTreeKind $ create' $ ktype `kfun` ktype
                , expected = viewTreeKind $ create' ktype
                }
          )

      step "ty2 :: {? Nat ?} Bool = ?"
      mkType
        "ty2"
        [ ConstructTApp
        , Move Child1
        , constructTCon tNat
        , Move Parent
        , Move Parent
        , Move Child2
        , constructTCon tBool
        ]
      hty2 <-
        getType "ty2" >>= \case
          Just (TApp _ (THole m c1) c2) -> do
            forgetTypeMetadata c1 @?= forgetTypeMetadata (create' $ tcon tNat)
            forgetTypeMetadata c2 @?= forgetTypeMetadata (create' $ tcon tBool)
            pure $ getID m
          e -> assertFailure' $ "unexpected form of ty2: " <> show e
      step "ty2 mismatch info"
      ty2tk <-
        getSelectionTypeOrKind sid
          $ SelectionDef
          $ DefSelection (qualifyName scope "ty2")
          $ Just
          $ NodeSelection SigNode hty2
      zeroTKIds ty2tk
        @?= zeroTKIds
          ( Kind
              $ Mismatch
                { got = viewTreeKind $ create' ktype
                , -- We require @expected@ to be @?@, matching the behaviour of an empty hole.
                  -- Arguably we should change both this and the empty hole case to
                  -- expose that we expect @* -> *@, see
                  -- https://github.com/hackworthltd/primer/issues/81
                  expected = viewTreeKind $ create' khole
                }
          )

zeroTKIds :: TypeOrKind -> TypeOrKind
zeroTKIds = \case
  Type om -> Type $ zOMIds om
  Kind om -> Kind $ zOMIds om
  where
    zOMIds :: OkOrMismatch -> OkOrMismatch
    zOMIds = \case
      Ok t -> Ok $ zTIds t
      Mismatch t1 t2 -> Mismatch (zTIds t1) (zTIds t2)
    zTIds :: Tree -> Tree
    zTIds = treeIds .~ "0"
