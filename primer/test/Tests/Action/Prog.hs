{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Tests.Action.Prog where

import Foreword

import Control.Monad.Fresh
import qualified Data.Map.Strict as Map
import Optics
import Primer.Action (
  Action (
    ConstructAnn,
    ConstructArrowL,
    ConstructLam,
    ConstructLet,
    ConstructTCon,
    ConstructVar,
    Delete,
    EnterType,
    Move
  ),
  ActionError (NameCapture),
  Movement (Branch, Child1, Child2),
 )
import Primer.App (
  App (..),
  Log (..),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgAction (..),
  ProgError (..),
  Question (VariablesInScope),
  Selection (..),
  defaultTypeDefs,
  handleEditRequest,
  handleQuestion,
  importModules,
  lookupASTDef,
  newApp,
  newEmptyApp,
  newEmptyProg,
  newProg,
  tcWholeProg,
 )
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Def (..),
  Expr' (..),
  GVarName,
  ID (ID),
  Kind (KType),
  Meta (..),
  TmVarRef (..),
  TyConName,
  Type' (..),
  TypeDef (..),
  ValCon (..),
  defAST,
  defName,
  getID,
  typeDefAST,
  _exprMeta,
  _exprTypeMeta,
  _id,
  _typeMeta,
 )
import Primer.Core.DSL (
  S,
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  create,
  emptyHole,
  gvar,
  hole,
  lAM,
  lam,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
 )
import Primer.Core.Utils (forgetIDs)
import Primer.Module (Module (moduleDefs, moduleTypes))
import Primer.Name
import Primer.Typecheck (mkTypeDefMap)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@=?), (@?=))
import TestM (TestM, evalTestM)
import TestUtils (withPrimDefs)
import Tests.Typecheck (checkProgWellFormed)

unit_empty_actions_only_change_the_log :: Assertion
unit_empty_actions_only_change_the_log = progActionTest defaultEmptyProg [] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[]]}

-- We can move to the default def in a program
-- (this may only exist at the start of a session)
unit_move_to_def_main :: Assertion
unit_move_to_def_main = progActionTest defaultEmptyProg [MoveToDef "main"] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[MoveToDef "main"]]}

-- Expression actions are tested in ActionTest - here we just check that we can modify the correct
-- def.
unit_move_to_def_and_construct_let :: Assertion
unit_move_to_def_and_construct_let =
  progActionTest defaultEmptyProg [MoveToDef "other", BodyAction [ConstructLet (Just "x")]] $
    expectSuccess $ \prog prog' ->
      case astDefExpr <$> lookupASTDef "other" (moduleDefs $ progModule prog') of
        Just Let{} ->
          -- Check that main is unchanged
          Map.lookup "main" (moduleDefs $ progModule prog') @?= Map.lookup "main" (moduleDefs $ progModule prog)
        _ -> assertFailure "definition not found"

unit_rename_def :: Assertion
unit_rename_def =
  progActionTest defaultEmptyProg [RenameDef "other" "foo"] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just "foo"
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just "main"

unit_rename_def_to_same_name_as_existing_def :: Assertion
unit_rename_def_to_same_name_as_existing_def =
  progActionTest defaultEmptyProg [RenameDef "main" "main"] $
    expectError (@?= DefAlreadyExists "main")

unit_rename_def_to_same_name_as_existing_def_prim :: Assertion
unit_rename_def_to_same_name_as_existing_def_prim =
  progActionTest defaultFullProg [RenameDef "other" "toUpper"] $
    expectError (@?= DefAlreadyExists "toUpper")

unit_rename_def_referenced :: Assertion
unit_rename_def_referenced =
  progActionTest
    defaultEmptyProg
    [ MoveToDef "main"
    , BodyAction [ConstructVar $ GlobalVarRef "other"]
    , RenameDef "other" "foo"
    ]
    $ expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just "foo"
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just "main"
      fmap (set _exprMeta () . astDefExpr) (defAST =<< Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just (Var () $ GlobalVarRef "foo")

unit_rename_def_recursive :: Assertion
unit_rename_def_recursive =
  progActionTest
    defaultEmptyProg
    [ MoveToDef "main"
    , BodyAction [ConstructVar $ GlobalVarRef "main"]
    , RenameDef "main" "foo"
    ]
    $ expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just "foo"
      fmap (set _exprMeta () . astDefExpr) (defAST =<< Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just (Var () $ GlobalVarRef "foo")

unit_delete_def :: Assertion
unit_delete_def =
  progActionTest defaultEmptyProg [DeleteDef "other"] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just "main"

unit_delete_def_unknown_id :: Assertion
unit_delete_def_unknown_id =
  progActionTest defaultEmptyProg [DeleteDef "unknown"] $
    expectError (@?= DefNotFound "unknown")

unit_delete_def_used_id :: Assertion
unit_delete_def_used_id =
  progActionTest defaultEmptyProg [MoveToDef "main", BodyAction [ConstructVar $ GlobalVarRef "other"], DeleteDef "other"] $
    expectError (@?= DefInUse "other")

-- 'foo = foo' shouldn't count as "in use" and block deleting itself
unit_delete_def_recursive :: Assertion
unit_delete_def_recursive =
  progActionTest defaultEmptyProg [MoveToDef "main", BodyAction [ConstructVar $ GlobalVarRef "main"], DeleteDef "main"] $
    expectSuccess $ \prog prog' -> Map.delete "main" (moduleDefs $ progModule prog) @?= moduleDefs (progModule prog')

unit_move_to_unknown_def :: Assertion
unit_move_to_unknown_def =
  progActionTest defaultEmptyProg [MoveToDef "unknown"] $ expectError (@?= DefNotFound "unknown")

unit_rename_unknown_def :: Assertion
unit_rename_unknown_def =
  progActionTest defaultEmptyProg [RenameDef "unknown" "foo"] $ expectError (@?= DefNotFound "unknown")

unit_construct_let_without_moving_to_def_first :: Assertion
unit_construct_let_without_moving_to_def_first =
  progActionTest defaultEmptyProg [BodyAction [ConstructLet (Just "x")]] $ expectError (@?= NoDefSelected)

unit_create_def :: Assertion
unit_create_def = progActionTest defaultEmptyProg [CreateDef $ Just "newDef"] $
  expectSuccess $ \_ prog' -> do
    case lookupASTDef "newDef" (moduleDefs $ progModule prog') of
      Nothing -> assertFailure $ show $ moduleDefs $ progModule prog'
      Just def -> do
        astDefName def @?= "newDef"
        astDefExpr def @?= EmptyHole (Meta 4 Nothing Nothing)

unit_create_def_clash_prim :: Assertion
unit_create_def_clash_prim =
  progActionTest defaultFullProg [CreateDef $ Just "toUpper"] $
    expectError (@?= DefAlreadyExists "toUpper")

unit_create_typedef :: Assertion
unit_create_typedef =
  let lst =
        ASTTypeDef
          { astTypeDefName = "List"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors =
              [ ValCon "Nil" []
              , ValCon "Cons" [TVar () "a", TApp () (TCon () "List") (TVar () "a")]
              ]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
      tree =
        ASTTypeDef
          { astTypeDefName = "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "Node" [TVar () "a", TApp () (TCon () "List") (TApp () (TCon () "Tree") (TVar () "a"))]]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
   in progActionTest defaultEmptyProg [AddTypeDef lst, AddTypeDef tree] $
        expectSuccess $
          \_ prog' -> do
            case Map.elems $ moduleTypes $ progModule prog' of
              [lst', tree'] -> do
                TypeDefAST lst @=? lst'
                TypeDefAST tree @=? tree'
              _ -> assertFailure $ show $ moduleTypes $ progModule prog'

-- "List" is unknown here
unit_create_typedef_bad_1 :: Assertion
unit_create_typedef_bad_1 =
  let td =
        ASTTypeDef
          { astTypeDefName = "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "Node" [TVar () "a", TApp () (TCon () "List") (TApp () (TCon () "Tree") (TVar () "a"))]]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "UnknownTypeConstructor \"List\"")

-- duplicate type(names) added
unit_create_typedef_bad_2 :: Assertion
unit_create_typedef_bad_2 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named TypeDefs\"")

-- Forbid duplicate constructor names within one type
unit_create_typedef_bad_3 :: Assertion
unit_create_typedef_bad_3 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors =
              [ ValCon "C" []
              , ValCon "C" []
              ]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named constructor (perhaps in different typedefs)\"")

-- Forbid duplicate constructor names across types
unit_create_typedef_bad_4 :: Assertion
unit_create_typedef_bad_4 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T1"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "T2"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named constructor (perhaps in different typedefs)\"")

-- Forbid duplicate parameter names
unit_create_typedef_bad_5 :: Assertion
unit_create_typedef_bad_5 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("a", KType), ("a", KType)]
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between parameter-names and constructor-names\"")

-- Forbid clash between type name and parameter name
unit_create_typedef_bad_6 :: Assertion
unit_create_typedef_bad_6 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("T", KType)]
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between type-def-name and parameter-names\"")

-- Forbid clash between parameter name and constructor name
unit_create_typedef_bad_7 :: Assertion
unit_create_typedef_bad_7 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon "a" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between parameter-names and constructor-names\"")

-- Forbid clash between type name and name of a primitive type
unit_create_typedef_bad_prim :: Assertion
unit_create_typedef_bad_prim =
  let td =
        ASTTypeDef
          { astTypeDefName = "Char"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultFullProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named TypeDefs\"")

-- Allow clash between type name and constructor name in one type
unit_create_typedef_8 :: Assertion
unit_create_typedef_8 =
  let td =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "T" []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectSuccess $ \_ prog' -> Map.elems (moduleTypes (progModule prog')) @?= [TypeDefAST td]

-- Allow clash between type name and constructor name across types
unit_create_typedef_9 :: Assertion
unit_create_typedef_9 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon "C" []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = "C"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectSuccess $ \_ prog' -> Map.elems (moduleTypes (progModule prog')) @?= [TypeDefAST td2, TypeDefAST td1]

unit_construct_arrow_in_sig :: Assertion
unit_construct_arrow_in_sig =
  progActionTest defaultEmptyProg [MoveToDef "other", SigAction [ConstructArrowL, Move Child1]] $
    expectSuccess $ \_ prog' ->
      case lookupASTDef "other" (moduleDefs $ progModule prog') of
        Just def ->
          -- Check that the signature is an arrow type
          case astDefType def of
            TFun _ lhs _ ->
              -- Check that the selection is focused on the lhs, as we instructed
              case progSelection prog' of
                Just (Selection d (Just NodeSelection{nodeType = SigNode, nodeId})) -> do
                  d @?= astDefName def
                  nodeId @?= getID lhs
                _ -> assertFailure "no selection"
            _ -> assertFailure "not a function"
        _ -> assertFailure "definition not found"

unit_sigaction_creates_holes :: Assertion
unit_sigaction_creates_holes =
  let acts =
        [ -- main :: Char
          MoveToDef "main"
        , SigAction [ConstructTCon "Char"]
        , -- other :: Char; other = main
          MoveToDef "other"
        , SigAction [ConstructTCon "Char"]
        , BodyAction [ConstructVar $ GlobalVarRef "main"]
        , -- main :: Int
          -- We expect this to change 'other' to contain a hole
          MoveToDef "main"
        , SigAction [Delete, ConstructTCon "Int"]
        ]
   in progActionTest defaultFullProg acts $
        expectSuccess $ \_ prog' ->
          case lookupASTDef "other" (moduleDefs $ progModule prog') of
            Just def ->
              -- Check that the definition is a non-empty hole
              case astDefExpr def of
                Hole _ (Var _ (GlobalVarRef "main")) -> pure ()
                _ -> assertFailure "expected {? main ?}"
            _ -> assertFailure "definition not found"

unit_copy_paste_duplicate :: Assertion
unit_copy_paste_duplicate = do
  let ((p, fromType, fromExpr, _toType, _toExpr), maxID) = create $ do
        mainType <- tforall "a" KType (tvar "a" `tfun` (tcon "Maybe" `tapp` tEmptyHole))
        mainExpr <- lAM "b" $ lam "x" $ con "Just" `aPP` tvar "b" `app` lvar "x"
        let mainDef = ASTDef "main" mainExpr mainType
        blankDef <- ASTDef "blank" <$> emptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing}
            & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST mainDef), ("blank", DefAST blankDef)]
          , getID mainType
          , getID mainExpr
          , getID (astDefType blankDef)
          , getID (astDefExpr blankDef)
          )
      fromDef = "main"
      toDef = "blank"
  let a = newApp{appProg = p}
      actions = [MoveToDef toDef, CopyPasteSig (fromDef, fromType) [], CopyPasteBody (fromDef, fromExpr) []]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef fromDef (moduleDefs $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef fromDef (moduleDefs $ progModule r)
            assertBool "equal to toDef" $ src /= lookupASTDef "blank" (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) "blank" src) @?= clearIDs (lookupASTDef "blank" (moduleDefs $ progModule r))

-- ∀a . (∀b,c . a -> b -> ∀d. c -> d)  -> ∀c. ?
-- copy         ^------------------^
-- paste                                      ^
-- should result in
-- ∀a . (∀b,c . a -> b -> ∀d. c -> d)  -> ∀c. a -> ? -> ∀d. ? -> d
--
-- This tests that we handle scoping correctly:
-- - The a is in-scope: so copied
-- - The b is out-of-scope, so replaced with a hole
-- - The c is out-of-scope (even though the target has a 'c' in-scope, it is a
--   different binder), so replace with a hole
-- - The d is bound within the copied subtree, so it is in-scope
unit_copy_paste_type_scoping :: Assertion
unit_copy_paste_type_scoping = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tvar "a" `tfun` tvar "b" `tfun` tforall "d" KType (tvar "c" `tfun` tvar "d")
        let skel r = tforall "a" KType $ tfun (tforall "b" KType $ tforall "c" KType $ pure toCopy) $ tforall "c" KType r
        defInitial <- ASTDef "main" <$> emptyHole <*> skel tEmptyHole
        expected <- ASTDef "main" <$> emptyHole <*> skel (tvar "a" `tfun` tEmptyHole `tfun` tforall "d" KType (tEmptyHole `tfun` tvar "d"))
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef "main", CopyPasteSig ("main", srcID) [Move Child1, Move Child2, Move Child1]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in -- clearIDs (set (_Just % #defName) "blank" src ) @?= clearIDs (Map.lookup toDef (progDefs r))
          clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- ∀a b.a ~> ∀a.a
unit_raise :: Assertion
unit_raise = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tvar "a"
        defInitial <- ASTDef "main" <$> emptyHole <*> tforall "a" KType (tforall "b" KType $ pure toCopy)
        expected <- ASTDef "main" <$> emptyHole <*> tforall "a" KType (tvar "a")
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef "main", CopyPasteSig ("main", srcID) [Move Child1, Delete]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- ∀a. List a -> ∀b. b -> Pair a b
-- /\a . λ x . case x of Nil -> ? ; Cons y ys -> /\@b z -> Pair @a @b y z
-- copy the Just @a y into the hole to get
-- /\a . λ x . case x of Nil -> lettype b = ? in let y = ? : a in Pair @a @b y z ; Cons y ys -> /\@b z -> Pair @a @b y z
unit_copy_paste_expr_1 :: Assertion
unit_copy_paste_expr_1 = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        ty <- tforall "a" KType $ (tcon "List" `tapp` tvar "a") `tfun` tforall "b" KType (tvar "b" `tfun` (tcon "Pair" `tapp` tvar "a" `tapp` tvar "b"))
        let toCopy' = con "MakePair" `aPP` tvar "a" `aPP` tvar "b" `app` lvar "y" `app` lvar "z" -- want different IDs for the two occurences in expected
        toCopy <- toCopy'
        let skel r =
              lAM "a" $
                lam "x" $
                  case_
                    (lvar "x")
                    [ branch "Nil" [] r
                    , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ lAM "b" $ lam "z" $ pure toCopy
                    ]
        expectPasted <- con "MakePair" `aPP` tvar "a" `aPP` tEmptyHole `app` emptyHole `app` emptyHole
        -- TODO: in the future we may want to insert let bindings for variables
        -- which are out of scope in the target, and produce something like
        -- expectPasted <- letType "b" tEmptyHole $ let_ "y" (emptyHole `ann` tvar "a") $ let_ "z" (emptyHole `ann` tvar "b") toCopy'
        defInitial <- ASTDef "main" <$> skel emptyHole <*> pure ty
        expected <- ASTDef "main" <$> skel (pure expectPasted) <*> pure ty
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef "main", CopyPasteBody ("main", srcID) [Move Child1, Move Child1, Move (Branch "Nil")]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

unit_copy_paste_ann :: Assertion
unit_copy_paste_ann = do
  let ((p, fromAnn), maxID) = create $ do
        toCopy <- tcon "Bool"
        mainDef <- ASTDef "main" <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        blankDef <- ASTDef "blank" <$> emptyHole `ann` tEmptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing} & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST mainDef), ("blank", DefAST blankDef)]
          , getID toCopy
          )
  let a = newApp{appProg = p}
      actions = [MoveToDef "blank", CopyPasteBody ("main", fromAnn) [EnterType]]
  let (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef "main" (moduleDefs $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef "main" (moduleDefs $ progModule r)
            assertBool "equal to blank" $ src /= lookupASTDef "blank" (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) "blank" src) @?= clearIDs (lookupASTDef "blank" (moduleDefs $ progModule r))

unit_copy_paste_ann2sig :: Assertion
unit_copy_paste_ann2sig = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tcon "Bool"
        defInitial <- ASTDef "main" <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        expected <- ASTDef "main" <$> emptyHole `ann` pure toCopy <*> tcon "Bool"
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef "main", CopyPasteSig ("main", srcID) []]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

unit_copy_paste_sig2ann :: Assertion
unit_copy_paste_sig2ann = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tcon "Bool"
        defInitial <- ASTDef "main" <$> emptyHole <*> pure toCopy
        expected <- ASTDef "main" <$> emptyHole `ann` tcon "Bool" <*> pure toCopy
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef "main", CopyPasteBody ("main", srcID) [ConstructAnn, EnterType]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- VariablesInScope sees imported terms
unit_import_vars :: Assertion
unit_import_vars =
  let test = do
        p <- defaultFullProg
        importModules [progModule p]
        gets (Map.assocs . moduleDefs . progModule . appProg) >>= \case
          [(i, DefAST d)] -> do
            a' <- get
            (_, vs) <- runReaderT (handleQuestion (VariablesInScope i $ getID $ astDefExpr d)) a'
            pure $
              assertBool "VariablesInScope did not report the imported Int.+" $
                any ((== "Int.+") . fst) vs
          _ -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Can reference something in an imported module (both types and terms)
unit_import_reference :: Assertion
unit_import_reference =
  let test = do
        p <- defaultFullProg
        importModules [progModule p]
        prog <- gets appProg
        case (findGlobalByName prog "toUpper", Map.assocs $ moduleDefs $ progModule prog) of
          (Just toUpperDef, [(i, _)]) -> do
            _ <-
              handleEditRequest
                [ MoveToDef i
                , SigAction [ConstructTCon "Char"]
                , BodyAction [ConstructVar $ GlobalVarRef $ defName toUpperDef]
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported toUpper"
          (Just _, _) -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Can copy and paste from an imported module
unit_copy_paste_import :: Assertion
unit_copy_paste_import =
  let test = do
        p <- defaultFullProg
        importModules [progModule p]
        prog <- gets appProg
        case (findGlobalByName prog "other", Map.assocs $ moduleDefs $ progModule prog) of
          (Just (DefAST other), [(i, _)]) -> do
            let fromDef = astDefName other
                fromType = getID $ astDefType other
                fromExpr = getID $ astDefExpr other
            _ <-
              handleEditRequest
                [ MoveToDef i
                , CopyPasteSig (fromDef, fromType) []
                , CopyPasteBody (fromDef, fromExpr) []
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported 'other'"
          (Just _, _) -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

unit_rename_def_capture :: Assertion
unit_rename_def_capture =
  progActionTest defaultEmptyProg [MoveToDef "other", BodyAction [ConstructLam $ Just "foo"], RenameDef "main" "foo"] $
    expectError (@?= ActionError NameCapture)

unit_RenameType :: Assertion
unit_RenameType =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <- emptyHole `ann` (tcon "T" `tapp` tcon "Bool")
              ASTDef "def" x <$> tEmptyHole
          ]
    )
    [RenameType "T" "T'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T'" prog'
      astTypeDefName td @?= "T'"
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              emptyHole `ann` (tcon "T'" `tapp` tcon "Bool")
          )

unit_RenameType_clash :: Assertion
unit_RenameType_clash =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameType "T" "Int"]
    $ expectError (@?= TypeDefAlreadyExists "Int")

unit_RenameCon :: Assertion
unit_RenameCon =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <-
                hole
                  ( hole
                      (con "A")
                  )
              ASTDef "def" x <$> tEmptyHole
          ]
    )
    [RenameCon "T" "A" "A'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T" prog'
      astTypeDefConstructors td
        @?= [ ValCon "A'" [TCon () "Bool", TCon () "Bool", TCon () "Bool"]
            , ValCon "B" [TVar () "b"]
            ]
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole
                ( hole
                    (con "A'")
                )
          )

unit_RenameCon_clash :: Assertion
unit_RenameCon_clash =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <-
                hole
                  ( hole
                      (con "A")
                  )
              ASTDef "def" x <$> tEmptyHole
          ]
    )
    [RenameCon "T" "A" "True"]
    $ expectError (@?= ConAlreadyExists "True")

unit_RenameTypeParam :: Assertion
unit_RenameTypeParam =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameTypeParam "T" "b" "b'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T" prog'
      astTypeDefParameters td @?= [("a", KType), ("b'", KType)]
      astTypeDefConstructors td
        @?= [ ValCon "A" [TCon () "Bool", TCon () "Bool", TCon () "Bool"]
            , ValCon "B" [TVar () "b'"]
            ]

unit_RenameTypeParam_clash :: Assertion
unit_RenameTypeParam_clash =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameTypeParam "T" "a" "b"]
    $ expectError (@?= ParamAlreadyExists "b")

unit_AddCon :: Assertion
unit_AddCon =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <-
                case_
                  (emptyHole `ann` (tcon "T" `tapp` tcon "Bool" `tapp` tcon "Int"))
                  [ branch "A" [] emptyHole
                  , branch "B" [] emptyHole
                  ]
              ASTDef "def" x <$> tEmptyHole
          ]
    )
    [AddCon "T" 1 "C"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T" prog'
      astTypeDefConstructors td
        @?= [ ValCon "A" [TCon () "Bool", TCon () "Bool", TCon () "Bool"]
            , ValCon "C" []
            , ValCon "B" [TVar () "b"]
            ]
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon "T" `tapp` tcon "Bool" `tapp` tcon "Int"))
                [ branch "A" [] emptyHole
                , branch "C" [] emptyHole
                , branch "B" [] emptyHole
                ]
          )

unit_SetConFieldType :: Assertion
unit_SetConFieldType =
  progActionTest
    ( defaultProgEditableTypeDefs . sequence . pure $ do
        x <- con "A" `app` lvar "x" `app` (gvar "y" `ann` tcon "Bool")
        ASTDef "def" x <$> tEmptyHole
    )
    [SetConFieldType "T" "A" 1 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T" prog'
      astTypeDefConstructors td
        @?= [ ValCon "A" [TCon () "Bool", TCon () "Int", TCon () "Bool"]
            , ValCon "B" [TVar () "b"]
            ]
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              con "A" `app` lvar "x" `app` hole (gvar "y" `ann` tcon "Bool")
          )

unit_SetConFieldType_partial_app :: Assertion
unit_SetConFieldType_partial_app =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con "A" `app` lvar "x"
        sequence
          [ ASTDef "def" x <$> tcon "T"
          ]
    )
    [SetConFieldType "T" "A" 1 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole $
                con "A" `app` lvar "x"
          )

unit_SetConFieldType_case :: Assertion
unit_SetConFieldType_case =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <-
          case_
            (emptyHole `ann` (tcon "T" `tapp` tEmptyHole `tapp` tEmptyHole))
            [ branch
                "A"
                [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                (lvar "y")
            , branch "B" [] emptyHole
            ]
        sequence
          [ ASTDef "def" x <$> tcon "Bool"
          ]
    )
    [SetConFieldType "T" "A" 1 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon "T" `tapp` tEmptyHole `tapp` tEmptyHole))
                [ branch
                    "A"
                    [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                    (hole $ lvar "y")
                , branch "B" [] emptyHole
                ]
          )

unit_AddConField :: Assertion
unit_AddConField =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con "A" `app` con "True"
        sequence
          [ ASTDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField "T" "A" 1 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef "T" prog'
      astTypeDefConstructors td
        @?= [ ValCon "A" [TCon () "Bool", TCon () "Int", TCon () "Bool", TCon () "Bool"]
            , ValCon "B" [TVar () "b"]
            ]
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              con "A" `app` con "True" `app` emptyHole
          )

unit_AddConField_partial_app :: Assertion
unit_AddConField_partial_app =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con "A" `app` con "True"
        sequence
          [ ASTDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField "T" "A" 2 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole $ con "A" `app` con "True"
          )

unit_AddConField_case :: Assertion
unit_AddConField_case =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <-
          case_
            (emptyHole `ann` (tcon "T" `tapp` tEmptyHole `tapp` tEmptyHole))
            [ branch
                "A"
                [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                (lvar "y")
            , branch "B" [] emptyHole
            ]
        sequence
          [ ASTDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField "T" "A" 2 $ TCon () "Int"]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef "def" prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon "T" `tapp` tEmptyHole `tapp` tEmptyHole))
                [ branch
                    "A"
                    [("x", Nothing), ("y", Nothing), ("a19", Nothing), ("z", Nothing)]
                    (lvar "y")
                , branch "B" [] emptyHole
                ]
          )

-- * Utilities

findGlobalByName :: Prog -> GVarName -> Maybe Def
findGlobalByName p n = Map.lookup n . foldMap moduleDefs $ progModule p : progImports p

-- We use a program with two defs: "main" and "other"
defaultEmptyProg :: MonadFresh ID m => m Prog
defaultEmptyProg = do
  mainExpr <- emptyHole
  mainType <- tEmptyHole
  otherExpr <- emptyHole
  otherType <- tEmptyHole
  let mainDef = ASTDef "main" mainExpr mainType
      otherDef = ASTDef "other" otherExpr otherType
   in pure $
        newEmptyProg
          { progSelection =
              Just $
                Selection (astDefName mainDef) $
                  Just
                    NodeSelection
                      { nodeType = BodyNode
                      , nodeId = 1
                      , meta = Left (Meta 1 Nothing Nothing)
                      }
          }
          & #progModule
          % #moduleDefs
          .~ Map.fromList [(astDefName mainDef, DefAST mainDef), (astDefName otherDef, DefAST otherDef)]

unit_good_defaultEmptyProg :: Assertion
unit_good_defaultEmptyProg = checkProgWellFormed defaultEmptyProg

-- `defaultEmptyProg`, plus all primitive definitions (types and terms),
-- and all builtin types.
defaultFullProg :: MonadFresh ID m => m Prog
defaultFullProg = do
  p <- defaultEmptyProg
  withPrimDefs $ \m ->
    pure $
      over (#progModule % #moduleTypes) (defaultTypeDefs <>)
        . over (#progModule % #moduleDefs) ((DefPrim <$> m) <>)
        $ p

findTypeDef :: TyConName -> Prog -> IO ASTTypeDef
findTypeDef d p = maybe (assertFailure "couldn't find typedef") pure $ (typeDefAST <=< Map.lookup d) $ p ^. (#progModule % #moduleTypes)

findDef :: GVarName -> Prog -> IO ASTDef
findDef d p = maybe (assertFailure "couldn't find def") pure $ (defAST <=< Map.lookup d) $ p ^. (#progModule % #moduleDefs)

-- We use the same type definition for all tests related to editing type definitions
defaultProgEditableTypeDefs :: MonadFresh ID f => f [ASTDef] -> f Prog
defaultProgEditableTypeDefs ds = do
  p <- defaultEmptyProg
  ds' <- ds
  let tds =
        [ TypeDefAST
            ASTTypeDef
              { astTypeDefName = "T"
              , astTypeDefParameters = [("a", KType), ("b", KType)]
              , astTypeDefConstructors = [ValCon "A" (replicate 3 $ TCon () "Bool"), ValCon "B" [TVar () "b"]]
              , astTypeDefNameHints = []
              }
        ]
  pure $
    p
      & (#progModule % #moduleTypes) %~ ((mkTypeDefMap tds <> defaultTypeDefs) <>)
      & (#progModule % #moduleDefs) %~ (Map.fromList ((\d -> (astDefName d, DefAST d)) <$> ds') <>)

unit_good_defaultFullProg :: Assertion
unit_good_defaultFullProg = checkProgWellFormed defaultFullProg

_defIDs :: Traversal' ASTDef ID
_defIDs = #astDefExpr % (_exprMeta % _id `adjoin` _exprTypeMeta % _id) `adjoin` #astDefType % _typeMeta % _id

-- Tests that the result is successful, and then applies the given test to the returned program
-- The test takes two arguments: the original input program and the output program
expectSuccess :: (Prog -> Prog -> Assertion) -> Prog -> Either ProgError Prog -> Assertion
expectSuccess f p = \case
  Right r -> f p r
  Left e -> assertFailure $ show e

expectError :: (ProgError -> Assertion) -> Prog -> Either ProgError Prog -> Assertion
expectError f _ = \case
  Right r -> assertFailure $ show r
  Left e -> f e

-- Run the given ProgActions against the given Prog, and pass the result to the given test function
progActionTest :: S Prog -> [ProgAction] -> (Prog -> Either ProgError Prog -> Assertion) -> Assertion
progActionTest inputProg actions testOutput = do
  let (prog, maxID) = create inputProg
  let a = newEmptyApp{appProg = prog}
  testOutput prog $ fst $ runAppTestM maxID a (handleEditRequest actions)

newtype AppTestM a = AppTestM {unAppTestM :: StateT App (ExceptT ProgError TestM) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFresh ID
    , MonadFresh NameCounter
    , MonadState App
    , MonadError ProgError
    )

runAppTestM :: ID -> App -> AppTestM a -> (Either ProgError a, App)
runAppTestM startID a m =
  case evalTestM startID $ runExceptT $ flip runStateT a $ unAppTestM m of
    Left err -> (Left err, a)
    Right (res, app') -> (Right res, app')
