{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Tests.Action.Prog where

import Foreword

import Control.Monad.Fresh
import Data.Generics.Uniplate.Data (transformBi)
import Data.List.Extra (anySame)
import qualified Data.Map.Strict as Map
import Optics
import Primer.Action (
  Action (
    ConstructAnn,
    ConstructArrowL,
    ConstructLet,
    ConstructTCon,
    ConstructVar,
    Delete,
    EnterType,
    Move
  ),
  ActionError (ImportNameClash),
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
  Question (GenerateName, VariablesInScope),
  Selection (..),
  handleEditRequest,
  handleQuestion,
  importModules,
  lookupASTDef,
  newApp,
  newEmptyApp,
  newEmptyProg,
  newProg,
  progAllModules,
  tcWholeProg,
 )
import Primer.Builtins (builtinModule, cCons, cJust, cMakePair, cNil, tBool, tList, tMaybe, tPair)
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Def (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName),
  ID (ID),
  Kind (KType),
  Meta (..),
  ModuleName (ModuleName, unModuleName),
  TmVarRef (..),
  TyConName,
  Type,
  Type' (..),
  TypeDef (..),
  ValCon (..),
  ValConName,
  defAST,
  defName,
  getID,
  qualifyName,
  typeDefAST,
  typeDefName,
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
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes), mkTypeDefMap, moduleDefsQualified, moduleTypesQualified)
import Primer.Name
import Primer.Primitives (primitiveGVar, primitiveModule, tChar)
import Primer.Typecheck (TypeError (UnknownTypeConstructor))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@=?), (@?=))
import TestM (TestM, evalTestM)
import TestUtils (constructTCon)
import qualified TestUtils
import Tests.Typecheck (checkProgWellFormed)

unit_empty_actions_only_change_the_log :: Assertion
unit_empty_actions_only_change_the_log = progActionTest defaultEmptyProg [] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[]]}

-- We can move to the default def in a program
-- (this may only exist at the start of a session)
unit_move_to_def_main :: Assertion
unit_move_to_def_main = progActionTest defaultEmptyProg [moveToDef "main"] $
  expectSuccess $ \prog prog' ->
    prog' @?= prog{progLog = Log [[moveToDef "main"]]}

-- Expression actions are tested in ActionTest - here we just check that we can modify the correct
-- def.
unit_move_to_def_and_construct_let :: Assertion
unit_move_to_def_and_construct_let =
  progActionTest defaultEmptyProg [moveToDef "other", BodyAction [ConstructLet (Just "x")]] $
    expectSuccess $ \prog prog' ->
      case astDefExpr <$> lookupASTDef' "other" (moduleDefs $ progModule prog') of
        Just Let{} ->
          -- Check that main is unchanged
          Map.lookup "main" (moduleDefs $ progModule prog') @?= Map.lookup "main" (moduleDefs $ progModule prog)
        _ -> assertFailure "definition not found"

unit_rename_def :: Assertion
unit_rename_def =
  progActionTest defaultEmptyProg [renameDef "other" "foo"] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just (gvn "foo")
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just (gvn "main")

unit_rename_def_to_same_name_as_existing_def :: Assertion
unit_rename_def_to_same_name_as_existing_def =
  progActionTest defaultEmptyProg [renameDef "main" "main"] $
    expectError (@?= DefAlreadyExists (gvn "main"))

unit_rename_def_to_same_name_as_existing_def_prim :: Assertion
unit_rename_def_to_same_name_as_existing_def_prim =
  progActionTest defaultFullProg [renameDef "other" "toUpper"] $
    expectError (@?= DefAlreadyExists (gvn "toUpper"))

unit_rename_def_referenced :: Assertion
unit_rename_def_referenced =
  progActionTest
    defaultEmptyProg
    [ moveToDef "main"
    , BodyAction [ConstructVar $ globalVarRef "other"]
    , renameDef "other" "foo"
    ]
    $ expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just (gvn "foo")
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just (gvn "main")
      fmap (set _exprMeta () . astDefExpr) (defAST =<< Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just (Var () $ globalVarRef "foo")

unit_rename_def_recursive :: Assertion
unit_rename_def_recursive =
  progActionTest
    defaultEmptyProg
    [ moveToDef "main"
    , BodyAction [ConstructVar $ globalVarRef "main"]
    , renameDef "main" "foo"
    ]
    $ expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just (gvn "foo")
      fmap (set _exprMeta () . astDefExpr) (defAST =<< Map.lookup "foo" (moduleDefs $ progModule prog')) @?= Just (Var () $ globalVarRef "foo")

unit_delete_def :: Assertion
unit_delete_def =
  progActionTest defaultEmptyProg [deleteDef "other"] $
    expectSuccess $ \_ prog' -> do
      fmap defName (Map.lookup "other" (moduleDefs $ progModule prog')) @?= Nothing
      fmap defName (Map.lookup "main" (moduleDefs $ progModule prog')) @?= Just (gvn "main")

unit_delete_def_unknown_id :: Assertion
unit_delete_def_unknown_id =
  progActionTest defaultEmptyProg [deleteDef "unknown"] $
    expectError (@?= DefNotFound (gvn "unknown"))

unit_delete_def_used_id :: Assertion
unit_delete_def_used_id =
  progActionTest defaultEmptyProg [moveToDef "main", BodyAction [ConstructVar $ globalVarRef "other"], deleteDef "other"] $
    expectError (@?= DefInUse (gvn "other"))

-- 'foo = foo' shouldn't count as "in use" and block deleting itself
unit_delete_def_recursive :: Assertion
unit_delete_def_recursive =
  progActionTest defaultEmptyProg [moveToDef "main", BodyAction [ConstructVar $ globalVarRef "main"], deleteDef "main"] $
    expectSuccess $ \prog prog' -> Map.delete "main" (moduleDefs $ progModule prog) @?= moduleDefs (progModule prog')

unit_move_to_unknown_def :: Assertion
unit_move_to_unknown_def =
  progActionTest defaultEmptyProg [moveToDef "unknown"] $ expectError (@?= DefNotFound (gvn "unknown"))

unit_rename_unknown_def :: Assertion
unit_rename_unknown_def =
  progActionTest defaultEmptyProg [renameDef "unknown" "foo"] $ expectError (@?= DefNotFound (gvn "unknown"))

unit_construct_let_without_moving_to_def_first :: Assertion
unit_construct_let_without_moving_to_def_first =
  progActionTest defaultEmptyProg [BodyAction [ConstructLet (Just "x")]] $ expectError (@?= NoDefSelected)

unit_create_def :: Assertion
unit_create_def = progActionTest defaultEmptyProg [CreateDef $ Just "newDef"] $
  expectSuccess $ \_ prog' -> do
    case lookupASTDef' "newDef" (moduleDefs $ progModule prog') of
      Nothing -> assertFailure $ show $ moduleDefs $ progModule prog'
      Just def -> do
        astDefName def @?= gvn "newDef"
        astDefExpr def @?= EmptyHole (Meta 4 Nothing Nothing)

unit_create_def_clash_prim :: Assertion
unit_create_def_clash_prim =
  progActionTest defaultFullProg [CreateDef $ Just "toUpper"] $
    expectError (@?= DefAlreadyExists (gvn "toUpper"))

unit_create_typedef :: Assertion
unit_create_typedef =
  let lst =
        ASTTypeDef
          { astTypeDefName = tcn "List"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors =
              [ ValCon (vcn "Nil") []
              , ValCon (vcn "Cons") [TVar () "a", TApp () (TCon () (tcn "List")) (TVar () "a")]
              ]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
      tree =
        ASTTypeDef
          { astTypeDefName = tcn "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon (vcn "Node") [TVar () "a", TApp () (TCon () (tcn "List")) (TApp () (TCon () (tcn "Tree")) (TVar () "a"))]]
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
          { astTypeDefName = tcn "Tree"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon (vcn "Node") [TVar () "a", TApp () (TCon () $ tcn "List") (TApp () (TCon () $ tcn "Tree") (TVar () "a"))]]
          , astTypeDefNameHints = ["xs", "ys", "zs"]
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= (TypeDefError $ show $ UnknownTypeConstructor (tcn "List")))

-- duplicate type(names) added
unit_create_typedef_bad_2 :: Assertion
unit_create_typedef_bad_2 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = tcn "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = tcn "T"
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
          { astTypeDefName = tcn "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors =
              [ ValCon (vcn "C") []
              , ValCon (vcn "C") []
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
          { astTypeDefName = tcn "T1"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (vcn "C") []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = tcn "T2"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (vcn "C") []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectError (@?= TypeDefError "InternalError \"Duplicate-ly-named constructor (perhaps in different typedefs)\"")

-- Forbid duplicate parameter names
unit_create_typedef_bad_5 :: Assertion
unit_create_typedef_bad_5 =
  let td =
        ASTTypeDef
          { astTypeDefName = tcn "T"
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
          { astTypeDefName = tcn "T"
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
          { astTypeDefName = tcn "T"
          , astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = [ValCon (vcn "a") []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectError (@?= TypeDefError "InternalError \"Duplicate names in one tydef: between parameter-names and constructor-names\"")

-- Forbid clash between type name and name of a primitive type
unit_create_typedef_bad_prim :: Assertion
unit_create_typedef_bad_prim =
  let td =
        ASTTypeDef
          { astTypeDefName = tcn "Char"
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
          { astTypeDefName = tcn "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (vcn "T") []]
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td] $
        expectSuccess $ \_ prog' -> Map.elems (moduleTypes (progModule prog')) @?= [TypeDefAST td]

-- Allow clash between type name and constructor name across types
unit_create_typedef_9 :: Assertion
unit_create_typedef_9 =
  let td1 =
        ASTTypeDef
          { astTypeDefName = tcn "T"
          , astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (vcn "C") []]
          , astTypeDefNameHints = []
          }
      td2 =
        ASTTypeDef
          { astTypeDefName = tcn "C"
          , astTypeDefParameters = []
          , astTypeDefConstructors = []
          , astTypeDefNameHints = []
          }
   in progActionTest defaultEmptyProg [AddTypeDef td1, AddTypeDef td2] $
        expectSuccess $ \_ prog' -> Map.elems (moduleTypes (progModule prog')) @?= [TypeDefAST td2, TypeDefAST td1]

unit_construct_arrow_in_sig :: Assertion
unit_construct_arrow_in_sig =
  progActionTest defaultEmptyProg [moveToDef "other", SigAction [ConstructArrowL, Move Child1]] $
    expectSuccess $ \_ prog' ->
      case lookupASTDef' "other" (moduleDefs $ progModule prog') of
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
          moveToDef "main"
        , SigAction [ConstructTCon (mainModuleNameText, "Char")]
        , -- other :: Char; other = main
          moveToDef "other"
        , SigAction [ConstructTCon (mainModuleNameText, "Char")]
        , BodyAction [ConstructVar $ GlobalVarRef $ gvn "main"]
        , -- main :: Int
          -- We expect this to change 'other' to contain a hole
          moveToDef "main"
        , SigAction [Delete, ConstructTCon (mainModuleNameText, "Int")]
        ]
   in progActionTest defaultFullProg acts $
        expectSuccess $ \_ prog' ->
          case lookupASTDef' "other" (moduleDefs $ progModule prog') of
            Just def ->
              -- Check that the definition is a non-empty hole
              case astDefExpr def of
                Hole _ (Var _ (GlobalVarRef n)) | n == gvn "main" -> pure ()
                _ -> assertFailure "expected {? main ?}"
            _ -> assertFailure "definition not found"

unit_copy_paste_duplicate :: Assertion
unit_copy_paste_duplicate = do
  let fromDef = gvn "main"
      toDef = gvn "blank"
      ((p, fromType, fromExpr, _toType, _toExpr), maxID) = create $ do
        mainType <- tforall "a" KType (tvar "a" `tfun` (tcon tMaybe `tapp` tEmptyHole))
        mainExpr <- lAM "b" $ lam "x" $ con cJust `aPP` tvar "b" `app` lvar "x"
        let mainDef = ASTDef fromDef mainExpr mainType
        blankDef <- ASTDef toDef <$> emptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing}
            & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST mainDef), ("blank", DefAST blankDef)]
          , getID mainType
          , getID mainExpr
          , getID (astDefType blankDef)
          , getID (astDefExpr blankDef)
          )
  let a = newApp{appProg = p}
      actions = [MoveToDef toDef, CopyPasteSig (fromDef, fromType) [], CopyPasteBody (fromDef, fromExpr) []]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef fromDef (moduleDefsQualified $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef fromDef (moduleDefsQualified $ progModule r)
            assertBool "equal to toDef" $ src /= lookupASTDef' "blank" (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) toDef src) @?= clearIDs (lookupASTDef' "blank" (moduleDefs $ progModule r))

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
  let mainName = gvn "main"
      ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tvar "a" `tfun` tvar "b" `tfun` tforall "d" KType (tvar "c" `tfun` tvar "d")
        let skel r = tforall "a" KType $ tfun (tforall "b" KType $ tforall "c" KType $ pure toCopy) $ tforall "c" KType r
        defInitial <- ASTDef mainName <$> emptyHole <*> skel tEmptyHole
        expected <- ASTDef mainName <$> emptyHole <*> skel (tvar "a" `tfun` tEmptyHole `tfun` tforall "d" KType (tEmptyHole `tfun` tvar "d"))
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef mainName, CopyPasteSig (mainName, srcID) [Move Child1, Move Child2, Move Child1]]
      (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg pExpected <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcpExpected, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let clearIDs = set (traversed % #_DefAST % _defIDs) 0
       in clearIDs (moduleDefs $ progModule r) @?= clearIDs (moduleDefs $ progModule tcpExpected)

-- ∀a b.a ~> ∀a.a
unit_raise :: Assertion
unit_raise = do
  let mainName' = "main"
      mainName = gvn mainName'
      ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tvar "a"
        defInitial <- ASTDef mainName <$> emptyHole <*> tforall "a" KType (tforall "b" KType $ pure toCopy)
        expected <- ASTDef mainName <$> emptyHole <*> tforall "a" KType (tvar "a")
        pure
          ( newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(mainName', DefAST defInitial)]
          , getID toCopy
          , newEmptyProg & #progModule % #moduleDefs .~ Map.fromList [(mainName', DefAST expected)]
          )
  let a = newEmptyApp{appProg = pInitial}
      actions = [MoveToDef mainName, CopyPasteSig (mainName, srcID) [Move Child1, Delete]]
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
  let mainName' = "main"
      mainName = gvn mainName'
      ((pInitial, srcID, pExpected), maxID) = create $ do
        ty <- tforall "a" KType $ (tcon tList `tapp` tvar "a") `tfun` tforall "b" KType (tvar "b" `tfun` (tcon tPair `tapp` tvar "a" `tapp` tvar "b"))
        let toCopy' = con cMakePair `aPP` tvar "a" `aPP` tvar "b" `app` lvar "y" `app` lvar "z" -- want different IDs for the two occurences in expected
        toCopy <- toCopy'
        let skel r =
              lAM "a" $
                lam "x" $
                  case_
                    (lvar "x")
                    [ branch cNil [] r
                    , branch cCons [("y", Nothing), ("ys", Nothing)] $ lAM "b" $ lam "z" $ pure toCopy
                    ]
        expectPasted <- con cMakePair `aPP` tvar "a" `aPP` tEmptyHole `app` emptyHole `app` emptyHole
        -- TODO: in the future we may want to insert let bindings for variables
        -- which are out of scope in the target, and produce something like
        -- expectPasted <- letType "b" tEmptyHole $ let_ "y" (emptyHole `ann` tvar "a") $ let_ "z" (emptyHole `ann` tvar "b") toCopy'
        defInitial <- ASTDef mainName <$> skel emptyHole <*> pure ty
        expected <- ASTDef mainName <$> skel (pure expectPasted) <*> pure ty
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [(mainName', DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [(mainName', DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [MoveToDef mainName, CopyPasteBody (mainName, srcID) [Move Child1, Move Child1, Move (Branch cNil)]]
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
  let fromDef' = "main"
      fromDef = gvn fromDef'
      toDef' = "blank"
      toDef = gvn toDef'
      ((p, fromAnn), maxID) = create $ do
        toCopy <- tcon tBool
        mainDef <- ASTDef fromDef <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        blankDef <- ASTDef toDef <$> emptyHole `ann` tEmptyHole <*> tEmptyHole
        pure
          ( newProg{progSelection = Nothing} & #progModule % #moduleDefs .~ Map.fromList [(fromDef', DefAST mainDef), ("blank", DefAST blankDef)]
          , getID toCopy
          )
  let a = newApp{appProg = p}
      actions = [MoveToDef toDef, CopyPasteBody (fromDef, fromAnn) [EnterType]]
  let (result, _) = runAppTestM maxID a $ (,) <$> tcWholeProg p <*> handleEditRequest actions
  case result of
    Left e -> assertFailure $ show e
    Right (tcp, r) ->
      -- use the typechecked input p, as the result will have had a typecheck run, so
      -- we need the cached kinds to match up
      let src = lookupASTDef' fromDef' (moduleDefs $ progModule tcp)
          clearIDs = set (_Just % _defIDs) 0
       in do
            src @?= lookupASTDef' fromDef' (moduleDefs $ progModule r)
            assertBool "equal to blank" $ src /= lookupASTDef' toDef' (moduleDefs $ progModule r)
            clearIDs (set (_Just % #astDefName) toDef src) @?= clearIDs (lookupASTDef' toDef' (moduleDefs $ progModule r))

unit_copy_paste_ann2sig :: Assertion
unit_copy_paste_ann2sig = do
  let ((pInitial, srcID, pExpected), maxID) = create $ do
        toCopy <- tcon tBool
        defInitial <- astDef "main" <$> emptyHole `ann` pure toCopy <*> tEmptyHole
        expected <- astDef "main" <$> emptyHole `ann` pure toCopy <*> tcon tBool
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [moveToDef "main", copyPasteSig ("main", srcID) []]
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
        toCopy <- tcon tBool
        defInitial <- astDef "main" <$> emptyHole <*> pure toCopy
        expected <- astDef "main" <$> emptyHole `ann` tcon tBool <*> pure toCopy
        pure
          ( newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST defInitial)]
          , getID toCopy
          , newProg & #progModule % #moduleDefs .~ Map.fromList [("main", DefAST expected)]
          )
  let a = newApp{appProg = pInitial}
      actions = [moveToDef "main", copyPasteBody ("main", srcID) [ConstructAnn, EnterType]]
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
        importModules [builtinModule, primitiveModule]
        gets (Map.assocs . moduleDefsQualified . progModule . appProg) >>= \case
          [(i, DefAST d)] -> do
            a' <- get
            (_, vs) <- runReaderT (handleQuestion (VariablesInScope i $ getID $ astDefExpr d)) a'
            pure $
              assertBool "VariablesInScope did not report the imported Int.+" $
                any ((== primitiveGVar "Int.+") . fst) vs
          _ -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Can reference something in an imported module (both types and terms)
unit_import_reference :: Assertion
unit_import_reference =
  let test = do
        importModules [builtinModule, primitiveModule]
        prog <- gets appProg
        case (findGlobalByName prog $ primitiveGVar "toUpper", Map.assocs $ moduleDefsQualified $ progModule prog) of
          (Just toUpperDef, [(i, _)]) -> do
            _ <-
              handleEditRequest
                [ MoveToDef i
                , SigAction [constructTCon tChar]
                , BodyAction [ConstructVar $ GlobalVarRef $ defName toUpperDef]
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported toUpper"
          (Just _, _) -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

unit_import_twice_1 :: Assertion
unit_import_twice_1 =
  let test = do
        importModules [builtinModule]
        importModules [builtinModule]
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> err @?= ActionError (ImportNameClash [moduleName builtinModule])
        Right _ -> assertFailure "Expected importModules to error, since module names clash with prior import"

unit_import_twice_2 :: Assertion
unit_import_twice_2 =
  let test = do
        importModules [builtinModule, builtinModule]
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> err @?= ActionError (ImportNameClash [moduleName builtinModule])
        Right _ -> assertFailure "Expected importModules to error, since module names clash within one import"

-- Can copy and paste from an imported module
unit_copy_paste_import :: Assertion
unit_copy_paste_import =
  let test = do
        importModules [builtinModule]
        ty <- tcon tBool `tfun` tcon tBool
        e <- lam "x" $ lvar "x"
        let def = ASTDef (TestUtils.gvn ["M"] "foo") e ty
        let m =
              Module
                { moduleName = ModuleName ["M"]
                , moduleTypes = mempty
                , moduleDefs = Map.singleton "foo" $ DefAST def
                }
        importModules [m]
        prog <- gets appProg
        case (findGlobalByName prog $ TestUtils.gvn ["M"] "foo", Map.assocs $ moduleDefsQualified $ progModule prog) of
          (Just (DefAST fooDef), [(i, _)]) -> do
            let fromDef = astDefName fooDef
                fromType = getID $ astDefType fooDef
                fromExpr = getID $ astDefExpr fooDef
            _ <-
              handleEditRequest
                [ MoveToDef i
                , CopyPasteSig (fromDef, fromType) []
                , CopyPasteBody (fromDef, fromExpr) []
                ]
            pure $ pure ()
          (Nothing, _) -> pure $ assertFailure "Could not find the imported 'foo'"
          (Just _, _) -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

unit_RenameType :: Assertion
unit_RenameType =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <- emptyHole `ann` (tcon tT `tapp` tcon (tcn "Bool"))
              astDef "def" x <$> tEmptyHole
          ]
    )
    [RenameType tT "T'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef (tcn "T'") prog'
      astTypeDefName td @?= tcn "T'"
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              emptyHole `ann` (tcon (tcn "T'") `tapp` tcon (tcn "Bool"))
          )

unit_RenameType_clash :: Assertion
unit_RenameType_clash =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameType tT "Int"]
    $ expectError (@?= TypeDefAlreadyExists (tcn "Int"))

unit_RenameCon :: Assertion
unit_RenameCon =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <-
                hole
                  ( hole
                      (con cA)
                  )
              astDef "def" x <$> tEmptyHole
          ]
    )
    [RenameCon tT cA "A'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef tT prog'
      astTypeDefConstructors td
        @?= [ ValCon (vcn "A'") [TCon () (tcn "Bool"), TCon () (tcn "Bool"), TCon () (tcn "Bool")]
            , ValCon cB [TVar () "b"]
            ]
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole
                ( hole
                    (con $ vcn "A'")
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
                      (con cA)
                  )
              astDef "def" x <$> tEmptyHole
          ]
    )
    [RenameCon tT cA "True"]
    $ expectError (@?= ConAlreadyExists (vcn "True"))

unit_RenameTypeParam :: Assertion
unit_RenameTypeParam =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameTypeParam tT "b" "b'"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef tT prog'
      astTypeDefParameters td @?= [("a", KType), ("b'", KType)]
      astTypeDefConstructors td
        @?= [ ValCon cA [TCon () (tcn "Bool"), TCon () (tcn "Bool"), TCon () (tcn "Bool")]
            , ValCon cB [TVar () "b'"]
            ]

unit_RenameTypeParam_clash :: Assertion
unit_RenameTypeParam_clash =
  progActionTest
    (defaultProgEditableTypeDefs $ pure [])
    [RenameTypeParam tT "a" "b"]
    $ expectError (@?= ParamAlreadyExists "b")

unit_AddCon :: Assertion
unit_AddCon =
  progActionTest
    ( defaultProgEditableTypeDefs $
        sequence
          [ do
              x <-
                case_
                  (emptyHole `ann` (tcon tT `tapp` tcon (tcn "Bool") `tapp` tcon (tcn "Int")))
                  [ branch cA [] emptyHole
                  , branch cB [] emptyHole
                  ]
              astDef "def" x <$> tEmptyHole
          ]
    )
    [AddCon tT 1 "C"]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef tT prog'
      astTypeDefConstructors td
        @?= [ ValCon cA [TCon () (tcn "Bool"), TCon () (tcn "Bool"), TCon () (tcn "Bool")]
            , ValCon (vcn "C") []
            , ValCon cB [TVar () "b"]
            ]
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon tT `tapp` tcon (tcn "Bool") `tapp` tcon (tcn "Int")))
                [ branch cA [] emptyHole
                , branch (vcn "C") [] emptyHole
                , branch cB [] emptyHole
                ]
          )

unit_SetConFieldType :: Assertion
unit_SetConFieldType =
  progActionTest
    ( defaultProgEditableTypeDefs . sequence . pure $ do
        x <- con cA `app` lvar "x" `app` (gvar (gvn "y") `ann` tcon (tcn "Bool"))
        astDef "def" x <$> tEmptyHole
    )
    [SetConFieldType tT cA 1 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef tT prog'
      astTypeDefConstructors td
        @?= [ ValCon cA [TCon () (tcn "Bool"), TCon () (tcn "Int"), TCon () (tcn "Bool")]
            , ValCon cB [TVar () "b"]
            ]
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              con cA `app` lvar "x" `app` hole (gvar (gvn "y") `ann` tcon (tcn "Bool"))
          )

unit_SetConFieldType_partial_app :: Assertion
unit_SetConFieldType_partial_app =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con cA `app` lvar "x"
        sequence
          [ astDef "def" x <$> tcon tT
          ]
    )
    [SetConFieldType tT cA 1 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole $
                con cA `app` lvar "x"
          )

unit_SetConFieldType_case :: Assertion
unit_SetConFieldType_case =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <-
          case_
            (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
            [ branch
                cA
                [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                (lvar "y")
            , branch cB [] emptyHole
            ]
        sequence
          [ astDef "def" x <$> tcon (tcn "Bool")
          ]
    )
    [SetConFieldType tT cA 1 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
                [ branch
                    cA
                    [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                    (hole $ lvar "y")
                , branch cB [] emptyHole
                ]
          )

unit_SetConFieldType_shadow :: Assertion
unit_SetConFieldType_shadow =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <-
          case_
            (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
            [ branch
                cA
                [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                (lam "y" (lvar "y") `app` lvar "y")
            , branch cB [] emptyHole
            ]
        sequence
          [ astDef "def" x <$> tcon (tcn "Bool")
          ]
    )
    [SetConFieldType tT cA 1 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
                [ branch
                    cA
                    [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                    -- only the free `y` should be put in to a hole
                    (lam "y" (lvar "y") `app` hole (lvar "y"))
                , branch cB [] emptyHole
                ]
          )

unit_AddConField :: Assertion
unit_AddConField =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con cA `app` con (vcn "True")
        sequence
          [ astDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField tT cA 1 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      td <- findTypeDef tT prog'
      astTypeDefConstructors td
        @?= [ ValCon cA [TCon () (tcn "Bool"), TCon () (tcn "Int"), TCon () (tcn "Bool"), TCon () (tcn "Bool")]
            , ValCon cB [TVar () "b"]
            ]
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              con cA `app` con (vcn "True") `app` emptyHole
          )

unit_AddConField_partial_app :: Assertion
unit_AddConField_partial_app =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <- con cA `app` con (vcn "True")
        sequence
          [ astDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField tT cA 2 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              hole $ con cA `app` con (vcn "True")
          )

unit_AddConField_case :: Assertion
unit_AddConField_case =
  progActionTest
    ( defaultProgEditableTypeDefs $ do
        x <-
          case_
            (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
            [ branch
                cA
                [("x", Nothing), ("y", Nothing), ("z", Nothing)]
                (lvar "y")
            , branch cB [] emptyHole
            ]
        sequence
          [ astDef "def" x <$> tEmptyHole
          ]
    )
    [AddConField tT cA 2 $ TCon () (tcn "Int")]
    $ expectSuccess $ \_ prog' -> do
      def <- findDef (gvn "def") prog'
      forgetIDs (astDefExpr def)
        @?= forgetIDs
          ( fst . create $
              case_
                (emptyHole `ann` (tcon tT `tapp` tEmptyHole `tapp` tEmptyHole))
                [ branch
                    cA
                    [("x", Nothing), ("y", Nothing), ("a19", Nothing), ("z", Nothing)]
                    (lvar "y")
                , branch cB [] emptyHole
                ]
          )

-- Check that we see name hints from imported modules
-- (This differs from the tests in Tests.Question by testing the actual action,
-- rather than the underlying functionality)
unit_generate_names_import :: Assertion
unit_generate_names_import =
  let test = do
        importModules [builtinModule]
        gets (Map.assocs . moduleDefsQualified . progModule . appProg) >>= \case
          [(i, DefAST d)] -> do
            a' <- get
            ns <-
              runReaderT
                (handleQuestion (GenerateName i (getID $ astDefExpr d) $ Left $ Just $ TCon () tBool))
                a'
            pure $ ns @?= ["p", "q"]
          _ -> pure $ assertFailure "Expected one def 'main' from newEmptyApp"
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- * Utilities

findGlobalByName :: Prog -> GVarName -> Maybe Def
findGlobalByName p n = Map.lookup n . foldMap moduleDefsQualified $ progAllModules p

-- We use a program with two defs: "main" and "other"
defaultEmptyProg :: MonadFresh ID m => m Prog
defaultEmptyProg = do
  mainExpr <- emptyHole
  mainType <- tEmptyHole
  otherExpr <- emptyHole
  otherType <- tEmptyHole
  let mainDef = astDef "main" mainExpr mainType
      otherDef = astDef "other" otherExpr otherType
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
          .~ Map.fromList [(astDefBaseName mainDef, DefAST mainDef), (astDefBaseName otherDef, DefAST otherDef)]

unit_good_defaultEmptyProg :: Assertion
unit_good_defaultEmptyProg = checkProgWellFormed defaultEmptyProg

-- `defaultEmptyProg`, plus all primitive definitions (types and terms)
-- and all builtin types, all moved into the editable module
-- NB: this means that primitive constructors are unusable, since they
-- will not typecheck (we now have only a "Main.Char" type, not a
-- "Primitive.Char" type), but we can now test our error handling for
-- adding types whose name clashes with that of a primitive etc.
defaultFullProg :: MonadFresh ID m => m Prog
defaultFullProg = do
  p <- defaultEmptyProg
  let m = moduleName $ progModule p
      -- We need to move the primitives, which requires renaming
      -- unit_defaultFullModule_no_clash ensures that there will be no clashes
      renamed :: [Module]
      renamed = transformBi (const m) [builtinModule, primitiveModule]
      renamedTypes = renamed ^.. folded % #moduleTypes % folded
      renamedDefs = foldOf (folded % #moduleDefs) renamed
  pure $
    p & #progModule % #moduleTypes %~ (mkTypeDefMap renamedTypes <>)
      & #progModule % #moduleDefs %~ (renamedDefs <>)

findTypeDef :: TyConName -> Prog -> IO ASTTypeDef
findTypeDef d p = maybe (assertFailure "couldn't find typedef") pure $ (typeDefAST <=< Map.lookup d) $ p ^. (#progModule % to moduleTypesQualified)

findDef :: GVarName -> Prog -> IO ASTDef
findDef d p = maybe (assertFailure "couldn't find def") pure $ (defAST <=< Map.lookup d) $ p ^. (#progModule % to moduleDefsQualified)

-- We use the same type definition for all tests related to editing type definitions
-- (This is added to `defaultFullProg`)
-- The qualified name for this is recorded in 'tT', and its constructors are 'cA' and 'cB'
defaultProgEditableTypeDefs :: MonadFresh ID f => f [ASTDef] -> f Prog
defaultProgEditableTypeDefs ds = do
  p <- defaultFullProg
  ds' <- ds
  let tds =
        [ TypeDefAST
            ASTTypeDef
              { astTypeDefName = tT
              , astTypeDefParameters = [("a", KType), ("b", KType)]
              , astTypeDefConstructors = [ValCon cA (replicate 3 $ TCon () (tcn "Bool")), ValCon cB [TVar () "b"]]
              , astTypeDefNameHints = []
              }
        ]
  pure $
    p
      & (#progModule % #moduleTypes) %~ (mkTypeDefMap tds <>)
      & (#progModule % #moduleDefs) %~ (Map.fromList ((\d -> (baseName $ astDefName d, DefAST d)) <$> ds') <>)

tT :: TyConName
tT = tcn "T"

cA :: ValConName
cA = vcn "A"

cB :: ValConName
cB = vcn "B"

unit_good_defaultFullProg :: Assertion
unit_good_defaultFullProg = checkProgWellFormed defaultFullProg

-- All primitives,builtins and defaultEmptyProg things have distinct base names (defaultFullProg expects this)
unit_defaultFullProg_no_clash :: Assertion
unit_defaultFullProg_no_clash =
  let (p, _) = create defaultEmptyProg
      ms = progModule p : [builtinModule, primitiveModule]
      typeNames = ms ^.. folded % #moduleTypes % folded % to typeDefName % #baseName
      termNames = ms ^.. folded % #moduleDefs % to Map.keys % folded
   in do
        assertBool "Expected every type making up defaultFullProg to have distinct names" $ not $ anySame typeNames
        assertBool "Expected every term making up defaultFullProg to have distinct names" $ not $ anySame termNames

unit_rename_module :: Assertion
unit_rename_module =
  let test = do
        importModules [builtinModule]
        handleEditRequest [RenameModule ["Module2"]]
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right p -> unModuleName (moduleName $ progModule p) @?= ["Module2"]

unit_rename_module_clash :: Assertion
unit_rename_module_clash =
  let test = do
        importModules [builtinModule]
        handleEditRequest [RenameModule ["Builtins"]]
      a = newEmptyApp
   in do
        unModuleName (moduleName builtinModule) @?= ["Builtins"]
        case fst $ runAppTestM (ID $ appIdCounter a) a test of
          Left err -> err @?= RenameModuleNameClash
          Right _ -> assertFailure "Expected RenameModule to error, since module names clash with prior import"

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

-- Looks up a definition, ignoring any module prefix
-- Useful in these tests so we don't have to specify
-- the name of the module all the time
lookupASTDef' :: Name -> Map Name Def -> Maybe ASTDef
lookupASTDef' name = defAST <=< Map.lookup name

astDefBaseName :: ASTDef -> Name
astDefBaseName = baseName . astDefName

-- Some helpers to run actions on the current module
mainModuleName :: ModuleName
mainModuleName = moduleName $ progModule newEmptyProg

mainModuleNameText :: NonEmpty Text
mainModuleNameText = unName <$> unModuleName mainModuleName

moveToDef :: Name -> ProgAction
moveToDef = MoveToDef . qualifyName mainModuleName

renameDef :: Name -> Text -> ProgAction
renameDef = RenameDef . qualifyName mainModuleName

deleteDef :: Name -> ProgAction
deleteDef = DeleteDef . gvn

tcn :: Name -> TyConName
tcn = TestUtils.tcn $ unModuleName mainModuleName

vcn :: Name -> ValConName
vcn = TestUtils.vcn $ unModuleName mainModuleName

gvn :: Name -> GVarName
gvn = TestUtils.gvn $ unModuleName mainModuleName

astDef :: Name -> Expr -> Type -> ASTDef
astDef = ASTDef . gvn

copyPasteSig :: (Name, ID) -> [Action] -> ProgAction
copyPasteSig (d, i) = CopyPasteSig (gvn d, i)

copyPasteBody :: (Name, ID) -> [Action] -> ProgAction
copyPasteBody (d, i) = CopyPasteBody (gvn d, i)

globalVarRef :: Name -> TmVarRef
globalVarRef = GlobalVarRef . gvn
