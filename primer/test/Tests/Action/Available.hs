{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Action.Available where

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Hedgehog (
  GenT,
  LabelName,
  PropertyT,
  annotate,
  annotateShow,
  collect,
  discard,
  failure,
  footnoteShow,
  label,
  success,
  (===),
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllWithT)
import Hedgehog.Range qualified as Range
import Optics (ix, toListOf, (%), (.~), (^..), _head)
import Primer.API (viewProg)
import Primer.Action (
  ActionError (CaseBindsClash, NameCapture),
  Movement (Child1, Child2),
  enterType,
  move,
  moveExpr,
  moveType,
  toProgActionInput,
  toProgActionNoInput,
 )
import Primer.Action.Available (
  InputAction (MakeCon, MakeLAM, MakeLam, RenameForall, RenameLAM, RenameLam, RenameLet),
  NoInputAction (MakeCase, Raise, RaiseType),
  Option (Option),
 )
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  DefSelection (..),
  EditAppM,
  Editable (..),
  Level (Beginner, Expert, Intermediate),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgError (ActionError, DefAlreadyExists),
  Selection' (..),
  TypeDefConsSelection (TypeDefConsSelection),
  TypeDefNodeSelection (TypeDefConsNodeSelection, TypeDefParamNodeSelection),
  TypeDefSelection (TypeDefSelection),
  appProg,
  checkAppWellFormed,
  checkProgWellFormed,
  handleEditRequest,
  nextProgID,
  progAllDefs,
  progAllTypeDefs,
  progAllTypeDefsMeta,
  progCxt,
  progImports,
  progModules,
  progSmartHoles,
  runEditAppM,
 )
import Primer.App.Base (TypeDefConsFieldSelection (..))
import Primer.Builtins (builtinModuleName, cCons, cFalse, cTrue, tBool, tList, tNat)
import Primer.Core (
  Expr,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  Kind (KFun, KType),
  ModuleName (ModuleName, unModuleName),
  Type,
  TypeMeta,
  getID,
  mkSimpleModuleName,
  moduleNamePretty,
  qualifyName,
  _typeMeta,
 )
import Primer.Core.DSL (
  S,
  ann,
  app,
  branch,
  case_,
  con,
  create',
  emptyHole,
  gvar,
  hole,
  lAM,
  lam,
  let_,
  letrec,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
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
  moduleDefsQualified,
  moduleName,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (clearMeta, clearTypeMeta, testNoSevereLogs)
import Primer.TypeDef (ASTTypeDef (astTypeDefConstructors), TypeDef (TypeDefAST, TypeDefPrim), ValCon (..), astTypeDefParameters, typeDefAST)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles, SmartHoles),
  buildTypingContextFromModules,
  checkEverything,
  typeDefs,
 )
import Primer.Zipper (focus)
import System.FilePath ((</>))
import Tasty (Property, withDiscards, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import Tests.Action.Prog (defaultEmptyProg, findGlobalByName, mkEmptyTestApp, runAppTestM)
import Tests.Typecheck (
  TypeCacheAlpha (TypeCacheAlpha),
  runTypecheckTestM,
  runTypecheckTestMIn,
 )
import Text.Pretty.Simple (pShowNoColor)

-- | Comprehensive DSL test.
test_1 :: TestTree
test_1 = mkTests [create' builtinModule] $ create' $ comprehensiveWellTyped $ mkSimpleModuleName "M"

data Output = Output
  { defActions :: [OfferedAction]
  , bodyActions :: [(ID, [OfferedAction])]
  , sigActions :: [(ID, [OfferedAction])]
  }
  deriving stock (Show)
data OfferedAction
  = NoInput Available.NoInputAction
  | Input Available.InputAction Available.Options
  deriving stock (Show)

-- | Golden tests for the available actions at each node of the definition, for each level.
mkTests :: [Module] -> (GVarName, Def) -> TestTree
mkTests _ (_, DefPrim _) = error "mkTests is unimplemented for primitive definitions."
mkTests deps (defName, DefAST def') =
  let d = defName
      m = Module (qualifiedModule d) mempty $ Map.singleton (baseName d) $ DefAST def'
      cxt = buildTypingContextFromModules deps NoSmartHoles
      def = case runTypecheckTestMIn
        cxt
        (checkEverything NoSmartHoles CheckEverything{trusted = deps, toCheck = [m]}) of
        Left err -> error $ "mkTests: no typecheck: " <> show err
        Right [m'] -> case Map.toList $ moduleDefs m' of
          [(_, DefAST def'')] -> def''
          _ -> error "mkTests: expected exactly one definition in checked module"
        _ -> error "mkTests: expected exactly one module checked modules"
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
      enumeratePairs = (,) <$> enumerate <*> enumerate
      defs = Map.singleton defName $ DefAST def
      typeDefs = foldMap' @[] moduleTypesQualified [create' builtinModule, primitiveModule]
      offered level id = \case
        Available.NoInput a -> NoInput a
        Available.Input a ->
          Input a
            . fromMaybe (error "id not found")
            $ Available.options typeDefs defs cxt level (Right def) id a
   in testGroup testName $
        enumeratePairs
          <&> \(level, mut) ->
            let defActions = map (offered level $ SelectionDef $ DefSelection defName Nothing) $ Available.forDef defs level mut d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map (offered level $ SelectionDef $ DefSelection defName $ Just $ NodeSelection BodyNode id) $
                            Available.forBody
                              typeDefs
                              level
                              mut
                              (astDefExpr def)
                              id
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map (offered level (SelectionDef $ DefSelection defName $ Just $ NodeSelection SigNode id)) $ Available.forSig level mut (astDefType def) id
                        )
                    )
                    . toListOf (_typeMeta % _id)
                    $ astDefType def
             in goldenVsString (show level <> ":" <> show mut) ("test/outputs/available-actions" </> testName </> show level <> "-" <> show mut <> ".fragment") $
                  pure . BS.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor $
                    Output
                      { defActions
                      , bodyActions
                      , sigActions
                      }

-- We should not offer to delete a definition that is in use, as that
-- action cannot possibly succeed
unit_def_in_use :: Assertion
unit_def_in_use =
  let (d, defs) = create' $ do
        let foo = qualifyName (ModuleName ["M"]) "foo"
        fooDef <- ASTDef <$> emptyHole <*> tEmptyHole
        let bar = qualifyName (ModuleName ["M"]) "bar"
        barDef <- ASTDef <$> gvar foo <*> tEmptyHole
        let ds = [(foo, DefAST fooDef), (bar, DefAST barDef)]
        pure (foo, Map.fromList ds)
   in for_
        enumerate
        ( \l ->
            Available.forDef defs l Editable d
              @?= [Available.Input Available.RenameDef, Available.NoInput Available.DuplicateDef]
        )

-- | A helper type for 'tasty_available_actions_actions',
-- describing where a particular option came from.
data Provenance
  = -- | This option was offered by the 'Available.options' API
    Offered
  | -- | This option is free-form entry. For example, this simulates
    -- renaming a definition to a hand-entered name.
    StudentProvided
  deriving stock (Show)

tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, pure primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      a <- forAllT $ genApp SmartHoles cxt
      let allTypes = progAllTypeDefsMeta $ appProg a
      let allDefs = progAllDefs $ appProg a
      let isMutable = \case
            Editable -> True
            NonEditable -> False
      let genDef :: Map name (Editable, def) -> GenT WT (Maybe (LabelName, (Editable, (name, def))))
          genDef m =
            second (\(n, (e, t)) -> (e, (n, t)))
              <<$>> case partition (isMutable . fst . snd) $ Map.toList m of
                ([], []) -> pure Nothing
                (mut, []) -> Just . ("all mut",) <$> Gen.element mut
                ([], immut) -> Just . ("all immut",) <$> Gen.element immut
                (mut, immut) -> Just . ("mixed mut/immut",) <$> Gen.frequency [(9, Gen.element mut), (1, Gen.element immut)]
      (defMut, typeOrTermDef) <-
        maybe discard (\(t, x) -> label t >> pure x)
          =<< forAllT
            ( Gen.choice
                [ second (second Left) <<$>> genDef allTypes
                , second (second Right) <<$>> genDef allDefs
                ]
            )
      collect defMut
      case typeOrTermDef of
        Left (_, t) ->
          label "type" >> case t of
            TypeDefPrim{} -> label "Prim"
            TypeDefAST{} -> label "AST"
        Right (_, t) ->
          label "term" >> case t of
            DefPrim{} -> label "Prim"
            DefAST{} -> label "AST"
      (loc, acts) <- case typeOrTermDef of
        Left (defName, def) ->
          (fmap snd . forAllWithT fst) case typeDefAST def of
            Nothing -> Gen.discard
            Just def' ->
              let typeDefSel = SelectionTypeDef . TypeDefSelection defName
                  forTypeDef = ("forTypeDef", (typeDefSel Nothing, Available.forTypeDef l defMut))
               in Gen.frequency
                    [ (1, pure forTypeDef)
                    ,
                      ( 2
                      , case astTypeDefParameters def' of
                          [] -> pure forTypeDef
                          ps -> do
                            (p, _) <- Gen.element ps
                            pure
                              ( "forTypeDefParamNode"
                              ,
                                ( typeDefSel $ Just $ TypeDefParamNodeSelection p
                                , Available.forTypeDefParamNode l defMut
                                )
                              )
                      )
                    ,
                      ( 5
                      , case astTypeDefConstructors def' of
                          [] -> pure forTypeDef
                          cs -> do
                            ValCon{valConName, valConArgs} <- Gen.element cs
                            let typeDefConsNodeSel = typeDefSel . Just . TypeDefConsNodeSelection . TypeDefConsSelection valConName
                                forTypeDefConsNode = ("forTypeDefConsNode", (typeDefConsNodeSel Nothing, Available.forTypeDefConsNode l defMut))
                            case valConArgs of
                              [] -> pure forTypeDefConsNode
                              as ->
                                Gen.frequency
                                  [ (1, pure forTypeDefConsNode)
                                  ,
                                    ( 5
                                    , do
                                        (n, t) <- Gen.element $ zip [0 ..] as
                                        i <- Gen.element $ t ^.. typeIDs
                                        pure
                                          ( "forTypeDefConsFieldNode"
                                          ,
                                            ( typeDefConsNodeSel . Just $ TypeDefConsFieldSelection n i
                                            , Available.forTypeDefConsFieldNode l defMut def' valConName n i
                                            )
                                          )
                                    )
                                  ]
                      )
                    ]
        Right (defName, def) ->
          fmap (first (SelectionDef . DefSelection defName) . snd) . forAllWithT fst . Gen.frequency $
            catMaybes
              [ Just (1, pure ("forDef", (Nothing, Available.forDef (snd <$> allDefs) l defMut defName)))
              , defAST def <&> \d' -> (2,) $ do
                  let ty = astDefType d'
                      ids = ty ^.. typeIDs
                  i <- Gen.element ids
                  let hedgehogMsg = "forSig id " <> show i
                  pure (hedgehogMsg, (Just $ NodeSelection SigNode i, Available.forSig l defMut ty i))
              , defAST def <&> \d' -> (7,) $ do
                  let expr = astDefExpr d'
                      ids = expr ^.. exprIDs
                  i <- Gen.element ids
                  let hedgehogMsg = "forBody id " <> show i
                  pure (hedgehogMsg, (Just $ NodeSelection BodyNode i, Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut expr i))
              ]
      annotateShow loc
      case acts of
        [] -> label "no offered actions" >> success
        acts' -> do
          def <-
            bitraverse
              (maybe (annotate "primitive type def" >> failure) pure . typeDefAST . snd)
              (maybe (annotate "primitive def" >> failure) pure . defAST . snd)
              typeOrTermDef
          --let bias = Nothing
          -- these seem to be the only two action which still cause errors
          --let bias = Just $ Available.Input Available.AddCon
          let bias = Just $ Available.NoInput Available.AddConField
          action <- maybe (forAllT $ Gen.element acts') pure $ (bias <*) . guard . (`elem` acts') =<< bias
          collect action
          footnoteShow action
          case action of
            act@(Available.Input Available.AddCon) | Just act /= bias -> discard
            act@(Available.NoInput Available.AddConField) | Just act /= bias -> discard
            Available.NoInput act' -> do
              progActs <-
                either (\e -> annotateShow e >> failure) pure $
                  toProgActionNoInput (map snd $ progAllDefs $ appProg a) def loc act'
              annotateShow progActs
              actionSucceeds (handleEditRequest progActs) a
            Available.Input act' -> do
              Available.Options{Available.opts, Available.free} <-
                maybe (annotate "id not found" >> failure) pure $
                  Available.options
                    (map snd $ progAllTypeDefs $ appProg a)
                    (map snd $ progAllDefs $ appProg a)
                    (progCxt $ appProg a)
                    l
                    def
                    loc
                    act'
              let opts' = [Gen.element $ (Offered,) <$> opts | not (null opts)]
              let opts'' =
                    opts' <> case free of
                      Available.FreeNone -> []
                      Available.FreeVarName -> [(StudentProvided,) . flip Available.Option Nothing <$> (unName <$> genName)]
                      Available.FreeInt -> [(StudentProvided,) . flip Available.Option Nothing <$> (show <$> Gen.integral (Range.linear @Integer 0 1_000_000_000))]
                      Available.FreeChar -> [(StudentProvided,) . flip Available.Option Nothing . T.singleton <$> Gen.unicode]
              case opts'' of
                [] -> annotate "no options" >> success
                options -> do
                  opt <- forAllT $ Gen.choice options
                  footnoteShow opt
                  progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def loc (snd opt) act'
                  annotateShow progActs
                  actionSucceedsOrCapture (fst opt) (handleEditRequest progActs) a
  where
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) ProgError a ->
      App ->
      PropertyT WT (Either ProgError a, App)
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT ()
    actionSucceeds m a =
      runEditAppMLogs m a >>= \case
        -- copy output to primer-app to view prog - use as `initialProg` arg to `AppProg` in `App.tsx`
        (Left err, _) -> annotateShow err >> annotate (TL.unpack $ encodeToLazyText $ viewProg $ appProg a) >> failure
        (Right _, a') -> ensureSHNormal a'
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT ()
    actionSucceedsOrCapture p m a = do
      a' <- runEditAppMLogs m a
      case (p, a') of
        (StudentProvided, (Left (ActionError NameCapture), _)) -> do
          label "name-capture with entered name"
          annotate "ignoring name capture error as was generated name, not offered one"
        (StudentProvided, (Left (ActionError (CaseBindsClash{})), _)) -> do
          label "name-clash with entered name"
          annotate "ignoring name clash error as was generated name, not offered one"
        (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
          label "rename def name clash with entered name"
          annotate "ignoring def already exists error as was generated name, not offered one"
        (_, (Left err, _)) -> annotateShow err >> failure
        (_, (Right _, a'')) -> ensureSHNormal a''
    ensureSHNormal a = case checkAppWellFormed a of
      Left err -> annotateShow err >> failure
      Right a' -> TypeCacheAlpha a === TypeCacheAlpha a'

-- 'Raise' works when moving checkable terms into synthesisable position
unit_raise_sh :: Assertion
unit_raise_sh =
  let test :: HasCallStack => S Expr -> S Expr -> Assertion
      test t1 t2 =
        offeredActionTest
          SmartHoles
          Beginner
          (emptyHole `app` t1 `app` emptyHole)
          (InExpr [Child1, Child2])
          (Left Raise)
          (t2 `app` emptyHole)
      testSyn :: HasCallStack => S Expr -> Assertion
      testSyn e = test e e
      testChk :: HasCallStack => S Expr -> Assertion
      testChk t = test t (t `ann` tEmptyHole)
   in do
        testSyn emptyHole
        testChk $ lam "x" (lvar "x")

-- 'Raise' does not unnecessarily recreate case branches. It used to do so due
-- to implementation reasons, losing work (by deleting what was on their RHSs).
unit_raise_case_branch_ann :: Assertion
unit_raise_case_branch_ann =
  offeredActionTest
    SmartHoles
    Beginner
    (lam "x" (case_ (lvar "x") [branch cTrue [] $ lam "y" emptyHole, branch cFalse [] emptyHole]) `ann` (tcon tBool `tfun` (tcon tBool `tfun` tcon tNat)))
    ([] `InType` [Child2])
    (Left Raise)
    (lam "x" (case_ (lvar "x") [branch cTrue [] $ hole $ lam "y" emptyHole, branch cFalse [] emptyHole]) `ann` (tcon tBool `tfun` tcon tNat))

unit_raise_case_branch_def :: Assertion
unit_raise_case_branch_def =
  offeredActionTestSig
    SmartHoles
    Beginner
    (lam "x" $ case_ (lvar "x") [branch cTrue [] $ lam "y" emptyHole, branch cFalse [] emptyHole])
    (tcon tBool `tfun` (tcon tBool `tfun` tcon tNat))
    [Child2]
    (Left RaiseType)
    ( lam "x" $ case_ (lvar "x") [branch cTrue [] $ hole $ lam "y" emptyHole, branch cFalse [] emptyHole]
    , tcon tBool `tfun` tcon tNat
    )

unit_raise_type_term :: Assertion
unit_raise_type_term =
  offeredActionTest
    SmartHoles
    Beginner
    (hole (emptyHole `ann` (tcon tBool `tfun` tcon tNat)) `ann` tcon tNat)
    ([Child1, Child1] `InType` [Child1])
    (Left Raise)
    (hole (emptyHole `ann` tcon tBool) `ann` tcon tNat)

unit_sat_con_1 :: Assertion
unit_sat_con_1 =
  offeredActionTest
    SmartHoles
    Intermediate
    (emptyHole `ann` (tEmptyHole `tfun` tEmptyHole))
    (InExpr [Child1])
    (Right (MakeCon, Option "Cons" $ Just $ unName <$> unModuleName builtinModuleName))
    (hole (con cCons [emptyHole, emptyHole]) `ann` (tEmptyHole `tfun` tEmptyHole))

unit_sat_con_2 :: Assertion
unit_sat_con_2 =
  offeredActionTest
    SmartHoles
    Intermediate
    (emptyHole `ann` ((tcon tList `tapp` tcon tNat) `tfun` (tcon tList `tapp` tcon tNat)))
    (InExpr [Child1])
    (Right (MakeCon, Option "Cons" $ Just $ unName <$> unModuleName builtinModuleName))
    (hole (con cCons [emptyHole, emptyHole]) `ann` ((tcon tList `tapp` tcon tNat) `tfun` (tcon tList `tapp` tcon tNat)))

-- The various @let@ constructs inherit the directionality of their body.
-- This is a regression test, as in the past this was the case for @let@ but not @letrec@.
-- Note that @MakeCase@ is only offered for synthesisable terms.
unit_case_let :: Assertion
unit_case_let = do
  let testOffered :: HasCallStack => S Expr -> Assertion
      testOffered e =
        offeredActionTest
          SmartHoles
          Intermediate
          e
          (InExpr [])
          (Left MakeCase)
          (case_ e [])
  testOffered (let_ "x" emptyHole emptyHole)
  -- testOffered (letType "a" tEmptyHole emptyHole) -- TODO: add this test when the typechecker supports letType
  testOffered (letrec "x" emptyHole tEmptyHole emptyHole)
  let testNotOffered :: HasCallStack => S Expr -> Assertion
      testNotOffered e =
        offeredActionTestNotOffered
          SmartHoles
          Intermediate
          e
          (InExpr [])
          MakeCase
  testNotOffered (let_ "x" emptyHole $ lam "y" $ lvar "y")
  -- testNotOffered (letType "a" tEmptyHole $ lam "y" $ lvar "y") -- TODO: add this test when the typechecker supports letType
  testNotOffered (letrec "x" emptyHole tEmptyHole $ lam "y" $ lvar "y")

data MovementListBody
  = InExpr [Movement]
  | InType [Movement] [Movement]

data MovementList
  = InBody MovementListBody
  | InSig [Movement]

exprMoves :: MovementListBody -> [Movement]
exprMoves = \case
  InExpr ms -> ms
  InType ms _ -> ms

typeMoves :: MovementListBody -> Maybe [Movement]
typeMoves = \case
  InExpr _ -> Nothing
  InType _ ms -> Just ms

-- | Apply the action to the node in the input expression pointed to by the
-- 'Movement' (starting from the root), checking that it would actually be offered
-- there, and then checking the result matches the expected output, up to renaming
-- of IDs and changing cached types.
offeredActionTest ::
  HasCallStack =>
  SmartHoles ->
  Level ->
  S Expr ->
  MovementListBody ->
  Either NoInputAction (InputAction, Option) ->
  S Expr ->
  Assertion
offeredActionTest sh l inputExpr position action expectedOutput = do
  offeredActionTest' sh l (ASTDef <$> inputExpr <*> tEmptyHole) (InBody position) action >>= \case
    Left err -> assertFailure $ show err
    Right result -> clearMeta (astDefExpr result) @?= clearMeta (create' expectedOutput)

offeredActionTestSig ::
  HasCallStack =>
  SmartHoles ->
  Level ->
  S Expr ->
  S Type ->
  [Movement] ->
  Either NoInputAction (InputAction, Option) ->
  (S Expr, S Type) ->
  Assertion
offeredActionTestSig sh l inputExpr inputType position action expectedOutput = do
  offeredActionTest' sh l (ASTDef <$> inputExpr <*> inputType) (InSig position) action >>= \case
    Left err -> assertFailure $ show err
    Right (ASTDef resExpr resTy) -> do
      clearTypeMeta resTy @?= clearTypeMeta (create' $ snd expectedOutput)
      clearMeta resExpr @?= clearMeta (create' $ fst expectedOutput)

offeredActionTestNotOffered ::
  HasCallStack =>
  SmartHoles ->
  Level ->
  S Expr ->
  MovementListBody ->
  NoInputAction ->
  Assertion
offeredActionTestNotOffered sh l inputExpr position action = do
  offeredActionTest' sh l (ASTDef <$> inputExpr <*> tEmptyHole) (InBody position) (Left action) >>= \case
    Left ActionNotOffered{} -> pure ()
    Left err -> assertFailure $ show err
    Right _ -> assertFailure "action was unexpectedly offered"

-- Helper for offeredActionTest'
data OAT
  = ActionNotOffered Available.Action [Available.Action]
  | OptionNotOffered Option [Option]
  | ErrorRunningAction ProgError
  deriving stock (Show)

-- Looks at actions offered for the given position and distinguishes the following cases
-- - the requested action is not offered
-- - the requested action is offered, but the requested option is not offered
-- - the action (and possible option) were offered, but running them resulted in an error
-- - the action (and possible option) were offered, and running succeeds
offeredActionTest' ::
  SmartHoles ->
  Level ->
  S ASTDef ->
  MovementList ->
  Either NoInputAction (InputAction, Option) ->
  IO (Either OAT ASTDef)
offeredActionTest' sh l inputDef position action = do
  let progRaw = create' $ do
        ms <- sequence [builtinModule]
        prog0 <- defaultEmptyProg
        d <- inputDef
        pure $
          prog0
            & (#progModules % _head % #moduleDefs % ix "main" .~ DefAST d)
            & (#progImports .~ ms)
            -- Temporarily disable smart holes, so what is written in unit tests is what is in the prog
            & (#progSmartHoles .~ NoSmartHoles)
  -- Typecheck everything to fill in typecaches.
  -- This lets us test offered names for renaming variable binders.
  let progChecked = runTypecheckTestM NoSmartHoles $ checkProgWellFormed progRaw
  let (modules, expr, sig, exprDef, exprDefName, prog) = case progChecked of
        Left err -> error $ "offeredActionTest: no typecheck: " <> show err
        Right p -> case progModules p of
          [m] -> case moduleDefs m M.!? "main" of
            Just (DefAST def@(ASTDef e t)) -> (progImports p, e, t, def, qualifyName (moduleName m) "main", p & #progSmartHoles .~ sh)
            _ -> error "offeredActionTest: didn't find 'main'"
          _ -> error "offeredActionTest: expected exactly one progModule"
  let id' = evalTestM (nextProgID prog) $
        runExceptT $
          flip runReaderT (buildTypingContextFromModules modules sh) $
            case position of
              InBody pos' -> do
                ez <- foldlM (flip moveExpr) (focus expr) $ exprMoves pos'
                case typeMoves pos' of
                  Nothing -> pure $ getID ez
                  Just ms -> do
                    tz' <- enterType ez
                    tz <- foldlM (flip moveType) tz' ms
                    pure $ getID tz
              InSig moves -> getID <$> foldlM (flip move) (focus sig) moves
  id <- case id' of
    Left err -> assertFailure $ show err
    Right i' -> pure i'
  let cxt = buildTypingContextFromModules modules sh
  let defs = foldMap' moduleDefsQualified modules
  let offered = case position of
        InBody _ -> Available.forBody cxt.typeDefs l Editable expr id
        InSig _ -> Available.forSig l Editable sig id
  let options = Available.options cxt.typeDefs defs cxt l (Right exprDef) (SelectionDef $ DefSelection exprDefName $ Just $ NodeSelection BodyNode id)
  action' <- case action of
    Left a ->
      pure $
        if Available.NoInput a `elem` offered
          then Right $ toProgActionNoInput @TypeMeta (foldMap' moduleDefsQualified $ progModules prog) (Right exprDef) (SelectionDef $ DefSelection exprDefName $ Just $ NodeSelection BodyNode id) a
          else Left $ ActionNotOffered (Available.NoInput a) offered
    Right (a, o) -> do
      if Available.Input a `elem` offered
        then case options a of
          Nothing -> assertFailure "Available.options returned Nothing"
          Just os ->
            pure $
              if o `elem` os.opts
                then Right $ toProgActionInput (Right exprDef) (SelectionDef $ DefSelection exprDefName $ Just $ NodeSelection BodyNode id) o a
                else Left $ OptionNotOffered o os.opts
        else pure $ Left $ ActionNotOffered (Available.Input a) offered
  action'' <- for action' $ \case
    Left err -> assertFailure $ show err
    Right a -> pure a
  x <- for action'' $ \action''' -> do
    let result = defAST <=< flip findGlobalByName exprDefName
    let assertJust = maybe (assertFailure "Lost 'main' after action") pure
    (res, _) <- runAppTestM (nextProgID prog) (mkEmptyTestApp prog) (handleEditRequest action''')
    rr <- traverse (assertJust . result) res
    pure $ first ErrorRunningAction rr
  pure $ join x

-- Correct names offered when running actions
-- NB: Bools are offered names "p", "q"; functions get "f","g"; nats get "i","j","n","m"
offeredNamesTest :: HasCallStack => S Expr -> MovementListBody -> InputAction -> Text -> S Expr -> Assertion
offeredNamesTest initial moves act name =
  offeredActionTest
    NoSmartHoles
    Expert
    initial
    moves
    (Right (act, Option name Nothing))

-- Note that lambdas are the only form which we have interesting name info when
-- we initially create them.
unit_make_lam_names :: Assertion
unit_make_lam_names =
  offeredNamesTest
    (emptyHole `ann` (tcon tNat `tfun` tcon tBool))
    (InExpr [Child1])
    MakeLam
    "i"
    (lam "i" emptyHole `ann` (tcon tNat `tfun` tcon tBool))

unit_rename_lam_names :: Assertion
unit_rename_lam_names =
  offeredNamesTest
    (lam "x" emptyHole `ann` (tcon tNat `tfun` tcon tBool))
    (InExpr [Child1])
    RenameLam
    "i"
    (lam "i" emptyHole `ann` (tcon tNat `tfun` tcon tBool))

unit_make_LAM_names :: Assertion
unit_make_LAM_names = do
  offeredNamesTest
    (emptyHole `ann` tforall "a" KType (tcon tBool))
    (InExpr [Child1])
    MakeLAM
    "α"
    (lAM "α" emptyHole `ann` tforall "a" KType (tcon tBool))
  offeredNamesTest
    (emptyHole `ann` tforall "a" (KFun KType KType) (tcon tBool))
    (InExpr [Child1])
    MakeLAM
    "f"
    (lAM "f" emptyHole `ann` tforall "a" (KFun KType KType) (tcon tBool))

unit_rename_LAM_names :: Assertion
unit_rename_LAM_names = do
  offeredNamesTest
    (lAM "x" emptyHole `ann` tforall "a" KType (tcon tBool))
    (InExpr [Child1])
    RenameLAM
    "α"
    (lAM "α" emptyHole `ann` tforall "a" KType (tcon tBool))
  offeredNamesTest
    (lAM "x" emptyHole `ann` tforall "a" (KFun KType KType) (tcon tBool))
    (InExpr [Child1])
    RenameLAM
    "f"
    (lAM "f" emptyHole `ann` tforall "a" (KFun KType KType) (tcon tBool))

-- nb: renaming let cares about the type of the bound var, not of the let
unit_rename_let_names :: Assertion
unit_rename_let_names =
  offeredNamesTest
    (let_ "x" (emptyHole `ann` tcon tBool) emptyHole)
    (InExpr [])
    RenameLet
    "p"
    (let_ "p" (emptyHole `ann` tcon tBool) emptyHole)

{-
-- TODO: reinstate once the TC handles let type!
-- See https://github.com/hackworthltd/primer/issues/5
--unit_rename_lettype_names :: Assertion
--unit_rename_lettype_names = do
  offeredNamesTest
    (letType "x" (tcon tBool) emptyHole)
    (InExpr [])
    RenameLet
    "p"
    (letType "p" (tcon tBool) emptyHole)
  offeredNamesTest
    (letType "x" (tcon tBool `tfun` tcon tBool) $ emptyHole)
    (InExpr [])
    RenameLet
    "p"
    (letType "x" (tcon tBool `tfun` tcon tBool) $ emptyHole)
-}

unit_rename_letrec_names :: Assertion
unit_rename_letrec_names =
  offeredNamesTest
    (letrec "x" emptyHole (tcon tBool) emptyHole)
    (InExpr [])
    RenameLet
    "p"
    (letrec "p" emptyHole (tcon tBool) emptyHole)

-- There is no unit_rename_pattern_names as can only move to a pattern by ID, which is awkward here

unit_rename_forall_names :: Assertion
unit_rename_forall_names = do
  offeredNamesTest
    (emptyHole `ann` tforall "a" KType (tcon tBool))
    ([] `InType` [])
    RenameForall
    "α"
    (emptyHole `ann` tforall "α" KType (tcon tBool))
  offeredNamesTest
    (emptyHole `ann` tforall "a" (KFun KType KType) (tcon tBool))
    ([] `InType` [])
    RenameForall
    "f"
    (emptyHole `ann` tforall "f" (KFun KType KType) (tcon tBool))

{-
-- TODO: reinstate once the TC handles let type!
-- See https://github.com/hackworthltd/primer/issues/5
--unit_rename_tlet_names :: Assertion
--unit_rename_tlet_names = do
  offeredNamesTest
    (emptyHole `ann` tlet "a" (tcon tNat) tEmptyHole)
    ([] `InType` [])
    RenameLet
    "α"
    (emptyHole `ann` tlet "α" (tcon tNat) tEmptyHole)
  offeredNamesTest
    (emptyHole `ann` tlet "a" (tcon tList) tEmptyHole)
    ([] `InType` [])
    RenameLet
    "f"
    (emptyHole `ann` tlet "f" (tcon tList) tEmptyHole)
-}
