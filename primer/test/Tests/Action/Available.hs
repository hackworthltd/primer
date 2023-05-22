{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Action.Available where

import Foreword hiding (mod)

import Control.Monad.Log (WithSeverity)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as M
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
  (===), Gen, forAll,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics (ix, toListOf, (%), (.~), _head)
import Primer.Action (
  ActionError (CaseBindsClash, NameCapture),
  Movement (Child1, Child2),
  enterType,
  moveExpr,
  moveType,
  toProgActionInput,
  toProgActionNoInput, ProgAction (AddTypeDef,CreateDef),
 )
import Primer.Action.Available (
  InputAction (MakeCon, MakeLAM, MakeLam, RenameForall, RenameLAM, RenameLam, RenameLet),
  NoInputAction (MakeCase, Raise),
  Option (Option),
 )
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  Editable (..),
  Level (Beginner, Expert, Intermediate),
  NodeType (..),
  Prog (..),
  ProgError (ActionError, DefAlreadyExists),
  MutationRequest(Edit),
  appProg,
  checkAppWellFormed,
  checkProgWellFormed,
  handleEditRequest,
  nextProgID,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  progImports,
  progModules,
  progSmartHoles,
  runEditAppM, handleMutationRequest, MutationRequest (Undo, Redo), Log (unlog), mkApp, appIdCounter,
 )
import Primer.Builtins (builtinModuleName, cCons, tBool, tList, tNat)
import Primer.Core (
  Expr,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  Kind (KFun, KType),
  ModuleName (ModuleName, unModuleName),
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
  tfun, tvar,
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
import Primer.Gen.Core.Typed (WT, forAllT, propertyWT, freshNameForCxt)
import Primer.Gen.App (extendCxtByModules)
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (..),
  builtinModule,
  moduleDefsQualified,
  moduleName,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (clearMeta, testNoSevereLogs)
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
import Primer.TypeDef (ASTTypeDef(..), TypeDef (..))

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
            $ Available.options typeDefs defs cxt level def id a
   in testGroup testName $
        enumeratePairs
          <&> \(level, mut) ->
            let defActions = map (offered level Nothing) $ Available.forDef defs level mut d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map (offered level (Just (BodyNode, id))) $
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
                        , map (offered level (Just (SigNode, id))) $ Available.forSig level mut (astDefType def) id
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
      (defName,defMut,defLoc) <- maybe discard forAll (pickPos $ appProg a)
      -- TODO: use runRandomAction
      let defMap = fmap snd $ progAllDefs $ appProg a
      let (def, loc,acts) = case defLoc of
            Left d -> (d, Nothing,Available.forDef defMap l defMut defName)
            Right (d,SigNode, i) -> (DefAST d, Just (SigNode, i), Available.forSig l defMut (astDefType d) i)
            Right (d,BodyNode, i) -> (DefAST d, Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut (astDefExpr d) i)
      case acts of
        [] -> label "no offered actions" >> success
        acts' -> do
          action <- forAllT $ Gen.element acts'
          collect action
          case action of
            Available.NoInput act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              progActs <-
                either (\e -> annotateShow e >> failure) pure $
                  toProgActionNoInput (map snd $ progAllDefs $ appProg a) def' defName loc act'
              actionSucceeds (handleEditRequest progActs) a
            Available.Input act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              Available.Options{Available.opts, Available.free} <-
                maybe (annotate "id not found" >> failure) pure $
                  Available.options
                    (map snd $ progAllTypeDefs $ appProg a)
                    (map snd $ progAllDefs $ appProg a)
                    (progCxt $ appProg a)
                    l
                    def'
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
                  progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' defName loc (snd opt) act'
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
        (Left err, _) -> annotateShow err >> failure
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
      Right a' -> TypeCacheAlpha a === TypeCacheAlpha a' >> checkUndo a
    checkUndo a = runEditAppMLogs (handleMutationRequest Undo) a >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> runEditAppMLogs (handleMutationRequest Redo) a' >>= \case
          (Left err, _) -> annotateShow err >> failure
          (Right _, a'') -> TypeCacheAlpha a === TypeCacheAlpha a''

--gives def name and perhaps a node inside it (if Nothing, then has selected the definition itself)
-- If the outer Maybe is Nothing, then there were no definitions at all!
pickPos :: Prog -> Maybe (Gen (GVarName, Editable, Either Def (ASTDef, NodeType, ID)))
pickPos p = ((\(defName, (editable, def)) -> (defName, editable,) <$> pickLoc def) =<<) <$> pickDef
  where
    isMutable = \case
            Editable -> True
            NonEditable -> False
    pickDef = case partition (isMutable . fst . snd) $ Map.toList $ progAllDefs p of
        ([], []) -> Nothing
        (mut, []) -> Just $ Gen.element mut
        ([], immut) -> Just $ Gen.element immut
        (mut, immut) -> Just $ Gen.frequency [(9, Gen.element mut), (1, Gen.element immut)]
    pickLoc d =
          Gen.frequency $
            catMaybes
              [ Just (1, pure $ Left d)
              , defAST d <&> \d' -> (2,) . Gen.element $ fmap (Right . (d',SigNode,)) $ toListOf typeIDs $ astDefType d'
              , defAST d <&> \d' -> (7,) . Gen.element $ fmap (Right . (d',BodyNode,)) $ toListOf exprIDs $ astDefExpr d'
              ]

-- TODO: if I work in PropertyT, should I revive the labels I dropped?
-- 'Nothing' means that a somewhat-expected problem occured:
-- - picked a node with no actions available
-- - picked an action with no options available
-- - entered a free-choice option and had name-clashing issues
runRandomAvailableAction :: Level -> App -> PropertyT WT (Maybe App)
runRandomAvailableAction l a = do
      (defName,defMut,defLoc) <- maybe discard forAll (pickPos $ appProg a)
      let defMap = fmap snd $ progAllDefs $ appProg a
      let (def, loc,acts) = case defLoc of
            Left d -> (d, Nothing,Available.forDef defMap l defMut defName)
            Right (d,SigNode, i) -> (DefAST d, Just (SigNode, i), Available.forSig l defMut (astDefType d) i)
            Right (d,BodyNode, i) -> (DefAST d, Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut (astDefExpr d) i)
      case acts of
        [] -> label "no offered actions" >> pure Nothing
        acts' -> do
          action <- forAllT $ Gen.element acts'
          collect action
          case action of
            Available.NoInput act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              progActs <-
                either (\e -> annotateShow e >> failure) pure $
                  toProgActionNoInput (map snd $ progAllDefs $ appProg a) def' defName loc act'
              Just <$> actionSucceeds (handleEditRequest progActs) a
            Available.Input act' -> do
              def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              Available.Options{Available.opts, Available.free} <-
                maybe (annotate "id not found" >> failure) pure $
                  Available.options
                    (map snd $ progAllTypeDefs $ appProg a)
                    (map snd $ progAllDefs $ appProg a)
                    (progCxt $ appProg a)
                    l
                    def'
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
                [] -> annotate "no options" >> pure Nothing
                options -> do
                  opt <- forAllT $ Gen.choice options
                  progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' defName loc (snd opt) act'
                  actionSucceedsOrCapture (fst opt) (handleEditRequest progActs) a
  where
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) ProgError a ->
      App ->
      PropertyT WT (Either ProgError a, App)
    runEditAppMLogs m a' = case runPureLog $ runEditAppM m a' of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT App
    actionSucceeds m a' =
      runEditAppMLogs m a' >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a'') -> pure a''
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT (Maybe App)
    actionSucceedsOrCapture p m a' = do
      a'' <- runEditAppMLogs m a'
      case (p, a'') of
        (StudentProvided, (Left (ActionError NameCapture), _)) -> do
          label "name-capture with entered name"
          annotate "ignoring name capture error as was generated name, not offered one"
          pure Nothing
        (StudentProvided, (Left (ActionError (CaseBindsClash{})), _)) -> do
          label "name-clash with entered name"
          annotate "ignoring name clash error as was generated name, not offered one"
          pure Nothing
        (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
          label "rename def name clash with entered name"
          annotate "ignoring def already exists error as was generated name, not offered one"
          pure Nothing
        (_, (Left err, _)) -> annotateShow err >> failure
        (_, (Right _, a''')) -> pure $ Just a'''

-- helper type for tasty_undo_redo
data Act = AddTm | AddTy
  | Un | Re | Avail
  deriving stock Show

tasty_undo_redo :: Property
tasty_undo_redo = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, pure primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      let annotateShow' :: HasCallStack => App -> PropertyT WT ()
          annotateShow' = withFrozenCallStack $ annotateShow . (\p -> (progModules p, progLog p, redoLog p)) . appProg
      a <- forAllT $ genApp SmartHoles cxt
      annotateShow' a
      n <- forAll $ Gen.int $ Range.linear 1 20
      a' <- iterateNM n a $ \a' -> runRandomAction l a'
      annotateShow' a'
      if null $ unlog $ progLog $ appProg a' -- TODO: expose a "log-is-null" helper from App?
        -- It is possible for the random actions to undo everything!
        then success
        else do
          a'' <- runEditAppMLogs (handleMutationRequest Undo) a'
          annotateShow' a''
          a''' <- runEditAppMLogs (handleMutationRequest Redo) a''
          annotateShow' a'''
          TypeCacheAlpha a' === TypeCacheAlpha a'''
  where
    -- TODO: dry
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) ProgError a ->
      App ->
      PropertyT WT App
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> case r of
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> pure a'
    runRandomAction l a = do
      act <- forAll $ Gen.frequency $ second pure <$> [
        (2,AddTm)
        ,(1,AddTy)
        ,(if null $ unlog $ progLog $ appProg a then 0 else 1,Un) -- TODO: expose a "log-is-null" helper from App?
        ,(if null $ unlog $ redoLog $ appProg a then 0 else 1,Re) -- TODO: expose a "log-is-null" helper from App?
        ,(5,Avail)
                                    ]
      case act of
        AddTm -> do
          let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
          n <- forAllT $ Gen.choice [Just . unName <$> n', pure Nothing]
          m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
          runEditAppMLogs (handleMutationRequest $ Edit [CreateDef m n]) a
        AddTy -> do
          m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
          let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
          n <- qualifyName m <$> forAllT n'
          runEditAppMLogs (handleMutationRequest $ Edit [AddTypeDef n $ ASTTypeDef [] [] []]) a
        Un -> runEditAppMLogs (handleMutationRequest Undo) a
        Re -> runEditAppMLogs (handleMutationRequest Redo) a
        Avail -> fromMaybe a <$> runRandomAvailableAction l a

--------------------------------------
-- debugging/regression
unit_tmp_1 :: Assertion
unit_tmp_1 = do
  let modName = ModuleName ["M", "0"]
  let t = "a"
  let d = "a1"
  let (pos1, pos2, def) = create' $ do
        let ty = do
              v <- tvar "x"
              fa <- tforall "x" KType (pure v)
              pure (getID v, getID fa, fa)
        (_,p2,a) <- ty
        astDefExpr <- emptyHole `ann` (pure a)
        (p1,_,astDefType) <- ty
        pure (p1,p2,ASTDef
               { astDefExpr
               , astDefType
               })
  let mod = Module
              { moduleName = modName
              , moduleTypes = M.singleton t $ TypeDefAST
                                                   ASTTypeDef
                                                     { astTypeDefParameters = []
                                                     , astTypeDefConstructors = []
                                                     , astTypeDefNameHints = []
                                                     }
              , moduleDefs = M.singleton d $ DefAST def
              }
  let a = mkApp 8 (toEnum 208) Prog
            { progImports = []
            , progModules = [mod]
            , progSelection = Nothing
            , progSmartHoles = SmartHoles
            , progLog = mempty
            , redoLog = mempty
            }
            -- a1 sig 7 is  the tvar x  MakeFun
            -- a1 body 4 is  the forall DeleteType
  let toAct p act = case toProgActionNoInput (map snd $ progAllDefs $ appProg a) def (qualifyName modName d) (Just p) act of
          Left err -> assertFailure $ show err
          Right act' -> pure act'
  act1 <- toAct (SigNode , pos1) Available.MakeFun
  act2 <- toAct (BodyNode , pos2) Available.DeleteType
  case checkAppWellFormed a of
    Left err -> assertFailure $ show err
    Right aChecked -> do
      res <- runAppTestM (appIdCounter aChecked) aChecked $ do
        void $ handleEditRequest act1
        void $ handleEditRequest act2
      case res of
        (Left err, _) -> assertFailure $ show err
        (Right _, _) -> pure ()
-- this test came from the following hedgehog failure
{-
test/Test.hs
  Tests
    Action
      Available
        undo redo: FAIL (0.22s)
            ✗ undo redo failed at test/Tests/Action/Available.hs:430:46
              after 1 test and 7 shrinks.

                  ┏━━ test/Tests/Action/Available.hs ━━━
              374 ┃ runRandomAvailableAction :: Level -> App -> PropertyT WT (Maybe App)
              375 ┃ runRandomAvailableAction l a = do
              376 ┃       (defName,defMut,defLoc) <- maybe discard forAll (pickPos $ appProg a)
                  ┃       │ ( GlobalName
                  ┃       │     { qualifiedModule = ModuleName { unModuleName = "M" :| [ "0" ] }
                  ┃       │     , baseName = "a1"
                  ┃       │     }
                  ┃       │ , Editable
                  ┃       │ , Right
                  ┃       │     ( ASTDef
                  ┃       │         { astDefExpr =
                  ┃       │             Ann
                  ┃       │               (Meta
                  ┃       │                  2
                  ┃       │                  (Just
                  ┃       │                     (TCEmb
                  ┃       │                        TCBoth
                  ┃       │                          { tcChkedAt =
                  ┃       │                              TForall
                  ┃       │                                ()
                  ┃       │                                LocalName { unLocalName = "x" }
                  ┃       │                                KType
                  ┃       │                                (TVar () LocalName { unLocalName = "x" })
                  ┃       │                          , tcSynthed =
                  ┃       │                              TForall
                  ┃       │                                ()
                  ┃       │                                LocalName { unLocalName = "x" }
                  ┃       │                                KType
                  ┃       │                                (TVar () LocalName { unLocalName = "x" })
                  ┃       │                          }))
                  ┃       │                  Nothing)
                  ┃       │               (EmptyHole
                  ┃       │                  (Meta
                  ┃       │                     3
                  ┃       │                     (Just
                  ┃       │                        (TCEmb
                  ┃       │                           TCBoth
                  ┃       │                             { tcChkedAt =
                  ┃       │                                 TForall
                  ┃       │                                   ()
                  ┃       │                                   LocalName { unLocalName = "x" }
                  ┃       │                                   KType
                  ┃       │                                   (TVar () LocalName { unLocalName = "x" })
                  ┃       │                             , tcSynthed = TEmptyHole ()
                  ┃       │                             }))
                  ┃       │                     Nothing))
                  ┃       │               (TForall
                  ┃       │                  (Meta 4 (Just KType) Nothing)
                  ┃       │                  LocalName { unLocalName = "x" }
                  ┃       │                  KType
                  ┃       │                  (TVar
                  ┃       │                     (Meta 5 (Just KType) Nothing) LocalName { unLocalName = "x" }))
                  ┃       │         , astDefType =
                  ┃       │             TForall
                  ┃       │               (Meta 6 (Just KType) Nothing)
                  ┃       │               LocalName { unLocalName = "x" }
                  ┃       │               KType
                  ┃       │               (TVar
                  ┃       │                  (Meta 7 (Just KType) Nothing) LocalName { unLocalName = "x" })
                  ┃       │         }
                  ┃       │     , SigNode
                  ┃       │     , 7
                  ┃       │     )
                  ┃       │ )
                  ┃       │ ( GlobalName
                  ┃       │     { qualifiedModule = ModuleName { unModuleName = "M" :| [ "0" ] }
                  ┃       │     , baseName = "a1"
                  ┃       │     }
                  ┃       │ , Editable
                  ┃       │ , Right
                  ┃       │     ( ASTDef
                  ┃       │         { astDefExpr =
                  ┃       │             Hole
                  ┃       │               (Meta
                  ┃       │                  12
                  ┃       │                  (Just
                  ┃       │                     (TCEmb
                  ┃       │                        TCBoth
                  ┃       │                          { tcChkedAt =
                  ┃       │                              TForall
                  ┃       │                                ()
                  ┃       │                                LocalName { unLocalName = "x" }
                  ┃       │                                KType
                  ┃       │                                (TFun () (TVar () LocalName { unLocalName = "x" }) (TEmptyHole ()))
                  ┃       │                          , tcSynthed = TEmptyHole ()
                  ┃       │                          }))
                  ┃       │                  Nothing)
                  ┃       │               (Ann
                  ┃       │                  (Meta
                  ┃       │                     2
                  ┃       │                     (Just
                  ┃       │                        (TCSynthed
                  ┃       │                           (TForall
                  ┃       │                              ()
                  ┃       │                              LocalName { unLocalName = "x" }
                  ┃       │                              KType
                  ┃       │                              (TVar () LocalName { unLocalName = "x" }))))
                  ┃       │                     Nothing)
                  ┃       │                  (EmptyHole
                  ┃       │                     (Meta
                  ┃       │                        3
                  ┃       │                        (Just
                  ┃       │                           (TCEmb
                  ┃       │                              TCBoth
                  ┃       │                                { tcChkedAt =
                  ┃       │                                    TForall
                  ┃       │                                      ()
                  ┃       │                                      LocalName { unLocalName = "x" }
                  ┃       │                                      KType
                  ┃       │                                      (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                , tcSynthed = TEmptyHole ()
                  ┃       │                                }))
                  ┃       │                        Nothing))
                  ┃       │                  (TForall
                  ┃       │                     (Meta 4 (Just KType) Nothing)
                  ┃       │                     LocalName { unLocalName = "x" }
                  ┃       │                     KType
                  ┃       │                     (TVar
                  ┃       │                        (Meta 5 (Just KType) Nothing) LocalName { unLocalName = "x" })))
                  ┃       │         , astDefType =
                  ┃       │             TForall
                  ┃       │               (Meta 6 (Just KType) Nothing)
                  ┃       │               LocalName { unLocalName = "x" }
                  ┃       │               KType
                  ┃       │               (TFun
                  ┃       │                  (Meta 10 (Just KType) Nothing)
                  ┃       │                  (TVar
                  ┃       │                     (Meta 7 (Just KType) Nothing) LocalName { unLocalName = "x" })
                  ┃       │                  (TEmptyHole (Meta 11 (Just KHole) Nothing)))
                  ┃       │         }
                  ┃       │     , BodyNode
                  ┃       │     , 4
                  ┃       │     )
                  ┃       │ )
              377 ┃       let defMap = fmap snd $ progAllDefs $ appProg a
              378 ┃       let (def, loc,acts) = case defLoc of
              379 ┃             Left d -> (d, Nothing,Available.forDef defMap l defMut defName)
              380 ┃             Right (d,SigNode, i) -> (DefAST d, Just (SigNode, i), Available.forSig l defMut (astDefType d) i)
              381 ┃             Right (d,BodyNode, i) -> (DefAST d, Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut (astDefExpr d) i)
              382 ┃       case acts of
              383 ┃         [] -> label "no offered actions" >> pure Nothing
              384 ┃         acts' -> do
              385 ┃           action <- forAllT $ Gen.element acts'
                  ┃           │ NoInput MakeFun
                  ┃           │ NoInput DeleteType
              386 ┃           collect action
              387 ┃           case action of
              388 ┃             Available.NoInput act' -> do
              389 ┃               def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              390 ┃               progActs <-
              391 ┃                 either (\e -> annotateShow e >> failure) pure $
              392 ┃                   toProgActionNoInput (map snd $ progAllDefs $ appProg a) def' defName loc act'
              393 ┃               Just <$> actionSucceeds (handleEditRequest progActs) a
              394 ┃             Available.Input act' -> do
              395 ┃               def' <- maybe (annotate "primitive def" >> failure) pure $ defAST def
              396 ┃               Available.Options{Available.opts, Available.free} <-
              397 ┃                 maybe (annotate "id not found" >> failure) pure $
              398 ┃                   Available.options
              399 ┃                     (map snd $ progAllTypeDefs $ appProg a)
              400 ┃                     (map snd $ progAllDefs $ appProg a)
              401 ┃                     (progCxt $ appProg a)
              402 ┃                     l
              403 ┃                     def'
              404 ┃                     loc
              405 ┃                     act'
              406 ┃               let opts' = [Gen.element $ (Offered,) <$> opts | not (null opts)]
              407 ┃               let opts'' =
              408 ┃                     opts' <> case free of
              409 ┃                       Available.FreeNone -> []
              410 ┃                       Available.FreeVarName -> [(StudentProvided,) . flip Available.Option Nothing <$> (unName <$> genName)]
              411 ┃                       Available.FreeInt -> [(StudentProvided,) . flip Available.Option Nothing <$> (show <$> Gen.integral (Range.linear @Integer 0 1_000_000_000))]
              412 ┃                       Available.FreeChar -> [(StudentProvided,) . flip Available.Option Nothing . T.singleton <$> Gen.unicode]
              413 ┃               case opts'' of
              414 ┃                 [] -> annotate "no options" >> pure Nothing
              415 ┃                 options -> do
              416 ┃                   opt <- forAllT $ Gen.choice options
              417 ┃                   progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' defName loc (snd opt) act'
              418 ┃                   actionSucceedsOrCapture (fst opt) (handleEditRequest progActs) a
              419 ┃   where
              420 ┃     runEditAppMLogs ::
              421 ┃       HasCallStack =>
              422 ┃       EditAppM (PureLog (WithSeverity ())) ProgError a ->
              423 ┃       App ->
              424 ┃       PropertyT WT (Either ProgError a, App)
              425 ┃     runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
              426 ┃       (r, logs) -> testNoSevereLogs logs >> pure r
              427 ┃     actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT App
              428 ┃     actionSucceeds m a =
              429 ┃       runEditAppMLogs m a >>= \case
              430 ┃         (Left err, _) -> annotateShow err >> failure
                  ┃         │ ActionError
                  ┃         │   (CustomFailure Delete "internal error: lost ID after typechecking")
                  ┃         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
              431 ┃         (Right _, a') -> pure a'
              432 ┃     -- If we submit our own name rather than an offered one, then
              433 ┃     -- we should expect that name capture/clashing may happen
              434 ┃     actionSucceedsOrCapture :: HasCallStack => Provenance -> EditAppM (PureLog (WithSeverity ())) ProgError a -> App -> PropertyT WT (Maybe App)
              435 ┃     actionSucceedsOrCapture p m a = do
              436 ┃       a' <- runEditAppMLogs m a
              437 ┃       case (p, a') of
              438 ┃         (StudentProvided, (Left (ActionError NameCapture), _)) -> do
              439 ┃           label "name-capture with entered name"
              440 ┃           annotate "ignoring name capture error as was generated name, not offered one"
              441 ┃           pure Nothing
              442 ┃         (StudentProvided, (Left (ActionError (CaseBindsClash{})), _)) -> do
              443 ┃           label "name-clash with entered name"
              444 ┃           annotate "ignoring name clash error as was generated name, not offered one"
              445 ┃           pure Nothing
              446 ┃         (StudentProvided, (Left DefAlreadyExists{}, _)) -> do
              447 ┃           label "rename def name clash with entered name"
              448 ┃           annotate "ignoring def already exists error as was generated name, not offered one"
              449 ┃           pure Nothing
              450 ┃         (_, (Left err, _)) -> annotateShow err >> failure
              451 ┃         (_, (Right _, a'')) -> pure $ Just a''

                  ┏━━ test/Tests/Action/Available.hs ━━━
              458 ┃ tasty_undo_redo :: Property
              459 ┃ tasty_undo_redo = withTests 500 $
              460 ┃   withDiscards 2000 $
              461 ┃     propertyWT [] $ do
              462 ┃       l <- forAllT $ Gen.element enumerate
                  ┃       │ Beginner
              463 ┃       cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, pure primitiveModule]]
                  ┃       │ []
              464 ┃       -- We only test SmartHoles mode (which is the only supported user-facing
              465 ┃       -- mode - NoSmartHoles is only used for internal sanity testing etc)
              466 ┃       let annotateShow' :: HasCallStack => App -> PropertyT WT ()
              467 ┃           annotateShow' = withFrozenCallStack $ annotateShow . (\p -> (progModules p, progLog p, redoLog p)) . appProg
              468 ┃       a <- forAllT $ genApp SmartHoles cxt
                  ┃       │ App
                  ┃       │   { currentState =
                  ┃       │       AppState
                  ┃       │         { idCounter = 8
                  ┃       │         , nameCounter = NC 208
                  ┃       │         , prog =
                  ┃       │             Prog
                  ┃       │               { progImports = []
                  ┃       │               , progModules =
                  ┃       │                   [ Module
                  ┃       │                       { moduleName = ModuleName { unModuleName = "M" :| [ "0" ] }
                  ┃       │                       , moduleTypes =
                  ┃       │                           fromList
                  ┃       │                             [ ( "a"
                  ┃       │                               , TypeDefAST
                  ┃       │                                   ASTTypeDef
                  ┃       │                                     { astTypeDefParameters = []
                  ┃       │                                     , astTypeDefConstructors = []
                  ┃       │                                     , astTypeDefNameHints = []
                  ┃       │                                     }
                  ┃       │                               )
                  ┃       │                             ]
                  ┃       │                       , moduleDefs =
                  ┃       │                           fromList
                  ┃       │                             [ ( "a1"
                  ┃       │                               , DefAST
                  ┃       │                                   ASTDef
                  ┃       │                                     { astDefExpr =
                  ┃       │                                         Ann
                  ┃       │                                           (Meta
                  ┃       │                                              2
                  ┃       │                                              (Just
                  ┃       │                                                 (TCEmb
                  ┃       │                                                    TCBoth
                  ┃       │                                                      { tcChkedAt =
                  ┃       │                                                          TForall
                  ┃       │                                                            ()
                  ┃       │                                                            LocalName { unLocalName = "x" }
                  ┃       │                                                            KType
                  ┃       │                                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                                      , tcSynthed =
                  ┃       │                                                          TForall
                  ┃       │                                                            ()
                  ┃       │                                                            LocalName { unLocalName = "x" }
                  ┃       │                                                            KType
                  ┃       │                                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                                      }))
                  ┃       │                                              Nothing)
                  ┃       │                                           (EmptyHole
                  ┃       │                                              (Meta
                  ┃       │                                                 3
                  ┃       │                                                 (Just
                  ┃       │                                                    (TCEmb
                  ┃       │                                                       TCBoth
                  ┃       │                                                         { tcChkedAt =
                  ┃       │                                                             TForall
                  ┃       │                                                               ()
                  ┃       │                                                               LocalName { unLocalName = "x" }
                  ┃       │                                                               KType
                  ┃       │                                                               (TVar
                  ┃       │                                                                  () LocalName { unLocalName = "x" })
                  ┃       │                                                         , tcSynthed = TEmptyHole ()
                  ┃       │                                                         }))
                  ┃       │                                                 Nothing))
                  ┃       │                                           (TForall
                  ┃       │                                              (Meta 4 (Just KType) Nothing)
                  ┃       │                                              LocalName { unLocalName = "x" }
                  ┃       │                                              KType
                  ┃       │                                              (TVar
                  ┃       │                                                 (Meta 5 (Just KType) Nothing)
                  ┃       │                                                 LocalName { unLocalName = "x" }))
                  ┃       │                                     , astDefType =
                  ┃       │                                         TForall
                  ┃       │                                           (Meta 6 (Just KType) Nothing)
                  ┃       │                                           LocalName { unLocalName = "x" }
                  ┃       │                                           KType
                  ┃       │                                           (TVar
                  ┃       │                                              (Meta 7 (Just KType) Nothing)
                  ┃       │                                              LocalName { unLocalName = "x" })
                  ┃       │                                     }
                  ┃       │                               )
                  ┃       │                             ]
                  ┃       │                       }
                  ┃       │                   ]
                  ┃       │               , progSelection = Nothing
                  ┃       │               , progSmartHoles = SmartHoles
                  ┃       │               , progLog = Log { unlog = [] }
                  ┃       │               , redoLog = Log { unlog = [] }
                  ┃       │               }
                  ┃       │         }
                  ┃       │   , initialState =
                  ┃       │       AppState
                  ┃       │         { idCounter = 8
                  ┃       │         , nameCounter = NC 208
                  ┃       │         , prog =
                  ┃       │             Prog
                  ┃       │               { progImports = []
                  ┃       │               , progModules =
                  ┃       │                   [ Module
                  ┃       │                       { moduleName = ModuleName { unModuleName = "M" :| [ "0" ] }
                  ┃       │                       , moduleTypes =
                  ┃       │                           fromList
                  ┃       │                             [ ( "a"
                  ┃       │                               , TypeDefAST
                  ┃       │                                   ASTTypeDef
                  ┃       │                                     { astTypeDefParameters = []
                  ┃       │                                     , astTypeDefConstructors = []
                  ┃       │                                     , astTypeDefNameHints = []
                  ┃       │                                     }
                  ┃       │                               )
                  ┃       │                             ]
                  ┃       │                       , moduleDefs =
                  ┃       │                           fromList
                  ┃       │                             [ ( "a1"
                  ┃       │                               , DefAST
                  ┃       │                                   ASTDef
                  ┃       │                                     { astDefExpr =
                  ┃       │                                         Ann
                  ┃       │                                           (Meta
                  ┃       │                                              2
                  ┃       │                                              (Just
                  ┃       │                                                 (TCEmb
                  ┃       │                                                    TCBoth
                  ┃       │                                                      { tcChkedAt =
                  ┃       │                                                          TForall
                  ┃       │                                                            ()
                  ┃       │                                                            LocalName { unLocalName = "x" }
                  ┃       │                                                            KType
                  ┃       │                                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                                      , tcSynthed =
                  ┃       │                                                          TForall
                  ┃       │                                                            ()
                  ┃       │                                                            LocalName { unLocalName = "x" }
                  ┃       │                                                            KType
                  ┃       │                                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                                      }))
                  ┃       │                                              Nothing)
                  ┃       │                                           (EmptyHole
                  ┃       │                                              (Meta
                  ┃       │                                                 3
                  ┃       │                                                 (Just
                  ┃       │                                                    (TCEmb
                  ┃       │                                                       TCBoth
                  ┃       │                                                         { tcChkedAt =
                  ┃       │                                                             TForall
                  ┃       │                                                               ()
                  ┃       │                                                               LocalName { unLocalName = "x" }
                  ┃       │                                                               KType
                  ┃       │                                                               (TVar
                  ┃       │                                                                  () LocalName { unLocalName = "x" })
                  ┃       │                                                         , tcSynthed = TEmptyHole ()
                  ┃       │                                                         }))
                  ┃       │                                                 Nothing))
                  ┃       │                                           (TForall
                  ┃       │                                              (Meta 4 (Just KType) Nothing)
                  ┃       │                                              LocalName { unLocalName = "x" }
                  ┃       │                                              KType
                  ┃       │                                              (TVar
                  ┃       │                                                 (Meta 5 (Just KType) Nothing)
                  ┃       │                                                 LocalName { unLocalName = "x" }))
                  ┃       │                                     , astDefType =
                  ┃       │                                         TForall
                  ┃       │                                           (Meta 6 (Just KType) Nothing)
                  ┃       │                                           LocalName { unLocalName = "x" }
                  ┃       │                                           KType
                  ┃       │                                           (TVar
                  ┃       │                                              (Meta 7 (Just KType) Nothing)
                  ┃       │                                              LocalName { unLocalName = "x" })
                  ┃       │                                     }
                  ┃       │                               )
                  ┃       │                             ]
                  ┃       │                       }
                  ┃       │                   ]
                  ┃       │               , progSelection = Nothing
                  ┃       │               , progSmartHoles = SmartHoles
                  ┃       │               , progLog = Log { unlog = [] }
                  ┃       │               , redoLog = Log { unlog = [] }
                  ┃       │               }
                  ┃       │         }
                  ┃       │   }
              469 ┃       annotateShow' a
                  ┃       │ ( [ Module
                  ┃       │       { moduleName = ModuleName { unModuleName = "M" :| [ "0" ] }
                  ┃       │       , moduleTypes =
                  ┃       │           fromList
                  ┃       │             [ ( "a"
                  ┃       │               , TypeDefAST
                  ┃       │                   ASTTypeDef
                  ┃       │                     { astTypeDefParameters = []
                  ┃       │                     , astTypeDefConstructors = []
                  ┃       │                     , astTypeDefNameHints = []
                  ┃       │                     }
                  ┃       │               )
                  ┃       │             ]
                  ┃       │       , moduleDefs =
                  ┃       │           fromList
                  ┃       │             [ ( "a1"
                  ┃       │               , DefAST
                  ┃       │                   ASTDef
                  ┃       │                     { astDefExpr =
                  ┃       │                         Ann
                  ┃       │                           (Meta
                  ┃       │                              2
                  ┃       │                              (Just
                  ┃       │                                 (TCEmb
                  ┃       │                                    TCBoth
                  ┃       │                                      { tcChkedAt =
                  ┃       │                                          TForall
                  ┃       │                                            ()
                  ┃       │                                            LocalName { unLocalName = "x" }
                  ┃       │                                            KType
                  ┃       │                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                      , tcSynthed =
                  ┃       │                                          TForall
                  ┃       │                                            ()
                  ┃       │                                            LocalName { unLocalName = "x" }
                  ┃       │                                            KType
                  ┃       │                                            (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                      }))
                  ┃       │                              Nothing)
                  ┃       │                           (EmptyHole
                  ┃       │                              (Meta
                  ┃       │                                 3
                  ┃       │                                 (Just
                  ┃       │                                    (TCEmb
                  ┃       │                                       TCBoth
                  ┃       │                                         { tcChkedAt =
                  ┃       │                                             TForall
                  ┃       │                                               ()
                  ┃       │                                               LocalName { unLocalName = "x" }
                  ┃       │                                               KType
                  ┃       │                                               (TVar () LocalName { unLocalName = "x" })
                  ┃       │                                         , tcSynthed = TEmptyHole ()
                  ┃       │                                         }))
                  ┃       │                                 Nothing))
                  ┃       │                           (TForall
                  ┃       │                              (Meta 4 (Just KType) Nothing)
                  ┃       │                              LocalName { unLocalName = "x" }
                  ┃       │                              KType
                  ┃       │                              (TVar
                  ┃       │                                 (Meta 5 (Just KType) Nothing) LocalName { unLocalName = "x" }))
                  ┃       │                     , astDefType =
                  ┃       │                         TForall
                  ┃       │                           (Meta 6 (Just KType) Nothing)
                  ┃       │                           LocalName { unLocalName = "x" }
                  ┃       │                           KType
                  ┃       │                           (TVar
                  ┃       │                              (Meta 7 (Just KType) Nothing) LocalName { unLocalName = "x" })
                  ┃       │                     }
                  ┃       │               )
                  ┃       │             ]
                  ┃       │       }
                  ┃       │   ]
                  ┃       │ , Log { unlog = [] }
                  ┃       │ , Log { unlog = [] }
                  ┃       │ )
              470 ┃       n <- forAll $ Gen.int $ Range.linear 1 20
                  ┃       │ 2
              471 ┃       a' <- iterateNM n a $ \a' -> runRandomAction l a'
              472 ┃       annotateShow' a'
              473 ┃       if null $ unlog $ progLog $ appProg a' -- TODO: expose a "log-is-null" helper from App?
              474 ┃         -- It is possible for the random actions to undo everything!
              475 ┃         then success
              476 ┃         else do
              477 ┃           a'' <- runEditAppMLogs (handleMutationRequest Undo) a'
              478 ┃           annotateShow' a''
              479 ┃           a''' <- runEditAppMLogs (handleMutationRequest Redo) a''
              480 ┃           annotateShow' a'''
              481 ┃           TypeCacheAlpha a' === TypeCacheAlpha a'''
              482 ┃   where
              483 ┃     -- TODO: dry
              484 ┃     runEditAppMLogs ::
              485 ┃       HasCallStack =>
              486 ┃       EditAppM (PureLog (WithSeverity ())) ProgError a ->
              487 ┃       App ->
              488 ┃       PropertyT WT App
              489 ┃     runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
              490 ┃       (r, logs) -> testNoSevereLogs logs >> case r of
              491 ┃         (Left err, _) -> annotateShow err >> failure
              492 ┃         (Right _, a') -> pure a'
              493 ┃     runRandomAction l a = do
              494 ┃       act <- forAll $ Gen.frequency $ second pure <$> [
                  ┃       │ Avail
                  ┃       │ Avail
              495 ┃         (2,AddTm)
              496 ┃         ,(1,AddTy)
              497 ┃         ,(if null $ unlog $ progLog $ appProg a then 0 else 1,Un) -- TODO: expose a "log-is-null" helper from App?
              498 ┃         ,(if null $ unlog $ redoLog $ appProg a then 0 else 1,Re) -- TODO: expose a "log-is-null" helper from App?
              499 ┃         ,(5,Avail)
              500 ┃                                     ]
              501 ┃       case act of
              502 ┃         AddTm -> do
              503 ┃           let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
              504 ┃           n <- forAllT $ Gen.choice [Just . unName <$> n', pure Nothing]
              505 ┃           m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
              506 ┃           runEditAppMLogs (handleMutationRequest $ Edit [CreateDef m n]) a
              507 ┃         AddTy -> do
              508 ┃           m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg a
              509 ┃           let n' = local (extendCxtByModules $ progModules $ appProg a) freshNameForCxt
              510 ┃           n <- qualifyName m <$> forAllT n'
              511 ┃           runEditAppMLogs (handleMutationRequest $ Edit [AddTypeDef n $ ASTTypeDef [] [] []]) a
              512 ┃         Un -> runEditAppMLogs (handleMutationRequest Undo) a
              513 ┃         Re -> runEditAppMLogs (handleMutationRequest Redo) a
              514 ┃         Avail -> fromMaybe a <$> runRandomAvailableAction l a

              This failure can be reproduced by running:
              > recheck (Size 33) (Seed 4268413180681694343 17092859468972210393) undo redo

          Use '--pattern "$NF ~ /undo redo/" --hedgehog-replay "Size 33 Seed 4268413180681694343 17092859468972210393"' to reproduce from the command-line.

1 out of 1 tests failed (0.49s)
-}
--------------------------------------

iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
iterateNM n a f
  | n <= 0 = pure a
  | otherwise = f a >>= \fa -> iterateNM (n - 1) fa f

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

unit_sat_con_1 :: Assertion
unit_sat_con_1 =
  offeredActionTest
    SmartHoles
    Intermediate
    (emptyHole `ann` (tEmptyHole `tfun` tEmptyHole))
    (InExpr [Child1])
    (Right (MakeCon, Option "Cons" $ Just $ unName <$> unModuleName builtinModuleName))
    (hole (con cCons [emptyHole, emptyHole] `ann` tEmptyHole) `ann` (tEmptyHole `tfun` tEmptyHole))

unit_sat_con_2 :: Assertion
unit_sat_con_2 =
  offeredActionTest
    SmartHoles
    Intermediate
    (emptyHole `ann` ((tcon tList `tapp` tcon tNat) `tfun` (tcon tList `tapp` tcon tNat)))
    (InExpr [Child1])
    (Right (MakeCon, Option "Cons" $ Just $ unName <$> unModuleName builtinModuleName))
    (hole (con cCons [emptyHole, emptyHole] `ann` tEmptyHole) `ann` ((tcon tList `tapp` tcon tNat) `tfun` (tcon tList `tapp` tcon tNat)))

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

data MovementList
  = InExpr [Movement]
  | InType [Movement] [Movement]

exprMoves :: MovementList -> [Movement]
exprMoves = \case
  InExpr ms -> ms
  InType ms _ -> ms

typeMoves :: MovementList -> Maybe [Movement]
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
  MovementList ->
  Either NoInputAction (InputAction, Option) ->
  S Expr ->
  Assertion
offeredActionTest sh l inputExpr position action expectedOutput = do
  offeredActionTest' sh l inputExpr position action >>= \case
    Left err -> assertFailure $ show err
    Right result -> clearMeta result @?= clearMeta (create' expectedOutput)

offeredActionTestNotOffered ::
  HasCallStack =>
  SmartHoles ->
  Level ->
  S Expr ->
  MovementList ->
  NoInputAction ->
  Assertion
offeredActionTestNotOffered sh l inputExpr position action = do
  offeredActionTest' sh l inputExpr position (Left action) >>= \case
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
  S Expr ->
  MovementList ->
  Either NoInputAction (InputAction, Option) ->
  IO (Either OAT Expr)
offeredActionTest' sh l inputExpr position action = do
  let progRaw = create' $ do
        ms <- sequence [builtinModule]
        prog0 <- defaultEmptyProg
        e <- inputExpr
        d <- ASTDef e <$> tEmptyHole
        pure $
          prog0
            & (#progModules % _head % #moduleDefs % ix "main" .~ DefAST d)
            & (#progImports .~ ms)
            -- Temporarily disable smart holes, so what is written in unit tests is what is in the prog
            & (#progSmartHoles .~ NoSmartHoles)
  -- Typecheck everything to fill in typecaches.
  -- This lets us test offered names for renaming variable binders.
  let progChecked = runTypecheckTestM NoSmartHoles $ checkProgWellFormed progRaw
  let (modules, expr, exprDef, exprDefName, prog) = case progChecked of
        Left err -> error $ "offeredActionTest: no typecheck: " <> show err
        Right p -> case progModules p of
          [m] -> case moduleDefs m M.!? "main" of
            Just (DefAST def@(ASTDef e _)) -> (progImports p, e, def, qualifyName (moduleName m) "main", p & #progSmartHoles .~ sh)
            _ -> error "offeredActionTest: didn't find 'main'"
          _ -> error "offeredActionTest: expected exactly one progModule"
  let id' = evalTestM (nextProgID prog) $
        runExceptT $
          flip runReaderT (buildTypingContextFromModules modules sh) $
            do
              ez <- foldlM (flip moveExpr) (focus expr) $ exprMoves position
              case typeMoves position of
                Nothing -> pure $ getID ez
                Just ms -> do
                  tz' <- enterType ez
                  tz <- foldlM (flip moveType) tz' ms
                  pure $ getID tz
  id <- case id' of
    Left err -> assertFailure $ show err
    Right i' -> pure i'
  let cxt = buildTypingContextFromModules modules sh
  let defs = foldMap' moduleDefsQualified modules
  let offered = Available.forBody cxt.typeDefs l Editable expr id
  let options = Available.options cxt.typeDefs defs cxt l exprDef (Just (BodyNode, id))
  action' <- case action of
    Left a ->
      pure $
        if Available.NoInput a `elem` offered
          then Right $ toProgActionNoInput (foldMap' moduleDefsQualified $ progModules prog) exprDef exprDefName (Just (BodyNode, id)) a
          else Left $ ActionNotOffered (Available.NoInput a) offered
    Right (a, o) -> do
      if Available.Input a `elem` offered
        then case options a of
          Nothing -> assertFailure "Available.options returned Nothing"
          Just os ->
            pure $
              if o `elem` os.opts
                then Right $ toProgActionInput exprDef exprDefName (Just (BodyNode, id)) o a
                else Left $ OptionNotOffered o os.opts
        else pure $ Left $ ActionNotOffered (Available.Input a) offered
  action'' <- for action' $ \case
    Left err -> assertFailure $ show err
    Right a -> pure a
  x <- for action'' $ \action''' -> do
    let result = fmap astDefExpr . defAST <=< flip findGlobalByName exprDefName
    let assertJust = maybe (assertFailure "Lost 'main' after action") pure
    (res, _) <- runAppTestM (nextProgID prog) (mkEmptyTestApp prog) (handleEditRequest action''')
    rr <- traverse (assertJust . result) res
    pure $ first ErrorRunningAction rr
  pure $ join x

-- Correct names offered when running actions
-- NB: Bools are offered names "p", "q"; functions get "f","g"; nats get "i","j","n","m"
offeredNamesTest :: HasCallStack => S Expr -> MovementList -> InputAction -> Text -> S Expr -> Assertion
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
