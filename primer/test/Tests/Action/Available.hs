{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Action.Available where

import Foreword

import Control.Monad.Log (WithSeverity)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Hedgehog (
  GenT,
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
import Hedgehog.Range qualified as Range
import Optics (ix, toListOf, (%), (.~), (^..), _head)
import Primer.Action (
  ActionError (CaseBindsClash, CaseBranchAlreadyExists, NameCapture),
  Movement (Child1, Child2),
  ProgAction (AddTypeDef, CreateDef),
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
  EvalBoundedInterpReq (EvalBoundedInterpReq),
  EvalFullReq (EvalFullReq),
  EvalReq (EvalReq),
  Level (Beginner, Expert, Intermediate),
  MutationRequest (Edit),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgError (
    ActionError,
    ConAlreadyExists,
    DefAlreadyExists,
    EvalError,
    TypeDefAlreadyExists,
    TypeDefModifyNameClash
  ),
  Question (GenerateName, VariablesInScope),
  Selection' (..),
  TypeDefConsSelection (TypeDefConsSelection, con, field),
  TypeDefNodeSelection (TypeDefConsNodeSelection, TypeDefParamNodeSelection),
  TypeDefParamSelection (TypeDefParamSelection, kindMeta, param),
  TypeDefSelection (..),
  appProg,
  checkAppWellFormed,
  checkProgWellFormed,
  handleEditRequest,
  handleEvalBoundedInterpRequest,
  handleEvalFullRequest,
  handleEvalRequest,
  handleMutationRequest,
  handleQuestion,
  nextProgID,
  progAllDefs,
  progAllTypeDefsMeta,
  progCxt,
  progDefMap,
  progImports,
  progModules,
  progSmartHoles,
  progTypeDefMap,
  redoLogEmpty,
  runEditAppM,
  undoLogEmpty,
 )
import Primer.App qualified as App
import Primer.App.Base (TypeDefConsFieldSelection (..))
import Primer.App.Utils (forgetProgTypecache)
import Primer.Builtins (builtinModuleName, cCons, cFalse, cTrue, tBool, tList, tNat)
import Primer.Core (
  Expr,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  Kind' (..),
  KindMeta,
  ModuleName (unModuleName),
  Pattern (PatPrim),
  TyConName,
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
  caseFB_,
  case_,
  char,
  con,
  create',
  emptyHole,
  gvar,
  hole,
  kfun,
  ktype,
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
  generateIDs,
  typeIDs,
 )
import Primer.Def (
  ASTDef (..),
  Def (DefAST, DefPrim),
  defAST,
 )
import Primer.Eval (EvalError (NotRedex), NormalOrderOptions (StopAtBinders, UnderBinders))
import Primer.EvalFullInterp (
  Timeout (MicroSec),
 )
import Primer.EvalFullStep (Dir (Chk))
import Primer.Examples (comprehensiveWellTyped)
import Primer.Gen.App (genApp)
import Primer.Gen.Core.Raw (genName)
import Primer.Gen.Core.Typed (
  WT,
  forAllT,
  genChar,
  genInt,
  genSyn,
  genWTKind,
  genWTType,
  isolateWT,
  propertyWT,
 )
import Primer.Log (
  PureLogT,
  runPureLogT,
 )
import Primer.Module (
  Module (Module, moduleDefs),
  builtinModule,
  moduleDefsQualified,
  moduleName,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.App (
  runAppTestM,
 )
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (clearMeta, clearTypeMeta, testNoSevereLogs)
import Primer.TypeDef (
  ASTTypeDef (ASTTypeDef, astTypeDefConstructors),
  ValCon (..),
  astTypeDefParameters,
  forgetTypeDefMetadata,
  typeDefAST,
 )
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
import Tests.Action.Prog (defaultEmptyProg, findGlobalByName, mkEmptyTestApp, readerToState)
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
      typeDefs = foldMap' @[] moduleTypesQualified $ create' $ sequence [builtinModule, primitiveModule]
      offered level id = \case
        Available.NoInput a -> NoInput a
        Available.Input a ->
          Input a
            . fromMaybe (error "id not found")
            $ Available.options typeDefs defs cxt level (Right def) id a
   in testGroup testName $
        enumeratePairs <&> \(level, mut) ->
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

-- Any offered action will complete successfully,
-- other than one with a student-specified name that introduces capture.
tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported student-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      a <- forAllT $ genApp SmartHoles cxt
      (def'', loc, action) <- forAllT $ genAction l a
      annotateShow def''
      annotateShow loc
      annotateShow action
      let (_defName, def) = (bimap fst fst def'', bimap snd snd def'')
      collect action
      pa <- toProgAction l a (def, loc, action)
      case pa of
        NoOpt progActs -> void $ actionSucceeds (handleEditRequest progActs) a
        OptOffered _ progActs -> void $ actionSucceeds (handleEditRequest progActs) a
        OptGen _ progActs -> void $ actionSucceedsOrCapture (handleEditRequest progActs) a
        NoOfferedOpts -> annotate "no options" >> success

-- Running multiple "things" one after the other will succeed
-- (other than student-specified names causing capture), where a "thing" is one of:
--   - an offered action
--   - adding a new (type or term) definition
--   - undo or redo
--   - running some full evaluation
--   - asking a @Question@
tasty_multiple_requests_accepted :: Property
tasty_multiple_requests_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.choice $ map sequence [[], [builtinModule], [builtinModule, primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported student-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      app0 <- forAllT $ genApp SmartHoles cxt
      numActions <- forAllT $ Gen.int $ Range.linear 1 20
      let appDefs = foldMap' (M.keys . moduleDefsQualified) . progModules . appProg
          genAction' a' =
            Gen.frequency $
              second pure
                <$> catMaybes
                  [ Just (3, AddTm)
                  , Just (2, AddTy)
                  , Just (1, Eval1)
                  , if null $ appDefs a' then Nothing else Just (1, EvalFull)
                  , Just (1, Question)
                  , if null $ appDefs a' then Nothing else Just (1, EvalBoundedInterp)
                  , if undoLogEmpty $ appProg a' then Nothing else Just (2, Undo)
                  , if redoLogEmpty $ appProg a' then Nothing else Just (2, Redo)
                  , Just (1, RenameModule)
                  , Just (10, AvailAct)
                  ]
      void $ iterateNM numActions app0 $ \appN ->
        forAllT (genAction' appN) >>= \a -> do
          collect a
          case a of
            AddTm -> do
              m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg appN
              n <- forAllT $ Gen.choice [Just . unName <$> genName, pure Nothing]
              actionSucceedsOrCapture (handleMutationRequest $ Edit [CreateDef m n]) appN
                >>= ignoreCaptureClash appN
            AddTy -> do
              m <- forAllT $ Gen.element $ fmap moduleName $ progModules $ appProg appN
              n <- qualifyName m <$> forAllT genName
              actionSucceedsOrCapture (handleMutationRequest $ Edit [AddTypeDef n $ ASTTypeDef [] [] []]) appN
                >>= ignoreCaptureClash appN
            Eval1 -> do
              (e', _) <- forAllT genSyn
              e <- generateIDs e'
              i <- forAllT $ Gen.element $ e ^.. exprIDs
              actionSucceedsOrNotRedex (readerToState $ handleEvalRequest $ EvalReq e i) appN
            EvalFull -> do
              g <- forAllT $ Gen.element $ appDefs appN
              tld <- gvar g
              steps <- forAllT $ Gen.integral $ Range.linear 0 100
              optsN <- forAllT $ Gen.element @[] [StopAtBinders, UnderBinders]
              actionSucceeds (readerToState $ handleEvalFullRequest $ EvalFullReq tld Chk steps optsN) appN
            EvalBoundedInterp -> do
              g <- forAllT $ Gen.element $ appDefs appN
              tld <- gvar g
              usec <- forAllT $ Gen.integral $ Range.linear 0 1000
              actionSucceeds (readerToState $ handleEvalBoundedInterpRequest $ EvalBoundedInterpReq tld Chk (MicroSec usec)) appN
            Question -> do
              -- Obtain a non-exhaustive case warning if we add a new question
              let _w :: Question q -> ()
                  _w = \case
                    VariablesInScope{} -> ()
                    GenerateName{} -> ()
              (_, e, w) <- forAllT $ genLoc appN
              -- We only support questions in editable modules
              case e of
                Editable -> pure ()
                NonEditable -> discard
              (def, node) <- case w of
                SelectionDef (DefSelection{def, node = Just node}) -> pure (def, node.meta)
                SelectionDef (DefSelection{node = Nothing}) -> discard
                -- We don't currently support questions on typedefs
                SelectionTypeDef _ -> discard
              (_, q) <-
                forAllWithT fst $
                  Gen.choice
                    [ pure ("VariablesInScope", void $ handleQuestion (VariablesInScope def node))
                    , ("GenerateName",) . void . handleQuestion . GenerateName def node <$> do
                        k <- genWTKind
                        Gen.choice
                          [ Left <$> Gen.maybe (genWTType k)
                          , pure $ Right $ Just k
                          , pure $ Right Nothing
                          ]
                    ]
              actionSucceeds (readerToState q) appN
            Undo -> actionSucceeds (handleMutationRequest App.Undo) appN
            Redo -> actionSucceeds (handleMutationRequest App.Redo) appN
            AvailAct -> do
              (def'', loc, action) <- forAllT $ genAction l appN
              annotateShow def''
              annotateShow loc
              annotateShow action
              let def = bimap snd snd def''
              collect action
              pa <- toProgAction l appN (def, loc, action)
              case pa of
                NoOpt progActs -> actionSucceeds (handleEditRequest progActs) appN
                OptOffered _ progActs -> actionSucceeds (handleEditRequest progActs) appN
                OptGen _ progActs ->
                  actionSucceedsOrCapture (handleEditRequest progActs) appN
                    >>= ignoreCaptureClash appN
                NoOfferedOpts -> annotate "ignoring - no options" >> pure appN
            RenameModule -> do
              m <- forAllT $ moduleName <$> Gen.element (progModules $ appProg appN)
              n <- forAllT (Gen.nonEmpty (Range.linear 1 5) genName)
              actionSucceedsOrCapture (handleMutationRequest $ Edit [App.RenameModule m $ unName <$> n]) appN >>= ignoreCaptureClash appN
  where
    ignoreCaptureClash a = \case
      Succeed b -> pure b
      Capture -> label "ignoring - capture" >> pure a
      Clash -> label "ignoring - clash" >> pure a
    iterateNM :: Monad m => Int -> a -> (a -> m a) -> m a
    iterateNM n a f
      | n <= 0 = pure a
      | otherwise = f a >>= \fa -> iterateNM (n - 1) fa f

-- Helper for tasty_multiple_requests_accepted
data Act
  = AddTm
  | AddTy
  | Eval1
  | EvalFull
  | EvalBoundedInterp
  | Question
  | Undo
  | Redo
  | RenameModule
  | AvailAct
  deriving stock (Show)

-- Helpers for tasty_available_actions_accepted and tasty_chained_actions_undo_accepted

runEditAppMLogs ::
  (HasCallStack) =>
  EditAppM (PureLogT (WithSeverity ()) WT) ProgError a ->
  App ->
  PropertyT WT (Either ProgError a, App)
runEditAppMLogs m a = do
  (r, logs) <- lift $ isolateWT $ runPureLogT $ runEditAppM m a
  testNoSevereLogs logs >> pure r

actionSucceeds :: HasCallStack => EditAppM (PureLogT (WithSeverity ()) WT) ProgError a -> App -> PropertyT WT App
actionSucceeds m a =
  runEditAppMLogs m a >>= \case
    (Left err, _) -> annotateShow err >> failure
    (Right _, a') -> ensureSHNormal a' $> a'

actionSucceedsOrNotRedex :: HasCallStack => EditAppM (PureLogT (WithSeverity ()) WT) ProgError a -> App -> PropertyT WT App
actionSucceedsOrNotRedex m a =
  runEditAppMLogs m a >>= \case
    (Left (EvalError NotRedex), _) -> do
      label "name-capture with entered name"
      annotate "ignoring name capture error as was generated name, not offered one"
      pure a
    (Left err, _) -> annotateShow err >> failure
    (Right _, a') -> ensureSHNormal a' $> a'

-- Helper for tasty_available_actions_accepted and tasty_chained_actions_undo_accepted
-- Similar to 'actionSucceeds' but bearing in mind that
-- if we submit our own name rather than an offered one, then
-- we should expect that name capture/clashing may happen
actionSucceedsOrCapture :: HasCallStack => EditAppM (PureLogT (WithSeverity ()) WT) ProgError a -> App -> PropertyT WT (SucceedOrCapture App)
actionSucceedsOrCapture m a = do
  a' <- runEditAppMLogs m a
  case a' of
    (Left (ActionError NameCapture), _) -> do
      label "name-capture with entered name"
      annotate "ignoring name capture error as was generated name, not offered one"
      pure Capture
    (Left (ActionError (CaseBindsClash{})), _) -> do
      label "name-clash with entered name"
      annotate "ignoring name clash error as was generated name, not offered one"
      pure Clash
    (Left DefAlreadyExists{}, _) -> do
      label "rename def name clash with entered name"
      annotate "ignoring def already exists error as was generated name, not offered one"
      pure Clash
    (Left (ActionError (CaseBranchAlreadyExists (PatPrim _))), _) -> do
      label "add duplicate primitive case branch"
      annotate "ignoring CaseBranchAlreadyExistsPrim error as was generated constructor"
      pure Clash
    (Left (TypeDefAlreadyExists _), _) -> do
      pure Clash
    (Left (ConAlreadyExists _), _) -> do
      pure Clash
    (Left (TypeDefModifyNameClash _), _) -> do
      pure Clash
    (Left err, _) -> annotateShow err >> failure
    (Right _, a'') -> ensureSHNormal a'' $> Succeed a''

data SucceedOrCapture a
  = Succeed a
  | Capture
  | Clash

-- Helper for tasty_available_actions_accepted and tasty_chained_actions_undo_accepted
ensureSHNormal :: App -> PropertyT WT ()
ensureSHNormal a = case checkAppWellFormed a of
  Left err -> annotateShow err >> failure
  Right a' ->
    TypeCacheAlpha (forgetProgTypecache $ appProg a)
      === TypeCacheAlpha (forgetProgTypecache $ appProg a')

genLoc ::
  forall m.
  Monad m =>
  App ->
  GenT
    m
    ( Either
        (TyConName, ASTTypeDef TypeMeta KindMeta)
        (GVarName, Def)
    , Editable
    , Selection' ID
    )
genLoc a = do
  let allTypes = progAllTypeDefsMeta $ appProg a
      allASTTypes = M.mapMaybe (traverse typeDefAST) allTypes
  let allDefs = progAllDefs $ appProg a
  let isMutable = \case
        Editable -> True
        NonEditable -> False
  let genDef :: Map name (Editable, def) -> GenT m (Maybe (Editable, (name, def)))
      genDef m =
        (\(n, (e, t)) -> (e, (n, t)))
          <<$>> case partition (isMutable . fst . snd) $ Map.toList m of
            ([], []) -> pure Nothing
            (mut, []) -> Just <$> Gen.element mut
            ([], immut) -> Just <$> Gen.element immut
            (mut, immut) -> Just <$> Gen.frequency [(9, Gen.element mut), (1, Gen.element immut)]
  (defMut, typeOrTermDef) <-
    maybe Gen.discard pure
      =<< Gen.choice
        [ second Left <<$>> genDef allASTTypes
        , second Right <<$>> genDef allDefs
        ]
  loc <- case typeOrTermDef of
    Left (defName, def) ->
      let typeDefSel = SelectionTypeDef . TypeDefSelection defName
       in Gen.frequency
            [ (1, pure $ typeDefSel Nothing)
            ,
              ( 2
              , case astTypeDefParameters def of
                  [] -> pure $ typeDefSel Nothing
                  ps -> do
                    (p, k) <- Gen.element ps
                    let typeDefParamNodeSel = typeDefSel . Just . TypeDefParamNodeSelection . TypeDefParamSelection p
                    Gen.frequency
                      [
                        ( 1
                        , pure
                            (typeDefParamNodeSel Nothing)
                        )
                      ,
                        ( 3
                        , do
                            let allKindIDs = \case
                                  KHole m -> [getID m]
                                  KType m -> [getID m]
                                  KFun m k1 k2 -> [getID m] <> allKindIDs k1 <> allKindIDs k2
                            id <- Gen.element @[] $ allKindIDs k
                            pure
                              (typeDefParamNodeSel $ Just id)
                        )
                      ]
              )
            ,
              ( 5
              , case astTypeDefConstructors def of
                  [] -> pure $ typeDefSel Nothing
                  cs -> do
                    ValCon{valConName, valConArgs} <- Gen.element cs
                    let typeDefConsNodeSel = typeDefSel . Just . TypeDefConsNodeSelection . TypeDefConsSelection valConName
                    case valConArgs of
                      [] -> pure $ typeDefConsNodeSel Nothing
                      as ->
                        Gen.frequency
                          [ (1, pure $ typeDefConsNodeSel Nothing)
                          ,
                            ( 5
                            , do
                                (n, t) <- Gen.element $ zip [0 ..] as
                                i <- Gen.element $ t ^.. typeIDs
                                pure
                                  (typeDefConsNodeSel . Just $ TypeDefConsFieldSelection n i)
                            )
                          ]
              )
            ]
    Right (defName, def) ->
      fmap (SelectionDef . DefSelection defName)
        . Gen.frequency
        $ catMaybes
          [ Just (1, pure Nothing)
          , defAST def <&> \d' -> (2,) $ do
              let ty = astDefType d'
                  ids = ty ^.. typeIDs
              i <- Gen.element ids
              pure (Just $ NodeSelection SigNode i)
          , defAST def <&> \d' -> (7,) $ do
              let expr = astDefExpr d'
                  ids = expr ^.. exprIDs
              i <- Gen.element ids
              pure (Just $ NodeSelection BodyNode i)
          ]
  pure (typeOrTermDef, defMut, loc)

genAction ::
  forall m.
  Monad m =>
  Level ->
  App ->
  GenT
    m
    ( Either
        (TyConName, ASTTypeDef TypeMeta KindMeta)
        (GVarName, Def)
    , Selection' ID
    , Maybe Available.Action
    )
genAction l a = do
  let allTypes = map (forgetTypeDefMetadata . snd) $ progAllTypeDefsMeta $ appProg a
      allDefs = progDefMap $ appProg a
  (loc, mut, s) <- genLoc a
  let acts = case (loc, s) of
        (Left (_, def), SelectionTypeDef (TypeDefSelection defName Nothing)) ->
          Available.forTypeDef l mut allTypes allDefs defName def
        (Left (_, def), SelectionTypeDef (TypeDefSelection defName (Just (TypeDefParamNodeSelection n))))
          | Nothing <- n.kindMeta -> Available.forTypeDefParamNode n.param l mut allTypes allDefs defName def
          | Just i <- n.kindMeta -> Available.forTypeDefParamKindNode n.param i l mut allTypes allDefs defName def
        (Left (_, def), SelectionTypeDef (TypeDefSelection defName (Just (TypeDefConsNodeSelection n))))
          | Nothing <- n.field -> Available.forTypeDefConsNode l mut allTypes allDefs defName def
          | Just f <- n.field -> Available.forTypeDefConsFieldNode n.con f.index f.meta l mut allTypes allDefs defName def
        (Right (_, _), SelectionDef (DefSelection defName Nothing)) ->
          Available.forDef allDefs l mut defName
        (Right (_, DefAST def), SelectionDef (DefSelection _ (Just (NodeSelection SigNode i)))) ->
          Available.forSig l mut (astDefType def) i
        (Right (_, DefAST def), SelectionDef (DefSelection _ (Just (NodeSelection BodyNode i)))) ->
          Available.forBody allTypes l mut (astDefExpr def) i
        _ -> error $ "Invariant failed in output of genLoc, got loc=" <> show loc <> ", s=" <> show s
  case acts of
    [] -> pure (loc, s, Nothing)
    acts' -> do
      action <- Gen.element acts'
      pure (loc, s, Just action)

data PA
  = NoOpt [ProgAction]
  | -- | An option the API offered
    OptOffered Option [ProgAction]
  | -- | A free-form "student provided" option
    OptGen Option [ProgAction]
  | -- | An 'ActionInput' but with no offered options
    NoOfferedOpts

toProgAction ::
  Monad m =>
  Level ->
  App ->
  ( Either (ASTTypeDef TypeMeta KindMeta) Def
  , Selection' ID
  , Maybe Available.Action
  ) ->
  PropertyT m PA
toProgAction l a (def, loc, action) = do
  def' <- case (defAST <$> def, action) of
    (Left d, _) -> pure $ Left d
    (Right Nothing, Nothing) -> discard
    (Right Nothing, Just act) -> do
      annotate "Expected no action to be available on a primitive"
      annotateShow def
      annotateShow act
      failure
    (Right (Just d), _) -> pure $ Right d
  case action of
    Nothing -> discard
    Just action' -> case action' of
      Available.NoInput act' -> do
        progActs <-
          either (\e -> annotateShow e >> failure) pure $
            toProgActionNoInput (progDefMap $ appProg a) def' loc act'
        pure $ NoOpt progActs
      Available.Input act' -> do
        Available.Options{Available.opts, Available.free} <-
          maybe (annotate "id not found" >> failure) pure $
            Available.options
              (progTypeDefMap $ appProg a)
              (progDefMap $ appProg a)
              (progCxt $ appProg a)
              l
              def'
              loc
              act'
        let opts' = [Gen.element $ (OptOffered,) <$> opts | not (null opts)]
        let opts'' =
              opts' <> case free of
                Available.FreeNone -> []
                Available.FreeVarName -> [((OptGen,) . (\t -> Available.Option t Nothing False)) . unName <$> genName]
                Available.FreeInt -> [((OptGen,) . (\t -> Available.Option t Nothing False)) . show <$> genInt]
                Available.FreeChar -> [(OptGen,) . (\t -> Available.Option t Nothing False) . T.singleton <$> genChar]
        case opts'' of
          [] -> pure NoOfferedOpts
          options -> do
            (mkPA, opt) <- forAllWithT (show . snd) $ Gen.choice options
            progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' loc opt act'
            pure $ mkPA opt progActs

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
    (Right (MakeCon, Option "Cons" (Just $ unName <$> unModuleName builtinModuleName) False))
    (hole (con cCons [emptyHole, emptyHole]) `ann` (tEmptyHole `tfun` tEmptyHole))

unit_sat_con_2 :: Assertion
unit_sat_con_2 =
  offeredActionTest
    SmartHoles
    Intermediate
    (emptyHole `ann` ((tcon tList `tapp` tcon tNat) `tfun` (tcon tList `tapp` tcon tNat)))
    (InExpr [Child1])
    (Right (MakeCon, Option "Cons" (Just $ unName <$> unModuleName builtinModuleName) False))
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

unit_case_prim :: Assertion
unit_case_prim =
  offeredActionTest
    SmartHoles
    Intermediate
    (char 'a')
    (InExpr [])
    (Left MakeCase)
    (caseFB_ (char 'a') [] emptyHole)

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
        ms <- sequence [builtinModule, primitiveModule]
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
          then Right $ toProgActionNoInput (foldMap' moduleDefsQualified $ progModules prog) (Right exprDef) (SelectionDef $ DefSelection exprDefName $ Just $ NodeSelection BodyNode id) a
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
    (res, _) <- runAppTestM (mkEmptyTestApp prog) (handleEditRequest action''')
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
    (Right (act, Option name Nothing False))

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
    (emptyHole `ann` tforall "a" ktype (tcon tBool))
    (InExpr [Child1])
    MakeLAM
    "α"
    (lAM "α" emptyHole `ann` tforall "a" ktype (tcon tBool))
  offeredNamesTest
    (emptyHole `ann` tforall "a" (kfun ktype ktype) (tcon tBool))
    (InExpr [Child1])
    MakeLAM
    "f"
    (lAM "f" emptyHole `ann` tforall "a" (kfun ktype ktype) (tcon tBool))

unit_rename_LAM_names :: Assertion
unit_rename_LAM_names = do
  offeredNamesTest
    (lAM "x" emptyHole `ann` tforall "a" ktype (tcon tBool))
    (InExpr [Child1])
    RenameLAM
    "α"
    (lAM "α" emptyHole `ann` tforall "a" ktype (tcon tBool))
  offeredNamesTest
    (lAM "x" emptyHole `ann` tforall "a" (kfun ktype ktype) (tcon tBool))
    (InExpr [Child1])
    RenameLAM
    "f"
    (lAM "f" emptyHole `ann` tforall "a" (kfun ktype ktype) (tcon tBool))

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
    (emptyHole `ann` tforall "a" ktype (tcon tBool))
    ([] `InType` [])
    RenameForall
    "α"
    (emptyHole `ann` tforall "α" ktype (tcon tBool))
  offeredNamesTest
    (emptyHole `ann` tforall "a" (kfun ktype ktype) (tcon tBool))
    ([] `InType` [])
    RenameForall
    "f"
    (emptyHole `ann` tforall "f" (kfun ktype ktype) (tcon tBool))

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
