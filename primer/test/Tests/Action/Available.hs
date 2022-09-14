module Tests.Action.Available where

import Foreword

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
  ActionInput (..),
  ActionName (..),
  OfferedAction (..),
  UserInput (
    ChooseConstructor,
    ChooseOrEnterName,
    ChooseTypeConstructor,
    ChooseTypeVariable,
    ChooseVariable
  ),
 )
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
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
import Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha), runTypecheckTestMIn)
import Text.Pretty.Simple (pShowNoColor)
import Control.Monad.Log (PureLoggingT, runPureLoggingT, mapLogMessage, LoggingT, WithSeverity)
import qualified Data.Sequence as Seq
import Primer.Log (ConvertLogMessage (convert))

-- | Comprehensive DSL test.
test_1 :: TestTree
test_1 = mkTests [builtinModule] $ create' $ comprehensiveWellTyped $ mkSimpleModuleName "M"

data Output = Output
  { defActions :: [ActionName]
  , bodyActions :: [(ID, [ActionName])]
  , sigActions :: [(ID, [ActionName])]
  }
  deriving (Show)

-- | Golden tests for the available actions at each node of the definition, for each level.
mkTests :: [Module] -> (GVarName, Def) -> TestTree
mkTests _ (_, DefPrim _) = error "mkTests is unimplemented for primitive definitions."
mkTests deps (defName, DefAST def') =
  let d = defName
      m = Module (qualifiedModule d) mempty $ Map.singleton (baseName d) $ DefAST def'
      def = case runTypecheckTestMIn
        (buildTypingContextFromModules deps NoSmartHoles)
        (checkEverything NoSmartHoles CheckEverything{trusted = deps, toCheck = [m]}) of
        Left err -> error $ "mkTests: no typecheck: " <> show err
        Right [m'] -> case Map.toList $ moduleDefs m' of
          [(_, DefAST def'')] -> def''
          _ -> error "mkTests: expected exactly one definition in checked module"
        _ -> error "mkTests: expected exactly one module checked modules"
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
      enumeratePairs = (,) <$> enumerate <*> enumerate
   in testGroup testName $
        enumeratePairs
          <&> \(level, mut) ->
            -- We sort the offered actions to make the test output more stable
            let defActions = sort' $ map name $ actionsForDef level (Map.singleton defName (mut, DefAST def)) d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , sort' $
                            map name $
                              actionsForDefBody
                                (foldMap @[] moduleTypesQualified [builtinModule, primitiveModule])
                                level
                                defName
                                mut
                                id
                                (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , sort' $ map name $ actionsForDefSig level defName mut id (astDefType def)
                        )
                    )
                    . toListOf (_typeMeta % _id)
                    $ astDefType def
             in goldenVsString (show level) ("test/outputs/available-actions" </> testName </> show level <> "-" <> show mut <> ".fragment") $
                  pure . BS.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor $
                    Output
                      { defActions
                      , bodyActions
                      , sigActions
                      }
  where
    -- To avoid having an Ord instance on ActionName just for this test, we
    -- can just sort by how the action is shown.
    sort' :: [ActionName] -> [ActionName]
    sort' = sortOn $ show @_ @Text

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
        pure (foo, Map.fromList $ fmap (second (Editable,)) ds)
   in for_
        enumerate
        ( \l ->
            description <$> actionsForDef l defs d
              @?= ["Rename this definition", "Duplicate this definition"]
        )

tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      cxt <- forAllT $ Gen.element [[], [builtinModule], [builtinModule, primitiveModule]]
      -- We only test SmartHoles mode (which is the only supported user-facing
      -- mode - NoSmartHoles is only used for internal sanity testing etc)
      a <- forAllT $ genApp SmartHoles cxt
      let allDefs = progAllDefs $ appProg a
      let isMutable = \case
            Editable -> True
            NonEditable -> False
      (defName, (defMut, def)) <- case partition (isMutable . fst . snd) $ Map.toList allDefs of
        ([], []) -> discard
        (mut, []) -> label "all mut" >> forAllT (Gen.element mut)
        ([], immut) -> label "all immut" >> forAllT (Gen.element immut)
        (mut, immut) -> label "mixed mut/immut" >> forAllT (Gen.frequency [(9, Gen.element mut), (1, Gen.element immut)])
      collect defMut
      case def of
        DefAST{} -> label "AST"
        DefPrim{} -> label "Prim"
      (loc, acts) <-
        fmap snd . forAllWithT fst $
          Gen.frequency $
            catMaybes
              [ Just (1, pure ("actionsForDef", (Nothing, actionsForDef l allDefs defName)))
              , defAST def <&> \d' -> (2,) $ do
                  let ty = astDefType d'
                      ids = ty ^.. typeIDs
                  i <- Gen.element ids
                  let ann = "actionsForDefSig id " <> show i
                  pure (ann, (Just $ Left i, actionsForDefSig l defName defMut i ty))
              , defAST def <&> \d' -> (7,) $ do
                  let expr = astDefExpr d'
                      ids = expr ^.. exprIDs
                  i <- Gen.element ids
                  let ann = "actionsForDefBody id " <> show i
                  pure (ann, (Just $ Right i, actionsForDefBody (snd <$> progAllTypeDefs (appProg a)) l defName defMut i expr))
              ]
      case acts of
        [] -> label "no offered actions" >> success
        acts' -> do
          action <- forAllWithT (toS . description) $ Gen.element acts'
          let checkActionInput = \case
                InputRequired (ChooseConstructor _ f) -> do
                  label "ChooseConstructor"
                  let cons = allValConNames $ appProg a
                  if null cons
                    then label "no valcons, skip" >> success
                    else do
                      c <- forAllT $ Gen.element cons
                      let act' = f $ globalNameToQualifiedText c
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                InputRequired (ChooseTypeConstructor f) -> do
                  label "ChooseTypeConstructor"
                  let cons = allTyConNames $ appProg a
                  if null cons
                    then label "no tycons, skip" >> success
                    else do
                      c <- forAllT $ Gen.element cons
                      let act' = f $ globalNameToQualifiedText c
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                InputRequired (ChooseOrEnterName _ opts f) -> do
                  label "ChooseOrEnterName"
                  annotateShow opts
                  let anOpt = (True,) <$> Gen.element opts
                      other = (False,) <$> genName
                  (wasOffered, n) <- forAllT $ if null opts then other else Gen.choice [anOpt, other]
                  let act' = f n
                  annotateShow act'
                  (if wasOffered then actionSucceeds else actionSucceedsOrCapture) (handleEditRequest act') a
                InputRequired (ChooseVariable _ f) -> do
                  label "ChooseVariable"
                  let vars = case loc of
                        Nothing -> error "actionsForDef only ever gives ChooseOrEnterName or NoInputRequired"
                        Just (Left _) -> error "Shouldn't offer ChooseVariable in a type!"
                        Just (Right i) -> case focusOn i . astDefExpr =<< defAST def of
                          Nothing -> error "cannot focus on an id in the expr?"
                          Just ez ->
                            let (_, lvars, gvars) = variablesInScopeExpr (snd <$> allDefs) $ locToEither ez
                             in map (LocalVarRef . fst) lvars <> map (GlobalVarRef . fst) gvars
                  if null vars
                    then label "no vars, skip" >> success
                    else do
                      v <- forAllT $ Gen.element vars
                      let act' = f v
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                InputRequired (ChooseTypeVariable f) -> do
                  label "ChooseTypeVariable"
                  let vars = case loc of
                        Nothing -> error "actionsForDef only ever gives ChooseOrEnterName or NoInputRequired"
                        Just (Left i) -> case focusOnTy i . astDefType =<< defAST def of
                          Nothing -> error "cannot focus on an id in the type?"
                          Just tz -> fst <$> variablesInScopeTy tz
                        Just (Right i) -> case focusOn i . astDefExpr =<< defAST def of
                          Nothing -> error "cannot focus on an id in the expr?"
                          Just ez ->
                            let (tyvars, _, _) = variablesInScopeExpr (snd <$> allDefs) $ locToEither ez
                             in fst <$> tyvars
                  if null vars
                    then label "no tyvars, skip" >> success
                    else do
                      v <- forAllT $ Gen.element vars
                      let act' = f $ unName $ unLocalName v
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                NoInputRequired act' -> do
                  label "NoInputRequired"
                  annotateShow act'
                  actionSucceeds (handleEditRequest act') a
                AskQuestion q act' -> do
                  label "GenerateName (recurses)"
                  answer <- querySucceeds (handleQuestion q) a
                  checkActionInput $ act' answer
          collect $ description action
          checkActionInput $ input action
  where
    actionSucceeds :: HasCallStack
                   => EditAppM (LoggingT TestLog (PureLoggingT (Seq TestLog) Identity)) a
                   -> App -> PropertyT WT ()
    -- REVIEW/TODO: runIdentity . runPureLoggingT . mapLogMessage singleton
    --   should be factored out - I use it elsewhere ...
    --   also TestLog
    actionSucceeds m a = case runIdentity $ runPureLoggingT $ mapLogMessage Seq.singleton $ runEditAppM m a of
      ((Left err, _),logs) -> do
        annotateShow err
        annotateShow logs
        failure
      ((Right _, a'),logs) -> do
        ensureEmptyLogs logs
        ensureSHNormal a'
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack
                            => EditAppM (LoggingT TestLog (PureLoggingT (Seq TestLog) Identity)) a
                            -> App -> PropertyT WT ()
    actionSucceedsOrCapture m a = case runIdentity $ runPureLoggingT $ mapLogMessage Seq.singleton $ runEditAppM m a of
      ((Left (ActionError NameCapture), _),logs) -> do
        label "name-capture with entered name"
        annotate "ignoring name capture error as was generated name, not offered one"
        ensureEmptyLogs logs
      ((Left (ActionError (CaseBindsClash{})), _), logs) -> do
        label "name-clash with entered name"
        annotate "ignoring name clash error as was generated name, not offered one"
        ensureEmptyLogs logs
      ((Left DefAlreadyExists{}, _), logs) -> do
        label "rename def name clash with entered name"
        annotate "ignoring def already exists error as was generated name, not offered one"
        ensureEmptyLogs logs
      ((Left err, _),logs) -> do
        annotateShow err
        annotateShow logs
        failure
      ((Right _, a'),logs) ->         do
        ensureEmptyLogs logs
        ensureSHNormal a'
    ensureEmptyLogs logs = if Seq.null logs then success else annotateShow logs >> failure
    ensureSHNormal a = case checkAppWellFormed a of
      Left err -> annotateShow err >> failure
      Right a' -> TypeCacheAlpha a === TypeCacheAlpha a'
    querySucceeds :: HasCallStack => QueryAppM a -> App -> PropertyT WT a
    querySucceeds m a = case runQueryAppM m a of
      Left err -> annotateShow err >> failure
      Right x -> pure x
    globalNameToQualifiedText n = (fmap unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

type TestLog = WithSeverity TestLogMessage
newtype TestLogMessage = TestLogMessage Text
  deriving newtype Show

instance ConvertLogMessage Text TestLogMessage where
  convert = TestLogMessage
