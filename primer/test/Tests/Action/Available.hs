module Tests.Action.Available where

import Foreword

import Control.Monad.Log (WithSeverity)
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
import Hedgehog.Range qualified as Range
import Optics (toListOf, (%), (^..))
import Primer.Action (ActionError (CaseBindsClash, NameCapture), toProgActionInput, toProgActionNoInput)
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  Editable (..),
  NodeType (..),
  ProgError (ActionError, DefAlreadyExists),
  appProg,
  checkAppWellFormed,
  handleEditRequest,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  runEditAppM,
 )
import Primer.Core (
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  ModuleName (ModuleName),
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
import Primer.Log (PureLog, runPureLog)
import Primer.Module (
  Module (Module, moduleDefs),
  builtinModule,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Test.Util (testNoSevereLogs)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles, SmartHoles),
  buildTypingContextFromModules,
  checkEverything,
 )
import System.FilePath ((</>))
import Tasty (Property, withDiscards, withTests)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))
import Tests.Typecheck (TypeCacheAlpha (TypeCacheAlpha), runTypecheckTestMIn)
import Text.Pretty.Simple (pShowNoColor)

-- | Comprehensive DSL test.
test_1 :: TestTree
test_1 = mkTests [builtinModule] $ create' $ comprehensiveWellTyped $ mkSimpleModuleName "M"

data Output = Output
  { defActions :: [OfferedAction]
  , bodyActions :: [(ID, [OfferedAction])]
  , sigActions :: [(ID, [OfferedAction])]
  }
  deriving (Show)
data OfferedAction
  = NoInput Available.NoInputAction
  | Input Available.InputAction Available.Options
  deriving (Show)

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
      typeDefs = foldMap @[] moduleTypesQualified [builtinModule, primitiveModule]
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
             in goldenVsString (show level) ("test/outputs/available-actions" </> testName </> show level <> "-" <> show mut <> ".fragment") $
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
              [ Just (1, pure ("actionsForDef", (Nothing, Available.forDef (snd <$> allDefs) l defMut defName)))
              , defAST def <&> \d' -> (2,) $ do
                  let ty = astDefType d'
                      ids = ty ^.. typeIDs
                  i <- Gen.element ids
                  let ann = "actionsForDefSig id " <> show i
                  pure (ann, (Just (SigNode, i), Available.forSig l defMut ty i))
              , defAST def <&> \d' -> (7,) $ do
                  let expr = astDefExpr d'
                      ids = expr ^.. exprIDs
                  i <- Gen.element ids
                  let ann = "actionsForDefBody id " <> show i
                  pure (ann, (Just (BodyNode, i), Available.forBody (snd <$> progAllTypeDefs (appProg a)) l defMut expr i))
              ]
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
              let opts' = [Gen.element opts | not (null opts)]
              let opts'' =
                    opts' <> case free of
                      Available.FreeNone -> []
                      Available.FreeVarName -> [flip Available.Option Nothing <$> (unName <$> genName)]
                      Available.FreeInt -> [flip Available.Option Nothing <$> (show <$> Gen.integral (Range.linear @Integer 0 1_000_000_000))]
                      Available.FreeChar -> [flip Available.Option Nothing . T.singleton <$> Gen.unicode]
              case opts'' of
                [] -> annotate "no options" >> success
                options -> do
                  opt <- forAllT $ Gen.choice options
                  progActs <- either (\e -> annotateShow e >> failure) pure $ toProgActionInput def' defName loc opt act'
                  actionSucceedsOrCapture (handleEditRequest progActs) a
  where
    runEditAppMLogs ::
      HasCallStack =>
      EditAppM (PureLog (WithSeverity ())) a ->
      App ->
      PropertyT WT (Either ProgError a, App)
    runEditAppMLogs m a = case runPureLog $ runEditAppM m a of
      (r, logs) -> testNoSevereLogs logs >> pure r
    actionSucceeds :: HasCallStack => EditAppM (PureLog (WithSeverity ())) a -> App -> PropertyT WT ()
    actionSucceeds m a =
      runEditAppMLogs m a >>= \case
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> ensureSHNormal a'
    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
    actionSucceedsOrCapture :: HasCallStack => EditAppM (PureLog (WithSeverity ())) a -> App -> PropertyT WT ()
    actionSucceedsOrCapture m a =
      runEditAppMLogs m a >>= \case
        (Left (ActionError NameCapture), _) -> do
          label "name-capture with entered name"
          annotate "ignoring name capture error as was generated name, not offered one"
        (Left (ActionError (CaseBindsClash{})), _) -> do
          label "name-clash with entered name"
          annotate "ignoring name clash error as was generated name, not offered one"
        (Left DefAlreadyExists{}, _) -> do
          label "rename def name clash with entered name"
          annotate "ignoring def already exists error as was generated name, not offered one"
        (Left err, _) -> annotateShow err >> failure
        (Right _, a') -> ensureSHNormal a'
    ensureSHNormal a = case checkAppWellFormed a of
      Left err -> annotateShow err >> failure
      Right a' -> TypeCacheAlpha a === TypeCacheAlpha a'
