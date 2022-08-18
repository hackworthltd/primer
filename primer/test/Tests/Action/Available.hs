{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
module Tests.Action.Available where

import Foreword

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate, partition)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Gen.App (genApp)
import Gen.Core.Typed (WT, forAllT, propertyWT)
import Hedgehog (PropertyT, annotateShow, discard, failure, success, label, collect, assert, annotate)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllWithT)
import Optics (toListOf, (%), (^..), (%~))
import Primer.Action (ActionInput (..), ActionName (..), OfferedAction (..), UserInput (ChooseOrEnterName, ChooseTypeConstructor, ChooseConstructor, ChooseVariable, ChooseTypeVariable), ActionError (NameCapture, CaseBindsClash), ProgAction (..), Action (..), Movement (..))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.App (App, EditAppM(..), Prog (..), appProg, handleEditRequest, runEditAppM, progAllModules, progAllDefs, Mutability (Mutable, Immutable), allTyConNames, allValConNames, lookupASTDef, ProgError (ActionError), progAllTypeDefs, mkApp, defaultLog, checkAppWellFormed, getSharedScope, getSharedScopeTy,)
import Primer.Core (
  ASTDef (..),
  Def (DefAST, DefPrim),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  ModuleName (ModuleName, unModuleName),
  mkSimpleModuleName,
  moduleNamePretty,
  qualifyName,
  _typeMeta, defType, defAST, TmVarRef (GlobalVarRef, LocalVarRef), LocalName (unLocalName), ASTTypeDef (..), ValCon (..), TypeDef (..), Kind (..), Type' (..), TypeCacheBoth (..), TypeCache (..), Expr' (..), Meta (..), getID,
 )
import Primer.Core.DSL (
  create',
  emptyHole,
  gvar,
  tEmptyHole, letrec, tvar, ann, tforall, tapp,
 )
import Primer.Core.Utils (
  exprIDs, typeIDs,
 )
import Primer.Examples (comprehensive)
import Primer.Module (moduleDefsQualified, moduleTypesQualified, Module (..))
import Primer.Name (Name (unName))
import Primer.Typecheck (SmartHoles (NoSmartHoles,SmartHoles), buildTypingContextFromModules)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=), assertFailure)
import TestUtils (Property, withDiscards, withTests)
import Text.Pretty.Simple (pShowNoColor)
import Primer.Builtins (builtinModule)
import Primer.Primitives (primitiveModule)
import Gen.Core.Raw (genName)
import Primer.Questions (variablesInScopeExpr, variablesInScopeTy, Question (GenerateName), generateNameExpr, generateNameTy)
import Primer.Zipper (focusOn, locToEither, focusOnTy, Loc' (InType), target, up, replace, unfocusType, unfocusExpr)
import TestM (evalTestM)
import Tests.Action.Prog (progActionTest, expectSuccess, defaultEmptyProg)
import Primer.Pretty (prettyExpr, prettyPrintExpr, compact)

-- | Comprehensive DSL test.
test_1 :: TestTree
test_1 = mkTests $ create' $ comprehensive $ mkSimpleModuleName "M"

data Output = Output
  { defActions :: [ActionName]
  , bodyActions :: [(ID, [ActionName])]
  , sigActions :: [(ID, [ActionName])]
  }
  deriving (Show)

-- | Golden tests for the available actions at each node of the definition, for each level.
mkTests :: (GVarName, Def) -> TestTree
mkTests (_, DefPrim _) = error "mkTests is unimplemented for primitive definitions."
mkTests (defName, DefAST def) =
  let d = defName
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
      enumeratePairs = (,) <$> enumerate <*> enumerate
   in testGroup testName $
        enumeratePairs
          <&> \(level,mut) ->
            let defActions = map name $ actionsForDef level (Map.singleton defName (mut,DefAST def)) d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefBody (foldMap @[] moduleTypesQualified [builtinModule, primitiveModule]) level defName mut id (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefSig level defName mut id (astDefType def)
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
        pure (foo, Map.fromList $ fmap (second (Mutable,)) ds)
   in for_
        enumerate
        ( \l ->
            description <$> actionsForDef l defs d
              @?= ["Rename this definition", "Duplicate this definition"]
        )

-- TODO: on 542dc3b3b1231f7c219b207691a1a2e1e0520663
-- we have a lost id on creation of an annotation
-- See --hedgehog-replay "Size 37 Seed 10944778205474799014 10690135159793852375"

-- TODO/REVIEW: how to ensure this is kept up to date with changes in action offerings
-- we have "RenameCon" actions - these are not advertised yet (and presumably should be?)
-- similarly, eval , questions etc
tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      sh <- forAllT $ Gen.element [{-NoSmartHoles ,-} SmartHoles] -- REVIEW: do we care about NoSmartHoles? We offer lots of "bad" actions in that case
      cxt <- forAllT $ Gen.element [[], [builtinModule], [builtinModule, primitiveModule]]
      a <- forAllT $ genApp sh cxt
      let allDefs =  progAllDefs $ appProg a
      let isMutable = \case
            Mutable -> True
            Immutable -> False
      (defName, (defMut, def)) <- case partition (isMutable . fst . snd) $ Map.toList allDefs of
        ([],[]) -> discard
        (mut,[]) -> label "all mut" >> forAllT ( Gen.element mut)
        ([],immut) -> label "all immut" >> forAllT (Gen.element immut)
        (mut,immut) -> label "mixed mut/immut" >> forAllT (Gen.frequency [(9,Gen.element mut),(1,Gen.element immut)])
      -- TODO: should test primitives also (i.e. they should have no? actions)
      collect defMut
      case def of
        DefAST {} -> label "AST"
        DefPrim {} -> label "Prim"
        {-
      _ <- case def' of
        (mut,DefAST d) -> collect mut >> pure d
        _ -> discard
-}
      (loc,acts) <- fmap snd . forAllWithT fst $ Gen.choice $ catMaybes
      -- TODO: maybe get better test coverage if reduce frequency of actionsForDef?
         [ Just $ pure ("actionsForDef",(Nothing,actionsForDef l allDefs defName))
         , Just $ do
             let ty = defType def
                 ids = ty ^.. typeIDs
             i <- Gen.element ids
             let ann = "actionsForDefSig id " <> show i
             pure (ann,(Just $ Left i,actionsForDefSig l defName defMut i ty))
         , defAST def <&> \d' -> do
             let expr = astDefExpr d'
                 ids = expr ^.. exprIDs -- TODO: this gives ids in the expression, including in bindings; it also  gives ids in type annotations etc, but this is ok, we will just not offer any actions there
             i <- Gen.element ids
             let ann = "actionsForDefBody id " <> show i
             pure (ann, (Just $ Right i, actionsForDefBody (snd <$> progAllTypeDefs (appProg a)) l defName defMut i expr))
         ]
      case acts of
        [] -> success
        acts' -> do
          action <- forAllWithT (toS . description) $ Gen.element acts'
          let checkActionInput = \case
                InputRequired (ChooseConstructor _ f) -> do
                  -- We only test that existing constructors are accepted
                  -- TODO/REVIEW: we should revisit this action -- perhaps it should contain a list of constructors?
                  label "ChooseConstructor"
                  let cons = allValConNames $ appProg a
                  if null cons
                    then label "no valcons, skip" >> success -- TODO: should we even offer the action in that case?
                    else do
                      c <- forAllT $ Gen.element cons
                      let act' = f $ globalNameToQualifiedText c
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                InputRequired (ChooseTypeConstructor f) -> do
                  -- We only test that existing constructors are accepted
                  -- TODO/REVIEW: we should revisit this action -- perhaps it should contain a list of constructors?
                  label "ChooseTypeConstructor"
                  let cons = allTyConNames $ appProg a
                  if null cons
                    then label "no tycons, skip" >> success -- TODO: should we even offer the action in that case?
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
                  (wasOffered, n) <- forAllT $ if null opts then other else Gen.choice [anOpt,other]
                  let act' = f n
                  annotateShow act'
                  (if wasOffered then actionSucceeds else actionSucceedsOrCapture) (handleEditRequest act') a
                InputRequired (ChooseVariable _ f) -> do
                  -- TODO/REVIEW: we should revisit this action -- perhaps it should contain a list of constructors?
                  label "ChooseVariable"
                  let vars = case loc of
                       Nothing -> error "actionsForDef only ever gives ChooseOrEnterName or NoInputRequired"
                       Just (Left _) -> error "Shouldn't offer ChooseVariable in a type!"
                       Just (Right i) -> case focusOn i . astDefExpr =<< defAST def of
                          Nothing -> error "cannot focus on an id in the expr?"
                          Just ez -> let (_,lvars,gvars) = variablesInScopeExpr (snd <$> allDefs) $ locToEither ez
                                     in map (LocalVarRef . fst) lvars <> map (GlobalVarRef . fst) gvars
                  if null vars
                    then label "no vars, skip" >> success -- TODO: should we even offer the action in that case?
                    else do
                      v <- forAllT $ Gen.element vars
                      let act' = f v
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                InputRequired (ChooseTypeVariable f) -> do
                  -- TODO/REVIEW: we should revisit this action -- perhaps it should contain a list of constructors?
                  label "ChooseTypeVariable"
                  let vars = case loc of
                       Nothing -> error "actionsForDef only ever gives ChooseOrEnterName or NoInputRequired"
                       Just (Left i) -> case focusOnTy i (defType def) of
                          Nothing -> error "cannot focus on an id in the type?"
                          Just tz -> fst <$> variablesInScopeTy tz
                       Just (Right i) -> case focusOn i . astDefExpr =<< defAST def of
                          Nothing -> error "cannot focus on an id in the expr?"
                          Just ez -> let (tyvars,_,_) = variablesInScopeExpr (snd <$> allDefs) $ locToEither ez
                                     in fst <$> tyvars
                  if null vars
                    then label "no tyvars, skip" >> success -- TODO: should we even offer the action in that case?
                    else do
                      v <- forAllT $ Gen.element vars
                      let act' = f $ unName $ unLocalName v
                      annotateShow act'
                      actionSucceeds (handleEditRequest act') a
                NoInputRequired act' -> label "NoInputRequired" >> annotateShow act' >> actionSucceeds (handleEditRequest act') a
                -- The actual generated names don't really matter for next actions
                -- except for shadowing/capture? Do we reject any names?
                -- NB: the action-continuation tends to be a ChooseOrEnterName, and if we decide to enter a "bad" name, we may still get a NameCapture error.
                -- TODO: I am not sure how to write this test to account for this slim chance of failure.
                AskQuestion (GenerateName def' i tk) a' -> do
                  label "GenerateName (recurses)"
                  -- TODO: this is a horrible hack...
                  let names = case lookupASTDef def' (snd <$> allDefs) <&> \def'' ->
                         (focusOnTy i (astDefType def''), focusOn i (astDefExpr def'')) of
                        Nothing -> error "invalid GenerateName: no such def"
                        Just (Nothing, Nothing) -> error "invalid GenerateName: no such id"
                        Just (Just tz, _) -> runReader (generateNameTy tk tz) $ buildTypingContextFromModules (progAllModules $ appProg a) (progSmartHoles $ appProg a)
                        Just (_, Just loc) -> runReader (generateNameExpr tk $ locToEither loc) $ buildTypingContextFromModules (progAllModules $ appProg a) (progSmartHoles $ appProg a)
                  checkActionInput $ a' names
                _ -> error "VariablesInScope question is never an offered action"
          checkActionInput $ input action
  where
--    actionSucceeds :: HasCallStack => EditAppM a -> App -> PropertyT WT ()
{-    actionSucceeds m a = case runEditAppM m a of
      (Left err, a') -> annotateShow err >> annotateShow a' >> failure
      (Right _, _) -> pure ()
-}
    actionSucceeds m a = case evalTestM 99999 $ runStateT (runExceptT m) a of
      (Left err, a') -> annotateShow err >> annotateShow a' >> failure
      (Right _, _) -> pure ()

    -- If we submit our own name rather than an offered one, then
    -- we should expect that name capture/clashing may happen
--    actionSucceedsOrCapture :: HasCallStack => EditAppM a -> App -> PropertyT WT ()
    actionSucceedsOrCapture m a = pure ()
{-
    actionSucceedsOrCapture m a = case runEditAppM m a of
      (Left (ActionError NameCapture), _) -> do
        label "name-capture with entered name"
        annotate "ignoring name capture error as was generated name, not offered one"
      (Left (ActionError (CaseBindsClash{})), _) -> do
        label "name-clash with entered name"
        annotate "ignoring name clash error as was generated name, not offered one"
      (Left err, _) -> annotateShow err >> failure
      (Right _, _) -> pure ()
-}
    globalNameToQualifiedText n = (fmap unName $ unModuleName $ qualifiedModule n, unName $ baseName n)
