module Tests.Action.Available where

import Foreword

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Gen.App (genApp)
import Gen.Core.Typed (WT, forAllT, propertyWT)
import Hedgehog (PropertyT, annotateShow, discard, failure)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllWithT)
import Optics (toListOf, (%))
import Primer.Action (ActionInput (..), ActionName (..), OfferedAction (..))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.App (App, EditAppM, Prog (..), appProg, handleEditRequest, runEditAppM, progAllModules, progAllDefs)
import Primer.Core (
  ASTDef (..),
  Def (DefAST, DefPrim),
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
 )
import Primer.Examples (comprehensive)
import Primer.Module (moduleDefsQualified)
import Primer.Name (Name (unName))
import Primer.Typecheck (SmartHoles (NoSmartHoles,SmartHoles))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))
import TestUtils (Property, withDiscards, withTests)
import Text.Pretty.Simple (pShowNoColor)

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
                        , map name $ actionsForDefBody level defName mut id (astDefExpr def)
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
        pure (foo, Map.fromList ds)
   in for_
        enumerate
        ( \l ->
            description <$> actionsForDef l defs d
              @?= ["Rename this definition", "Duplicate this definition"]
        )

-- TODO/REVIEW: how to ensure this is kept up to date with changes in action offerings
-- we have "RenameCon" actions - these are not advertised yet (and presumably should be?)
-- similarly, eval , questions etc
tasty_available_actions_accepted :: Property
tasty_available_actions_accepted = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      l <- forAllT $ Gen.element enumerate
      sh <- forAllT $ Gen.element [NoSmartHoles, SmartHoles]
      a <- forAllT $ genApp sh [] -- [builtinModule, primitiveModule] -- TODO: consider bigger context
      let allDefs = fmap snd $ progAllDefs $ appProg a
      (defName, def') <- case Map.toList allDefs of
        [] -> discard
        ds -> forAllT $ Gen.element ds
      -- TODO: should test primitives also (i.e. they should have no? actions)
      _ <- case def' of
        DefAST d -> pure d
        _ -> discard
      -- TODO: other sorts of action... actionsForDef{,Body,Sig}
      act <- forAllWithT name' $ Gen.element $ actionsForDef l allDefs defName
      case input act of
        --        InputRequired a' -> _
        NoInputRequired act' -> annotateShow act' >> actionSucceeds (handleEditRequest act') a
        --        AskQuestion q a' -> _
        _ -> discard -- TODO: care about this!
        {-
              i <- forAllT $ Gen.element $ t ^.. exprIDs
              a <- forAllWithT name' $ Gen.element $ actionsForDefBody l n i t
        -}
  where
    name' a = toS $ case name a of
      Code t -> t
      Prose t -> t
    actionSucceeds :: HasCallStack => EditAppM a -> App -> PropertyT WT ()
    actionSucceeds m a = case runEditAppM m a of
      (Left err, _) -> annotateShow err >> failure
      (Right _, _) -> pure ()
