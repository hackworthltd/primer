module Tests.Action.Available where

import Foreword

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List.Extra (enumerate)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import GHC.Err (error)
import Optics (toListOf, (%))
import Primer.Action (ActionName (..), OfferedAction (description, name))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.App (Editable (Editable))
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
 )
import Primer.Def (
  ASTDef (..),
  Def (DefAST, DefPrim),
 )
import Primer.Examples (comprehensiveWellTyped)
import Primer.Module (
  Module (Module, moduleDefs),
  builtinModule,
  moduleTypesQualified,
  primitiveModule,
 )
import Primer.Name (Name (unName))
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles),
  buildTypingContextFromModules,
  checkEverything,
 )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))
import Tests.Typecheck (runTypecheckTestMIn)
import Text.Pretty.Simple (pShowNoColor)

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
