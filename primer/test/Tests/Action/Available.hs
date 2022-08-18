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
import Primer.Examples (comprehensive)
import Primer.Name (Name (unName))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, (@?=))
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
   in testGroup testName $
        enumerate
          <&> \level ->
            let defActions = map name $ actionsForDef level (Map.singleton defName $ DefAST def) d
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefBody level defName id (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefSig level defName id (astDefType def)
                        )
                    )
                    . toListOf (_typeMeta % _id)
                    $ astDefType def
             in goldenVsString (show level) ("test/outputs/available-actions" </> testName </> show level <> ".fragment") $
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
