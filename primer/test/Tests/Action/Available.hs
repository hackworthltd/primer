module Tests.Action.Available where

import Foreword

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.Extra (enumerate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Err (error)
import Optics (toListOf, (%))
import Primer.Action (ActionName (..), OfferedAction (name))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.Core (
  ASTDef (..),
  Def (DefAST, DefPrim),
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  mkSimpleModuleName,
  moduleNamePretty,
  _typeMeta,
 )
import Primer.Core.DSL (
  create',
 )
import Primer.Examples (comprehensive)
import Primer.Name (Name (unName))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit ()
import TestUtils (exprIDs)
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
mkTests :: Def -> TestTree
mkTests (DefPrim _) = error "mkTests is unimplemented for primitive definitions."
mkTests (DefAST def) =
  let defName = astDefName def
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
   in testGroup testName $
        enumerate
          <&> \level ->
            let defActions = map name $ actionsForDef level mempty def
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefBody level def id (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefSig level def id (astDefType def)
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
