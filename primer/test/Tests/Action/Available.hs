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
  GVarName,
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
import Primer.Core.Utils (
  exprIDs,
 )
import Primer.Examples (comprehensive)
import Primer.Name (Name (unName))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit ()
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
  let d = (defName, def)
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
   in testGroup testName $
        enumerate
          <&> \level ->
            let defActions = name <$> actionsForDef level mempty d
                bodyActions =
                  fmap
                    ( \id ->
                        ( id
                        , name <$> actionsForDefBody level defName id (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  fmap
                    ( \id ->
                        ( id
                        , name <$> actionsForDefSig level defName id (astDefType def)
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
