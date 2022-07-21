module Tests.Pretty where

import Foreword

import Data.ByteString.Lazy qualified as BS
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Primer.Core (ASTDef (..), Def (..), mkSimpleModuleName)
import Primer.Core.DSL (create')
import Primer.Examples (comprehensive)
import Primer.Pretty (defaultPrettyOptions, prettyExpr, prettyType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_comp :: TestTree
test_comp = testGroup "Comprehensive" [checkExpr, checkType]

checkExpr :: TestTree
checkExpr = goldenVsString "Expr" "test/outputs/Pretty/Expr" $ do
  case snd $ create' $ comprehensive $ mkSimpleModuleName "Module" of
    DefPrim _ -> exitFailure
    DefAST e -> docToBS (prettyExpr defaultPrettyOptions $ astDefExpr e)

checkType :: TestTree
checkType = goldenVsString "Type" "test/outputs/Pretty/Type" $ do
  case create' $ fmap snd $ comprehensive $ mkSimpleModuleName "Module" of
    DefPrim _ -> exitFailure
    DefAST e -> docToBS (prettyType defaultPrettyOptions $ astDefType e)

docToBS :: Doc AnsiStyle -> IO BS.ByteString
docToBS =
  pure
    . BS.fromStrict
    . encodeUtf8
    . renderStrict
    . layoutSmart defaultLayoutOptions

createComp :: Def
createComp = snd $ create' $ comprehensive $ mkSimpleModuleName "Module"