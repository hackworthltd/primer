module Tests.Pretty where

import Foreword

import Data.ByteString.Lazy qualified as BS
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Primer.Core (ASTDef (..), Def (..), mkSimpleModuleName)
import Primer.Core.DSL (S, create')
import Primer.Examples (comprehensive)
import Primer.Pretty (defaultPrettyOptions, prettyExpr, prettyType)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_comp :: TestTree
test_comp = testGroup "Comprehensive" [compExpr, compType]

compExpr :: TestTree
compExpr = exprTest "Expr" "test/outputs/Pretty/Expr" comp

compType :: TestTree
compType = typeTest "Type" "test/outputs/Pretty/Type" comp

comp :: S Def
comp = fmap snd $ comprehensive $ mkSimpleModuleName "Module"

typeTest :: TestName -> FilePath -> S Def -> TestTree
typeTest name path d = goldenVsString name path $ do
  case create' d of
    DefPrim _ -> exitFailure
    DefAST e -> docToBS (prettyType defaultPrettyOptions $ astDefType e)

exprTest :: TestName -> FilePath -> S Def -> TestTree
exprTest name path d = goldenVsString name path $ do
  case create' d of
    DefPrim _ -> exitFailure
    DefAST e -> docToBS (prettyExpr defaultPrettyOptions $ astDefExpr e)

docToBS :: Doc AnsiStyle -> IO BS.ByteString
docToBS =
  pure
    . BS.fromStrict
    . encodeUtf8
    . renderStrict
    . layoutSmart defaultLayoutOptions

createComp :: Def
createComp = snd $ create' $ comprehensive $ mkSimpleModuleName "Module"