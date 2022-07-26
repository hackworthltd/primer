module Tests.Pretty where

import Foreword hiding (not)

import Data.ByteString.Lazy qualified as BS
import Data.String
import Data.Text qualified as T
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Primer.Core (ASTDef (..), Def (..), GVarName, GlobalName (baseName), ModuleName, mkSimpleModuleName)
import Primer.Core.DSL (S, create')
import Primer.Examples (comprehensive, not)
import Primer.Name (unName)
import Primer.Pretty (defaultPrettyOptions, prettyExpr, prettyType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_examples :: TestTree
test_examples = testGroup "Examples" $ map prettyTestGroup [comprehensive, not]

type PrettyTestHandler = (ASTDef -> Doc AnsiStyle, Text)

exprHandler :: PrettyTestHandler
exprHandler = (prettyExpr defaultPrettyOptions . astDefExpr, "Expr")

typeHandler :: PrettyTestHandler
typeHandler = (prettyType defaultPrettyOptions . astDefType, "Type")

prettyTest :: String -> Def -> PrettyTestHandler -> TestTree
prettyTest name def handler = goldenVsString hname path bs
  where
    hname = T.unpack (snd handler)
    path = "test/outputs/Pretty/" ++ name ++ "/" ++ hname
    bs = case def of
      DefPrim _ -> exitFailure
      DefAST e -> docToBS (fst handler e)

prettyTestGroup :: (ModuleName -> S (GVarName, Def)) -> TestTree
prettyTestGroup x = testGroup dname (map (prettyTest dname d) [exprHandler, typeHandler])
  where
    (n, d) = create' $ x $ mkSimpleModuleName "Module"
    dname = T.unpack . unName . baseName $ n

docToBS :: Doc AnsiStyle -> IO BS.ByteString
docToBS =
  pure
    . BS.fromStrict
    . encodeUtf8
    . renderStrict
    . layoutSmart defaultLayoutOptions
