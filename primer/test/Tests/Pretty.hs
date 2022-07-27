module Tests.Pretty where

import Foreword hiding (not)

import Data.ByteString.Lazy qualified as BS
import Data.String
import Data.Text qualified as T
import Prettyprinter (defaultLayoutOptions, layoutSmart, line)
import Prettyprinter.Internal.Type (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import Primer.Core (ASTDef (..), Def (..), GVarName, GlobalName (baseName), ModuleName, mkSimpleModuleName)
import Primer.Core.DSL (S, create')
import Primer.Examples (comprehensive, not)
import Primer.Name (unName)
import Primer.Pretty (PrettyOptions (optionSetName), compact, prettyExpr, prettyType, sparse)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_examples :: TestTree
test_examples = prettyTestMultiple "Examples" [comprehensive, not] [sparse, compact]

prettyTestMultiple :: TestName -> [ModuleName -> S (GVarName, Def)] -> [PrettyOptions] -> TestTree
prettyTestMultiple name x options = testGroup name $ map (prettyTestGroup handlers) x
  where
    handlers = exprhandlers ++ typehandlers
    exprhandlers = map exprHandler options
    typehandlers = map typeHandler options

type PrettyTestHandler = (ASTDef -> Doc AnsiStyle, Text)

exprHandler :: PrettyOptions -> PrettyTestHandler
exprHandler opts = (prettyExpr opts . astDefExpr, "Expr (" <> optionSetName opts <> ")")

typeHandler :: PrettyOptions -> PrettyTestHandler
typeHandler opts = (prettyType opts . astDefType, "Type (" <> optionSetName opts <> ")")

prettyTest :: String -> Def -> PrettyTestHandler -> TestTree
prettyTest name def handler = goldenVsString hname path bs
  where
    hname = T.unpack (snd handler)
    path = "test/outputs/Pretty/" ++ name ++ "/" ++ hname ++ ".ansi"
    bs = case def of
      DefPrim _ -> exitFailure
      DefAST e -> docToBS (fst handler e)

prettyTestGroup :: [PrettyTestHandler] -> (ModuleName -> S (GVarName, Def)) -> TestTree
prettyTestGroup handlers x = testGroup dname (map (prettyTest dname d) handlers)
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
    . (<> line)
