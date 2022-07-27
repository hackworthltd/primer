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
import Primer.Pretty (PrettyOptions, compact, prettyExpr, prettyType, sparse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

test_examples :: TestTree
test_examples = testGroup "Examples" $ map (prettyTestGroup [exprHandlerSparse, exprHandlerCompact, typeHandlerSparse, typeHandlerCompact]) [comprehensive, not]

type PrettyTestHandler = (ASTDef -> Doc AnsiStyle, Text)

exprHandler :: PrettyOptions -> Text -> PrettyTestHandler
exprHandler opts name = (prettyExpr opts . astDefExpr, name)

typeHandler :: PrettyOptions -> Text -> PrettyTestHandler
typeHandler opts name = (prettyType opts . astDefType, name)

exprHandlerSparse :: PrettyTestHandler
exprHandlerSparse = exprHandler sparse "Expr (Sparse)"

exprHandlerCompact :: PrettyTestHandler
exprHandlerCompact = exprHandler compact "Expr (Compact)"

typeHandlerSparse :: PrettyTestHandler
typeHandlerSparse = typeHandler sparse "Type (Sparse)"

typeHandlerCompact :: PrettyTestHandler
typeHandlerCompact = typeHandler compact "Type (Compact)"

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
