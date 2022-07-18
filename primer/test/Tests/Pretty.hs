module Tests.Pretty where

import Foreword

import Data.ByteString.Lazy qualified as BS
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Terminal (renderStrict)
import Primer.Core (ASTDef (..), Def (..), mkSimpleModuleName)
import Primer.Core.DSL (create')
import Primer.Examples (comprehensive)
import Primer.Pretty (defaultPrettyOptions, prettyExpr)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

test_1 :: TestTree
test_1 = goldenVsString "pretty" "test/outputs/pretty" $ do
  case snd $ create' $ comprehensive $ mkSimpleModuleName "Module" of
    DefPrim _ -> exitFailure
    DefAST e ->
      pure
        . BS.fromStrict
        . encodeUtf8
        . renderStrict
        . layoutSmart defaultLayoutOptions
        . prettyExpr defaultPrettyOptions
        $ astDefExpr e
