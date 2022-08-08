{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.Prelude.Logic where

import Foreword hiding (exp)

import Primer.Builtins (builtinModule)
import Primer.Builtins.DSL (bool_)
import Primer.Core (Expr)
import Primer.Core.DSL (app, create', gvar)
import Primer.EvalFull (Dir (Chk), evalFull)
import Primer.Module (moduleDefsQualified, moduleTypesQualified)
import Primer.Prelude (prelude)
import Primer.Prelude.Logic qualified as P
import Test.Tasty.HUnit (Assertion)
import TestM (evalTestM)
import Tests.EvalFull ((<~==>))

unit_not_correct :: Assertion
unit_not_correct = testNot True <> testNot False

testNot :: Bool -> Assertion
testNot b =
  let hnoteval :: Expr
      hnoteval = create' $ bool_ $ not b
      mynoteval = evalTestM 0 $ do
        x <- gvar P.not `app` bool_ b
        evalFull ty def n d x
   in mynoteval <~==> Right hnoteval
  where
    ty = moduleTypesQualified builtinModule <> moduleTypesQualified prelude'
    def = moduleDefsQualified builtinModule <> moduleDefsQualified prelude'
    n = 7
    d = Chk
    prelude' = create' prelude