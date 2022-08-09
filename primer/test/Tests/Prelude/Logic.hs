{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.Prelude.Logic where

import Foreword hiding (exp)

import Primer.Builtins (builtinModule)
import Primer.Builtins.DSL (bool_)
import Primer.Core (Expr, GVarName)
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

unit_and_correct :: Assertion
unit_and_correct = binaryCorrect P.and (&&)

unit_or_correct :: Assertion
unit_or_correct = binaryCorrect P.or (||)

unit_xor_correct :: Assertion
unit_xor_correct = binaryCorrect P.xor xor

unit_implies_correct :: Assertion
unit_implies_correct = binaryCorrect P.implies haskImplies

haskImplies :: Bool -> Bool -> Bool
haskImplies True True = True
haskImplies True False = False
haskImplies False False = True
haskImplies False True = True

binaryCorrect :: GVarName -> (Bool -> Bool -> Bool) -> Assertion
binaryCorrect name func = traverse_ (testBinary name func) [(x, y) | x <- [True, False], y <- [True, False]]

testBinary :: GVarName -> (Bool -> Bool -> Bool) -> (Bool, Bool) -> Assertion
testBinary name func (b1, b2) =
  let haskeval :: Expr
      haskeval = create' $ bool_ $ func b1 b2
      preleval = evalTestM 0 $ do
        x <- (gvar name `app` bool_ b1) `app` bool_ b2
        evalFull ty def n d x
   in preleval <~==> Right haskeval
  where
    ty = moduleTypesQualified builtinModule <> moduleTypesQualified prelude'
    def = moduleDefsQualified builtinModule <> moduleDefsQualified prelude'
    n = 50
    d = Chk
    prelude' = create' prelude
