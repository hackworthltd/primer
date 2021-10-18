module Tests.API where

import Foreword

import Gen.Core.Raw (evalExprGen, genExpr, genType)
import Hedgehog
import Primer.API (viewTreeExpr, viewTreeType)

hprop_viewTreeExpr_injective :: Property
hprop_viewTreeExpr_injective = property $ do
  e1 <- forAll $ evalExprGen 0 genExpr
  e2 <- forAll $ evalExprGen 0 genExpr
  when (e1 == e2) discard
  viewTreeExpr e1 /== viewTreeExpr e2

hprop_viewTreeType_injective :: Property
hprop_viewTreeType_injective = property $ do
  t1 <- forAll $ evalExprGen 0 genType
  t2 <- forAll $ evalExprGen 0 genType
  when (t1 == t2) discard
  viewTreeType t1 /== viewTreeType t2
