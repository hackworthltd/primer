module Tests.API where

import Foreword

import Gen.Core.Raw (evalExprGen, genExpr, genType)
import Hedgehog
import Primer.API (viewTreeExpr, viewTreeType)
import Primer.Core.DSL
import Test.Tasty.HUnit

import Primer.Core

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

-- regression tests to check we encode names into the tree

unit_viewTreeExpr_injective_con :: Assertion
unit_viewTreeExpr_injective_con =
  distinctTreeExpr (con "C") (con "D")

unit_viewTreeExpr_injective_lam :: Assertion
unit_viewTreeExpr_injective_lam =
  distinctTreeExpr (lam "x" emptyHole) (lam "y" emptyHole)

unit_viewTreeExpr_injective_LAM :: Assertion
unit_viewTreeExpr_injective_LAM =
  distinctTreeExpr (lAM "x" emptyHole) (lAM "y" emptyHole)

unit_viewTreeExpr_injective_var :: Assertion
unit_viewTreeExpr_injective_var =
  distinctTreeExpr (var "x") (var "y")

unit_viewTreeExpr_injective_globalvar :: Assertion
unit_viewTreeExpr_injective_globalvar =
  distinctTreeExpr (global 0) (global 1)

unit_viewTreeExpr_injective_let :: Assertion
unit_viewTreeExpr_injective_let =
  distinctTreeExpr (let_ "x" emptyHole emptyHole) (let_ "y" emptyHole emptyHole)

unit_viewTreeExpr_injective_lettype :: Assertion
unit_viewTreeExpr_injective_lettype =
  distinctTreeExpr (letType "x" tEmptyHole emptyHole) (letType "y" tEmptyHole emptyHole)

unit_viewTreeExpr_injective_letrec :: Assertion
unit_viewTreeExpr_injective_letrec =
  distinctTreeExpr (letrec "x" emptyHole tEmptyHole emptyHole) (letrec "y" emptyHole tEmptyHole emptyHole)

unit_viewTreeExpr_injective_case_conName :: Assertion
unit_viewTreeExpr_injective_case_conName =
  distinctTreeExpr (case_ emptyHole [branch "C" [("x", Nothing)] emptyHole]) (case_ emptyHole [branch "D" [("x", Nothing)] emptyHole])

unit_viewTreeExpr_injective_case_paramName :: Assertion
unit_viewTreeExpr_injective_case_paramName =
  distinctTreeExpr (case_ emptyHole [branch "C" [("x", Nothing)] emptyHole]) (case_ emptyHole [branch "C" [("y", Nothing)] emptyHole])

unit_viewTreeType_injective_con :: Assertion
unit_viewTreeType_injective_con =
  distinctTreeType (tcon "T") (tcon "S")

unit_viewTreeType_injective_var :: Assertion
unit_viewTreeType_injective_var =
  distinctTreeType (tvar "a") (tvar "b")

unit_viewTreeType_injective_forall_param :: Assertion
unit_viewTreeType_injective_forall_param =
  distinctTreeType (tforall "a" KType tEmptyHole) (tforall "b" KType tEmptyHole)

unit_viewTreeType_injective_forall_kind :: Assertion
unit_viewTreeType_injective_forall_kind =
  distinctTreeType (tforall "a" KType tEmptyHole) (tforall "a" KHole tEmptyHole)

distinctTreeExpr :: S Expr -> S Expr -> Assertion
distinctTreeExpr e1 e2 =
  let t1 = viewTreeExpr $ fst $ create e1
      t2 = viewTreeExpr $ fst $ create e2
   in assertBool ("non-injective viewTreeExpr: " ++ show t1) (t1 /= t2)

distinctTreeType :: S Type -> S Type -> Assertion
distinctTreeType e1 e2 =
  let t1 = viewTreeType $ fst $ create e1
      t2 = viewTreeType $ fst $ create e2
   in assertBool ("non-injective viewTreeType: " ++ show t1) (t1 /= t2)
