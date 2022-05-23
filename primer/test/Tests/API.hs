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
  distinctTreeExpr (con' ["M"] "C") (con' ["M"] "D")

unit_viewTreeExpr_injective_lam :: Assertion
unit_viewTreeExpr_injective_lam =
  distinctTreeExpr (lam "x" emptyHole) (lam "y" emptyHole)

unit_viewTreeExpr_injective_LAM :: Assertion
unit_viewTreeExpr_injective_LAM =
  distinctTreeExpr (lAM "x" emptyHole) (lAM "y" emptyHole)

unit_viewTreeExpr_injective_var :: Assertion
unit_viewTreeExpr_injective_var =
  distinctTreeExpr (lvar "x") (lvar "y")

unit_viewTreeExpr_injective_globalvar :: Assertion
unit_viewTreeExpr_injective_globalvar =
  distinctTreeExpr (gvar' ["M"] "0") (gvar' ["M"] "1")

-- When we changed how references were handled so 'Expr' had one constructor
-- that handled both local and global variable references, there was a
-- regression where they both rendered identically.
-- This is a regression test for said issue (which occurred before
-- global variables had a qualified name).
unit_viewTreeExpr_injective_locglobvar :: Assertion
unit_viewTreeExpr_injective_locglobvar =
  distinctTreeExpr (lvar "x") (gvar' ["M"] "x")

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
  distinctTreeExpr (case_ emptyHole [branch' (["M"], "C") [("x", Nothing)] emptyHole]) (case_ emptyHole [branch' (["M"], "D") [("x", Nothing)] emptyHole])

unit_viewTreeExpr_injective_case_paramName :: Assertion
unit_viewTreeExpr_injective_case_paramName =
  distinctTreeExpr (case_ emptyHole [branch' (["M"], "C") [("x", Nothing)] emptyHole]) (case_ emptyHole [branch' (["M"], "C") [("y", Nothing)] emptyHole])

unit_viewTreeType_injective_con :: Assertion
unit_viewTreeType_injective_con =
  distinctTreeType (tcon' ["M"] "T") (tcon' ["M"] "S")

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
  let t1 = viewTreeExpr $ create' e1
      t2 = viewTreeExpr $ create' e2
   in assertBool ("non-injective viewTreeExpr: " ++ show t1) (t1 /= t2)

distinctTreeType :: S Type -> S Type -> Assertion
distinctTreeType e1 e2 =
  let t1 = viewTreeType $ create' e1
      t2 = viewTreeType $ create' e2
   in assertBool ("non-injective viewTreeType: " ++ show t1) (t1 /= t2)
