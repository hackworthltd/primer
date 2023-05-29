-- | Tests for 'Primer.Zipper.bindersAbove'.
module Tests.Zipper.BindersAbove where

import Foreword

import Data.Set qualified as S
import Primer.Action (
  BranchMove (..),
  Movement (..),
  moveExpr,
 )
import Primer.Builtins (cSucc, cZero, tNat)
import Primer.Core (
  Expr,
  Pattern (PatCon),
 )
import Primer.Core.DSL
import Primer.Name (Name)
import Primer.Test.TestM (evalTestM)
import Primer.Typecheck (SmartHoles (NoSmartHoles), initialCxt)
import Primer.Zipper (bindersAbove, focus)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

unit_1 :: Assertion
unit_1 = bindersAboveTest emptyHole [] mempty

unit_2 :: Assertion
unit_2 =
  bindersAboveTest (lam "x" $ lam "y" emptyHole) [Child1] $ S.fromList ["x"]

unit_3 :: Assertion
unit_3 =
  bindersAboveTest
    (let_ "x" (lam "y" emptyHole) (lam "z" emptyHole))
    [Child1, Child1]
    (S.fromList ["y"])

unit_4 :: Assertion
unit_4 =
  bindersAboveTest
    (let_ "x" (lam "y" emptyHole) (lam "z" emptyHole))
    [Child2, Child1]
    (S.fromList ["x", "z"])

unit_5 :: Assertion
unit_5 =
  bindersAboveTest
    (letrec "x" (lam "y" emptyHole) (tcon tNat) (lam "z" emptyHole))
    [Child1, Child1]
    (S.fromList ["x", "y"])

unit_6 :: Assertion
unit_6 =
  bindersAboveTest
    (letrec "x" (lam "y" emptyHole) (tcon tNat) (lam "z" emptyHole))
    [Child2, Child1]
    (S.fromList ["x", "z"])

unit_7 :: Assertion
unit_7 =
  bindersAboveTest
    (case_ (lvar "x") [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole])
    []
    (S.fromList [])

unit_8 :: Assertion
unit_8 =
  bindersAboveTest
    (case_ (lvar "x") [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole])
    [Child1]
    (S.fromList [])

unit_9 :: Assertion
unit_9 =
  bindersAboveTest
    (case_ (lvar "x") [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole])
    [Branch $ Pattern $ PatCon cZero]
    (S.fromList [])

unit_10 :: Assertion
unit_10 =
  bindersAboveTest
    (case_ (lvar "x") [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole])
    [Branch $ Pattern $ PatCon cSucc]
    (S.fromList ["n"])

-- * Helpers

bindersAboveTest :: S Expr -> [Movement] -> S.Set Name -> Assertion
bindersAboveTest expr path expected =
  case evalTestM (i + 1) $ runExceptT $ runReaderT (foldlM (flip moveExpr) (focus e) path) (initialCxt NoSmartHoles) of
    Left err -> assertFailure $ show err
    Right z -> bindersAbove z @?= expected
  where
    (e, i) = create expr
