module Tests.AlphaEquality where

import Foreword

import Hedgehog hiding (Property, check, property)
import Primer.Builtins
import Primer.Core (
  Type', Expr,
 )
import Primer.Core.DSL
import Primer.Core.Utils (alphaEqTy, forgetTypeMetadata, alphaEq, forgetMetadata)
import Primer.Gen.Core.Raw (
  evalExprGen,
  genTyVarName,
  genType,
 )
import Tasty (Property, property)
import Test.Tasty.HUnit hiding (assert)

unit_1 :: Assertion
unit_1 =
  assertNotEqual
    (create_ (tcon tNat))
    (create_ (tcon tBool))

unit_2 :: Assertion
unit_2 =
  (@?=)
    (create_ (tcon tList `tapp` tcon tNat))
    (create_ (tcon tList `tapp` tcon tNat))

unit_3 :: Assertion
unit_3 =
  assertNotEqual
    (create_ (tcon tList `tapp` tcon tBool))
    (create_ (tcon tList `tapp` tcon tNat))

unit_4 :: Assertion
unit_4 =
  assertNotEqual
    (create_ (tcon tList `tapp` tcon tBool))
    (create_ (tcon tNat))

unit_5 :: Assertion
unit_5 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tcon tList `tapp` tvar "a"))
    (create_ (tcon tNat))

unit_6 :: Assertion
unit_6 =
  (@?=)
    (create_ (tforall "a" ktype $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "b" ktype $ tcon tList `tapp` tvar "b"))

unit_7 :: Assertion
unit_7 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "b" ktype $ tcon tList `tapp` tcon tBool))

unit_8 :: Assertion
unit_8 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tcon tBool))
    (create_ (tforall "b" (kfun ktype ktype) $ tcon tBool))

unit_9 :: Assertion
unit_9 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tforall "b" ktype $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "a" ktype $ tforall "b" ktype $ tcon tList `tapp` tvar "b"))

unit_10 :: Assertion
unit_10 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tcon tList `tapp` tvar "a"))
    (create_ (tcon tList `tapp` tforall "a" ktype (tvar "b")))

unit_11 :: Assertion
unit_11 =
  assertNotEqual
    (create_ (tforall "a" ktype $ tcon tBool `tfun` (tcon tList `tapp` tvar "a")))
    (create_ (tcon tBool `tfun` tforall "a" ktype (tcon tList `tapp` tvar "a")))

unit_repeated_names :: Assertion
unit_repeated_names =
  create_ (tforall "b" ktype (tforall "foo" ktype (tforall "x" ktype $ tvar "x")))
    @?= create_ (tforall "foo" ktype (tforall "foo" ktype (tforall "x" ktype $ tvar "x")))

tasty_refl :: Property
tasty_refl = property $ do
  t <- forgetTypeMetadata <$> forAll (evalExprGen 0 genType)
  assert $ alphaEqTy t t

tasty_alpha :: Property
tasty_alpha = property $ do
  s <- f <$> forAll (evalExprGen 0 genTyVarName)
  t <- f <$> forAll (evalExprGen 0 genTyVarName)
  s === t
  where
    f v = create_ $ tforall v ktype $ tvar v

unit_tm_1 :: Assertion
unit_tm_1 = alphaNotEqTm (con0 cTrue) (con0 cFalse)

unit_tm_2 :: Assertion
unit_tm_2 = alphaEqTm (con cCons [con0 cTrue, con0 cNil]) (con cCons [con0 cTrue, con0 cNil])

unit_tm_3 :: Assertion
unit_tm_3 = alphaNotEqTm (con cCons [con0 cFalse, con0 cNil]) (con cCons [con0 cTrue, con0 cNil])

unit_tm_4 :: Assertion
unit_tm_4 = alphaNotEqTm (con cCons [con0 cFalse, con0 cNil]) (con0 cTrue)

unit_tm_5 :: Assertion
unit_tm_5 = alphaNotEqTm (lam "x" $ con0 cTrue) (con0 cTrue)

unit_tm_6 :: Assertion
unit_tm_6 = alphaEqTm (lam "x" $ lvar "x") (lam "y" $ lvar "y")

unit_tm_7 :: Assertion
unit_tm_7 = alphaNotEqTm (lam "x" $ lvar "x") (lam "y" $ con0 cTrue)

unit_tm_8 :: Assertion
unit_tm_8 = alphaNotEqTm (lAM "x" emptyHole) (lam "y" emptyHole)

unit_tm_9 :: Assertion
unit_tm_9 = alphaNotEqTm (lam "x" $ lam "y" $ lvar "x") (lam "x" $ lam "y" $ lvar "y")

unit_tm_10 :: Assertion
unit_tm_10 = alphaNotEqTm (lam "x" $ con1 cJust $ lvar "x") (con1 cJust $ lam "x" $ lvar "x")

unit_tm_11 :: Assertion
unit_tm_11 = alphaNotEqTm (lam "x" $ lvar "x" `app` con0 cTrue) (lam "x" (lvar "x") `app` con0 cTrue)

unit_tm_repeated_names :: Assertion
unit_tm_repeated_names = alphaEqTm (lam "a" $ lam "b" $ lvar "x" `app` lvar "x") (lam "a" $ lam "a" $ lvar "x" `app` lvar "x")


unit_tm_tmp :: Assertion
unit_tm_tmp = alphaEqTm
  (lAM "x" $ lAM "y" $ lam "x" $ hole $ case_ emptyHole [branch cTrue [("x",Nothing)] emptyHole])
  (lAM "x" $ lAM "y" $ lam "x0" $ hole $ case_ emptyHole [branch cTrue [("x1",Nothing)] emptyHole])

create_ :: S (Type' a b) -> Alpha
create_ = Alpha . forgetTypeMetadata . create'

-- | Like @Type' ()@, but 'Eq' only compares up to alpha-equality.
newtype Alpha = Alpha (Type' () ())
  deriving stock (Show)

instance Eq Alpha where
  (Alpha x) == (Alpha y) = x `alphaEqTy` y

assertNotEqual :: Alpha -> Alpha -> Assertion
assertNotEqual s t = assertBool "types are equal" $ s /= t

alphaEqTm :: S Expr -> S Expr -> Assertion
alphaEqTm s t = assertBool "terms should be equal" $ alphaEq (forgetMetadata $ create' s) (forgetMetadata $ create' t)

alphaNotEqTm :: S Expr -> S Expr -> Assertion
alphaNotEqTm s t = assertBool "terms should not be equal" $ not $ alphaEq (forgetMetadata $ create' s) (forgetMetadata $ create' t)
