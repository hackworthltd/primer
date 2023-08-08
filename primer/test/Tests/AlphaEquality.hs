module Tests.AlphaEquality where

import Foreword

import Hedgehog hiding (Property, check, property)
import Primer.Builtins
import Primer.Core (
  Kind' (KFun, KType),
  Type',
 )
import Primer.Core.DSL
import Primer.Core.Utils (alphaEqTy, forgetTypeMetadata)
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
    (create_ (tforall "a" (KType ()) $ tcon tList `tapp` tvar "a"))
    (create_ (tcon tNat))

unit_6 :: Assertion
unit_6 =
  (@?=)
    (create_ (tforall "a" (KType ()) $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "b" (KType ()) $ tcon tList `tapp` tvar "b"))

unit_7 :: Assertion
unit_7 =
  assertNotEqual
    (create_ (tforall "a" (KType ()) $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "b" (KType ()) $ tcon tList `tapp` tcon tBool))

unit_8 :: Assertion
unit_8 =
  assertNotEqual
    (create_ (tforall "a" (KType ()) $ tcon tBool))
    (create_ (tforall "b" (KFun () (KType ()) (KType ())) $ tcon tBool))

unit_9 :: Assertion
unit_9 =
  assertNotEqual
    (create_ (tforall "a" (KType ()) $ tforall "b" (KType ()) $ tcon tList `tapp` tvar "a"))
    (create_ (tforall "a" (KType ()) $ tforall "b" (KType ()) $ tcon tList `tapp` tvar "b"))

unit_10 :: Assertion
unit_10 =
  assertNotEqual
    (create_ (tforall "a" (KType ()) $ tcon tList `tapp` tvar "a"))
    (create_ (tcon tList `tapp` tforall "a" (KType ()) (tvar "b")))

unit_11 :: Assertion
unit_11 =
  assertNotEqual
    (create_ (tforall "a" (KType ()) $ tcon tBool `tfun` (tcon tList `tapp` tvar "a")))
    (create_ (tcon tBool `tfun` tforall "a" (KType ()) (tcon tList `tapp` tvar "a")))

unit_repeated_names :: Assertion
unit_repeated_names =
  create_ (tforall "b" (KType ()) (tforall "foo" (KType ()) (tforall "x" (KType ()) $ tvar "x")))
    @?= create_ (tforall "foo" (KType ()) (tforall "foo" (KType ()) (tforall "x" (KType ()) $ tvar "x")))

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
    f v = create_ $ tforall v (KType ()) $ tvar v

create_ :: S (Type' a) -> Alpha
create_ = Alpha . forgetTypeMetadata . create'

-- | Like @Type' ()@, but 'Eq' only compares up to alpha-equality.
newtype Alpha = Alpha (Type' ())
  deriving stock (Show)

instance Eq Alpha where
  (Alpha x) == (Alpha y) = x `alphaEqTy` y

assertNotEqual :: Alpha -> Alpha -> Assertion
assertNotEqual s t = assertBool "types are equal" $ s /= t
