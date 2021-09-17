module Tests.AlphaEquality where

import Gen.Core.Raw
  ( evalExprGen,
    genName,
    genType,
  )
import Hedgehog hiding (check)
import Optics (set)
import Primer.Core
  ( Kind (KFun, KType),
    Type',
    _typeMeta,
  )
import Primer.Core.DSL
import Primer.Subst
import Test.Tasty.HUnit hiding (assert)

unit_1 :: Assertion
unit_1 =
  assertNotEqual
    (create' (tcon "Nat"))
    (create' (tcon "Bool"))

unit_2 :: Assertion
unit_2 =
  (@?=)
    (create' (tcon "List" `tapp` tcon "Nat"))
    (create' (tcon "List" `tapp` tcon "Nat"))

unit_3 :: Assertion
unit_3 =
  assertNotEqual
    (create' (tcon "List" `tapp` tcon "Bool"))
    (create' (tcon "List" `tapp` tcon "Nat"))

unit_4 :: Assertion
unit_4 =
  assertNotEqual
    (create' (tcon "List" `tapp` tcon "Bool"))
    (create' (tcon "Nat"))

unit_5 :: Assertion
unit_5 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon "List" `tapp` tvar "a"))
    (create' (tcon "Nat"))

unit_6 :: Assertion
unit_6 =
  (@?=)
    (create' (tforall "a" KType $ tcon "List" `tapp` tvar "a"))
    (create' (tforall "b" KType $ tcon "List" `tapp` tvar "b"))

unit_7 :: Assertion
unit_7 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon "List" `tapp` tvar "a"))
    (create' (tforall "b" KType $ tcon "List" `tapp` tcon "Bool"))

unit_8 :: Assertion
unit_8 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon "Bool"))
    (create' (tforall "b" (KFun KType KType) $ tcon "Bool"))

unit_9 :: Assertion
unit_9 =
  assertNotEqual
    (create' (tforall "a" KType $ tforall "b" KType $ tcon "List" `tapp` tvar "a"))
    (create' (tforall "a" KType $ tforall "b" KType $ tcon "List" `tapp` tvar "b"))

unit_10 :: Assertion
unit_10 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon "List" `tapp` tvar "a"))
    (create' (tcon "List" `tapp` tforall "a" KType (tvar "b")))

unit_11 :: Assertion
unit_11 =
  assertNotEqual
    (create' (tforall "a" KType $ tcon "Bool" `tfun` (tcon "List" `tapp` tvar "a")))
    (create' (tcon "Bool" `tfun` tforall "a" KType (tcon "List" `tapp` tvar "a")))

hprop_refl :: Property
hprop_refl = property $ do
  t <- set _typeMeta () <$> forAll (evalExprGen 0 genType)
  assert $ alphaEqTy t t

hprop_alpha :: Property
hprop_alpha = property $ do
  s <- f <$> forAll (evalExprGen 0 genName)
  t <- f <$> forAll (evalExprGen 0 genName)
  s === t
  where
    f v = create' $ tforall v KType $ tvar v

create' :: S (Type' a) -> Alpha
create' = Alpha . set _typeMeta () . fst . create

-- | Like @Type' ()@, but 'Eq' only compares up to alpha-equality.
newtype Alpha = Alpha (Type' ())
  deriving (Show)

instance Eq Alpha where
  (Alpha x) == (Alpha y) = x `alphaEqTy` y

assertNotEqual :: Alpha -> Alpha -> Assertion
assertNotEqual s t = assertBool "types are equal" $ s /= t
