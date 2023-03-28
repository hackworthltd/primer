module Tests.Prelude.Integer where

import Foreword

import Hedgehog (forAll)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as Range
import Primer.Builtins.DSL (
  bool_,
  list_,
 )
import Primer.Core.DSL (
  create',
  int,
 )
import Primer.Prelude.Integer qualified as P
import Primer.Primitives (
  tInt,
 )
import Tasty (Property, property, withTests)
import Tests.Prelude.Utils (functionOutput, (<===>))

tasty_min_prop :: Property
tasty_min_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  m <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.min [int n, int m] 20 <===> Right (create' $ int $ min m n)

tasty_max_prop :: Property
tasty_max_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  m <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.max [int n, int m] 20 <===> Right (create' $ int $ max m n)

tasty_negate_prop :: Property
tasty_negate_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.negate [int n] 20 <===> Right (create' $ int (-n))

tasty_abs_prop :: Property
tasty_abs_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.abs [int n] 40 <===> Right (create' $ int $ abs n)

-- NOTE: Termination bound is experimental, do not know how it varies with n, m
tasty_gcd_prop :: Property
tasty_gcd_prop = withTests 5 $ property $ do
  n <- forAll $ G.integral_ (Range.linearFrom 0 (-10) 10)
  m <- forAll $ G.integral_ (Range.linearFrom 0 (-10) 10)
  functionOutput P.gcd [int n, int m] 4000 <===> Right (create' $ int $ gcd n m)

-- NOTE: Termination bound is experimental, do not know how it varies with n, m
tasty_lcm_prop :: Property
tasty_lcm_prop = withTests 5 $ property $ do
  n <- forAll $ G.integral_ (Range.linearFrom 0 (-10) 10)
  m <- forAll $ G.integral_ (Range.linearFrom 0 (-10) 10)
  functionOutput P.lcm [int n, int m] 4000 <===> Right (create' $ int $ lcm n m)

tasty_even_prop :: Property
tasty_even_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.even [int n] 20 <===> Right (create' $ bool_ $ even n)

tasty_odd_prop :: Property
tasty_odd_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  functionOutput P.odd [int n] 20 <===> Right (create' $ bool_ $ odd n)

tasty_sum_prop :: Property
tasty_sum_prop = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant (-10) 10))
  functionOutput P.sum [list_ $ map int ns] 2000 <===> Right (create' $ int $ sum ns)

tasty_product_prop :: Property
tasty_product_prop = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant 1 10))
  functionOutput P.product [list_ $ map int ns] 2000 <===> Right (create' $ int $ product ns)
