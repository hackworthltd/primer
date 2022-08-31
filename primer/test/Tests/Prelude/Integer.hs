module Tests.Prelude.Integer where

import Foreword

import Hedgehog (MonadTest, forAll, (===))
import Hedgehog.Gen (integral_)
import Hedgehog.Range qualified as Range
import Optics (over)
import Primer.Builtins.DSL (bool_)
import Primer.Core (Expr, GVarName)
import Primer.Core.DSL (apps, create', gvar, int)
import Primer.EvalFull (Dir (Chk), EvalFullError, TerminationBound, evalFull)
import Primer.Module (builtinModule, moduleDefsQualified, moduleTypesQualified, primitiveModule)
import Primer.Prelude (prelude)
import Primer.Prelude.Integer qualified as P
import Tasty (Property, property, withTests)
import TestM (TestM, evalTestM)
import TestUtils (zeroIDs)
import Tests.EvalFull (evalResultExpr)

tasty_min_prop :: Property
tasty_min_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  m <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.min [int n, int m] 20 <===> Right (create' $ int $ min m n)

tasty_max_prop :: Property
tasty_max_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  m <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.max [int n, int m] 20 <===> Right (create' $ int $ max m n)

tasty_negate_prop :: Property
tasty_negate_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.negate [int n] 20 <===> Right (create' $ int (-n))

tasty_abs_prop :: Property
tasty_abs_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.abs [int n] 40 <===> Right (create' $ int $ abs n)

-- NOTE: Termination bound is experimental, do not know how it varies with n, m
tasty_gcd_prop :: Property
tasty_gcd_prop = withTests 5 $ property $ do
  n <- forAll $ integral_ (Range.linearFrom 0 (-10) 10)
  m <- forAll $ integral_ (Range.linearFrom 0 (-10) 10)
  functionOutput P.gcd [int n, int m] 4000 <===> Right (create' $ int $ gcd n m)

-- NOTE: Termination bound is experimental, do not know how it varies with n, m
tasty_lcm_prop :: Property
tasty_lcm_prop = withTests 5 $ property $ do
  n <- forAll $ integral_ (Range.linearFrom 0 (-10) 10)
  m <- forAll $ integral_ (Range.linearFrom 0 (-10) 10)
  functionOutput P.lcm [int n, int m] 4000 <===> Right (create' $ int $ lcm n m)

tasty_even_prop :: Property
tasty_even_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.even [int n] 20 <===> Right (create' $ bool_ $ even n)

tasty_odd_prop :: Property
tasty_odd_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.odd [int n] 20 <===> Right (create' $ bool_ $ odd n)

(<===>) :: (HasCallStack, MonadTest m) => Either EvalFullError Expr -> Either EvalFullError Expr -> m ()
x <===> y = withFrozenCallStack $ on (===) (over evalResultExpr zeroIDs) x y

-- Tests a prelude function
functionOutput :: GVarName -> [TestM Expr] -> TerminationBound -> Either EvalFullError Expr
functionOutput f args depth =
  evalTestM 0 $ do
    e <- apps (gvar f) args
    evalFull ty def n d e
  where
    mods = [builtinModule, primitiveModule, prelude']
    (ty, def) = mconcat $ map (\m -> (moduleTypesQualified m, moduleDefsQualified m)) mods
    n = depth
    d = Chk
    prelude' = create' prelude
