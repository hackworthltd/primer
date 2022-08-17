module Tests.Prelude.Integer where

import Foreword

import Hedgehog (MonadTest, forAll, (===))
import Hedgehog.Gen (integral_)
import Hedgehog.Range qualified as Range
import Optics (over)
import Primer.Core (Expr, GVarName)
import Primer.Core.DSL (create', gvar, int)
import Primer.EvalFull (Dir (Chk), EvalFullError, TerminationBound, evalFull)
import Primer.Module (builtinModule, moduleDefsQualified, moduleTypesQualified, primitiveModule)
import Primer.Prelude (prelude)
import Primer.Prelude.Integer qualified as P
import Primer.Prelude.Utils (apps)
import Tasty (Property, property)
import TestM (TestM, evalTestM)
import TestUtils (zeroIDs)
import Tests.EvalFull (evalResultExpr)

-- min x (x+1) = x
tasty_min_prop1 :: Property
tasty_min_prop1 = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.min [int n, int $ n + 1] 20 <===> Right (create' $ int n)

-- min x x = x
tasty_min_prop2 :: Property
tasty_min_prop2 = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.min [int n, int n] 20 <===> Right (create' $ int n)

-- max x (x+1) = x+1
tasty_max_prop1 :: Property
tasty_max_prop1 = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.max [int n, int $ n + 1] 20 <===> Right (create' $ int $ n + 1)

-- max x x = x
tasty_max_prop2 :: Property
tasty_max_prop2 = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.max [int n, int n] 20 <===> Right (create' $ int n)

tasty_negate_prop :: Property
tasty_negate_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.negate [int n] 20 <===> Right (create' $ int (-n))

tasty_abs_prop :: Property
tasty_abs_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput P.abs [int n] 40 <===> Right (create' $ int $ abs n)

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
