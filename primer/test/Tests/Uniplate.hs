module Tests.Uniplate where

import Data.Generics.Uniplate.Data (transform, universe)
import Foreword
import Primer.Core
import Primer.Core.DSL
import Test.Tasty.HUnit

-- In some places, but notably in importing, I rely on the fact
-- that 'universe' will look at the whole expression. However, it is
-- not totally clear that uniplate will do that, as we have mutual
-- recursion between the types Expr' and CaseBranch.
-- This tests that it does indeed look through this mutual recursion
unit_expr_universe_looks_in_branches :: Assertion
unit_expr_universe_looks_in_branches = [c | Con _ c <- universe e] @?= ["A"]
  where
    e = fst $ create $ case_ emptyHole [branch "C" [] $ con "A"]

-- similar to universe test, but for transform
unit_expr_transform_looks_in_branches :: Assertion
unit_expr_transform_looks_in_branches = transform f (e "A") @?= e "B"
  where
    e n = fst $ create $ case_ emptyHole [branch "C" [] $ con n]
    f = \case
      Con m "A" -> Con m "B"
      expr -> expr
