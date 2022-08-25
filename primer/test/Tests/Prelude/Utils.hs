module Tests.Prelude.Utils where

import Foreword
import Hedgehog (MonadTest, (===))
import Optics (over)
import Primer.Core (Expr, GVarName)
import Primer.Core.DSL (apps, create', gvar)
import Primer.EvalFull (Dir (Chk), EvalFullError, TerminationBound, evalFull)
import Primer.Module (builtinModule, moduleDefsQualified, moduleTypesQualified, primitiveModule)
import Primer.Prelude (prelude)
import TestM (TestM, evalTestM)
import TestUtils (zeroIDs)
import Tests.EvalFull (evalResultExpr)

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
