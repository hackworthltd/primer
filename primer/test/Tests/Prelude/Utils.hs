module Tests.Prelude.Utils where

import Data.String (String, unlines)
import Foreword hiding (exp, unlines)
import Hedgehog.Internal.Property
import Optics (over)
import Primer.Core (Expr, GVarName, Type)
import Primer.Core.DSL (apps', create', gvar)
import Primer.EvalFull (Dir (Chk), EvalFullError, TerminationBound, evalFull)
import Primer.Module (builtinModule, moduleDefsQualified, moduleTypesQualified, primitiveModule)
import Primer.Prelude (prelude)
import Primer.Pretty (prettyExpr, sparse)
import TestM (TestM, evalTestM)
import TestUtils (zeroIDs)
import Tests.EvalFull (evalResultExpr)
import Control.Monad.Log (DiscardLoggingT(discardLogging))
import Tests.Action.Available (TestLogMessage)

(<===>) :: (HasCallStack, MonadTest m) => Either EvalFullError Expr -> Either EvalFullError Expr -> m ()
x <===> y = withFrozenCallStack $ on compareExpr (over evalResultExpr zeroIDs) x y
  where
    compareExpr :: (HasCallStack, MonadTest m) => Either EvalFullError Expr -> Either EvalFullError Expr -> m ()
    compareExpr a b = do
      ok <- withFrozenCallStack $ eval (a == b)
      if ok
        then success
        else do
          annotate $
            unlines
              [ "Pretty Printed Output:"
              , "LHS____________________________________________________"
              , prettyWrap a
              , ""
              , "RHS____________________________________________________"
              , prettyWrap b
              , ""
              ]
          failDiff a b

    prettyWrap :: Either EvalFullError Expr -> String
    prettyWrap (Left err) = show err
    prettyWrap (Right exp) = show $ prettyExpr sparse exp

-- Tests a prelude function with Expr arguments
functionOutput :: GVarName -> [TestM Expr] -> TerminationBound -> Either EvalFullError Expr
functionOutput f args = functionOutput' f (map Left args)

-- Tests a prelude function with a combination of Expr/Type arguments to be applied
functionOutput' :: GVarName -> [Either (TestM Expr) (TestM Type)] -> TerminationBound -> Either EvalFullError Expr
functionOutput' f args depth =
  evalTestM 0 $ do
    e <- apps' (gvar f) args
    discardLogging $ evalFull @TestLogMessage ty def n d e
  where
    mods = [builtinModule, primitiveModule, prelude']
    (ty, def) = mconcat $ map (\m -> (moduleTypesQualified m, moduleDefsQualified m)) mods
    n = depth
    d = Chk
    prelude' = create' prelude
