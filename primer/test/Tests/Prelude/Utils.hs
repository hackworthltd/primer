module Tests.Prelude.Utils where

import Data.Sequence qualified as Seq
import Data.String (String, unlines)
import Foreword hiding (exp, unlines)
import Hedgehog.Internal.Property
import Optics (over)
import Primer.Core (Expr, GVarName, Type)
import Primer.Core.DSL (apps', create', gvar)
import Primer.EvalFull (Dir (Chk), EvalFullError, EvalFullLog, TerminationBound, evalFull)
import Primer.Log (runPureLogT)
import Primer.Module (builtinModule, moduleDefsQualified, moduleTypesQualified, primitiveModule)
import Primer.Prelude (prelude)
import Primer.Pretty (prettyExpr, sparse)
import Primer.Test.TestM (TestM, evalTestM)
import Primer.Test.Util (evalResultExpr, isSevereLog, zeroIDs)
import Prelude (error)

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
  let (r, logs) = evalTestM 0 $ runPureLogT $ do
        e <- apps' (gvar f) $ bimap lift lift <$> args
        evalFull @EvalFullLog ty def n d e
      severe = Seq.filter isSevereLog logs
   in if null severe
        then r
        else error $ unlines $ "There were severe logs:" : foldMap ((: []) . show) severe
  where
    mods = [builtinModule, primitiveModule, prelude']
    (ty, def) = mconcat $ map (\m -> (moduleTypesQualified m, moduleDefsQualified m)) mods
    n = depth
    d = Chk
    prelude' = create' prelude
