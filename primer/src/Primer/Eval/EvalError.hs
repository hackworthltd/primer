module Primer.Eval.EvalError (EvalError (..)) where

import Foreword

import Primer.Core (Expr)
import Primer.Core.Meta (ID)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Primitives (PrimFunError)

-- | Errors that can be raised during reduction.
--
-- Note: none of these should occur in normal operation.
-- If we get an EvalError, it means either:
-- - we provided bad input, e.g. an ID not in the given expression
-- - the typechecker has a bug which has allowed a badly-typed expression
-- - a previous invocation of 'step' created a badly-typed expression
--
-- The last case is most likely, since we currently don't typecheck the results of evaluation.
-- We should keep an eye on this to see if there are any issues.
data EvalError
  = -- | The node was not reducible
    NotRedex
  | -- | The node with the given ID could not be found in the expression
    NodeNotFound ID
  | -- | A lambda expression was annotated with a non-function type.
    -- The expression is the offending annotation node
    BadLambdaAnnotation Expr
  | -- | A big lambda expression was annotated with a non-forall type.
    -- The expression is the offending annotation node
    BadBigLambdaAnnotation Expr
  | -- | The outer constructor of a case scrutinee didn't match any of the constructors in the case
    -- branches.
    NoMatchingCaseBranch
  | -- | The number of bindings in a branch pattern doesn't match the number of arguments in the
    -- scrutinee.
    CaseBranchBindingLengthMismatch
  | -- | An error occurred while evaluating a primitive function.
    PrimFunError PrimFunError
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalError
