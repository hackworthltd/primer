module Primer.Eval.EvalError (EvalError (..)) where

import Foreword

import Primer.Core.Meta (ID)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)

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
  | -- | We did a reduction, but our assumptions were flawed when working out details
    InternalDetailError
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalError
