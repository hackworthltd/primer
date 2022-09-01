module Primer.Gen.API (
  genExprTreeOpts,
) where

import Foreword

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Primer.API (ExprTreeOpts (..))

genExprTreeOpts :: MonadGen m => m ExprTreeOpts
genExprTreeOpts = do
  patternsUnder <- Gen.bool
  pure ExprTreeOpts{patternsUnder}
