-- | Utilities useful across several types of tests.
module TestUtils (
  withPrimDefs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as Map
import Primer.Core (
  GVarName,
  ID,
  PrimDef (..),
  primFunType,
 )
import Primer.Primitives (allPrimDefs)

withPrimDefs :: MonadFresh ID m => (Map GVarName PrimDef -> m a) -> m a
withPrimDefs f = do
  defs <-
    for
      (Map.toList allPrimDefs)
      (\(name, p) -> PrimDef name <$> primFunType p)
  f
    (Map.fromList $ (\d -> (primDefName d, d)) <$> defs)
