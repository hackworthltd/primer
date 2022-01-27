-- | Utilities useful across several types of tests.
module TestUtils (
  withPrimDefs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (fresh))
import qualified Data.Map as Map
import Primer.Core (
  ID,
  PrimDef (..),
  PrimFun (..),
 )
import Primer.Name (Name)
import Primer.Primitives (allPrimDefs)

withPrimDefs :: MonadFresh ID m => (Map Name ID -> Map ID PrimDef -> m a) -> m a
withPrimDefs f = do
  defs <-
    for
      (Map.toList allPrimDefs)
      (\(name, p) -> PrimDef <$> fresh <*> pure name <*> primFunType p)
  f
    (Map.fromList $ (\d -> (primDefName d, primDefID d)) <$> defs)
    (Map.fromList $ (\d -> (primDefID d, d)) <$> defs)
