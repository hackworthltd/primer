-- | Utilities useful across several types of tests.
module TestUtils (
  withPrimDefs,
  constructTCon,
  constructCon,
  constructRefinedCon,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as Map
import Primer.Action (Action (ConstructCon, ConstructRefinedCon, ConstructTCon))
import Primer.Core (
  GVarName,
  ID,
  PrimDef (..),
  TyConName,
  ValConName,
  baseName,
  primFunType,
 )
import Primer.Name (Name (unName))
import Primer.Primitives (allPrimDefs)

withPrimDefs :: MonadFresh ID m => (Map GVarName PrimDef -> m a) -> m a
withPrimDefs f = do
  defs <-
    for
      (Map.toList allPrimDefs)
      (\(name, p) -> PrimDef name <$> primFunType p)
  f
    (Map.fromList $ (\d -> (primDefName d, d)) <$> defs)

-- impedence mismatch: ConstructTCon takes text, but tChar etc are TyConNames
constructTCon :: TyConName -> Action
constructTCon = ConstructTCon . unName . baseName

constructCon :: ValConName -> Action
constructCon = ConstructCon . unName . baseName

constructRefinedCon :: ValConName -> Action
constructRefinedCon = ConstructRefinedCon . unName . baseName
