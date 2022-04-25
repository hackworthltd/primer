-- | Utilities useful across several types of tests.
module TestUtils (
  withPrimDefs,
  constructTCon,
  constructCon,
  constructRefinedCon,
  tcn,
  vcn,
  gvn,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as Map
import Primer.Action (Action (ConstructCon, ConstructRefinedCon, ConstructTCon))
import Primer.Core (
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  ModuleName (unModuleName),
  PrimDef (..),
  TyConName,
  ValConName,
  primFunType,
  qualifyName,
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
constructTCon = ConstructTCon . toQualText

constructCon :: ValConName -> Action
constructCon = ConstructCon . toQualText

constructRefinedCon :: ValConName -> Action
constructRefinedCon = ConstructRefinedCon . toQualText

toQualText :: GlobalName k -> (Text, Text)
toQualText n = (unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

vcn :: ModuleName -> Name -> ValConName
vcn = qualifyName

tcn :: ModuleName -> Name -> TyConName
tcn = qualifyName

gvn :: ModuleName -> Name -> GVarName
gvn = qualifyName
