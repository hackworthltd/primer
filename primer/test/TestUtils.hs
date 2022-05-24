-- | Utilities useful across several types of tests.
module TestUtils (
  withPrimDefs,
  constructTCon,
  constructCon,
  constructRefinedCon,
  tcn,
  vcn,
  gvn,
  zeroIDs,
  zeroTypeIDs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as Map
import Optics (adjoin, over, set, (%))
import Primer.Action (Action (ConstructCon, ConstructRefinedCon, ConstructTCon))
import Primer.Core (
  Expr',
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  ID,
  ModuleName (ModuleName, unModuleName),
  PrimDef (..),
  TyConName,
  Type',
  ValConName,
  primFunType,
  qualifyName,
  setID,
  _exprMeta,
  _exprTypeMeta,
  _id,
  _typeMeta,
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

toQualText :: GlobalName k -> (NonEmpty Text, Text)
toQualText n = (map unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

vcn :: NonEmpty Name -> Name -> ValConName
vcn = qualifyName . ModuleName

tcn :: NonEmpty Name -> Name -> TyConName
tcn = qualifyName . ModuleName

gvn :: NonEmpty Name -> Name -> GVarName
gvn = qualifyName . ModuleName

-- | Replace all 'ID's in an Expr with 0.
zeroIDs :: (HasID a, HasID b) => Expr' a b -> Expr' a b
zeroIDs = set (_exprMeta % _id `adjoin` _exprTypeMeta % _id) 0

-- | Replace all 'ID's in a Type with 0.
zeroTypeIDs :: HasID a => Type' a -> Type' a
zeroTypeIDs = over _typeMeta (setID 0)
