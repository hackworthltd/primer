{-# OPTIONS_GHC -Wno-unused-local-binds #-}
-- | Utilities useful across several types of tests.
module TestUtils (
  (@?=),
  assertException,
  withPrimDefs,
  constructTCon,
  constructCon,
  constructRefinedCon,
  tcn,
  vcn,
  gvn,
  zeroIDs,
  zeroTypeIDs,
  clearMeta,
  clearTypeMeta,
  Property,
  property,
  withTests,
  withDiscards,
  Shadowing(..),
  noShadowing,
  noShadowingTy
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Tree (Tree (Node), foldTree)
import Data.Coerce (coerce)
import Data.String (String, fromString)
import Data.Typeable (typeOf)
import qualified Hedgehog as H
import Optics (over, set, view, (^.), (%))
import Primer.Action (Action (ConstructCon, ConstructRefinedCon, ConstructTCon))
import Primer.Core (
  Expr',_exprMetaLens, _type, _typeMetaLens,
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  HasMetadata (_metadata),
  ID,
  ModuleName (ModuleName, unModuleName),
  PrimDef (..),
  TyConName,
  Type',
  TypeMeta,
  ValConName,
  Value,
  primFunType,
  qualifyName,
  setID,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta, Expr, Type, TypeCache (..), TypeCacheBoth (TCBoth), LocalName (unLocalName)
 )
import Primer.Core.Utils (exprIDs)
import Primer.Name (Name (unName))
import Primer.Primitives (allPrimDefs)
import qualified Test.Tasty.Discover as TD
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
 )
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.Hedgehog as TH
import Primer.Zipper
import qualified Data.Generics.Uniplate.Data as U
import qualified Data.Set as Set

withPrimDefs :: MonadFresh ID m => (Map GVarName PrimDef -> m a) -> m a
withPrimDefs f = do
  defs <- for allPrimDefs (fmap PrimDef . primFunType)
  f defs

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
zeroIDs = set exprIDs 0

-- | Replace all 'ID's in a Type with 0.
zeroTypeIDs :: HasID a => Type' a -> Type' a
zeroTypeIDs = over _typeMeta (setID 0)

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearMeta :: Expr' ExprMeta TypeMeta -> Expr' (Maybe Value) (Maybe Value)
clearMeta = over _exprMeta (view _metadata) . over _exprTypeMeta (view _metadata)

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearTypeMeta :: Type' TypeMeta -> Type' (Maybe Value)
clearTypeMeta = over _typeMeta (view _metadata)

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
x @?= y = liftIO $ x HUnit.@?= y
infix 1 @?=

type ExceptionPredicate e = (e -> Bool)

assertException ::
  (HasCallStack, Exception e, MonadIO m, MonadCatch m) =>
  String ->
  ExceptionPredicate e ->
  m a ->
  m ()
assertException msg p action = do
  r <- try action
  case r of
    Right _ -> liftIO $ assertFailure $ msg <> " should have thrown " <> exceptionType <> ", but it succeeded"
    Left e -> liftIO $ assertBool (wrongException e) (p e)
  where
    wrongException e = msg <> " threw " <> show e <> ", but we expected " <> exceptionType
    exceptionType = (show . typeOf) p

-- | Work around tasty changes which give deprecation warnings for tasty-discover generated code
newtype Property = Property
  { unProperty :: H.Property
  }

instance TD.Tasty Property where
  tasty info =
    pure
      . TH.testPropertyNamed (TD.descriptionOf info) (fromString (TD.descriptionOf info))
      . unProperty

property :: HasCallStack => H.PropertyT IO () -> Property
property = Property . H.property

withTests :: H.TestLimit -> Property -> Property
withTests = coerce H.withTests

withDiscards :: H.DiscardLimit -> Property -> Property
withDiscards = coerce H.withDiscards

noShadowingTy :: Type -> Shadowing
noShadowingTy = checkShadowing . binderTreeTy

binderTreeTy :: Type -> Tree (Set Name)
binderTreeTy = U.para $ \ty children ->
      let bs = Set.map unLocalName $ getBoundHereTy ty
          metaChildren = case ty ^. _typeMetaLens % _type of
            Nothing -> mempty
            Just _k -> mempty -- there are no binders in kinds
      in Node bs $ children <> metaChildren

-- includes metadata
noShadowing :: Expr -> Shadowing
noShadowing = checkShadowing . binderTree

binderTree :: Expr -> Tree (Set Name)
binderTree = U.para $ \e exprChildren ->
      let bs = getBoundHere e Nothing
          typeChildren = case target . focusOnlyType <$> focusType (focus e) of
            Just ty -> [binderTreeTy ty]
            Nothing -> mempty
          metaChildren = case e ^. _exprMetaLens % _type of
            Nothing -> mempty
            Just (TCChkedAt ty) -> [binderTreeTy' ty]
            Just (TCSynthed ty) -> [binderTreeTy' ty]
            Just (TCEmb (TCBoth ty1 ty2)) -> [binderTreeTy' ty1, binderTreeTy' ty2]
      in Node bs $ exprChildren <> typeChildren <> metaChildren
  where
    binderTreeTy' :: Type' () -> Tree (Set Name)
    binderTreeTy' = U.para $ \ty children ->
      let bs = Set.map unLocalName $ getBoundHereTy ty
      in Node bs children

data Shadowing = ShadowingExists | ShadowingNotExists
  deriving (Eq, Show)

checkShadowing :: Tree (Set Name) -> Shadowing
checkShadowing t = if fst $ foldTree f t
  then ShadowingExists
  else ShadowingNotExists
  where
    f :: Set Name -> [(Bool,Set Name)] -> (Bool,Set Name)
    f bindsHere xs = let (shadowingInSubtrees,bindsSubTrees) = unzip xs
                         allSubtreeBinds = Set.unions bindsSubTrees
                         allBinds = bindsHere <> allSubtreeBinds
                         shadowing = or shadowingInSubtrees || not (Set.disjoint bindsHere allSubtreeBinds)
                     in (shadowing, allBinds)
