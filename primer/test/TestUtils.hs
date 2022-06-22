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
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Coerce (coerce)
import Data.String (String)
import Data.Typeable (typeOf)
import qualified Hedgehog as H
import Optics (over, set, view)
import Primer.Action (Action (ConstructCon, ConstructRefinedCon, ConstructTCon))
import Primer.Core (
  Expr',
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
  _typeMeta,
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
      . TH.testProperty (TD.descriptionOf info)
      . unProperty

property :: HasCallStack => H.PropertyT IO () -> Property
property = Property . H.property

withTests :: H.TestLimit -> Property -> Property
withTests = coerce H.withTests

withDiscards :: H.DiscardLimit -> Property -> Property
withDiscards = coerce H.withDiscards
