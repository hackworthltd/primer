{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Utilities useful across several types of tests.
module Primer.Test.Util (
  (@?=),
  ExceptionPredicate,
  assertException,
  primDefs,
  constructTCon,
  constructSaturatedCon,
  toQualText,
  tcn,
  vcn,
  gvn,
  zeroIDs,
  zeroTypeIDs,
  clearMeta,
  clearTypeMeta,
  LogMsg,
  isSevereLog,
  assertNoSevereLogs,
  testNoSevereLogs,
  failWhenSevereLogs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (Severity (Informational), WithSeverity (msgSeverity))
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.String (String)
import Data.Typeable (typeOf)
import Hedgehog (MonadTest, (===))
import Optics (over, set, view)
import Primer.Action (
  Action (ConstructSaturatedCon, ConstructTCon),
 )
import Primer.Core (
  Expr',
  ExprMeta,
  KindMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  HasMetadata (_metadata),
  ID,
  ModuleName (ModuleName, unModuleName),
  TyConName,
  Type',
  TypeMeta,
  ValConName,
  Value,
  qualifyName,
  _exprMeta,
  _exprTypeMeta,
  _exprKindMeta,
  _typeMeta,
  _typeKindMeta,
 )
import Primer.Core.Utils (exprIDs, typeIDs)
import Primer.Def (DefMap)
import Primer.Log (ConvertLogMessage (convert), PureLogT, runPureLogT)
import Primer.Module (Module (moduleDefs), primitiveModule)
import Primer.Name (Name (unName))
import Primer.Primitives (primitive)
import Test.Tasty.HUnit (
  Assertion,
  assertBool,
  assertFailure,
 )
import Test.Tasty.HUnit qualified as HUnit

primDefs :: MonadFresh ID m => m DefMap
primDefs = Map.mapKeys primitive . moduleDefs <$> primitiveModule

-- impedence mismatch: ConstructTCon takes text, but tChar etc are TyConNames
constructTCon :: TyConName -> Action
constructTCon = ConstructTCon . toQualText

constructSaturatedCon :: ValConName -> Action
constructSaturatedCon = ConstructSaturatedCon . toQualText

toQualText :: GlobalName k -> (NonEmpty Text, Text)
toQualText n = (map unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

vcn :: NonEmpty Name -> Name -> ValConName
vcn = qualifyName . ModuleName

tcn :: NonEmpty Name -> Name -> TyConName
tcn = qualifyName . ModuleName

gvn :: NonEmpty Name -> Name -> GVarName
gvn = qualifyName . ModuleName

-- | Replace all 'ID's in an Expr with 0.
zeroIDs :: (HasID a, HasID b, HasID c) => Expr' a b c -> Expr' a b c
zeroIDs = set exprIDs 0

-- | Replace all 'ID's in a Type with 0.
zeroTypeIDs :: (HasID a, HasID b) => Type' a b -> Type' a b
zeroTypeIDs = set typeIDs 0

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearMeta :: Expr' ExprMeta TypeMeta KindMeta -> Expr' (Maybe Value) (Maybe Value) ()
clearMeta = over _exprMeta (view _metadata) . over _exprTypeMeta (view _metadata) . over _exprKindMeta (const ())

-- | Clear the backend-created metadata (IDs and cached types) in the given expression
clearTypeMeta :: Type' TypeMeta KindMeta -> Type' (Maybe Value) ()
clearTypeMeta = over _typeMeta (view _metadata) . over _typeKindMeta (const ())

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

newtype LogMsg = LogMsg Text
  deriving newtype (Show)

instance Show l => ConvertLogMessage l LogMsg where
  convert = LogMsg . show

isSevereLog :: WithSeverity l -> Bool
isSevereLog l = msgSeverity l < Informational

assertNoSevereLogs :: (HasCallStack, Show l) => Seq (WithSeverity l) -> Assertion
assertNoSevereLogs logs =
  let severe = Seq.filter isSevereLog logs
   in if null severe
        then pure ()
        else assertFailure $ toS $ unlines $ "Test logged severe errors:" : foldMap' ((: []) . show) severe

testNoSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => Seq (WithSeverity l) -> m ()
testNoSevereLogs logs = Seq.filter isSevereLog logs === mempty

failWhenSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => PureLogT (WithSeverity l) m a -> m a
failWhenSevereLogs m = do
  (r, logs) <- runPureLogT m
  testNoSevereLogs logs
  pure r
