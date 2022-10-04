-- | Utilities useful across several types of tests.
module TestUtils (
  (@?=),
  ExceptionPredicate,
  assertException,
  primDefs,
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
  PureLogT,runPureLogT,runPureLog,firstSevere,
  PrimerLog,
  PrimerLogs,
  runPrimerLogs,
  assertNoSevereLogs,
  failWhenSevereLogs,
  runAPI,evalTestMNoSevereLogs,
) where

import Foreword

import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Data.Map qualified as Map
import Data.String (String)
import Data.Typeable (typeOf)
import Optics (over, set, view)
import Primer.API (
  Env (..),
  PrimerM,
  runPrimerM, SessionTXLog, WithTraceId,
 )
import Primer.Action (
  Action (ConstructCon, ConstructRefinedCon, ConstructTCon), ActionLog,
 )
import Primer.Core (
  Expr',
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  HasMetadata (_metadata),
  ModuleName (ModuleName, unModuleName),
  TyConName,
  Type',
  TypeMeta,
  ValConName,
  Value,
  qualifyName,
  setID,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta, ID,
 )
import Primer.Core.Utils (exprIDs)
import Primer.Database (
  ServiceCfg (..),
  runNullDb',
  serve,
 )
import Primer.Def (DefMap)
import Primer.Module (Module (moduleDefs), primitiveModule)
import Primer.Name (Name (unName))
import Primer.Primitives (primitive)
import StmContainers.Map qualified as StmMap
import Test.Tasty.HUnit (
  assertBool,
  assertFailure, Assertion,
 )
import Test.Tasty.HUnit qualified as HUnit
import Control.Monad.Log (LoggingT, WithSeverity (msgSeverity), PureLoggingT, runPureLoggingT, mapLogMessage, Severity (Error))
import qualified Data.Sequence as Seq
import Primer.Log (ConvertLogMessage (convert))
import Hedgehog (annotateShow, failure, MonadTest, annotate)
import TestM (evalTestM, TestM)

primDefs :: DefMap
primDefs = Map.mapKeys primitive $ moduleDefs primitiveModule

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

newtype PrimerLog = PrimerLog Text
  deriving newtype Show

instance ConvertLogMessage Text PrimerLog where
  convert = PrimerLog
{-
instance ConvertLogMessage Rel8DbLogMessage PrimerLog where
  convert = PrimerLog . show

instance ConvertLogMessage SomeException PrimerLog where
  convert = PrimerLog . show

instance ConvertLogMessage PrimerErr PrimerLog where
  convert (DatabaseErr e) = PrimerLog e
-}
instance ConvertLogMessage SessionTXLog PrimerLog where
  convert = PrimerLog . show

instance ConvertLogMessage ActionLog PrimerLog where
  convert = PrimerLog . show

--type PureLogT m = LoggingT (WithSeverity (WithTraceId PrimerLog)) (PureLoggingT (Seq (WithSeverity (WithTraceId PrimerLog))) m)
type PureLogT l m = LoggingT l (PureLoggingT (Seq l) m)
type PrimerLogs = PrimerM (PureLogT  (WithSeverity (WithTraceId PrimerLog)) IO)

-- TODO: common up with API.pureLogs?
runPureLogT :: Monad m => PureLogT l m a -> m (a, Seq l)
runPureLogT = runPureLoggingT . mapLogMessage Seq.singleton

-- TODO: use more often...
runPureLog  :: PureLogT l Identity a -> (a, Seq l)
runPureLog = runIdentity . runPureLogT

-- recall that Assertion = IO ()
evalTestMNoSevereLogs :: Show l => ID -> PureLogT (WithSeverity l) TestM a -> IO a
evalTestMNoSevereLogs i = evalTestM i . runPureLogT <&> \(a,msgs) -> assertNoSevereLogs msgs >> pure a

runPrimerLogs :: PrimerLogs a -> Env -> IO (a,Seq (WithSeverity (WithTraceId PrimerLog)))
runPrimerLogs m e = runPureLogT $ runPrimerM m e 

firstSevere :: Seq (WithSeverity l) -> Maybe (WithSeverity l)
firstSevere logs = 
-- Note that more-severe errors are earlier in the ordering
  case Seq.filter ((<= Error).msgSeverity) logs of
    Seq.Empty -> Nothing
    e Seq.:<| _ -> Just e

assertNoSevereLogs :: (HasCallStack, Show l) => Seq (WithSeverity l) -> IO ()
assertNoSevereLogs = firstSevere <&> \case
    Nothing -> pure ()
    Just e -> assertFailure $ "There was a severe error: " <> show e

failWhenSevereLogs :: (HasCallStack, MonadTest m, Show l) => Seq (WithSeverity l) -> m ()
failWhenSevereLogs = withFrozenCallStack $ firstSevere <&> \case
    Nothing -> pure ()
    Just e -> do
      annotate "There was a severe error:"
      annotateShow e
      failure


-- Run 2 threads: one that serves a 'NullDb', and one that runs Primer
-- API actions. This allows us to simulate a database and API service.
--
-- We run the database thread on the fork, because it will need to run
-- until it's terminated. The Primer API action will run on the main
-- thread and terminate the database thread when the API action runs
-- to completion or throws.
--
runAPI :: PrimerLogs () -> Assertion
runAPI action = do
  -- This is completely arbitrary and just for testing. In production,
  -- this value will be provided by the production environment and
  -- will likely come from the git rev used to build the production
  -- service.
  let version = "git123"
  dbOpQueue <- newTBQueueIO 1
  initialSessions <- StmMap.newIO
  _ <- forkIO $ void $ runNullDb' $ serve (ServiceCfg dbOpQueue version)
  (ret,logs) <- runPrimerLogs action $ Env initialSessions dbOpQueue version
  assertNoSevereLogs logs
  pure ret
