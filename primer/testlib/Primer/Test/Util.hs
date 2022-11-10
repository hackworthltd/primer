-- | Utilities useful across several types of tests.
module Primer.Test.Util (
  (@?=),
  (<~==>),
  ExceptionPredicate,
  assertException,
  builtinTypes,
  distinctIDs,
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
  runAPI,
  evalFullTest,
  evalResultExpr,
  LogMsg,
  isSevereLog,
  assertNoSevereLogs,
  testNoSevereLogs,
  testModule,
  testModules,
) where

import Foreword

import Control.Concurrent.STM (
  newTBQueueIO,
 )
import Control.Monad.Log (Severity (Informational), WithSeverity (msgSeverity))
import Data.List ((\\))
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.String (String)
import Data.String qualified as String
import Data.Typeable (typeOf)
import Hedgehog (MonadTest, (===))
import Optics
import Primer.API (
  Env (..),
  PrimerM,
  runPrimerM,
 )
import Primer.Action (
  Action (ConstructCon, ConstructRefinedCon, ConstructTCon),
 )
import Primer.Core (
  Expr,
  Expr',
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  HasID,
  HasMetadata (_metadata),
  ID,
  ModuleName (..),
  TyConName,
  Type',
  TypeMeta,
  ValConName,
  Value,
  qualifyName,
  setID,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL
import Primer.Core.Utils
import Primer.Database (
  ServiceCfg (..),
  runNullDb',
  serve,
 )
import Primer.Def
import Primer.EvalFull
import Primer.Log (ConvertLogMessage (convert), PureLogT, runPureLogT)
import Primer.Module
import Primer.Name (Name (unName))
import Primer.Primitives
import Primer.Test.TestM (
  evalTestM,
 )
import Primer.TypeDef (
  TypeDefMap,
 )
import StmContainers.Map qualified as StmMap
import Test.Tasty.HUnit (
  Assertion,
  assertBool,
  assertFailure,
 )
import Test.Tasty.HUnit qualified as HUnit

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

newtype LogMsg = LogMsg Text
  deriving newtype (Show)

instance Show l => ConvertLogMessage l LogMsg where
  convert = LogMsg . show

isSevereLog :: WithSeverity l -> Bool
isSevereLog l = msgSeverity l < Informational

assertNoSevereLogs :: Show l => Seq (WithSeverity l) -> Assertion
assertNoSevereLogs logs =
  let severe = Seq.filter isSevereLog logs
   in if null severe
        then pure ()
        else assertFailure $ toS $ unlines $ "Test logged severe errors:" : foldMap ((: []) . show) severe

testNoSevereLogs :: (HasCallStack, MonadTest m, Eq l, Show l) => Seq (WithSeverity l) -> m ()
testNoSevereLogs logs = Seq.filter isSevereLog logs === mempty

-- Run 2 threads: one that serves a 'NullDb', and one that runs Primer
-- API actions. This allows us to simulate a database and API service.
--
-- We run the database thread on the fork, because it will need to run
-- until it's terminated. The Primer API action will run on the main
-- thread and terminate the database thread when the API action runs
-- to completion or throws.
runAPI :: PrimerM (PureLogT (WithSeverity LogMsg) IO) a -> IO a
runAPI action = do
  -- This is completely arbitrary and just for testing. In production,
  -- this value will be provided by the production environment and
  -- will likely come from the git rev used to build the production
  -- service.
  let version = "git123"
  dbOpQueue <- newTBQueueIO 1
  initialSessions <- StmMap.newIO
  _ <- forkIO $ void $ runNullDb' $ serve (ServiceCfg dbOpQueue version)
  (r, logs) <- runPureLogT . runPrimerM action $ Env initialSessions dbOpQueue version
  assertNoSevereLogs logs
  pure r

evalFullTest :: ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> IO (Either EvalFullError Expr)
evalFullTest id_ tydefs globals n d e = do
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalFullLog tydefs globals n d e
  assertNoSevereLogs logs
  distinctIDs r
  pure r

distinctIDs :: Either EvalFullError Expr -> Assertion
distinctIDs e =
  let ids = e ^.. evalResultExpr % exprIDs
      nIds = length ids
      uniqIDs = S.fromList ids
      nDistinct = S.size uniqIDs
   in assertBool
        ( String.unlines
            [ "Failure: non-distinct ids; had " <> show nIds <> " ids, but only " <> show nDistinct <> " unique ones"
            , "The duplicates were"
            , show $ ids \\ S.toList uniqIDs
            ]
        )
        (nIds == nDistinct)

builtinTypes :: TypeDefMap
builtinTypes = moduleTypesQualified builtinModule

-- | Some generally-useful globals to have around when testing.
-- Currently: an AST identity function on Char and all builtins and
-- primitives
testModules :: [Module]
testModules = [builtinModule, primitiveModule, testModule]

testModule :: Module
testModule =
  let (ty, expr) = create' $ (,) <$> tcon tChar `tfun` tcon tChar <*> lam "x" (lvar "x")
   in Module
        { moduleName = ModuleName ["M"]
        , moduleTypes = mempty
        , moduleDefs =
            Map.singleton "idChar" $
              DefAST
                ASTDef
                  { astDefType = ty
                  , astDefExpr = expr
                  }
        }

evalResultExpr :: Traversal' (Either EvalFullError Expr) Expr
evalResultExpr = _Left % timedOut `adjoin` _Right
  where
    timedOut = prism TimedOut (Right . \case TimedOut e -> e)

(<~==>) :: HasCallStack => Either EvalFullError Expr -> Either EvalFullError Expr -> Assertion
x <~==> y = on (@?=) (over evalResultExpr zeroIDs) x y
