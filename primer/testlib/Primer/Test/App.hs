module Primer.Test.App (
  AppTestM,
  runAppTestM,
  comprehensive,
) where

import Foreword

import Control.Monad.Fresh (
  MonadFresh,
 )
import Control.Monad.Log (
  MonadLog,
  WithSeverity,
 )
import Data.Map.Strict qualified as Map
import Primer.App (
  App,
  FreshViaApp (..),
  Prog (..),
  ProgError (..),
  defaultProg,
  mkApp,
 )
import Primer.Core (
  baseName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (create)
import Primer.Core.Meta (
  ID,
 )
import Primer.Examples qualified as Examples
import Primer.Log (
  PureLogT,
  runPureLogT,
 )
import Primer.Module (
  Module (
    Module,
    moduleDefs,
    moduleName,
    moduleTypes
  ),
  builtinModule,
  primitiveModule,
 )
import Primer.Name (
  NameCounter,
 )
import Primer.Test.Util (
  LogMsg,
  assertNoSevereLogs,
 )

newtype AppTestT m a = AppTestT
  { unAppTestM ::
      ( StateT
          App
          ( ExceptT
              ProgError
              (PureLogT (WithSeverity LogMsg) m)
          )
      )
        a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLog (WithSeverity LogMsg)
    , MonadState App
    , MonadError ProgError
    , MonadCatch
    , MonadThrow
    )
  deriving
    ( MonadFresh ID
    , MonadFresh NameCounter
    )
    via FreshViaApp (AppTestT m)

type AppTestM a = AppTestT IO a

-- Recall that @Assertion = IO ()@
--
-- This is in 'MonadIO' as it asserts that there were no severe log
-- messages
runAppTestT :: (MonadIO m) => App -> AppTestT m a -> m (Either ProgError a, App)
runAppTestT a m = do
  (r, logs) <- runAppTestT' a m
  liftIO $ assertNoSevereLogs logs $> r

runAppTestT' :: (Monad m) => App -> AppTestT m a -> m ((Either ProgError a, App), Seq (WithSeverity LogMsg))
runAppTestT' a m = do
  (r, logs) <- runPureLogT $ runExceptT $ flip runStateT a $ unAppTestM m
  case r of
    Left err -> pure ((Left err, a), logs)
    Right (res, app') -> pure ((Right res, app'), logs)

runAppTestM :: App -> AppTestM a -> IO (Either ProgError a, App)
runAppTestM = runAppTestT

-- | An initial test 'App' instance that contains all default type
-- definitions (including primitive types), all primitive functions,
-- and a top-level definition with extensive coverage of Primer's
-- core language.
comprehensive :: App
comprehensive =
  let modName = mkSimpleModuleName "TestModule"
      ((builtinMod, primitiveMod, (defName, def)), id_) = create $ (,,) <$> builtinModule <*> primitiveModule <*> Examples.comprehensive modName
      testProg =
        defaultProg
          { progImports = [builtinMod, primitiveMod]
          , progModules =
              [ Module
                  { moduleName = mkSimpleModuleName "TestModule"
                  , moduleTypes = mempty
                  , moduleDefs = Map.singleton (baseName defName) def
                  }
              ]
          }
   in mkApp id_ (toEnum 0) testProg
