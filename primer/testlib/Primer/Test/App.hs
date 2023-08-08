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
  PureLog,
  runPureLog,
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

newtype AppTestM a = AppTestM
  { unAppTestM ::
      ( StateT
          App
          ( ExceptT
              ProgError
              (PureLog (WithSeverity LogMsg))
          )
      )
        a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLog (WithSeverity LogMsg)
    , MonadState App
    , MonadError ProgError
    )
  deriving
    ( MonadFresh ID
    , MonadFresh NameCounter
    )
    via FreshViaApp AppTestM

-- Recall that Assertion = IO ()
-- This is in IO as it asserts that there were no severe log messages
runAppTestM :: App -> AppTestM a -> IO (Either ProgError a, App)
runAppTestM a m =
  let (r, logs) = runAppTestM' a m
   in assertNoSevereLogs logs $> r

runAppTestM' :: App -> AppTestM a -> ((Either ProgError a, App), Seq (WithSeverity LogMsg))
runAppTestM' a m =
  case runPureLog $ runExceptT $ flip runStateT a $ unAppTestM m of
    (Left err, logs) -> ((Left err, a), logs)
    (Right (res, app'), logs) -> ((Right res, app'), logs)

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
