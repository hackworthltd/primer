{-# LANGUAGE OverloadedStrings #-}

module TestUtils (
  (@?=),
  assertException,
  insertSessionRow,
  runTmpDb,
  testApp,
  withDbSetup,
) where

import Foreword

import Data.ByteString.Lazy.UTF8 as BL
import Data.Map.Strict qualified as Map
import Data.String (String)
import Data.Text (unpack)
import Data.Typeable (typeOf)
import Database.PostgreSQL.Simple.Options qualified as Options
import Database.Postgres.Temp (
  DB,
  DirectoryType (Temporary),
  cacheAction,
  cacheConfig,
  cacheDirectoryType,
  cacheTemporaryDirectory,
  defaultCacheConfig,
  optionsToDefaultConfig,
  toConnectionString,
  withConfig,
  withDbCacheConfig,
 )
import GHC.Err (error)
import Hasql.Connection (
  Connection,
  acquire,
  release,
 )
import Hasql.Session (run, statement)
import Network.Socket.Free (getFreePort)
import Primer.App (
  App,
  Prog (..),
  defaultProg,
  mkApp,
 )
import Primer.Builtins (builtinModule)
import Primer.Core (
  baseName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (create)
import Primer.Database.Rel8.Rel8Db (
  Rel8Db,
  runRel8Db,
 )
import Primer.Database.Rel8.Schema as Schema hiding (app)
import Primer.Examples (comprehensive)
import Primer.Module (
  Module (
    Module,
    moduleDefs,
    moduleName,
    moduleTypes
  ),
 )
import Primer.Primitives (primitiveModule)
import Rel8 (
  Expr,
  Insert (Insert, into, onConflict, returning, rows),
  OnConflict (Abort),
  Returning (NumberOfRowsAffected),
  insert,
  values,
 )
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Process.Typed (
  proc,
  readProcessStdout,
  runProcess_,
 )
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
 )
import Test.Tasty.HUnit qualified as HUnit

-- The PostgreSQL host, username, and password can be chosen
-- statically, but we need to choose the port dynamically in order to
-- accommodate multiple simultaneous PostgreSQL instances.

host :: String
host = "localhost"

user :: String
user = "postgres"

password :: String
password = "primer"

connectDb :: DB -> IO Connection
connectDb =
  acquire . toConnectionString >=> either (maybe empty (error . unpack . decodeUtf8)) pure

-- | This action requires that the Sqitch script @primer-sqitch@ is in
-- the process's path. If you run this test via Nix, Nix will
-- guarantee that precondition.
deployDb :: Int -> DB -> IO ()
deployDb port _ =
  let url = "db:postgres://" <> user <> ":" <> password <> "@" <> host <> ":" <> show port
   in runProcess_ $ proc "primer-sqitch" ["deploy", "--verify", url]

-- | This action requires that the Sqitch script @primer-sqitch@ is in
-- the process's path. If you run this test via Nix, Nix will
-- guarantee that precondition.
sqitchEventChangeId :: IO String
sqitchEventChangeId = do
  (status, output) <- readProcessStdout $ proc "primer-sqitch" ["plan", "--max-count=1", "--format=format:%h", "--no-headers"]
  case status of
    ExitFailure n -> error $ "`primer-sqitch plan` failed with exit code " <> show n
    _ -> pure $ takeWhile (/= '\n') $ BL.toString output

withDbSetup :: (Connection -> IO ()) -> IO ()
withDbSetup f = do
  -- NOTE: there's a race where the returned port could be opened by
  -- another process before we can use it, but it's extremely unlikely
  -- to be triggered.
  port <- getFreePort
  let throwEither x = either throwIO pure =<< x
      dbConfig =
        optionsToDefaultConfig
          mempty
            { Options.port = pure port
            , Options.user = pure user
            , Options.password = pure password
            , Options.host = pure host
            }
  throwEither $ do
    tmpdir <- getCanonicalTemporaryDirectory
    let cc =
          defaultCacheConfig
            { cacheTemporaryDirectory = tmpdir
            , cacheDirectoryType = Temporary
            }
     in withDbCacheConfig cc $ \dbCache ->
          let combinedConfig = dbConfig <> cacheConfig dbCache
           in do
                hash_ <- sqitchEventChangeId
                migratedConfig <- throwEither $ cacheAction (tmpdir <> "/" <> hash_) (deployDb port) combinedConfig
                withConfig migratedConfig $ \db ->
                  bracket (connectDb db) release f

runTmpDb :: Rel8Db () -> IO ()
runTmpDb tests =
  withDbSetup $ \conn -> runRel8Db tests conn

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

-- | Like @MonadDb.insertSession@, but allows us to insert things
-- directly into the database that otherwise might not be permitted by
-- the type system. This is useful for testing purposes.
insertSessionRow :: Schema.SessionRow Expr -> Connection -> IO ()
insertSessionRow row conn =
  void $
    flip run conn $
      statement () $
        insert
          Insert
            { into = Schema.sessionRowSchema
            , rows =
                values
                  [ row
                  ]
            , onConflict = Abort
            , returning = NumberOfRowsAffected
            }

-- | An initial test 'App' instance that contains all default type
-- definitions (including primitive types), all primitive functions,
-- and a top-level definition with extensive coverage of Primer's
-- core language.
testApp :: App
testApp =
  let modName = mkSimpleModuleName "TestModule"
      ((defName, def), id_) = create $ comprehensive modName
      testProg =
        defaultProg
          { progImports = [builtinModule, primitiveModule]
          , progModules =
              [ Module
                  { moduleName = mkSimpleModuleName "TestModule"
                  , moduleTypes = mempty
                  , moduleDefs = Map.singleton (baseName defName) def
                  }
              ]
          }
   in mkApp id_ (toEnum 0) testProg
