{-# LANGUAGE OverloadedStrings #-}

module TestUtils (
  insertSessionRow,
  testApp,
  withDbSetup,
  lowPrecisionCurrentTime,
  runTmpDb,
  runTmpDbWithPool,
) where

import Foreword

import Control.Monad.Log (
  DiscardLoggingT,
  WithSeverity,
  discardLogging,
 )
import Data.ByteString.Lazy.UTF8 as BL
import Data.Map.Strict qualified as Map
import Data.String (String)
import Data.Time (
  UTCTime (..),
  diffTimeToPicoseconds,
  picosecondsToDiffTime,
 )
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
import Hasql.Pool (
  Pool,
  acquire,
  release,
  use,
 )
import Hasql.Session (statement)
import Network.Socket.Free (getFreePort)
import Primer.App (
  App,
  Prog (..),
  defaultProg,
  mkApp,
 )
import Primer.Core (
  baseName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (create)
import Primer.Database (
  LastModified (..),
  getCurrentTime,
 )
import Primer.Database.Rel8 (
  Rel8DbT,
  runRel8DbT,
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
  builtinModule,
  primitiveModule,
 )
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

-- The PostgreSQL host, username, and password can be chosen
-- statically, but we need to choose the port dynamically in order to
-- accommodate multiple simultaneous PostgreSQL instances.

host :: String
host = "localhost"

user :: String
user = "postgres"

password :: String
password = "primer"

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

withDbSetup :: (Pool -> IO ()) -> IO ()
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
                  bracket (acquire 1 (Just 1000000) $ toConnectionString db) release f

runTmpDb :: Rel8DbT (DiscardLoggingT (WithSeverity ()) IO) () -> IO ()
runTmpDb tests =
  withDbSetup $ \pool -> discardLogging $ runRel8DbT tests pool

-- | Some tests need access to the pool
runTmpDbWithPool :: (Pool -> Rel8DbT (DiscardLoggingT (WithSeverity ()) IO) ()) -> IO ()
runTmpDbWithPool tests =
  withDbSetup $ \pool -> discardLogging $ runRel8DbT (tests pool) pool

-- | Like @MonadDb.insertSession@, but allows us to insert things
-- directly into the database that otherwise might not be permitted by
-- the type system. This is useful for testing purposes.
insertSessionRow :: Schema.SessionRow Expr -> Pool -> IO ()
insertSessionRow row pool =
  void $
    use pool $
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

-- | PostgreSQL's timestamp type has a precision of 1 microsecond, but
-- 'getCurrentTime' has a precision of 1 picosecond. In order to
-- compare times for our tests, we need to truncate the precision of
-- the time returned by 'getCurrentTime'.
--
-- Ref:
-- https://www.postgresql.org/docs/13/datatype-datetime.html
--
-- Note: we should DRY this, see:
-- https://github.com/hackworthltd/primer/issues/273
lowPrecisionCurrentTime :: (MonadIO m) => m LastModified
lowPrecisionCurrentTime = do
  LastModified (UTCTime day time) <- getCurrentTime
  -- truncate to microseconds
  let time' = picosecondsToDiffTime $ diffTimeToPicoseconds time `div` 1000000 * 1000000
  pure $ LastModified $ UTCTime day time'
