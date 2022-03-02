module TestUtils (
  (@?=),
  anyException,
  assertException,
  insertSessionRow,
  withDbSetup,
) where

import Foreword hiding (try)

import Control.Monad.Catch (MonadCatch, try)
import Data.String (String)
import Data.Text (unpack)
import Data.Typeable (typeOf)
import qualified Database.PostgreSQL.Simple.Options as Options
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
import Primer.Database.Rel8.Schema as Schema (
  SessionRow (..),
  sessionRowSchema,
 )
import Rel8 (
  Expr,
  Insert (Insert, into, onConflict, returning, rows),
  OnConflict (Abort),
  Returning (NumberOfRowsAffected),
  insert,
  values,
 )
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  proc,
  runProcess_,
 )
import Test.Tasty.HUnit (
  assertBool,
  assertFailure,
 )
import qualified Test.Tasty.HUnit as HUnit

host :: String
host = "localhost"

port :: Int
port = 5432

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
deployDb :: DB -> IO ()
deployDb _ =
  let url = "db:postgres://" <> user <> ":" <> password <> "@" <> host <> ":" <> show port
   in runProcess_ $ proc "primer-sqitch" ["deploy", "--verify", url]

withDbSetup :: (Connection -> IO ()) -> IO ()
withDbSetup f =
  let throwEither x = either throwIO pure =<< x
      dbConfig =
        optionsToDefaultConfig
          mempty
            { Options.port = pure port
            , Options.user = pure user
            , Options.password = pure password
            , Options.host = pure host
            }
   in do
        throwEither $
          withSystemTempDirectory "primer-tmp-postgres" $ \tmpdir ->
            let cc =
                  defaultCacheConfig
                    { cacheTemporaryDirectory = tmpdir
                    , cacheDirectoryType = Temporary
                    }
             in withDbCacheConfig cc $ \dbCache ->
                  let combinedConfig = dbConfig <> cacheConfig dbCache
                   in do
                        migratedConfig <- throwEither $ cacheAction (tmpdir <> "/primer-rel8") deployDb combinedConfig
                        withConfig migratedConfig $ \db ->
                          bracket (connectDb db) release f

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

anyException :: ExceptionPredicate SomeException
anyException = const True

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
