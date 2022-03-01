module TestUtils (
  (@?=),
  withDbSetup,
) where

import Foreword (
  Alternative (empty),
  Applicative (pure),
  Eq,
  IO,
  Int,
  MonadIO (..),
  Monoid (mempty),
  Semigroup ((<>)),
  Show,
  bracket,
  decodeUtf8,
  either,
  maybe,
  show,
  throwIO,
  ($),
  (.),
  (=<<),
  (>=>),
 )

import Data.String (String)
import Data.Text (unpack)
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
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  proc,
  runProcess_,
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
