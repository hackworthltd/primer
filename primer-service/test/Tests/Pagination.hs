{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Pagination where

import Foreword

import Control.Monad.Extra (eitherM)
import Data.String (String)
import Data.Text (unpack)
import Data.UUID.V4 (nextRandom)
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
import Primer.App (newApp)
import Primer.Database (
  Session (Session),
  insertSession,
  listSessions,
  safeMkSessionName,
 )
import Primer.Database.Rel8 (
  SessionRow (SessionRow, app, gitversion, name, uuid),
  runRel8Db,
 )
import Primer.Pagination (
  Pagination (Pagination, page, size),
  firstPage,
  getNonNeg,
  getPositive,
  items,
  lastPage,
  meta,
  mkPositive,
  nextPage,
  pageSize,
  pagedDefaultClamp,
  prevPage,
  thisPage,
  totalItems,
 )
import Rel8 (Result)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (
  proc,
  runProcess_,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import qualified Test.Tasty.HUnit as HUnit

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
x @?= y = liftIO $ x HUnit.@?= y
infix 1 @?=

assertFailure :: MonadIO m => Text -> m a
assertFailure = liftIO . HUnit.assertFailure . toS

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

withSetup :: (Connection -> IO ()) -> IO ()
withSetup f =
  let throwEither = eitherM throwIO pure
      dbConfig =
        optionsToDefaultConfig
          mempty
            { Options.port = pure port
            , Options.user = pure user
            , Options.password = pure password
            , Options.host = pure host
            }
   in do
        throwEither . withSystemTempDirectory "primer-tmp-postgres" $ \tmpdir ->
          let cc =
                defaultCacheConfig
                  { cacheTemporaryDirectory = tmpdir
                  , cacheDirectoryType = Temporary
                  }
           in withDbCacheConfig cc $ \dbCache ->
                let combinedConfig = dbConfig <> cacheConfig dbCache
                 in do
                      migratedConfig <- throwEither $ cacheAction (tmpdir <> "/pagination") deployDb combinedConfig
                      withConfig migratedConfig $ \db ->
                        bracket (connectDb db) release f

mkSession :: Int -> IO (SessionRow Result)
mkSession n = do
  u <- nextRandom
  pure $
    SessionRow
      { uuid = u
      , gitversion = "test-version"
      , app = newApp
      , name = "name-" <> show n
      }

test_pagination :: TestTree
test_pagination = testCaseSteps "pagination" $ \step' ->
  withSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'
      let m = 345
      step "Insert all sessions"
      rows <- liftIO $ sortOn name <$> traverse mkSession [1 .. m]
      forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name))
      let expectedRows = fmap (\r -> Session (uuid r) (safeMkSessionName $ name r)) rows
      step "Get all, paged"
      onePos <- maybe (assertFailure "1 is positive") pure $ mkPositive 1
      pAllPaged <- pagedDefaultClamp (m + 2) (Pagination{page = onePos, size = Nothing}) listSessions
      getMeta pAllPaged @?= (m, m + 2, 1, Nothing, 1, Nothing, 1)
      items pAllPaged @?= expectedRows
      step "Get 25, paged"
      p25Paged <- pagedDefaultClamp (m + 2) (Pagination{page = onePos, size = mkPositive 25}) listSessions
      getMeta p25Paged @?= (m, 25, 1, Nothing, 1, Just 2, 14)
      items p25Paged @?= take 25 expectedRows
      step "Get 76-100, paged"
      fourPos <- maybe (assertFailure "4 is positive") pure $ mkPositive 4
      p75Paged <- pagedDefaultClamp (m + 2) (Pagination{page = fourPos, size = mkPositive 25}) listSessions
      getMeta p75Paged @?= (m, 25, 1, Just 3, 4, Just 5, 14)
      items p75Paged @?= take 25 (drop 75 expectedRows)
      step "Get crossing end, paged"
      fourteenPos <- maybe (assertFailure "14 is positive") pure $ mkPositive 14
      pLastPaged <- pagedDefaultClamp (m + 2) (Pagination{page = fourteenPos, size = mkPositive 25}) listSessions
      getMeta pLastPaged @?= (m, 25, 1, Just 13, 14, Nothing, 14)
      items pLastPaged @?= drop 325 expectedRows
  where
    -- Returns (totalItems,pageSize,firstPage,prevPage,thisPage,nextPage,lastPage) as Int/Maybe Int
    getMeta p =
      let m = meta p
       in ( getNonNeg $ totalItems m
          , getPositive $ pageSize m
          , getPositive $ firstPage m
          , getPositive <$> prevPage m
          , getPositive $ thisPage m
          , getPositive <$> nextPage m
          , getPositive $ lastPage m
          )
