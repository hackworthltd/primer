{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Pagination where

import Foreword

import Data.String (String)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple.Options qualified as Options
import Database.Postgres.Temp (
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
import Hasql.Pool (
  Pool,
  acquire,
  release,
 )
import Primer.App (newApp)
import Primer.Database (
  LastModified (..),
  Session (Session),
  insertSession,
  listSessions,
  safeMkSessionName,
 )
import Primer.Database.Rel8 (
  SessionRow (SessionRow, app, gitversion, lastmodified, name, uuid),
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
import Primer.Database.Rel8.Test.Util (
  deployDb,
  lowPrecisionCurrentTime,
  runTmpDb,
 )
import Primer.Test.Util ((@?=))
import Rel8 (Result)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.HUnit qualified as HUnit

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

-- Note: this action is ever so slightly different than the one in
-- primer-rel8-testlib, mainly because the latter is designed for
-- testing sqitch migrations, and here we don't need to worry about
-- that.
withSetup :: (Pool -> IO ()) -> IO ()
withSetup f =
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
                        migratedConfig <- throwEither $ cacheAction (tmpdir <> "/pagination") (deployDb port) combinedConfig
                        withConfig migratedConfig $ \db ->
                          bracket (acquire 1 (Just 1000000) $ toConnectionString db) release f

mkSession :: Int -> IO (SessionRow Result)
mkSession n = do
  u <- nextRandom
  now <- lowPrecisionCurrentTime
  pure $
    SessionRow
      { uuid = u
      , gitversion = "test-version"
      , app = newApp
      , name = "name-" <> show n
      , lastmodified = utcTime now
      }

test_pagination :: TestTree
test_pagination = testCaseSteps "pagination" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let m = 345
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse mkSession [1 .. m]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
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
