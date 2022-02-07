{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Pagination where

import Foreword

import Data.Aeson (encode)
import Data.Text (unpack)
import Data.UUID.V4 (nextRandom)
import qualified Database.Postgres.Temp as TempDb
import GHC.Err (error)
import Hasql.Connection (
  Connection,
  acquire,
  release,
 )
import Hasql.Session (run, sql)
import Primer.App (newApp)
import Primer.Database (
  OffsetLimit (OL, limit, offset),
  Page (pageContents, total),
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
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import qualified Test.Tasty.HUnit as HUnit

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
x @?= y = liftIO $ x HUnit.@?= y
infix 1 @?=

assertFailure :: MonadIO m => Text -> m a
assertFailure = liftIO . HUnit.assertFailure . toS

mkSession :: Int -> IO (SessionRow Result)
mkSession n = do
  u <- nextRandom
  pure $
    SessionRow
      { uuid = u
      , gitversion = "test-version"
      , app = encode newApp
      , name = "name-" <> show n
      }

test_pagination :: TestTree
test_pagination = testCaseSteps "pagination" $ \step' ->
  bracket startDb TempDb.stop $ \db -> do
    bracket (connect db) release $ \conn -> do
      void $ flip run conn do
        -- TODO: provide a way to initialize the database from
        -- "Primer.Database.Rel8", rather than just hard-wiring it here.
        --
        -- https://github.com/hackworthltd/primer/issues/242
        liftIO $ step' "Create schema"
        sql "CREATE TABLE public.sessions ( uuid uuid NOT NULL, gitversion text NOT NULL, app bytea NOT NULL, name text NOT NULL )"
        sql "ALTER TABLE ONLY public.sessions ADD CONSTRAINT sessions_pkey PRIMARY KEY (uuid)"
      flip runRel8Db conn $ do
        let step = liftIO . step'
        let m = 345
        step "Insert all sessions"
        rows <- liftIO $ sortOn name <$> traverse mkSession [1 .. m]
        forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name))
        let expectedRows = map (\r -> Session (uuid r) (safeMkSessionName $ name r)) rows
        step "Get all, offset+limit"
        pAll <- listSessions $ OL{offset = 0, limit = Nothing}
        total pAll @?= m
        pageContents pAll @?= expectedRows
        step "Get all, paged"
        onePos <- maybe (assertFailure "1 is positive") pure $ mkPositive 1
        pAllPaged <- pagedDefaultClamp (m + 2) (Pagination{page = onePos, size = Nothing}) listSessions
        getMeta pAllPaged @?= (m, m + 2, 1, Nothing, 1, Nothing, 1)
        items pAllPaged @?= expectedRows
        step "Get 25"
        p25 <- listSessions $ OL{offset = 0, limit = Just 25}
        total p25 @?= m
        pageContents p25 @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (take 25 rows)
        step "Get 25, paged"
        p25Paged <- pagedDefaultClamp (m + 2) (Pagination{page = onePos, size = mkPositive 25}) listSessions
        getMeta p25Paged @?= (m, 25, 1, Nothing, 1, Just 2, 14)
        items p25Paged @?= take 25 expectedRows
        step "Get 76-100"
        p75 <- listSessions $ OL{offset = 75, limit = Just 25}
        total p75 @?= m
        pageContents p75 @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (take 25 $ drop 75 rows)
        step "Get 76-100, paged"
        fourPos <- maybe (assertFailure "4 is positive") pure $ mkPositive 4
        p75Paged <- pagedDefaultClamp (m + 2) (Pagination{page = fourPos, size = mkPositive 25}) listSessions
        getMeta p75Paged @?= (m, 25, 1, Just 3, 4, Just 5, 14)
        items p75Paged @?= take 25 (drop 75 expectedRows)
        step "Get crossing end"
        pLast <- listSessions $ OL{offset = m - 10, limit = Just 25}
        total pLast @?= m
        pageContents pLast @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (drop (m - 10) rows)
        step "Get crossing end, paged"
        fourteenPos <- maybe (assertFailure "14 is positive") pure $ mkPositive 14
        pLastPaged <- pagedDefaultClamp (m + 2) (Pagination{page = fourteenPos, size = mkPositive 25}) listSessions
        getMeta pLastPaged @?= (m, 25, 1, Just 13, 14, Nothing, 14)
        items pLastPaged @?= drop 325 expectedRows
  where
    connect :: TempDb.DB -> IO Connection
    connect = acquire . TempDb.toConnectionString >=> either (maybe empty (error . unpack . decodeUtf8)) pure
    startDb = TempDb.start >>= either throwIO return
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
