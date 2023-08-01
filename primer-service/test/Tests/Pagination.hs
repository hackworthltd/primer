{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Pagination where

import Foreword

import Primer.App (newApp)
import Primer.Database (
  LastModified (..),
  Session (Session),
  insertSession,
  listSessions,
  safeMkSessionName,
 )
import Primer.Database.Selda.SQLite (
  SessionRow (..),
 )
import Primer.Database.Selda.Test.Util (
  mkSessionRow,
  runTmpDb,
 )
import Primer.Finite (packFinite)
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
  pagedDefault,
  prevPage,
  thisPage,
  totalItems,
 )
import Primer.Test.Util ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import Test.Tasty.HUnit qualified as HUnit

assertFailure :: MonadIO m => Text -> m a
assertFailure = liftIO . HUnit.assertFailure . toS

test_pagination :: TestTree
test_pagination = testCaseSteps "pagination" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let m = 345
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse mkSessionRow [1 .. m]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step "Get all, paged"
    onePos <- maybe (assertFailure "1 is positive") pure $ mkPositive 1
    pAllPaged <- pagedDefault (m + 2) (Pagination{page = onePos, size = Nothing}) listSessions
    getMeta pAllPaged @?= (m, m + 2, 1, Nothing, 1, Nothing, 1)
    items pAllPaged @?= expectedRows
    step "Get 25, paged"
    p25Paged <- pagedDefault (m + 2) (Pagination{page = onePos, size = packFinite 25}) listSessions
    getMeta p25Paged @?= (m, 25, 1, Nothing, 1, Just 2, 14)
    items p25Paged @?= take 25 expectedRows
    step "Get 76-100, paged"
    fourPos <- maybe (assertFailure "4 is positive") pure $ mkPositive 4
    p75Paged <- pagedDefault (m + 2) (Pagination{page = fourPos, size = packFinite 25}) listSessions
    getMeta p75Paged @?= (m, 25, 1, Just 3, 4, Just 5, 14)
    items p75Paged @?= take 25 (drop 75 expectedRows)
    step "Get crossing end, paged"
    fourteenPos <- maybe (assertFailure "14 is positive") pure $ mkPositive 14
    pLastPaged <- pagedDefault (m + 2) (Pagination{page = fourteenPos, size = packFinite 25}) listSessions
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
