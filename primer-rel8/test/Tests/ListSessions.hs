{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.ListSessions where

import Foreword

import Data.UUID.V4 (nextRandom)
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
 )
import Rel8 (Result)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  runTmpDb,
  (@?=),
 )

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

test_listSessions :: TestTree
test_listSessions = testCaseSteps "listSessions" $ \step' ->
  runTmpDb $ do
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
    step "Get 25"
    p25 <- listSessions $ OL{offset = 0, limit = Just 25}
    total p25 @?= m
    pageContents p25 @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (take 25 rows)
    step "Get 76-100"
    p75 <- listSessions $ OL{offset = 75, limit = Just 25}
    total p75 @?= m
    pageContents p75 @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (take 25 $ drop 75 rows)
    step "Get crossing end"
    pLast <- listSessions $ OL{offset = m - 10, limit = Just 25}
    total pLast @?= m
    pageContents pLast @?= map (\r -> Session (uuid r) (safeMkSessionName $ name r)) (drop (m - 10) rows)
