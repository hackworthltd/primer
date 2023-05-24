{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.FindSessions where

import Foreword

import Data.Text qualified as Text
import Primer.App (newApp)
import Primer.Database (
  LastModified (..),
  OffsetLimit (OL, limit, offset),
  Page (pageContents, total),
  Session (Session),
  findSessions,
  fromSessionName,
  insertSession,
  safeMkSessionName,
 )
import Primer.Database.Selda.SQLite (
  SessionRow (SessionRow, app, gitversion, lastmodified, name, uuid),
 )
import Primer.Database.Selda.Test.Util (
  mkSessionRow,
  mkSessionRow',
  runTmpDb,
 )
import Primer.Test.Util ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)

test_findSessions_several :: TestTree
test_findSessions_several = testCaseSteps "findSessions several hits" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "-30"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse mkSessionRow [1 .. 400]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 11
    pageContents pAll @?= expectedRows
    step $ "Find first 5 occurrences of " <> show substr <> " in session names"
    p5 <- findSessions substr $ OL{offset = 0, limit = Just 5}
    total p5 @?= 11
    pageContents p5 @?= take 5 expectedRows
    step $ "Find occurrences 6-10 of " <> show substr <> " in session names"
    p10 <- findSessions substr $ OL{offset = 5, limit = Just 5}
    total p10 @?= 11
    pageContents p10 @?= take 5 (drop 5 expectedRows)
    step $ "Find last occurrence of " <> show substr <> " in session names, crossing end"
    pLast <- findSessions substr $ OL{offset = 10, limit = Just 5}
    total pLast @?= 11
    pageContents pLast @?= drop 10 expectedRows

test_findSessions_exactly_1 :: TestTree
test_findSessions_exactly_1 = testCaseSteps "findSessions exactly 1 hit" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "-300"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse mkSessionRow [1 .. 400]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 1
    pageContents pAll @?= expectedRows
    step $ "Find all occurrences of " <> show substr <> " in session names (limit 50)"
    p50 <- findSessions substr $ OL{offset = 0, limit = Just 50}
    total p50 @?= 1
    pageContents p50 @?= expectedRows

test_findSessions_none :: TestTree
test_findSessions_none = testCaseSteps "findSessions no hits" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "-401"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse mkSessionRow [1 .. 400]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 0
    pageContents pAll @?= expectedRows
    step $ "Find all occurrences of " <> show substr <> " in session names (limit 50)"
    p50 <- findSessions substr $ OL{offset = 0, limit = Just 50}
    total p50 @?= 0
    pageContents p50 @?= expectedRows

test_findSessions_case_sensitive :: TestTree
test_findSessions_case_sensitive = testCaseSteps "findSessions is case sensitive" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "name"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse (mkSessionRow' mkName) [1 .. 6]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 3
    pageContents pAll @?= expectedRows
  where
    mkName n = if even n then "name-" <> show n else "NaMe-" <> show n

test_findSessions_unicode :: TestTree
test_findSessions_unicode = testCaseSteps "findSessions supports Unicode" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "ありがとう"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse (mkSessionRow' mkName) [1 .. 5]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 2
    pageContents pAll @?= expectedRows
  where
    mkName 0 = "Thank you"
    mkName 1 = "ありがとう"
    mkName 3 = "Merci"
    mkName 4 = "ありがとうございます"
    mkName 5 = "Danke"
    mkName _ = "Gracias"

test_findSessions_emoji :: TestTree
test_findSessions_emoji = testCaseSteps "findSessions supports emoji" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    let substr = "🤗😂"
    step "Insert all sessions"
    rows <- liftIO $ sortOn name <$> traverse (mkSessionRow' mkName) [1 .. 7]
    forM_ rows (\SessionRow{..} -> insertSession gitversion uuid newApp (safeMkSessionName name) (LastModified lastmodified))
    let expectedRows = filter (\(Session _ n _) -> substr `Text.isInfixOf` fromSessionName n) $ map (\r -> Session (uuid r) (safeMkSessionName $ name r) (LastModified $ lastmodified r)) rows
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAll <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAll @?= 3
    pageContents pAll @?= expectedRows
  where
    mkName 1 = "🤗"
    mkName 2 = "😂"
    mkName 3 = "🤗😂"
    mkName 4 = "😄😂🤣🤗 🦊 🦈"
    mkName 5 = "🤗😂😂"
    mkName 6 = "🤗🤗😂"
    mkName 7 = "🤗🤗😄"
    mkName _ = "👍🏽"

-- Note: no escape checks, as Selda doesn't currently support escaping
-- in LIKE queries.
