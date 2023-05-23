module Tests.NullDb where

import Foreword

import Data.Text qualified as Text
import Data.UUID.V4 (nextRandom)
import Primer.App (
  newApp,
  newEmptyApp,
 )
import Primer.Database (
  DbError (..),
  MonadDb (..),
  NullDbException (..),
  OffsetLimit (OL, limit, offset),
  Page (pageContents, total),
  Session (..),
  SessionData (..),
  SessionId,
  fromSessionName,
  getCurrentTime,
  newSessionId,
  runNullDb',
  safeMkSessionName,
 )
import Primer.Examples (
  even3App,
  mapOddApp,
 )
import Primer.Test.Util (
  assertException,
  (@?=),
 )
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)

-- 'Primer.Database.NullDbT' is only used to implement tests, but we
-- test it here to ensure that any tests built on top of it should
-- work.

expectedError :: SessionId -> NullDbException -> Bool
expectedError _ _ = True

test_insertSession_roundtrip :: TestTree
test_insertSession_roundtrip = testCaseSteps "insertSession database round-tripping" $ \step' ->
  runNullDb' $ do
    now1 <- getCurrentTime
    let step = liftIO . step'
    step "Insert even3App"
    let version = "git123"
    let name = safeMkSessionName "even3App"
    sessionId <- liftIO newSessionId
    insertSession version sessionId even3App name now1

    step "Retrieve it"
    result <- querySessionId sessionId
    result @?= Right (SessionData even3App name now1)

    now2 <- getCurrentTime
    let jpName = safeMkSessionName "サンプルプログラム"
    step "Insert mapOddApp with Japanese name"
    sid1 <- liftIO newSessionId
    insertSession version sid1 mapOddApp jpName now2
    r1 <- querySessionId sid1
    r1 @?= Right (SessionData mapOddApp jpName now2)

    now3 <- getCurrentTime
    let cnName = safeMkSessionName "示例程序"
    step "Insert even3App with simplified Chinese name"
    sid2 <- liftIO newSessionId
    insertSession version sid2 even3App cnName now3
    r2 <- querySessionId sid2
    r2 @?= Right (SessionData even3App cnName now3)

    now4 <- getCurrentTime
    let arName = safeMkSessionName "برنامج مثال"
    step "Insert mapOddApp with Arabic name"
    sid3 <- liftIO newSessionId
    insertSession version sid3 mapOddApp arName now4
    r3 <- querySessionId sid3
    r3 @?= Right (SessionData mapOddApp arName now4)

    now5 <- getCurrentTime
    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    step "Insert even3App with emoji name"
    sid4 <- liftIO newSessionId
    insertSession version sid4 even3App emName now5
    r4 <- querySessionId sid4
    r4 @?= Right (SessionData even3App emName now5)

test_insertSession_failure :: TestTree
test_insertSession_failure = testCaseSteps "insertSession failure modes" $ \step' ->
  runNullDb' $ do
    now <- getCurrentTime
    let step = liftIO . step'

    step "Insert program"
    let version = "git123"
    let name = safeMkSessionName "testApp"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name now

    step "Attempt to insert the same program and metadata again"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId mapOddApp name now

    step "Attempt to insert a different program with the same metadata"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId newEmptyApp name now

    step "Attempt to insert the same program with a different version"
    let newVersion = "new-" <> version
    assertException "insertSession" (expectedError sessionId) $ insertSession newVersion sessionId mapOddApp name now

    step "Attempt to insert the same program with a different name"
    let newName = safeMkSessionName "new name"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId mapOddApp newName now

    step "Attempt to insert the same program with a different timestamp"
    now' <- getCurrentTime
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId mapOddApp name now'

mkSession :: Int -> IO (SessionId, SessionData)
mkSession n = do
  u <- nextRandom
  now <- getCurrentTime
  pure (u, SessionData newApp (safeMkSessionName $ "name-" <> show n) now)

mkSession' :: (Int -> Text) -> Int -> IO (SessionId, SessionData)
mkSession' mkName n = do
  u <- nextRandom
  now <- getCurrentTime
  pure (u, SessionData newApp (safeMkSessionName $ mkName n) now)

test_listSessions :: TestTree
test_listSessions = testCaseSteps "listSessions" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let m = 345
    let version = "git123"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse mkSession [1 .. m]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) rows
    step "Get all, offset+limit"
    pAll <- listSessions $ OL{offset = 0, limit = Nothing}
    total pAll @?= m
    pageContents pAll @?= expectedRows
    step "Get 25"
    p25 <- listSessions $ OL{offset = 0, limit = Just 25}
    total p25 @?= m
    pageContents p25 @?= take 25 expectedRows
    step "Get 76-100"
    p75 <- listSessions $ OL{offset = 75, limit = Just 25}
    total p75 @?= m
    pageContents p75 @?= take 25 (drop 75 expectedRows)
    step "Get crossing end"
    pLast <- listSessions $ OL{offset = m - 10, limit = Just 25}
    total pLast @?= m
    pageContents pLast @?= drop (m - 10) expectedRows

test_findSessions_several :: TestTree
test_findSessions_several = testCaseSteps "findSessions several hits" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "-30"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse mkSession [1 .. 400]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let filteredResults = filter (\(_, SessionData _ n _) -> substr `Text.isInfixOf` fromSessionName n) rows
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) filteredResults
    step "Get all, offset+limit"
    pAllMatches <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAllMatches @?= 11
    pageContents pAllMatches @?= expectedRows
    step "Get 5"
    p5 <- findSessions substr $ OL{offset = 0, limit = Just 5}
    total p5 @?= 11
    pageContents p5 @?= take 5 expectedRows
    step "Get 6-10"
    p10 <- findSessions substr $ OL{offset = 5, limit = Just 5}
    total p10 @?= 11
    pageContents p10 @?= take 5 (drop 5 expectedRows)
    step "Get crossing end"
    pLast <- findSessions substr $ OL{offset = 10, limit = Just 5}
    total pLast @?= 11
    pageContents pLast @?= drop 10 expectedRows

test_findSessions_exactly_1 :: TestTree
test_findSessions_exactly_1 = testCaseSteps "findSessions exactly 1 hit" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "-300"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse mkSession [1 .. 400]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let filteredResults = filter (\(_, SessionData _ n _) -> substr `Text.isInfixOf` fromSessionName n) rows
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) filteredResults
    step "Get all, offset+limit"
    pAllMatches <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAllMatches @?= 1
    pageContents pAllMatches @?= expectedRows
    step "Get 50"
    p50 <- findSessions substr $ OL{offset = 0, limit = Just 50}
    total p50 @?= 1
    pageContents p50 @?= expectedRows

test_findSessions_none :: TestTree
test_findSessions_none = testCaseSteps "findSessions no hits" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "-401"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse mkSession [1 .. 400]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let expectedRows = []
    step "Get all, offset+limit"
    pAllMatches <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAllMatches @?= 0
    pageContents pAllMatches @?= expectedRows
    step "Get 50"
    p50 <- findSessions substr $ OL{offset = 0, limit = Just 50}
    total p50 @?= 0
    pageContents p50 @?= expectedRows

test_findSessions_case_sensitive :: TestTree
test_findSessions_case_sensitive = testCaseSteps "findSessions is case sensitive" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "name"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse (mkSession' evenOddName) [1 .. 6]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let filteredResults = filter (\(_, SessionData _ n _) -> substr `Text.isInfixOf` fromSessionName n) rows
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) filteredResults
    step $ "Find all occurrences of " <> show substr <> " in session names (no limit)"
    pAllMatches <- findSessions substr $ OL{offset = 0, limit = Nothing}
    total pAllMatches @?= 3
    pageContents pAllMatches @?= expectedRows
  where
    evenOddName n = if even n then "name-" <> show n else "NaMe-" <> show n

test_findSessions_unicode :: TestTree
test_findSessions_unicode = testCaseSteps "findSessions supports Unicode" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "ありがとう"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse (mkSession' mkName) [1 .. 5]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let filteredResults = filter (\(_, SessionData _ n _) -> substr `Text.isInfixOf` fromSessionName n) rows
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) filteredResults
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
  runNullDb' $ do
    let step = liftIO . step'
    let version = "git123"
    let substr = "🤗😂"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse (mkSession' mkName) [1 .. 7]
    forM_ rows (\(id_, SessionData app name now) -> insertSession version id_ app name now)
    let filteredResults = filter (\(_, SessionData _ n _) -> substr `Text.isInfixOf` fromSessionName n) rows
    let expectedRows = map (\(i, SessionData _ n t) -> Session i n t) filteredResults
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

test_updateSessionApp_roundtrip :: TestTree
test_updateSessionApp_roundtrip = testCaseSteps "updateSessionApp database round-tripping" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert a new session"
    now <- getCurrentTime
    let version = "git123"
    let name = safeMkSessionName "new app"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name now

    step "Update it with the same version and app"
    updateSessionApp version sessionId mapOddApp now
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData mapOddApp name now)

    step "Update it with a new version, but the same app"
    let newVersion = "new-" <> version
    updateSessionApp newVersion sessionId mapOddApp now
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData mapOddApp name now)

    step "Update it with a new app"
    updateSessionApp newVersion sessionId even3App now
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData even3App name now)

    step "Update it with a new timestamp"
    now' <- getCurrentTime
    updateSessionApp newVersion sessionId even3App now'
    r4 <- querySessionId sessionId
    r4 @?= Right (SessionData even3App name now')

test_updateSessionApp_failure :: TestTree
test_updateSessionApp_failure = testCaseSteps "updateSessionApp failure modes" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    now <- getCurrentTime
    let version = "git123"
    sessionId <- liftIO newSessionId
    assertException "updateSessionApp" (expectedError sessionId) $ updateSessionApp version sessionId newApp now

test_updateSessionName_roundtrip :: TestTree
test_updateSessionName_roundtrip = testCaseSteps "updateSessionName database round-tripping" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert a new session"
    now <- getCurrentTime
    let version = "git123"
    let name = safeMkSessionName "new app"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name now

    step "Update it with the same version and name"
    updateSessionName version sessionId name now
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData mapOddApp name now)

    step "Update it with a new version, but the same name"
    let newVersion = "new-" <> version
    updateSessionName newVersion sessionId name now
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData mapOddApp name now)

    step "Update it with a new name"
    let newName = safeMkSessionName "new new app"
    updateSessionName newVersion sessionId newName now
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData mapOddApp newName now)

    step "Update it with a new timestamp"
    now' <- getCurrentTime
    updateSessionName newVersion sessionId newName now'
    r4 <- querySessionId sessionId
    r4 @?= Right (SessionData mapOddApp newName now')

    step "Update it with a Japanese name"
    let jpName = safeMkSessionName "サンプルプログラム"
    updateSessionName newVersion sessionId jpName now'
    r5 <- querySessionId sessionId
    r5 @?= Right (SessionData mapOddApp jpName now')

    step "Update it with a simplified Chinese name"
    let cnName = safeMkSessionName "示例程序"
    updateSessionName newVersion sessionId cnName now'
    r6 <- querySessionId sessionId
    r6 @?= Right (SessionData mapOddApp cnName now')

    step "Update it with an Arabic name"
    let arName = safeMkSessionName "برنامج مثال"
    updateSessionName newVersion sessionId arName now'
    r7 <- querySessionId sessionId
    r7 @?= Right (SessionData mapOddApp arName now')

    step "Update it with an emoji name"
    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    updateSessionName newVersion sessionId emName now'
    r8 <- querySessionId sessionId
    r8 @?= Right (SessionData mapOddApp emName now')

test_updateSessionName_failure :: TestTree
test_updateSessionName_failure = testCaseSteps "updateSessionName failure modes" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    now <- getCurrentTime
    let version = "git123"
    let name = safeMkSessionName "this session doesn't exist"
    sessionId <- liftIO newSessionId
    assertException "updateSessionName" (expectedError sessionId) $ updateSessionName version sessionId name now

test_querySessionId :: TestTree
test_querySessionId = testCaseSteps "querySessionId corner cases" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert program"
    now <- getCurrentTime
    let version = "git123"
    let name = safeMkSessionName "test querySessionId"
    sessionId <- liftIO newSessionId
    insertSession version sessionId newApp name now

    step "Attempt to look up a session that doesn't exist"
    nonexistentSessionId <- liftIO newSessionId
    r1 <- querySessionId nonexistentSessionId
    r1 @?= Left (SessionIdNotFound nonexistentSessionId)

test_deleteSession :: TestTree
test_deleteSession = testCaseSteps "deleteSession" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert program"
    now <- getCurrentTime
    let version = "git123"
    let name = safeMkSessionName "test deleteSession"
    sessionId <- liftIO newSessionId
    insertSession version sessionId newApp name now

    step "Delete the session"
    r1 <- deleteSession sessionId
    r1 @?= Right ()

    step "Ensure the session has been deleted"
    r2 <- querySessionId sessionId
    r2 @?= Left (SessionIdNotFound sessionId)

    step "Try to delete the session again"
    r3 <- deleteSession sessionId
    r3 @?= Left (SessionIdNotFound sessionId)

    step "Try to delete a non-existent session"
    nonexistentSessionId <- liftIO newSessionId
    r4 <- deleteSession nonexistentSessionId
    r4 @?= Left (SessionIdNotFound nonexistentSessionId)

    step "Insert another new program"
    let name2 = safeMkSessionName "test deleteSession 2"
    sessionId2 <- liftIO newSessionId
    insertSession version sessionId2 newApp name2 now

    step "Delete the new session"
    r5 <- deleteSession sessionId2
    r5 @?= Right ()

    step "Attempt to update the deleted session"
    now' <- getCurrentTime
    assertException "deleteSession" (expectedError sessionId2) $ updateSessionApp version sessionId2 newApp now'
