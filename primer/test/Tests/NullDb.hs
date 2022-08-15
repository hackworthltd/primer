module Tests.NullDb where

import Foreword

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
  newSessionId,
  runNullDb',
  safeMkSessionName,
 )
import Primer.Examples (
  even3App,
  mapOddApp,
 )
import Test.Tasty
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  (@?=),
 )

-- 'Primer.Database.NullDbT' is only used to implement tests, but we
-- test it here to ensure that any tests built on top of it should
-- work.

expectedError :: SessionId -> NullDbException -> Bool
expectedError _ _ = True

test_insertSession_roundtrip :: TestTree
test_insertSession_roundtrip = testCaseSteps "insertSession database round-tripping" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    step "Insert even3App"
    let version = "git123"
    let name = safeMkSessionName "even3App"
    sessionId <- liftIO newSessionId
    insertSession version sessionId even3App name

    step "Retrieve it"
    result <- querySessionId sessionId
    result @?= Right (SessionData even3App name)

    let jpName = safeMkSessionName "サンプルプログラム"
    step "Insert mapOddApp with Japanese name"
    sid1 <- liftIO newSessionId
    insertSession version sid1 mapOddApp jpName
    r1 <- querySessionId sid1
    r1 @?= Right (SessionData mapOddApp jpName)

    let cnName = safeMkSessionName "示例程序"
    step "Insert even3App with simplified Chinese name"
    sid2 <- liftIO newSessionId
    insertSession version sid2 even3App cnName
    r2 <- querySessionId sid2
    r2 @?= Right (SessionData even3App cnName)

    let arName = safeMkSessionName "برنامج مثال"
    step "Insert mapOddApp with Arabic name"
    sid3 <- liftIO newSessionId
    insertSession version sid3 mapOddApp arName
    r3 <- querySessionId sid3
    r3 @?= Right (SessionData mapOddApp arName)

    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    step "Insert even3App with emoji name"
    sid4 <- liftIO newSessionId
    insertSession version sid4 even3App emName
    r4 <- querySessionId sid4
    r4 @?= Right (SessionData even3App emName)

test_insertSession_failure :: TestTree
test_insertSession_failure = testCaseSteps "insertSession failure modes" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert program"
    let version = "git123"
    let name = safeMkSessionName "testApp"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name

    step "Attempt to insert the same program and metadata again"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId mapOddApp name

    step "Attempt to insert a different program with the same metadata"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId newEmptyApp name

    step "Attempt to insert the same program with a different version"
    let newVersion = "new-" <> version
    assertException "insertSession" (expectedError sessionId) $ insertSession newVersion sessionId mapOddApp name

    step "Attempt to insert the same program with a different name"
    let newName = safeMkSessionName "new name"
    assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId mapOddApp newName

mkSession :: Int -> IO (SessionId, SessionData)
mkSession n = do
  u <- nextRandom
  pure (u, SessionData newApp $ safeMkSessionName $ "name-" <> show n)

test_listSessions :: TestTree
test_listSessions = testCaseSteps "listSessions" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'
    let m = 345
    let version = "git123"
    step "Insert all sessions"
    rows <- liftIO $ sortOn (sessionName . snd) <$> traverse mkSession [1 .. m]
    forM_ rows (\(id_, SessionData app name) -> insertSession version id_ app name)
    let expectedRows = map (\r -> Session (fst r) ((sessionName . snd) r)) rows
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

test_updateSessionApp_roundtrip :: TestTree
test_updateSessionApp_roundtrip = testCaseSteps "updateSessionApp database round-tripping" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert a new session"
    let version = "git123"
    let name = safeMkSessionName "new app"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name

    step "Update it with the same version and app"
    updateSessionApp version sessionId mapOddApp
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData mapOddApp name)

    step "Update it with a new version, but the same app"
    let newVersion = "new-" <> version
    updateSessionApp newVersion sessionId mapOddApp
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData mapOddApp name)

    step "Update it with a new app"
    updateSessionApp newVersion sessionId even3App
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData even3App name)

test_updateSessionApp_failure :: TestTree
test_updateSessionApp_failure = testCaseSteps "updateSessionApp failure modes" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    let version = "git123"
    sessionId <- liftIO newSessionId
    assertException "updateSessionApp" (expectedError sessionId) $ updateSessionApp version sessionId newApp

test_updateSessionName_roundtrip :: TestTree
test_updateSessionName_roundtrip = testCaseSteps "updateSessionName database round-tripping" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert a new session"
    let version = "git123"
    let name = safeMkSessionName "new app"
    sessionId <- liftIO newSessionId
    insertSession version sessionId mapOddApp name

    step "Update it with the same version and name"
    updateSessionName version sessionId name
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData mapOddApp name)

    step "Update it with a new version, but the same name"
    let newVersion = "new-" <> version
    updateSessionName newVersion sessionId name
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData mapOddApp name)

    step "Update it with a new name"
    let newName = safeMkSessionName "new new app"
    updateSessionName newVersion sessionId newName
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData mapOddApp newName)

    step "Update it with a Japanese name"
    let jpName = safeMkSessionName "サンプルプログラム"
    updateSessionName newVersion sessionId jpName
    r4 <- querySessionId sessionId
    r4 @?= Right (SessionData mapOddApp jpName)

    step "Update it with a simplified Chinese name"
    let cnName = safeMkSessionName "示例程序"
    updateSessionName newVersion sessionId cnName
    r5 <- querySessionId sessionId
    r5 @?= Right (SessionData mapOddApp cnName)

    step "Update it with an Arabic name"
    let arName = safeMkSessionName "برنامج مثال"
    updateSessionName newVersion sessionId arName
    r6 <- querySessionId sessionId
    r6 @?= Right (SessionData mapOddApp arName)

    step "Update it with an emoji name"
    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    updateSessionName newVersion sessionId emName
    r7 <- querySessionId sessionId
    r7 @?= Right (SessionData mapOddApp emName)

test_updateSessionName_failure :: TestTree
test_updateSessionName_failure = testCaseSteps "updateSessionName failure modes" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    let version = "git123"
    let name = safeMkSessionName "this session doesn't exist"
    sessionId <- liftIO newSessionId
    assertException "updateSessionName" (expectedError sessionId) $ updateSessionName version sessionId name

test_querySessionId :: TestTree
test_querySessionId = testCaseSteps "querySessionId corner cases" $ \step' ->
  runNullDb' $ do
    let step = liftIO . step'

    step "Insert program"
    let version = "git123"
    let name = safeMkSessionName "test querySessionId"
    sessionId <- liftIO newSessionId
    insertSession version sessionId newApp name

    step "Attempt to look up a session that doesn't exist"
    nonexistentSessionId <- liftIO newSessionId
    r1 <- querySessionId nonexistentSessionId
    r1 @?= Left (SessionIdNotFound nonexistentSessionId)
