{-# LANGUAGE BlockArguments #-}

module Tests.UpdateSessionName where

import Foreword

import Primer.App (
  newEmptyApp,
 )
import Primer.Database (
  SessionData (..),
  SessionId,
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
  updateSessionName,
 )
import Primer.Database.Selda (
  SeldaDbException (UpdateNameNonExistentSession),
 )
import Primer.Database.Selda.Test.Util (
  lowPrecisionCurrentTime,
  runTmpDb,
 )
import Primer.Test.Util (
  assertException,
  (@?=),
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)

expectedError :: SessionId -> SeldaDbException -> Bool
expectedError id_ (UpdateNameNonExistentSession s) = s == id_
expectedError _ _ = False

test_updateSessionName_roundtrip :: TestTree
test_updateSessionName_roundtrip = testCaseSteps "updateSessionName database round-tripping" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'

    step "Insert a new session"
    let version = "git123"
    let name = safeMkSessionName "new app"
    now <- lowPrecisionCurrentTime
    sessionId <- liftIO newSessionId
    insertSession version sessionId newEmptyApp name now

    step "Update it with the same version and name"
    updateSessionName version sessionId name now
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData newEmptyApp name now)

    step "Update it with a new version, but the same name"
    let newVersion = "new-" <> version
    updateSessionName newVersion sessionId name now
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData newEmptyApp name now)

    step "Update it with a new name"
    let newName = safeMkSessionName "new new app"
    updateSessionName newVersion sessionId newName now
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData newEmptyApp newName now)

    step "Update it with a new time"
    now' <- lowPrecisionCurrentTime
    updateSessionName newVersion sessionId newName now'
    r4 <- querySessionId sessionId
    r4 @?= Right (SessionData newEmptyApp newName now')

    step "Update it with a Japanese name"
    let jpName = safeMkSessionName "サンプルプログラム"
    updateSessionName newVersion sessionId jpName now'
    r5 <- querySessionId sessionId
    r5 @?= Right (SessionData newEmptyApp jpName now')

    step "Update it with a simplified Chinese name"
    let cnName = safeMkSessionName "示例程序"
    updateSessionName newVersion sessionId cnName now'
    r6 <- querySessionId sessionId
    r6 @?= Right (SessionData newEmptyApp cnName now')

    step "Update it with an Arabic name"
    let arName = safeMkSessionName "برنامج مثال"
    updateSessionName newVersion sessionId arName now'
    r7 <- querySessionId sessionId
    r7 @?= Right (SessionData newEmptyApp arName now')

    step "Update it with an emoji name"
    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    updateSessionName newVersion sessionId emName now'
    r8 <- querySessionId sessionId
    r8 @?= Right (SessionData newEmptyApp emName now')

test_updateSessionName_failure :: TestTree
test_updateSessionName_failure = testCaseSteps "updateSessionName failure modes" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    let version = "git123"
    let name = safeMkSessionName "this session doesn't exist"
    now <- lowPrecisionCurrentTime
    sessionId <- liftIO newSessionId
    assertException "updateSessionName" (expectedError sessionId) $ updateSessionName version sessionId name now
