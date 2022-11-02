{-# LANGUAGE BlockArguments #-}

module Tests.UpdateSessionApp where

import Foreword

import Primer.App (
  newApp,
  newEmptyApp,
 )
import Primer.Database (
  SessionData (..),
  SessionId,
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
  updateSessionApp,
 )
import Primer.Database.Rel8 (
  Rel8DbException (UpdateAppNonExistentSession),
 )
import Primer.Test.App (
  comprehensive,
 )
import Primer.Test.Util (
  assertException,
  (@?=),
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  lowPrecisionCurrentTime,
  runTmpDb,
 )

expectedError :: SessionId -> Rel8DbException -> Bool
expectedError id_ (UpdateAppNonExistentSession s) = s == id_
expectedError _ _ = False

test_updateSessionApp_roundtrip :: TestTree
test_updateSessionApp_roundtrip = testCaseSteps "updateSessionApp database round-tripping" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'

    step "Insert a new session"
    let version = "git123"
    let name = safeMkSessionName "new app"
    now <- lowPrecisionCurrentTime
    sessionId <- liftIO newSessionId
    insertSession version sessionId newEmptyApp name now

    step "Update it with the same version and app"
    updateSessionApp version sessionId newEmptyApp now
    r1 <- querySessionId sessionId
    r1 @?= Right (SessionData newEmptyApp name now)

    step "Update it with a new version, but the same app"
    let newVersion = "new-" <> version
    updateSessionApp newVersion sessionId newEmptyApp now
    r2 <- querySessionId sessionId
    r2 @?= Right (SessionData newEmptyApp name now)

    step "Update it with a new app"
    updateSessionApp newVersion sessionId comprehensive now
    r3 <- querySessionId sessionId
    r3 @?= Right (SessionData comprehensive name now)

    step "Update it with a new time"
    now' <- lowPrecisionCurrentTime
    updateSessionApp newVersion sessionId comprehensive now'
    r4 <- querySessionId sessionId
    r4 @?= Right (SessionData comprehensive name now')

test_updateSessionApp_failure :: TestTree
test_updateSessionApp_failure = testCaseSteps "updateSessionApp failure modes" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'

    step "Attempt to update a session that hasn't yet been inserted"
    let version = "git123"
    now <- lowPrecisionCurrentTime
    sessionId <- liftIO newSessionId
    assertException "updateSessionApp" (expectedError sessionId) $ updateSessionApp version sessionId newApp now
