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
  runRel8Db,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  testApp,
  withDbSetup,
  (@?=),
 )

expectedError :: SessionId -> Rel8DbException -> Bool
expectedError id_ (UpdateAppNonExistentSession s) = s == id_
expectedError _ _ = False

test_updateSessionApp_roundtrip :: TestTree
test_updateSessionApp_roundtrip = testCaseSteps "updateSessionApp database round-tripping" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Insert a new session"
      let version = "git123"
      let name = safeMkSessionName "new app"
      sessionId <- liftIO newSessionId
      insertSession version sessionId newEmptyApp name

      step "Update it with the same version and app"
      updateSessionApp version sessionId newEmptyApp
      r1 <- querySessionId sessionId
      r1 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new version, but the same app"
      let newVersion = "new-" <> version
      updateSessionApp newVersion sessionId newEmptyApp
      r2 <- querySessionId sessionId
      r2 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new app"
      updateSessionApp newVersion sessionId testApp
      r3 <- querySessionId sessionId
      r3 @?= Right (SessionData testApp name)

test_updateSessionApp_failure :: TestTree
test_updateSessionApp_failure = testCaseSteps "updateSessionApp failure modes" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Attempt to update a session that hasn't yet been inserted"
      let version = "git123"
      sessionId <- liftIO newSessionId
      assertException "updateSessionApp" (expectedError sessionId) $ updateSessionApp version sessionId newApp
