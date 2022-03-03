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
import Primer.Database.Rel8 (
  Rel8DbException (UpdateNameNonExistentSession),
  runRel8Db,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  withDbSetup,
  (@?=),
 )

expectedError :: SessionId -> Rel8DbException -> Bool
expectedError id_ (UpdateNameNonExistentSession s) = s == id_
expectedError _ _ = False

test_updateSessionName_roundtrip :: TestTree
test_updateSessionName_roundtrip = testCaseSteps "updateSessionName database round-tripping" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Insert a new session"
      let version = "git123"
      let name = safeMkSessionName "new app"
      sessionId <- liftIO newSessionId
      insertSession version sessionId newEmptyApp name

      step "Update it with the same version and name"
      updateSessionName version sessionId name
      r1 <- querySessionId sessionId
      r1 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new version, but the same name"
      let newVersion = "new-" <> version
      updateSessionName newVersion sessionId name
      r2 <- querySessionId sessionId
      r2 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new name"
      let newName = safeMkSessionName "new new app"
      updateSessionName newVersion sessionId newName
      r3 <- querySessionId sessionId
      r3 @?= Right (SessionData newEmptyApp newName)

test_updateSessionName_failure :: TestTree
test_updateSessionName_failure = testCaseSteps "updateSessionName failure modes" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Attempt to update a session that hasn't yet been inserted"
      let version = "git123"
      let name = safeMkSessionName "this session doesn't exist"
      sessionId <- liftIO newSessionId
      assertException "updateSessionName" (expectedError sessionId) $ updateSessionName version sessionId name
