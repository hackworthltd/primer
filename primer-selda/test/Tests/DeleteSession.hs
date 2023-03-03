{-# LANGUAGE BlockArguments #-}

module Tests.DeleteSession where

import Foreword

import Primer.App (
  newApp,
 )
import Primer.Database (
  DbError (SessionIdNotFound),
  SessionId,
  deleteSession,
  getCurrentTime,
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
  updateSessionApp,
 )
import Primer.Database.Selda (
  SeldaDbException (UpdateAppNonExistentSession),
 )
import Primer.Database.Selda.Test.Util (
  runTmpDb,
 )
import Primer.Test.Util (
  assertException,
  (@?=),
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)

expectedError :: SessionId -> SeldaDbException -> Bool
expectedError id_ (UpdateAppNonExistentSession s) = s == id_
expectedError _ _ = False

test_deleteSession :: TestTree
test_deleteSession = testCaseSteps "deleteSession" $ \step' ->
  runTmpDb $ do
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
