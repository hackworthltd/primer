{-# LANGUAGE BlockArguments #-}

module Tests.InsertSession where

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
 )
import Primer.Database.Rel8.Rel8Db (
  Rel8DbException (InsertError),
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
expectedError id_ (InsertError s _) = s == id_
expectedError _ _ = False

test_insertSession_roundtrip :: TestTree
test_insertSession_roundtrip = testCaseSteps "insertSession database round-tripping" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'
      step "Insert newApp"
      let version = "git123"
      let name = safeMkSessionName "testNewApp"
      sessionId <- liftIO newSessionId
      insertSession version sessionId testApp name
      step "Retrieve it"
      result <- querySessionId sessionId
      result @?= Right (SessionData testApp name)

test_insertSession_failure :: TestTree
test_insertSession_failure = testCaseSteps "insertSession failure modes" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Insert program"
      let version = "git123"
      let name = safeMkSessionName "testNewApp"
      sessionId <- liftIO newSessionId
      insertSession version sessionId newApp name

      step "Attempt to insert the same program and metadata again"
      assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId newApp name

      step "Attempt to insert a different program with the same metadata"
      assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId newEmptyApp name

      step "Attempt to insert the same program with a different version"
      let newVersion = "new-" <> version
      assertException "insertSession" (expectedError sessionId) $ insertSession newVersion sessionId newApp name

      step "Attempt to insert the same program with a different name"
      let newName = safeMkSessionName "new name"
      assertException "insertSession" (expectedError sessionId) $ insertSession version sessionId newApp newName
