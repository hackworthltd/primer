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
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  runTmpDb,
  testApp,
  (@?=),
 )

expectedError :: SessionId -> Rel8DbException -> Bool
expectedError id_ (InsertError s _) = s == id_
expectedError _ _ = False

test_insertSession_roundtrip :: TestTree
test_insertSession_roundtrip = testCaseSteps "insertSession database round-tripping" $ \step' ->
  runTmpDb $ do
    let step = liftIO . step'
    step "Insert testApp"
    let version = "git123"
    let name = safeMkSessionName "testApp"
    sessionId <- liftIO newSessionId
    insertSession version sessionId testApp name

    step "Retrieve it"
    result <- querySessionId sessionId
    result @?= Right (SessionData testApp name)

    let jpName = safeMkSessionName "サンプルプログラム"
    step "Insert app with Japanese name"
    sid1 <- liftIO newSessionId
    insertSession version sid1 testApp jpName
    r1 <- querySessionId sid1
    r1 @?= Right (SessionData testApp jpName)

    let cnName = safeMkSessionName "示例程序"
    step "Insert app with simplified Chinese name"
    sid2 <- liftIO newSessionId
    insertSession version sid2 testApp cnName
    r2 <- querySessionId sid2
    r2 @?= Right (SessionData testApp cnName)

    let arName = safeMkSessionName "برنامج مثال"
    step "Insert app with Arabic name"
    sid3 <- liftIO newSessionId
    insertSession version sid3 testApp arName
    r3 <- querySessionId sid3
    r3 @?= Right (SessionData testApp arName)

    let emName = safeMkSessionName "😄😂🤣🤗 🦊 🦈"
    step "Insert app with emoji name"
    sid4 <- liftIO newSessionId
    insertSession version sid4 testApp emName
    r4 <- querySessionId sid4
    r4 @?= Right (SessionData testApp emName)

test_insertSession_failure :: TestTree
test_insertSession_failure = testCaseSteps "insertSession failure modes" $ \step' ->
  runTmpDb $ do
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
