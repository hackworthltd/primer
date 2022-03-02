{-# LANGUAGE BlockArguments #-}

module Tests.UpdateSessionApp where

import Foreword

import Primer.App (
  newApp,
  newEmptyApp,
 )
import Primer.Database (
  SessionData (..),
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
  updateSessionApp,
 )
import Primer.Database.Rel8 (
  isUpdateAppError,
  runRel8Db,
 )
import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (
  expectFailBecause,
 )
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  withDbSetup,
  (@?=),
 )

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
      r1 <- querySessionId version sessionId
      r1 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new version, but the same app"
      let newVersion = "new-" <> version
      updateSessionApp newVersion sessionId newEmptyApp
      r2 <- querySessionId newVersion sessionId
      r2 @?= Right (SessionData newEmptyApp name)

      step "Update it with a new app"
      updateSessionApp newVersion sessionId newApp
      r3 <- querySessionId newVersion sessionId
      r3 @?= Right (SessionData newApp name)

      -- Note: at the moment, we ignore the stored Primer version when
      -- we query the database. This is a bit odd, but it's not yet
      -- clear whether it'll be useful to include the version in the
      -- query.
      --
      -- See https://github.com/hackworthltd/primer/issues/268
      step "We can still query the program using the old version"
      r4 <- querySessionId version sessionId
      r4 @?= Right (SessionData newApp name)

-- Not yet implemented. See:
-- https://github.com/hackworthltd/primer/issues/269
test_updateSessionApp_failure :: TestTree
test_updateSessionApp_failure = expectFailBecause "Not yet implemented" $
  testCaseSteps "updateSessionApp failure modes" $ \step' ->
    withDbSetup \conn -> do
      flip runRel8Db conn $ do
        let step = liftIO . step'

        step "Attempt to update a session that hasn't yet been inserted"
        let version = "git123"
        sessionId <- liftIO newSessionId
        assertException "updateSessionApp" isUpdateAppError $ updateSessionApp version sessionId newApp
