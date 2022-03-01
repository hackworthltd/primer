{-# LANGUAGE BlockArguments #-}

module Tests.InsertSession where

import Foreword

import Primer.App (newApp)
import Primer.Database (
  SessionData (..),
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
 )
import Primer.Database.Rel8 (
  runRel8Db,
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import qualified Test.Tasty.HUnit as HUnit
import TestUtils (withDbSetup)

(@?=) :: (MonadIO m, Eq a, Show a) => a -> a -> m ()
x @?= y = liftIO $ x HUnit.@?= y
infix 1 @?=

test_insertSession :: TestTree
test_insertSession = testCaseSteps "insertSession" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'
      step "Insert newApp"
      let version = "git123"
      let name = safeMkSessionName "testNewApp"
      sessionId <- liftIO newSessionId
      insertSession version sessionId newApp name
      step "Retrieve it"
      result <- querySessionId version sessionId
      result @?= Right (SessionData newApp name)
