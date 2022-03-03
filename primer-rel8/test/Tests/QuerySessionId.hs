{-# LANGUAGE BlockArguments #-}

module Tests.QuerySessionId where

import Foreword

import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import Primer.App (
  newApp,
 )
import Primer.Database (
  SessionData (..),
  defaultSessionName,
  fromSessionName,
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
 )
import Primer.Database.Rel8.Rel8Db (
  isLoadSessionProgramDecodingError,
  runRel8Db,
 )
import qualified Primer.Database.Rel8.Schema as Schema (
  SessionRow (SessionRow, app, gitversion, name, uuid),
 )
import Rel8 (lit)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)
import TestUtils (
  assertException,
  insertSessionRow,
  withDbSetup,
  (@?=),
 )

-- Note: 'querySessionId' gets plenty of coverage in our other unit
-- tests by virtue of the fact we use it to retrieve results that we
-- insert into the database using 'insertSession' etc. Therefore,
-- these tests are focused on finding corner cases and testing for
-- particular failure modes.
--
-- Note that several of these corner cases are things that should
-- "never happen" because our types make them impossible, but we test
-- them anyway (using the raw database interface to circumvent our
-- types) to ensure we can handle database corruption, bugs, schema
-- migration issues, etc.

test_querySessionId :: TestTree
test_querySessionId = testCaseSteps "querySessionId corner cases" $ \step' ->
  withDbSetup \conn -> do
    flip runRel8Db conn $ do
      let step = liftIO . step'

      step "Insert program"
      let version = "git123"
      let name = safeMkSessionName "test querySessionId"
      sessionId <- liftIO newSessionId
      insertSession version sessionId newApp name

      step "Attempt to look up a session that doesn't exist"
      nonexistentSessionId <- liftIO newSessionId
      r1 <- querySessionId version nonexistentSessionId
      r1 @?= Left ("No such session ID " <> UUID.toText nonexistentSessionId)

      step "Attempt to fetch a session whose program is invalid"
      invalidProgramSessionId <- liftIO newSessionId
      let invalidProgramName = safeMkSessionName "this program is broken"
      let invalidProgramRow =
            lit
              Schema.SessionRow
                { Schema.uuid = invalidProgramSessionId
                , Schema.gitversion = version
                , Schema.app = Aeson.encode ()
                , Schema.name = fromSessionName invalidProgramName
                }
      liftIO $ insertSessionRow invalidProgramRow conn
      assertException "querySessionId" isLoadSessionProgramDecodingError $ querySessionId version invalidProgramSessionId

      step "Attempt to fetch a session whose name is invalid"
      invalidNameSessionId <- liftIO newSessionId
      let invalidName = ""
      let invalidNameRow =
            lit
              Schema.SessionRow
                { Schema.uuid = invalidNameSessionId
                , Schema.gitversion = version
                , Schema.app = Aeson.encode newApp
                , Schema.name = invalidName
                }
      liftIO $ insertSessionRow invalidNameRow conn
      r3 <- querySessionId version invalidNameSessionId
      -- In this scenario, we should get the program back with the
      -- default session name, rather than the invalid name we used to
      -- store it in the database.
      r3 @?= Right (SessionData newApp defaultSessionName)
