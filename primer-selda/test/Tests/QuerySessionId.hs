{-# LANGUAGE BlockArguments #-}

module Tests.QuerySessionId where

import Foreword

import Data.Aeson qualified as Aeson (
  encode,
 )
import Primer.App (
  newApp,
 )
import Primer.Database (
  DbError (SessionIdNotFound),
  LastModified (..),
  SessionData (..),
  defaultSessionName,
  insertSession,
  newSessionId,
  querySessionId,
  safeMkSessionName,
 )
import Primer.Database.Selda.SQLite qualified as Schema (
  SessionRow (SessionRow, app, gitversion, lastmodified, name, uuid),
 )
import Primer.Database.Selda.Test.Util (
  insertSessionRow,
  lowPrecisionCurrentTime,
  runTmpDb,
 )
import Primer.Test.Util ((@?=))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCaseSteps)

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
  runTmpDb $ do
    let step = liftIO . step'

    step "Insert program"
    now <- lowPrecisionCurrentTime
    let version = "git123"
    let name = safeMkSessionName "test querySessionId"
    sessionId <- liftIO newSessionId
    insertSession version sessionId newApp name now

    step "Attempt to look up a session that doesn't exist"
    nonexistentSessionId <- liftIO newSessionId
    r1 <- querySessionId nonexistentSessionId
    r1 @?= Left (SessionIdNotFound nonexistentSessionId)

    step "Attempt to fetch a session whose name is invalid"
    invalidNameSessionId <- liftIO newSessionId
    let invalidName = ""
    let invalidNameRow =
          Schema.SessionRow
            { Schema.uuid = invalidNameSessionId
            , Schema.gitversion = version
            , Schema.app = Aeson.encode newApp
            , Schema.name = invalidName
            , Schema.lastmodified = utcTime now
            }
    insertSessionRow invalidNameRow
    r3 <- querySessionId invalidNameSessionId
    -- In this scenario, we should get the program back with the
    -- default session name, rather than the invalid name we used to
    -- store it in the database.
    r3 @?= Right (SessionData newApp defaultSessionName now)
