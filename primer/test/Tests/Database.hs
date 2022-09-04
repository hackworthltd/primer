module Tests.Database where

import Foreword

import Control.Concurrent.STM (
  atomically,
  isEmptyTBQueue,
  newEmptyTMVarIO,
  newTBQueueIO,
  writeTBQueue,
 )
import Data.Text qualified as Text
import Primer.API (
  Env (..),
  PrimerIO,
  addSession,
  edit,
  listSessions,
  renameSession,
  runPrimerIO,
 )
import Primer.App (
  MutationRequest (Edit),
  ProgAction (CreateDef),
 )
import Primer.Core (
  mkSimpleModuleName,
 )
import Primer.Database (
  OffsetLimit (OL),
  Op (LoadSession),
  ServiceCfg (..),
  defaultSessionName,
  fromSessionName,
  mkSessionName,
  runNullDb,
  safeMkSessionName,
  serve,
 )
import Primer.Examples (
  even3App,
 )
import StmContainers.Map qualified as StmMap
import Test.Tasty
import Test.Tasty.HUnit

test_unmodified :: TestTree
test_unmodified =
  testGroup
    "unmodified"
    [ testSessionName'
        "foobar"
        "foobar"
    , testSessionName'
        "foo bar"
        "foo bar"
    , testSessionName'
        "preserve punctuation"
        "... this is a session: it's mine!"
    , testSessionName'
        "preserve whitespace"
        "This    is a    \tsession"
    , testSessionName'
        "emoji"
        "😄😂🤣🤗 🦊 🦈"
    ]
  where
    testSessionName' testName t = testSessionName testName t t

test_modified :: TestTree
test_modified =
  testGroup
    "modified"
    [ testGroup
        "newline"
        [ testSessionName
            "start"
            "\nfoo bar"
            "foo bar"
        , testSessionName
            "middle"
            "foo\nbar"
            "foo"
        , testSessionName
            "end"
            "foo bar\n"
            "foo bar"
        , testSessionName
            "end, after space"
            "foo bar  \n"
            "foo bar"
        , testSessionName
            "start and middle"
            "\nfoo\nbar"
            "foo"
        , testSessionName
            "strip whitespace"
            "   \nfoo bar baz  \n  "
            "foo bar baz"
        ]
    , let tooLong = toS . concat $ replicate 7 ['0' .. '9']
       in testSessionName
            "truncate at 64"
            tooLong
            (Text.take 64 tooLong)
    ]

test_invalid :: TestTree
test_invalid =
  testGroup
    "invalid"
    [ testSessionName'
        "empty"
        ""
    , testSessionName'
        "all whitespace"
        " \t\n  \n"
    ]
  where
    testSessionName' testName t =
      testGroup
        testName
        [ testCase "unsafe" $ case mkSessionName t of
            Nothing -> pure ()
            Just _ -> assertFailure "name is valid"
        , testCase "safe" $
            safeMkSessionName t @?= defaultSessionName
        ]

test_insert_empty_q :: TestTree
test_insert_empty_q = empty_q_harness "database Insert leaves an empty op queue" $ do
  void $ addSession "even3App" even3App

test_updateapp_empty_q :: TestTree
test_updateapp_empty_q = empty_q_harness "database UpdateApp leaves an empty op queue" $ do
  sid <- addSession "even3App" even3App
  void $ edit sid $ Edit [CreateDef (mkSimpleModuleName "Even3") $ Just "newDef"]

test_updatename_empty_q :: TestTree
test_updatename_empty_q = empty_q_harness "database UpdateName leaves an empty op queue" $ do
  sid <- addSession "even3App" even3App
  void $ renameSession sid "even3App'"

test_loadsession_empty_q :: TestTree
test_loadsession_empty_q = empty_q_harness "database LoadSession leaves an empty op queue" $ do
  sid <- addSession "even3App" even3App
  -- No easy way to do this from the API, so we do it here by hand.
  callback <- liftIO newEmptyTMVarIO
  q <- asks dbOpQueue
  ss <- asks sessions
  void $ liftIO $ atomically $ writeTBQueue q $ LoadSession sid ss callback

test_listsessions_empty_q :: TestTree
test_listsessions_empty_q = empty_q_harness "database ListSessions leaves an empty op queue" $ do
  void $ addSession "even3App" even3App
  void $ listSessions True $ OL 0 $ Just 100

testSessionName :: TestName -> Text -> Text -> TestTree
testSessionName testName t expected =
  testGroup
    testName
    [ testCase "unsafe" $ case mkSessionName t of
        Nothing -> assertFailure "name is invalid"
        Just sn -> fromSessionName sn @?= expected
    , testCase "safe" $
        fromSessionName (safeMkSessionName t) @?= expected
    ]

empty_q_harness :: Text -> PrimerIO () -> TestTree
empty_q_harness desc test = testCaseSteps (toS desc) $ \step' -> do
  dbOpQueue <- newTBQueueIO 4
  inMemorySessions <- StmMap.newIO
  dbSessions <- StmMap.newIO
  let version = "git123"
  nullDbProc <- async $ runNullDb dbSessions $ serve $ ServiceCfg dbOpQueue version
  testProc <- async $ flip runPrimerIO (Env inMemorySessions dbOpQueue version) $ do
    test
    -- Give 'nullDbProc' time to empty the queue.
    liftIO $ threadDelay 100000
  (p, result) <- waitAnyCatchCancel [nullDbProc, testProc]
  case result of
    Right _ ->
      if p == testProc
        then do
          step' "Check that the database op queue is empty"
          qempty <- liftIO $ atomically $ isEmptyTBQueue dbOpQueue
          assertBool "Queue should be empty" qempty
        else assertFailure "the impossible happened: nullDbProc exited"
    Left e ->
      if p == testProc
        then assertFailure $ "nullDbProc threw an exception: " <> show e
        else assertFailure $ "testProc threw an exception: " <> show e
