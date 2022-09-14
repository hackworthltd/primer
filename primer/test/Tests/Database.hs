{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tests.Database where

import Foreword

import Control.Concurrent.STM (
  atomically,
  isEmptyTBQueue,
  newEmptyTMVarIO,
  newTBQueueIO,
  writeTBQueue,
 )
import Control.Monad.Trans (
  MonadTrans,
 )
import Control.Monad.Trans.Identity (
  IdentityT (..),
 )
import Data.Text qualified as Text
import Primer.API (
  Env (..),
  PrimerIO,
  addSession,
  edit,
  renameSession,
  runPrimerIO, PrimerM(PrimerM), runPrimerM,
 )
import Primer.API qualified as API
import Primer.App (
  MutationRequest (Edit),
  ProgAction (CreateDef),
 )
import Primer.Core (
  mkSimpleModuleName,
 )
import Primer.Database (
  MonadDb (..),
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
import Control.Monad.Log (PureLoggingT, runPureLoggingT, WithSeverity, mapLogMessage, LoggingT, MonadLog, DiscardLoggingT (discardLogging))
import qualified Data.Sequence as Seq
import Primer.Log (ConvertLogMessage (convert))

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

insertTest :: PrimerIO ()
insertTest = do
  void $ addSession "even3App" even3App

pureLogs :: PrimerM (LoggingT l (PureLoggingT (Seq l) IO)) () -> PrimerIO (Seq l)
pureLogs m = PrimerM $ ReaderT $ fmap snd . runPureLoggingT . mapLogMessage Seq.singleton . runPrimerM m

dropLogs :: PrimerM (DiscardLoggingT l IO) a -> PrimerIO a
dropLogs m = PrimerM $ ReaderT $ discardLogging . runPrimerM m

newtype TestLog = TestLog Text
  deriving newtype (Eq, Show)
instance ConvertLogMessage Text TestLog where
  convert = TestLog

updateAppTest :: (MonadIO m, MonadThrow m, MonadLog (WithSeverity TestLog) m) => PrimerM m ()
updateAppTest = do
  sid <- addSession "even3App" even3App
  void $ edit sid $ Edit [CreateDef (mkSimpleModuleName "Even3") $ Just "newDef"]

updateNameTest ::(MonadIO m, MonadThrow m, MonadLog (WithSeverity TestLog) m) => PrimerM m ()
updateNameTest = do
  sid <- addSession "even3App" even3App
  void $ renameSession sid "even3App'"

loadSessionTest :: PrimerIO ()
loadSessionTest = do
  sid <- addSession "even3App" even3App
  -- No easy way to do this from the API, so we do it here by hand.
  callback <- liftIO newEmptyTMVarIO
  q <- asks dbOpQueue
  ss <- asks sessions
  void $ liftIO $ atomically $ writeTBQueue q $ LoadSession sid ss callback

listSessionsTest :: PrimerIO ()
listSessionsTest = do
  void $ addSession "even3App" even3App
  void $ API.listSessions True $ OL 0 $ Just 100

test_insert_empty_q :: TestTree
test_insert_empty_q = empty_q_harness "database Insert leaves an empty op queue" $ do
  insertTest

test_updateapp_empty_q :: TestTree
test_updateapp_empty_q = empty_q_harness_withLogs
  "database UpdateApp leaves an empty op queue"
  (Seq.empty @=?) -- REVIEW: Note that I can get this to fail, by adding a log message in handleMutationRequest
  updateAppTest

test_updatename_empty_q :: TestTree
test_updatename_empty_q = empty_q_harness_withLogs
  "database UpdateName leaves an empty op queue"
  (Seq.empty @=?) -- REVIEW: Note that I can get this to fail, by adding a log message in renameSession
  updateNameTest

test_loadsession_empty_q :: TestTree
test_loadsession_empty_q = empty_q_harness "database LoadSession leaves an empty op queue" $ do
  loadSessionTest

test_listsessions_empty_q :: TestTree
test_listsessions_empty_q = empty_q_harness "database ListSessions leaves an empty op queue" $ do
  listSessionsTest

test_insert_faildb :: TestTree
test_insert_faildb = faildb_harness "database Insert leaves behind an op" $ do
  insertTest

test_updateapp_faildb :: TestTree
test_updateapp_faildb = faildb_harness "database UpdateApp leaves behind an op" $ do
  dropLogs updateAppTest

test_updatename_faildb :: TestTree
test_updatename_faildb = faildb_harness "database UpdateName leaves behind an op" $ do
  dropLogs updateNameTest

test_loadsession_faildb :: TestTree
test_loadsession_faildb = faildb_harness "database LoadSession leaves behind an op" $ do
  loadSessionTest

test_listsessions_faildb :: TestTree
test_listsessions_faildb = faildb_harness "database ListSessions leaves behind an op" $ do
  listSessionsTest

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
empty_q_harness desc = empty_q_harness' desc pure

empty_q_harness_withLogs :: Text -> (Seq l -> Assertion)
  -> (forall m . (MonadIO m, MonadThrow m, MonadLog l m) => PrimerM m ())
  -> TestTree
empty_q_harness_withLogs desc f m = empty_q_harness' desc f $ pureLogs m

empty_q_harness' :: Text -> (a -> Assertion) -> PrimerIO a -> TestTree
empty_q_harness' desc f test = testCaseSteps (toS desc) $ \step' -> do
  dbOpQueue <- newTBQueueIO 4
  inMemorySessions <- StmMap.newIO
  dbSessions <- StmMap.newIO
  let version = "git123"
  nullDbProc <- async $ runNullDb dbSessions $ serve $ ServiceCfg dbOpQueue version
  testProc <- async $ flip runPrimerIO (Env inMemorySessions dbOpQueue version) $ do
    res <- test
    -- Give 'nullDbProc' time to empty the queue.
    liftIO $ threadDelay 100000
    pure res
  result <- waitEitherCatchCancel nullDbProc testProc
  case result of
    Right (Right r) -> do
          step' "Check that the database op queue is empty"
          qempty <- liftIO $ atomically $ isEmptyTBQueue dbOpQueue
          assertBool "Queue should be empty" qempty
          step' "Check custom assertion on return"
          f r
    Right (Left e) -> assertFailure $ "testProc threw an exception: " <> show e
    Left (Left e) -> assertFailure $ "nullDbProc threw an exception: " <> show e
    -- REVIEW: should 'serve' return a Void, so it is obvious that it will not return?
    Left (Right _) -> assertFailure "the impossible happened: nullDbProc exited"

-- | A "fail" database that fails on every operation.
newtype FailDbT m a = FailDbT {unFailDbT :: IdentityT m a}
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadFail
    , MonadPlus
    , MonadTrans
    )

-- | The 'FailDbT' monad transformer applied to 'IO'.
type FailDb a = FailDbT IO a

-- | A simple 'Exception' type for 'FailDb' computations.
newtype FailDbException = FailDbException Text
  deriving (Eq, Show)

instance Exception FailDbException

instance (MonadThrow m) => MonadDb (FailDbT m) where
  insertSession _ _ _ _ = throwM $ FailDbException "insertSession"
  updateSessionApp _ _ _ = throwM $ FailDbException "updateSessionApp"
  updateSessionName _ _ _ = throwM $ FailDbException "updateSessionName"
  listSessions _ = throwM $ FailDbException "listSessions"
  querySessionId _ = throwM $ FailDbException "querySessionId"

-- | Run a 'FailDbT' action in a transformer stack.
runFailDbT :: FailDbT m a -> m a
runFailDbT m = runIdentityT $ unFailDbT m

-- | Run a 'FailDb' action in 'IO'.
runFailDb :: FailDb a -> IO a
runFailDb = runFailDbT

-- REVIEW: NB: since we run with an empty initial db, we expect to fail on the first InsertSession, which is before any logs could be generated. Thus there is no point in a variant that checks the logs!
faildb_harness :: Text -> PrimerIO () -> TestTree
faildb_harness desc test = testCaseSteps (toS desc) $ \step' -> do
  dbOpQueue <- newTBQueueIO 4
  inMemorySessions <- StmMap.newIO
  let version = "git123"
  failDbProc <- async $ runFailDb $ serve $ ServiceCfg dbOpQueue version
  testProc <- async $ flip runPrimerIO (Env inMemorySessions dbOpQueue version) $ do
    test
    -- Give 'failDbProc' time to throw.
    liftIO $ threadDelay 100000
  (p, result) <- waitAnyCatchCancel [failDbProc, testProc]
  case result of
    Right _ ->
      if p == testProc
        then assertFailure "failDbProc should have thrown an exception, but it didn't (hint: we might need to increase the threadDelay)"
        else assertFailure "the impossible happened: failDbProc exited"
    Left e ->
      if p == failDbProc
        then do
          step' "Check that the database op queue is non-empty"
          qempty <- liftIO $ atomically $ isEmptyTBQueue dbOpQueue
          assertBool "Queue should not be empty" (not qempty)
        else assertFailure $ "testProc threw an exception: " <> show e
