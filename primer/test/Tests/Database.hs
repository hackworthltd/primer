{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Temporary workaround for GHC 9.6:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/23143
{-# OPTIONS -Wno-redundant-constraints #-}

module Tests.Database where

import Foreword

import Control.Monad.Trans (
  MonadTrans,
 )
import Control.Monad.Trans.Identity (
  IdentityT (..),
 )
import Data.Text qualified as Text
import Primer.Database (
  MonadDb (..),
  defaultSessionName,
  fromSessionName,
  mkSessionName,
  safeMkSessionName,
 )
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
        , testCase "safe"
            $ safeMkSessionName t
            @?= defaultSessionName
        ]

testSessionName :: TestName -> Text -> Text -> TestTree
testSessionName testName t expected =
  testGroup
    testName
    [ testCase "unsafe" $ case mkSessionName t of
        Nothing -> assertFailure "name is invalid"
        Just sn -> fromSessionName sn @?= expected
    , testCase "safe"
        $ fromSessionName (safeMkSessionName t)
        @?= expected
    ]

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
  deriving stock (Eq, Show)

instance Exception FailDbException

instance (MonadThrow m) => MonadDb (FailDbT m) where
  insertSession _ _ _ _ _ = throwM $ FailDbException "insertSession"
  updateSessionApp _ _ _ _ = throwM $ FailDbException "updateSessionApp"
  updateSessionName _ _ _ _ = throwM $ FailDbException "updateSessionName"
  listSessions _ = throwM $ FailDbException "listSessions"
  findSessions _ _ = throwM $ FailDbException "findSessions"
  querySessionId _ = throwM $ FailDbException "querySessionId"
  deleteSession _ = throwM $ FailDbException "deleteSession"

-- | Run a 'FailDbT' action in a transformer stack.
runFailDbT :: FailDbT m a -> m a
runFailDbT m = runIdentityT $ unFailDbT m

-- | Run a 'FailDb' action in 'IO'.
runFailDb :: FailDb a -> IO a
runFailDb = runFailDbT
