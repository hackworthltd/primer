module Tests.Database where

import Data.Text (Text)
import qualified Data.Text as Text
import Primer.Database (
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
    , let tooLong = Text.pack . concat $ replicate 7 ['0' .. '9']
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
