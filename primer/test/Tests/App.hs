module Tests.App where

import Foreword

import Primer.Action (
  ActionError (TypeError),
 )
import Primer.App (
  App,
  Prog,
  ProgError (ActionError),
  appProg,
  checkAppWellFormed,
  mkApp,
  mkAppSafe,
  newApp,
  newEmptyApp,
  nextProgID,
 )
import Primer.Core (
  ID,
 )
import Primer.Examples qualified as Examples
import Primer.Name (
  NameCounter,
 )
import Test.Tasty.HUnit (
  Assertion,
  assertFailure,
  (@?=),
 )

badEven3App :: App
badEven3App =
  let (p, i, n) = Examples.badEven3Prog
   in mkApp i n p

badEvenApp :: App
badEvenApp =
  let (p, i, n) = Examples.badEvenProg
   in mkApp i n p

badMapApp :: App
badMapApp =
  let (p, i, n) = Examples.badMapProg
   in mkApp i n p

expectWellFormed :: App -> Text -> Assertion
expectWellFormed a name =
  case checkAppWellFormed a of
    Left e -> assertFailure $ toS name <> " should be well-formed: " <> show e
    Right _ -> pure ()

expectTypeError :: ProgError -> Assertion
expectTypeError (ActionError (TypeError _)) = pure ()
expectTypeError e = assertFailure $ "Expected TypeError, but got " <> show e

expectNotWellFormed :: App -> Text -> Assertion
expectNotWellFormed a name =
  case checkAppWellFormed a of
    Left e -> expectTypeError e
    Right _ -> assertFailure $ toS name <> " should not be well-formed"

expectMkAppSafeSuccess :: (Prog, ID, NameCounter) -> Text -> Assertion
expectMkAppSafeSuccess (p, i, n) name =
  case mkAppSafe n p of
    Left _ -> assertFailure $ "mkAppSafe should succeed for " <> toS name
    Right app -> nextProgID (appProg app) @?= i

expectMkAppSafeFailure :: (Prog, ID, NameCounter) -> Text -> Assertion
expectMkAppSafeFailure (p, _, n) name =
  case mkAppSafe n p of
    Left e -> expectTypeError e
    Right _ -> assertFailure $ "mkAppSafe should fail for " <> toS name

unit_checkAppWellFormed_newApp :: Assertion
unit_checkAppWellFormed_newApp = expectWellFormed newApp "newApp"

unit_checkAppWellFormed_newEmptyApp :: Assertion
unit_checkAppWellFormed_newEmptyApp = expectWellFormed newEmptyApp "newEmptyApp"

unit_checkAppWellFormed_even3App :: Assertion
unit_checkAppWellFormed_even3App = expectWellFormed Examples.even3App "even3App"

unit_checkAppWellFormed_badEven3App :: Assertion
unit_checkAppWellFormed_badEven3App = expectNotWellFormed badEven3App "badEven3App"

unit_checkAppWellFormed_badEvenApp :: Assertion
unit_checkAppWellFormed_badEvenApp = expectNotWellFormed badEvenApp "badEvenApp"

unit_checkAppWellFormed_badMapApp :: Assertion
unit_checkAppWellFormed_badMapApp = expectNotWellFormed badMapApp "badMapApp"

unit_mkAppSafe_even3Prog :: Assertion
unit_mkAppSafe_even3Prog = expectMkAppSafeSuccess Examples.even3Prog "even3Prog"

unit_mkAppSafe_badEven3Prog :: Assertion
unit_mkAppSafe_badEven3Prog = expectMkAppSafeFailure Examples.badEven3Prog "badEven3Prog"

unit_mkAppSafe_badEvenProg :: Assertion
unit_mkAppSafe_badEvenProg = expectMkAppSafeFailure Examples.badEvenProg "badEvenProg"

unit_mkAppSafe_badMapProg :: Assertion
unit_mkAppSafe_badMapProg = expectMkAppSafeFailure Examples.badMapProg "badMapProg"
