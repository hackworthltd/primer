module Tests.Prog where

import Primer.App (
  Prog,
  nextProgID,
 )
import Primer.Core (
  ID,
 )
import Primer.Examples as Examples
import Primer.Name (
  NameCounter,
 )
import Test.Tasty.HUnit

expectSuccess :: (Prog, ID, NameCounter) -> Assertion
expectSuccess (p, expectedID, _) = nextProgID p @?= expectedID

unit_nextProgID_exampleEven3Prog :: Assertion
unit_nextProgID_exampleEven3Prog = expectSuccess Examples.even3Prog

-- Yes, even our "bad" examples should have valid next 'ID's.

unit_nextProgID_exampleBadEven3Prog :: Assertion
unit_nextProgID_exampleBadEven3Prog = expectSuccess Examples.badEven3Prog

unit_nextProgID_exampleBadEvenProg :: Assertion
unit_nextProgID_exampleBadEvenProg = expectSuccess Examples.badEvenProg

unit_nextProgID_exampleBadMapProg :: Assertion
unit_nextProgID_exampleBadMapProg = expectSuccess Examples.badMapProg
