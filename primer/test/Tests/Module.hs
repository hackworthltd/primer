module Tests.Module where

import Foreword

import Primer.Core (
  ID,
  ModuleName,
  mkSimpleModuleName,
 )
import Primer.Examples as Examples
import Primer.Module (
  Module,
  nextModuleID,
 )
import Test.Tasty.HUnit

expectSuccess :: (ModuleName -> (Module, ID)) -> Assertion
expectSuccess genModule =
  let (m, expectedID) = genModule $ mkSimpleModuleName "M"
   in nextModuleID m @?= expectedID

unit_nextModuleID_exampleMapModule :: Assertion
unit_nextModuleID_exampleMapModule = expectSuccess Examples.mapModule

unit_nextModuleID_exampleEvenOddModule :: Assertion
unit_nextModuleID_exampleEvenOddModule = expectSuccess Examples.evenOddModule
