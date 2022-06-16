module Tests.Utils where

import Foreword

import Primer.Core (
  Def,
  GVarName,
  ID (..),
  ModuleName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (
  S,
  create,
 )
import Primer.Core.Utils (
  nextID,
 )
import Primer.Examples as Examples
import Test.Tasty.HUnit

modName :: ModuleName
modName = mkSimpleModuleName "M"

genAST :: S (GVarName, Def) -> Def
genAST example = fst $ create $ example <&> snd

-- Note: here we are not trying to test that 'Examples.map' has a
-- particular next 'ID', only that 'nextID' returns whatever
-- 'Examples.map''s next 'ID' happens to be.
unit_nextID_exampleMap :: Assertion
unit_nextID_exampleMap = nextID (genAST $ Examples.map modName) @?= ID 41

-- See note for 'unit_nextID_exampleMap'.
unit_nextID_exampleEven :: Assertion
unit_nextID_exampleEven = nextID (genAST $ Examples.even modName) @?= ID 11

-- See note for 'unit_nextID_exampleMap'.
unit_nextID_exampleOdd :: Assertion
unit_nextID_exampleOdd = nextID (genAST $ Examples.odd modName) @?= ID 11

-- See note for 'unit_nextID_exampleMap'.
unit_nextID_exampleComprehensive :: Assertion
unit_nextID_exampleComprehensive = nextID (genAST $ Examples.comprehensive modName) @?= ID 47
