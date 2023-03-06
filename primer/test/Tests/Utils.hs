module Tests.Utils where

import Foreword

import Primer.Core (
  GVarName,
  ID (..),
  ModuleName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (
  S,
  create,
 )
import Primer.Def (Def)
import Primer.Def.Utils (
  nextID,
 )
import Primer.Examples as Examples
import Test.Tasty.HUnit hiding ((@?=), assertEqual)
import Hedgehog.Internal.Show (ValueDiff(ValueSame), mkValue, valueDiff, showPretty,
                               renderValueDiff)
import qualified Data.String as String

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
unit_nextID_exampleComprehensive = nextID (genAST $ Examples.comprehensive modName) @?= ID 49

-- Nice diffs when tasty-hunit tests fail (I should extract this to a library? NB: removed an arg from assertEqual! NB: no color support)
-- cf https://github.com/UnkindPartition/tasty/issues/226

-- from hedgehog https://hackage.haskell.org/package/hedgehog-1.2/docs/src/Hedgehog.Internal.Property.html#failDiff, modified to avoid MonadTest so can work with tasty-hunit
-- | Fails with an error that shows the difference between two values.
failDiff :: (Show a, Show b, HasCallStack) => a -> b -> Assertion
failDiff x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $ assertFailure $
        String.unlines [
            "Failed"
          , "━━ lhs ━━"
          , showPretty x
          , "━━ rhs ━━"
          , showPretty y
          ]
    Just vdiff@(ValueSame _) ->
      withFrozenCallStack $
        assertFailure $
        String.unlines ["━━━ Failed (no differences) ━━━",
                        renderValueDiff vdiff]
    Just vdiff ->
      withFrozenCallStack $
        assertFailure $
          String.unlines [ "━━━ Failed (- lhs) (+ rhs) ━━━",
                           renderValueDiff vdiff]

-- from tasty-hunit https://hackage.haskell.org/package/tasty-hunit-0.10.0.3/docs/src/Test.Tasty.HUnit.Orig.html#assertEqual, modified to use 'failDiff'
assertEqual
  :: (Eq a, Show a, HasCallStack)
  => a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertEqual expected actual =
  unless (actual == expected) $ failDiff expected actual

infix  1 @=?, @?=

-- | Asserts that the specified actual value is equal to the expected value
--   (with the expected value on the left-hand side).
(@=?)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
expected @=? actual = assertEqual expected actual

-- | Asserts that the specified actual value is equal to the expected value
--   (with the actual value on the left-hand side).
(@?=)
  :: (Eq a, Show a, HasCallStack)
  => a -- ^ The actual value
  -> a -- ^ The expected value
  -> Assertion
actual @?= expected = assertEqual actual expected
