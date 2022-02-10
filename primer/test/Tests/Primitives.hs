-- | Tests specific to primitives.
-- Note that there are other primitives-related tests in 'Primer.Typecheck', 'Primer.Eval' etc.
module Tests.Primitives where

import Foreword

import qualified Data.Map as M
import Gen.Core.Typed (forAllT, genPrimCon, propertyWT)
import Hedgehog (Property, assert)
import Hedgehog.Gen (element)
import Primer.App (defaultTypeDefs)
import Primer.Core (primConName)
import Primer.Primitives (allPrimTypeDefs)
import Primer.Typecheck (SmartHoles (NoSmartHoles), buildTypingContext)

hprop_all_prim_cons_have_typedef :: Property
hprop_all_prim_cons_have_typedef = propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
  c <- forAllT $ element =<< genPrimCon
  assert $ primConName c `elem` M.keys allPrimTypeDefs
