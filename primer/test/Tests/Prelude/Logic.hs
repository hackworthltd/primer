module Tests.Prelude.Logic where

import Foreword hiding (exp)

import Data.List.Extra (enumerate)
import Primer.Builtins.DSL (bool_)
import Primer.Core (GVarName)
import Primer.Core.DSL (create')
import Primer.Prelude.Logic qualified as P
import Tasty (Property, property, withTests)
import Tests.Prelude.Utils (functionOutput, (<===>))

tasty_not_correct :: Property
tasty_not_correct = withTests 1 $ property $ for_ enumerate $ \b ->
  functionOutput P.not [bool_ b] 400 <===> Right (create' $ bool_ $ not b)

tasty_and_correct :: Property
tasty_and_correct = binaryCorrect P.and (&&)

tasty_or_correct :: Property
tasty_or_correct = binaryCorrect P.or (||)

tasty_xor_correct :: Property
tasty_xor_correct = binaryCorrect P.xor xor

tasty_implies_correct :: Property
tasty_implies_correct = binaryCorrect P.implies implies
  where
    implies True x = x
    implies False _ = True

binaryCorrect :: GVarName -> (Bool -> Bool -> Bool) -> Property
binaryCorrect name func = withTests 1 $ property $ for_ enumerate $ \b1 -> for_ enumerate $ \b2 ->
  functionOutput name [bool_ b1, bool_ b2] 400 <===> Right (create' $ bool_ $ func b1 b2)
