module Tests.Prelude.Polymorphism where

import Foreword

import Hedgehog (forAll)
import Hedgehog.Gen (integral_)
import Hedgehog.Range qualified as Range
import Primer.Builtins (tBool)
import Primer.Builtins.DSL (bool_)
import Primer.Core.DSL (create', int, tcon)
import Primer.Prelude.Polymorphism qualified as P
import Primer.Primitives (tInt)
import Tasty (Property, property)
import Tests.Prelude.Utils (functionOutput', (<===>))

tasty_id_prop :: Property
tasty_id_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput' P.id [Right $ tcon tInt, Left $ int n] 20 <===> Right (create' $ int n)

tasty_const_prop :: Property
tasty_const_prop = property $ do
  n <- forAll $ integral_ (Range.constant (-10) 10)
  functionOutput'
    P.const
    [ Right $ tcon tInt
    , Left $ int n
    , Right $ tcon tBool
    , Left $ bool_ True
    ]
    20
    <===> Right (create' $ int n)
