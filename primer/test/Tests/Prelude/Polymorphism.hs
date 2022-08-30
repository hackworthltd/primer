module Tests.Prelude.Polymorphism where

import Foreword

import Hedgehog (forAll)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as Range
import Primer.Builtins (tBool, tList)
import Primer.Builtins.DSL (bool_, list_)
import Primer.Core.DSL (create', int, tapp, tcon)
import Primer.Prelude.Polymorphism qualified as P
import Primer.Primitives (tInt)
import Tasty (Property, property)
import Tests.Prelude.Utils (functionOutput', (<===>))

tasty_id_prop :: Property
tasty_id_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  b <- forAll G.bool_
  ns <- forAll $ G.list (Range.constant 0 10) (G.integral_ (Range.constant (-10) 10))
  functionOutput' P.id [Right $ tcon tInt, Left $ int n] 20 <===> Right (create' $ int n) -- Integer Test
  functionOutput' P.id [Right $ tcon tBool, Left $ bool_ b] 20 <===> Right (create' $ bool_ b) -- Bool Test
  functionOutput' P.id [Right $ tcon tList `tapp` tcon tInt, Left $ list_ tInt $ map int ns] 20 <===> Right (create' $ list_ tInt $ map int ns) -- List of Int Test

tasty_const_prop :: Property
tasty_const_prop = property $ do
  n <- forAll $ G.integral_ (Range.constant (-10) 10)
  b <- forAll G.bool_
  ns <- forAll $ G.list (Range.constant 0 10) (G.integral_ (Range.constant (-10) 10))
  functionOutput' -- Integer Test
    P.const
    [ Right $ tcon tInt
    , Left $ int n
    , Right $ tcon tBool
    , Left $ bool_ True
    ]
    20
    <===> Right (create' $ int n)
  functionOutput' -- Bool Test
    P.const
    [ Right $ tcon tBool
    , Left $ bool_ b
    , Right $ tcon tInt
    , Left $ int n
    ]
    20
    <===> Right (create' $ bool_ b)
  functionOutput' -- List of Int Test
    P.const
    [ Right $ tcon tList `tapp` tcon tInt
    , Left $ list_ tInt $ map int ns
    , Right $ tcon tInt
    , Left $ int n
    ]
    20
    <===> Right (create' $ list_ tInt $ map int ns)
