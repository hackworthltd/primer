module Tests.Prelude.Polymorphism where

import Foreword

import Hedgehog (forAll)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as Range
import Primer.Builtins (tBool, tList)
import Primer.Builtins.DSL (bool_, list_)
import Primer.Core.DSL (apps, create', gvar, int, lam, lvar, tapp, tcon)
import Primer.Prelude.Logic qualified as L
import Primer.Prelude.Polymorphism qualified as P
import Primer.Primitives (PrimDef (IntAdd), tInt)
import Primer.Primitives.DSL (pfun)
import Tasty (Property, property)
import Tests.Prelude.Utils (functionOutput, functionOutput', (<===>))

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

tasty_map_prop :: Property
tasty_map_prop = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant (-10) 10))
  bs <- forAll $ G.list (Range.linear 0 10) G.bool_
  let addOne = lam "x" $ apps (pfun IntAdd) [lvar "x", int 1]
   in functionOutput' -- Mapping over integers (+1)
        P.map
        [Right $ tcon tInt, Right $ tcon tInt, Left addOne, Left $ list_ tInt $ map int ns]
        1000
        <===> Right (create' $ list_ tInt $ map (int . (+ 1)) ns)
  functionOutput' -- Mapping over bools (not)
    P.map
    [Right $ tcon tBool, Right $ tcon tBool, Left (gvar L.not), Left $ list_ tBool $ map bool_ bs]
    1000
    <===> Right (create' $ list_ tBool $ map (bool_ . not) bs)

tasty_sum_prop :: Property
tasty_sum_prop = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant (-10) 10))
  functionOutput P.sum [list_ tInt $ map int ns] 2000 <===> Right (create' $ int $ sum ns)
