module Tests.Prelude.Polymorphism where

import Foreword

import Hedgehog (forAll)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as Range
import Primer.Builtins (
  cCons,
  tBool,
  tList,
 )
import Primer.Builtins.DSL (
  bool_,
  listOf,
  listSat_,
  list_,
 )
import Primer.Core.DSL (
  apps,
  char,
  conSat,
  create',
  gvar,
  int,
  lam,
  lvar,
  tapp,
  tcon,
 )
import Primer.Prelude.Logic qualified as L
import Primer.Prelude.Polymorphism qualified as P
import Primer.Primitives (
  PrimDef (IntAdd, IntMinus),
  tChar,
  tInt,
 )
import Primer.Primitives.DSL (pfun)
import Tasty (Property, property)
import Test.Tasty.HUnit (
  Assertion,
 )
import Tests.EvalFull ((<~==>))
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

tasty_map_prop :: Property
tasty_map_prop = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant (-10) 10))
  bs <- forAll $ G.list (Range.linear 0 10) G.bool_
  let addOne = lam "x" $ apps (pfun IntAdd) [lvar "x", int 1]
   in functionOutput' -- Mapping over integers (+1)
        P.map
        [Right $ tcon tInt, Right $ tcon tInt, Left addOne, Left $ list_ tInt $ map int ns]
        1000
        <===> Right (create' $ listSat_ tInt $ map (int . (+ 1)) ns)
  functionOutput' -- Mapping over bools (not)
    P.map
    [Right $ tcon tBool, Right $ tcon tBool, Left (gvar L.not), Left $ list_ tBool $ map bool_ bs]
    1000
    <===> Right (create' $ listSat_ tBool $ map (bool_ . not) bs)

-- Right fold over a list of characters with @cons@.
tasty_foldr_list_char :: Property
tasty_foldr_list_char = property $ do
  as <- forAll $ G.list (Range.linear 0 10) G.unicode
  as' <- forAll $ G.list (Range.linear 0 10) G.unicode
  let cons = lam "x" $ lam "xs" $ conSat cCons [tcon tChar] [lvar "x", lvar "xs"]
   in functionOutput'
        P.foldr
        [Right $ listOf (tcon tChar), Right $ listOf (tcon tChar), Left cons, Left $ list_ tChar $ map char as, Left $ list_ tChar $ map char as']
        1000
        <===> Right (create' $ list_ tChar $ map char (foldr (:) as as'))

{- HLINT ignore tasty_foldr_bool "Use and" -}
-- Right fold over a list of bools with logical @and@.
tasty_foldr_bool :: Property
tasty_foldr_bool = property $ do
  bs <- forAll $ G.list (Range.linear 0 10) G.bool_
  functionOutput'
    P.foldr
    [Right $ tcon tBool, Right $ tcon tBool, Left (gvar L.and), Left $ bool_ True, Left $ list_ tBool $ map bool_ bs]
    1000
    <===> Right (create' $ bool_ (foldr (&&) True bs))

-- Ensure that @foldr@ is right-associative.
tasty_foldr_right_assoc :: Property
tasty_foldr_right_assoc = property $ do
  ns <- forAll $ G.list (Range.linear 0 10) (G.integral_ (Range.constant (-10) 10))
  let subtract' = lam "x" $ lam "y" $ apps (pfun IntMinus) [lvar "x", lvar "y"]
  functionOutput'
    P.foldr
    [Right $ tcon tInt, Right $ tcon tInt, Left subtract', Left $ int 0, Left $ list_ tInt $ map int ns]
    1000
    <===> Right (create' $ int (foldr (-) 0 ns))

-- Ensure that @foldr@ terminates early when the folding function
-- short-circuits. If this does not hold, this test will time out.
unit_foldr_short_circuit :: Assertion
unit_foldr_short_circuit =
  let bs = replicate 100 False
   in do
        functionOutput'
          P.foldr
          [Right $ tcon tBool, Right $ tcon tBool, Left (gvar L.and), Left $ bool_ True, Left $ list_ tBool $ map bool_ bs]
          100
          <~==> Right (create' $ bool_ False)
