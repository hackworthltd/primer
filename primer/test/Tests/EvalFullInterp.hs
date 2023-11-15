{-# LANGUAGE ViewPatterns #-}

-- TODO: DRY with EvalFullStep tests
-- (This is copy-and-pasted from those tests with some tests dropped, in
-- particular we cannot test intermediate results. It was then hacked
-- to compile (mostly due to interp only working with empty metadata).
-- Finally, a few tests were added.)
module Tests.EvalFullInterp where

import Foreword hiding (unlines)

import Data.Map qualified as M
import Hedgehog hiding (Property, Var, check, property, test, withDiscards, withTests)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (LabelName (LabelName))
import Hedgehog.Range qualified as Range
import Primer.Builtins (
  cCons,
  cFalse,
  cJust,
  cMakePair,
  cNothing,
  cSucc,
  cTrue,
  cZero,
  tBool,
  tList,
  tMaybe,
  tNat,
  tPair,
 )
import Primer.Builtins.DSL (boolAnn, bool_, list_, nat)
import Primer.Core
import Primer.Core.DSL
import Primer.Core.Utils (
  forgetMetadata,
  generateIDs,
 )
import Primer.Def (DefMap)
import Primer.Eval
import Primer.EvalFullInterp (InterpError (..), Timeout (MicroSec), interp, mkGlobalEnv)
import Primer.Examples qualified as Examples (
  even,
  map,
  map',
  odd,
 )
import Primer.Gen.Core.Typed (forAllT, propertyWT)
import Primer.Module (
  builtinTypes,
  moduleDefsQualified,
 )
import Primer.Primitives (
  PrimDef (
    EqChar,
    HexToNat,
    IntAdd,
    IntEq,
    IntFromNat,
    IntGT,
    IntGTE,
    IntLT,
    IntLTE,
    IntMinus,
    IntMul,
    IntNeq,
    IntQuot,
    IntQuotient,
    IntRem,
    IntRemainder,
    IntToNat,
    IsSpace,
    NatToHex,
    PrimConst,
    ToUpper
  ),
  tChar,
  tInt,
 )
import Primer.Primitives.DSL (pfun)
import Primer.Test.Expected (
  Expected (defMap, expectedResult, expr),
  mapEven,
 )
import Primer.Test.Util (
  primDefs,
 )
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck (
  typeDefs,
 )
import Tasty (
  Property,
  property,
  withDiscards,
  withTests,
 )
import Test.Tasty.HUnit (Assertion, (@?=))
import Tests.Eval.Utils (genDirTm, hasTypeLets, testModules)
import Tests.Gen.Core.Typed (checkTest)

unit_throw_no_branch :: Assertion
unit_throw_no_branch =
  let e = create1 $ case_ (con0 cTrue `ann` tcon tBool) [branch cFalse [] emptyHole]
   in do
        s <- evalFullTest builtinTypes mempty Chk e
        s @?= Left (NoBranch (Left cTrue) [PatCon cFalse])

unit_1 :: Assertion
unit_1 =
  let e = create1 emptyHole
   in do
        s <- evalFullTest' (MicroSec 0) mempty mempty Syn e
        s @?= Left Timeout

unit_2 :: Assertion
unit_2 =
  let e = create1 emptyHole
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= Right e

-- Check we don't have shadowing issues in types
unit_3 :: Assertion
unit_3 =
  let (expr, expected) = create2 $ do
        e <- letType "a" (tvar "b") $ emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "a" `tapp` tforall "a" ktype (tvar "a") `tapp` tforall "b" ktype (tcon' ["M"] "S" `tapp` tvar "a" `tapp` tvar "b"))
        let b' = "b_1"
        expect <- emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "b" `tapp` tforall "a" ktype (tvar "a") `tapp` tforall b' ktype (tcon' ["M"] "S" `tapp` tvar "b" `tapp` tvar b'))
        pure (e, expect)
   in do
        s <- evalFullTest mempty mempty Syn expr
        s @?= Right expected

-- Check we don't have shadowing issues in terms
unit_4 :: Assertion
unit_4 =
  let (expr, expected) = create2 $ do
        e <- let_ "a" (lvar "b") $ con' ["M"] "C" [lvar "a", lam "a" (lvar "a"), lam "b" (con' ["M"] "D" [lvar "a", lvar "b"])]
        let b' = "b_1"
        expect <- con' ["M"] "C" [lvar "b", lam "a" (lvar "a"), lam b' (con' ["M"] "D" [lvar "b", lvar b'])]
        pure (e, expect)
   in do
        s <- evalFullTest mempty mempty Syn expr
        s @?= Right expected

unit_5 :: Assertion
unit_5 =
  let e = forgetMetadata $ create' $ letrec "x" (lvar "x") (tcon tBool) (lvar "x")
   in do
        s <- evalFullTest' (MicroSec 10_000) mempty mempty Syn e
        s @?= Left Timeout

unit_6 :: Assertion
unit_6 =
  let (e, expt) = create2 $ do
        tr <- con0 cTrue
        an <- ann (pure tr) (tcon tBool)
        pure (an, tr)
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= Right e
        t <- evalFullTest mempty mempty Chk e
        t @?= Right expt

unit_7 :: Assertion
unit_7 =
  let e = create1 $ do
        let l = lam "x" $ lvar "x" `app` lvar "x"
        (l `ann` tEmptyHole) `app` l
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= Right e

unit_8 :: Assertion
unit_8 =
  let n = 10
      e = mapEven n
   in do
        s <- evalFullTest builtinTypes (defMap e) Syn (forgetMetadata $ expr e)
        s @?= Right (forgetMetadata $ expectedResult e)

-- A worker/wrapper'd map
unit_9 :: Assertion
unit_9 =
  let n = 10
      modName = mkSimpleModuleName "TestModule"
      (globals, forgetMetadata -> e, forgetMetadata -> expected) = create' $ do
        (mapName, mapDef) <- Examples.map' modName
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
        expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
        let globs = [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
        expect <- list_ (take n $ cycle [con0 cTrue, con0 cFalse]) `ann` (tcon tList `tapp` tcon tBool)
        pure (globs, expr, expect)
   in do
        s <- evalFullTest builtinTypes (M.fromList globals) Syn e
        s @?= Right expected

-- A case redex must have an scrutinee which is an annotated constructor.
-- Plain constructors are not well-typed here, for bidirectionality reasons,
-- although they just fail to reduce rather than the evaluator throwing a type error.
unit_10 :: Assertion
unit_10 =
  let (s, t, expected) = create3 $ do
        annCase <-
          case_
            (con0 cZero `ann` tcon tNat)
            [ branch cZero [] $ con0 cTrue
            , branch cSucc [("n", Nothing)] $ con0 cFalse
            ]
        noannCase <-
          case_
            (con0 cZero)
            [ branch cZero [] $ con0 cTrue
            , branch cSucc [("n", Nothing)] $ con0 cFalse
            ]
        expect <- con0 cTrue
        pure (annCase, noannCase, expect)
   in do
        s' <- evalFullTest builtinTypes mempty Syn s
        s' @?= Right expected
        t' <- evalFullTest builtinTypes mempty Syn t
        t' @?= Right t

unit_11 :: Assertion
unit_11 =
  let modName = mkSimpleModuleName "TestModule"
      (globals, forgetMetadata -> e, forgetMetadata -> expected) = create' $ do
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let ty = tcon tNat `tfun` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
        let expr1 =
              let_ "x" (con0 cZero)
                $ lam "n" (con cMakePair [gvar evenName `app` lvar "n", lvar "x"])
                `ann` ty
        expr <- expr1 `app` con0 cZero
        let globs = [(evenName, evenDef), (oddName, oddDef)]
        expect <-
          con cMakePair [con0 cTrue, con0 cZero]
            `ann` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
        pure (globs, expr, expect)
   in do
        s <- evalFullTest builtinTypes (M.fromList globals) Syn e
        s @?= Right expected

unit_12 :: Assertion
unit_12 =
  let (e, expected) = create2 $ do
        -- 'f' is a bit silly here, but could just as well be a definition of 'even'
        let f =
              lam "x"
                $ case_
                  (lvar "x")
                  [ branch cZero [] $ con0 cTrue
                  , branch cSucc [("i", Nothing)] $ lvar "f" `app` lvar "i"
                  ]
        expr <- let_ "n" (con0 cZero) $ letrec "f" f (tcon tNat `tfun` tcon tBool) $ lvar "f" `app` lvar "n"
        expect <- con0 cTrue `ann` tcon tBool
        pure (expr, expect)
   in do
        s <- evalFullTest builtinTypes mempty Syn e
        s @?= Right expected

unit_13 :: Assertion
unit_13 =
  let (e, expected) = create2 $ do
        expr <- (lam "x" (con' ["M"] "C" [lvar "x", let_ "x" (con0 cTrue) (lvar "x"), lvar "x"]) `ann` (tcon tNat `tfun` tcon tBool)) `app` con0 cZero
        expect <- con' ["M"] "C" [con0 cZero, con0 cTrue, con0 cZero] `ann` tcon tBool
        pure (expr, expect)
   in do
        s <- evalFullTest builtinTypes mempty Syn e
        s @?= Right expected

unit_14 :: Assertion
unit_14 =
  let (e, expected) = create2 $ do
        expr <- (lam "x" (lam "x" $ lvar "x") `ann` (tcon tBool `tfun` (tcon tNat `tfun` tcon tNat))) `app` con0 cTrue `app` con0 cZero
        expect <- con0 cZero `ann` tcon tNat
        pure (expr, expect)
   in do
        s <- evalFullTest builtinTypes mempty Syn e
        s @?= Right expected

unit_15 :: Assertion
unit_15 =
  let (expr, expected) = create2 $ do
        let l = let_ "x" (lvar "y")
        let c a b = con' ["M"] "C" [a, b]
        e0 <- l $ lam "y" $ c (lvar "x") (lvar "y")
        let y' = "y_1"
        e5 <- lam y' $ c (lvar "y") (lvar y')
        pure (e0, e5)
   in do
        s <- evalFullTest builtinTypes mempty Syn expr
        s @?= Right expected

unit_map_hole :: Assertion
unit_map_hole =
  let n = 3
      modName = mkSimpleModuleName "TestModule"
      (globals, forgetMetadata -> expr, forgetMetadata -> expected) = create' $ do
        (mapName, mapDef) <- Examples.map modName
        let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
        e <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` emptyHole `app` lst
        let globs = [(mapName, mapDef)]
        expect <- list_ (take n $ ((emptyHole `ann` (tcon tNat `tfun` tcon tBool)) `app`) <$> iterate (con1 cSucc) (con0 cZero)) `ann` (tcon tList `tapp` tcon tBool)
        pure (M.fromList globs, e, expect)
   in do
        sO <- evalFullTest builtinTypes globals Syn expr
        sO @?= Right expected

unit_hole_ann_case :: Assertion
unit_hole_ann_case =
  let tm = create1 $ hole $ ann (case_ emptyHole []) (tcon tBool)
   in do
        t <- evalFullTest builtinTypes mempty Chk tm
        t @?= Right tm

-- Check we don't have variable capture in
-- let x = y in case ? of C x -> x ; D y -> x
unit_case_let_capture :: Assertion
unit_case_let_capture =
  let (expr, expected) = create2 $ do
        let l = let_ "x" (lvar "y")
        e0 <-
          l
            $ case_
              emptyHole
              [ branch' (["M"], "C") [("x", Nothing)] (lvar "x")
              , branch' (["M"], "D") [("y", Nothing)] (lvar "x")
              ]
        e6 <-
          case_
            emptyHole
            [ branch' (["M"], "C") [("x", Nothing)] (lvar "x")
            , branch' (["M"], "D") [("y_1", Nothing)] (lvar "y")
            ]
        pure (e0, e6)
   in do
        s <- evalFullTest builtinTypes mempty Syn expr
        s @?= Right expected

-- tlet x = C in D x x
--   ==>
-- D C C
unit_tlet :: Assertion
unit_tlet =
  let (expr, expected) = create2 $ do
        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tvar "x" `tapp` tvar "x")
        e4 <- ann emptyHole $ tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tcon' ["M"] "C"
        pure (e0, e4)
   in do
        r <- evalFullTest mempty mempty Syn expr
        r @?= Right expected

-- tlet x = C in ty ==> ty  when x not occur free in ty
unit_tlet_elide :: Assertion
unit_tlet_elide = do
  let (expr, expected) = create2 $ do
        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D")
        e1 <- ann emptyHole $ tcon' ["M"] "D"
        pure (e0, e1)
   in do
        r <- evalFullTest mempty mempty Syn expr
        r @?= Right expected

-- tlet x = x in x
-- x
unit_tlet_self_capture :: Assertion
unit_tlet_self_capture = do
  let (expr, expected) = create2 $ do
        e0 <- ann emptyHole $ tlet "x" (tvar "x") $ tvar "x"
        e1 <- ann emptyHole $ tvar "x"
        pure (e0, e1)
   in do
        r <- evalFullTest mempty mempty Syn expr
        r @?= Right expected

unit_closed_let_beta :: Assertion
unit_closed_let_beta =
  let (expr, expected) = create2 $ do
        e0 <-
          let_
            "x"
            (con0 cFalse `ann` tcon tBool)
            ( lam "y" (con cCons [lvar "x", lvar "y"])
                `ann` (tcon tBool `tfun` (tcon tList `tapp` tcon tBool))
            )
            `app` con0 cTrue
        e8 <-
          con
            cCons
            [ con0 cFalse
            , con0 cTrue
            ]
            `ann` (tcon tList `tapp` tcon tBool)
        pure (e0, e8)
   in do
        r <- evalFullTest mempty mempty Syn expr
        r @?= Right expected

unit_closed_single_lets :: Assertion
unit_closed_single_lets =
  let (expr, expected) = create2 $ do
        e0 <-
          let_ "x" (con0 cFalse)
            $ let_ "y" (con0 cTrue)
            $ con
              cMakePair
              [ lvar "x"
              , lvar "y"
              ]
        e4 <-
          con
            cMakePair
            [ con0 cFalse
            , con0 cTrue
            ]
        pure (e0, e4)
   in do
        r <- evalFullTest mempty mempty Syn expr
        r @?= Right expected

unit_let_self_capture :: Assertion
unit_let_self_capture =
  let ( forgetMetadata -> expr2
        , forgetMetadata -> expected2
        , forgetMetadata -> expr3
        , forgetMetadata -> expected3b
        ) = create' $ do
          e2 <- lam "x" $ let_ "x" (lvar "x") (lvar "x")
          expect2 <- lam "x" $ lvar "x"
          e3 <- lAM "x" $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")
          expect3b <- lAM "x" $ emptyHole `ann` tvar "x"
          pure
            ( e2
            , expect2
            , e3
            , expect3b
            )
   in do
        s2 <- evalFullTest mempty mempty Chk expr2
        s2 @?= Right expected2
        s3 <- evalFullTest mempty mempty Chk expr3
        s3 @?= Right expected3b

-- | Evaluation preserves types
-- (assuming we don't end with a 'LetType' in the term, as the typechecker
-- cannot currently deal with those)
tasty_type_preservation :: Property
tasty_type_preservation = withTests 1000
  $ withDiscards 2000
  $ propertyWT testModules
  $ do
    let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
    tds <- asks typeDefs
    (dir, forgetMetadata -> t, ty) <- genDirTm
    s <- liftIO (evalFullTest' (MicroSec 10_000) tds globs dir t)
    case s of
      Left err -> label ("error: " <> LabelName (show err)) >> success
      Right s' -> do
        label "NF"
        annotateShow s'
        if hasTypeLets s'
          then label "skipped due to LetType" >> success
          else do
            s'' <- checkTest ty =<< generateIDs s'
            s' === forgetMetadata s'' -- check no smart holes happened

---- Unsaturated primitives are stuck terms
unit_prim_stuck :: Assertion
unit_prim_stuck =
  let (forgetMetadata -> f, prims) = create' $ (,) <$> pfun ToUpper <*> primDefs
   in do
        s <- evalFullTest mempty prims Syn f
        s @?= Right f

unit_prim_toUpper :: Assertion
unit_prim_toUpper =
  unaryPrimTest
    ToUpper
    (char 'a')
    (char 'A')

unit_prim_isSpace_1 :: Assertion
unit_prim_isSpace_1 =
  unaryPrimTest
    IsSpace
    (char '\n')
    (boolAnn True)

unit_prim_isSpace_2 :: Assertion
unit_prim_isSpace_2 =
  unaryPrimTest
    IsSpace
    (char 'a')
    (boolAnn False)

tasty_prim_hex_nat :: Property
tasty_prim_hex_nat = withTests 20 . property $ do
  n <- forAllT $ Gen.integral $ Range.constant 0 50
  let ne = nat n
      (dir, forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ if n <= 15
            then
              (Chk,,,)
                <$> case_
                  ( pfun NatToHex
                      `app` ne
                  )
                  [ branch
                      cNothing
                      []
                      (con cNothing [])
                  , branch
                      cJust
                      [("x", Nothing)]
                      ( pfun HexToNat
                          `app` lvar "x"
                      )
                  ]
                <*> con cJust [ne]
                <*> primDefs
            else
              (Syn,,,)
                <$> pfun NatToHex
                `app` ne
                <*> con cNothing []
                `ann` (tcon tMaybe `tapp` tcon tChar)
                <*> primDefs
  s <- evalIO $ evalFullTest builtinTypes prims dir e
  s === Right r

unit_prim_char_eq_1 :: Assertion
unit_prim_char_eq_1 =
  binaryPrimTest
    EqChar
    (char 'a')
    (char 'a')
    (con0 cTrue `ann` tcon tBool)

unit_prim_char_eq_2 :: Assertion
unit_prim_char_eq_2 =
  binaryPrimTest
    EqChar
    (char 'a')
    (char 'A')
    (con0 cFalse `ann` tcon tBool)

unit_prim_char_partial :: Assertion
unit_prim_char_partial =
  let (forgetMetadata -> e, prims) =
        create'
          $ (,)
          <$> pfun EqChar
          `app` char 'a'
          <*> primDefs
   in do
        s <- evalFullTest mempty prims Syn e
        s @?= Right e

unit_prim_int_add :: Assertion
unit_prim_int_add =
  binaryPrimTest
    IntAdd
    (int 2)
    (int 2)
    (int 4)

unit_prim_int_add_big :: Assertion
unit_prim_int_add_big =
  binaryPrimTest
    IntAdd
    (int big)
    (int big)
    (int (2 * big :: Integer))
  where
    big = fromIntegral (maxBound :: Word64)

unit_prim_int_sub :: Assertion
unit_prim_int_sub =
  binaryPrimTest
    IntMinus
    (int 5)
    (int 3)
    (int 2)

unit_prim_int_sub_negative :: Assertion
unit_prim_int_sub_negative =
  binaryPrimTest
    IntMinus
    (int 3)
    (int 5)
    (int (-2))

unit_prim_int_mul :: Assertion
unit_prim_int_mul =
  binaryPrimTest
    IntMul
    (int 3)
    (int 2)
    (int 6)

unit_prim_int_quotient :: Assertion
unit_prim_int_quotient =
  binaryPrimTest
    IntQuotient
    (int 7)
    (int 3)
    (con cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_quotient_negative :: Assertion
unit_prim_int_quotient_negative =
  binaryPrimTest
    IntQuotient
    (int (-7))
    (int 3)
    (con cJust [int (-3)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_quotient_zero :: Assertion
unit_prim_int_quotient_zero =
  binaryPrimTest
    IntQuotient
    (int (-7))
    (int 0)
    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder :: Assertion
unit_prim_int_remainder =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int 3)
    (con cJust [int 1] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_1 :: Assertion
unit_prim_int_remainder_negative_1 =
  binaryPrimTest
    IntRemainder
    (int (-7))
    (int (-3))
    (con cJust [int (-1)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_2 :: Assertion
unit_prim_int_remainder_negative_2 =
  binaryPrimTest
    IntRemainder
    (int (-7))
    (int 3)
    (con cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_3 :: Assertion
unit_prim_int_remainder_negative_3 =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int (-3))
    (con cJust [int (-2)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_zero :: Assertion
unit_prim_int_remainder_zero =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int 0)
    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_quot :: Assertion
unit_prim_int_quot =
  binaryPrimTest
    IntQuot
    (int 7)
    (int 3)
    (int 2)

unit_prim_int_quot_negative :: Assertion
unit_prim_int_quot_negative =
  binaryPrimTest
    IntQuot
    (int (-7))
    (int 3)
    (int (-3))

unit_prim_int_quot_zero :: Assertion
unit_prim_int_quot_zero =
  binaryPrimTest
    IntQuot
    (int (-7))
    (int 0)
    (int 0)

unit_prim_int_rem :: Assertion
unit_prim_int_rem =
  binaryPrimTest
    IntRem
    (int 7)
    (int 3)
    (int 1)

unit_prim_int_rem_negative_1 :: Assertion
unit_prim_int_rem_negative_1 =
  binaryPrimTest
    IntRem
    (int (-7))
    (int (-3))
    (int (-1))

unit_prim_int_rem_negative_2 :: Assertion
unit_prim_int_rem_negative_2 =
  binaryPrimTest
    IntRem
    (int (-7))
    (int 3)
    (int 2)

unit_prim_int_rem_negative_3 :: Assertion
unit_prim_int_rem_negative_3 =
  binaryPrimTest
    IntRem
    (int 7)
    (int (-3))
    (int (-2))

unit_prim_int_rem_zero :: Assertion
unit_prim_int_rem_zero =
  binaryPrimTest
    IntRem
    (int 7)
    (int 0)
    (int 7)

unit_prim_int_eq_1 :: Assertion
unit_prim_int_eq_1 =
  binaryPrimTest
    IntEq
    (int 2)
    (int 2)
    (boolAnn True)

unit_prim_int_eq_2 :: Assertion
unit_prim_int_eq_2 =
  binaryPrimTest
    IntEq
    (int 2)
    (int 1)
    (boolAnn False)

unit_prim_int_neq_1 :: Assertion
unit_prim_int_neq_1 =
  binaryPrimTest
    IntNeq
    (int 2)
    (int 2)
    (boolAnn False)

unit_prim_int_neq_2 :: Assertion
unit_prim_int_neq_2 =
  binaryPrimTest
    IntNeq
    (int 2)
    (int 1)
    (boolAnn True)

unit_prim_int_less_than_1 :: Assertion
unit_prim_int_less_than_1 =
  binaryPrimTest
    IntLT
    (int 1)
    (int 2)
    (boolAnn True)

unit_prim_int_less_than_2 :: Assertion
unit_prim_int_less_than_2 =
  binaryPrimTest
    IntLT
    (int 1)
    (int 1)
    (boolAnn False)

unit_prim_int_less_than_or_equal_1 :: Assertion
unit_prim_int_less_than_or_equal_1 =
  binaryPrimTest
    IntLTE
    (int 1)
    (int 2)
    (boolAnn True)

unit_prim_int_less_than_or_equal_2 :: Assertion
unit_prim_int_less_than_or_equal_2 =
  binaryPrimTest
    IntLTE
    (int 1)
    (int 1)
    (boolAnn True)

unit_prim_int_less_than_or_equal_3 :: Assertion
unit_prim_int_less_than_or_equal_3 =
  binaryPrimTest
    IntLTE
    (int 2)
    (int 1)
    (boolAnn False)

unit_prim_int_greater_than_1 :: Assertion
unit_prim_int_greater_than_1 =
  binaryPrimTest
    IntGT
    (int 2)
    (int 1)
    (boolAnn True)

unit_prim_int_greater_than_2 :: Assertion
unit_prim_int_greater_than_2 =
  binaryPrimTest
    IntGT
    (int 1)
    (int 1)
    (boolAnn False)

unit_prim_int_greater_than_or_equal_1 :: Assertion
unit_prim_int_greater_than_or_equal_1 =
  binaryPrimTest
    IntGTE
    (int 1)
    (int 2)
    (boolAnn False)

unit_prim_int_greater_than_or_equal_2 :: Assertion
unit_prim_int_greater_than_or_equal_2 =
  binaryPrimTest
    IntGTE
    (int 1)
    (int 1)
    (boolAnn True)

unit_prim_int_greater_than_or_equal_3 :: Assertion
unit_prim_int_greater_than_or_equal_3 =
  binaryPrimTest
    IntGTE
    (int 2)
    (int 1)
    (boolAnn True)

unit_prim_int_toNat :: Assertion
unit_prim_int_toNat =
  unaryPrimTest
    IntToNat
    (int 0)
    (con cJust [nat 0] `ann` (tcon tMaybe `tapp` tcon tNat))

unit_prim_int_toNat_negative :: Assertion
unit_prim_int_toNat_negative =
  unaryPrimTest
    IntToNat
    (int (-1))
    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tNat))

unit_prim_int_fromNat :: Assertion
unit_prim_int_fromNat =
  unaryPrimTest
    IntFromNat
    (nat 4)
    (int 4)

unit_prim_ann :: Assertion
unit_prim_ann =
  let (forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ (,,)
          <$> ( pfun ToUpper
                  `ann` (tcon tChar `tfun` tcon tChar)
              )
          `app` (char 'a' `ann` tcon tChar)
          <*> char 'A'
          <*> primDefs
   in do
        s <- evalFullTest builtinTypes prims Syn e
        s @?= Right r

unit_prim_lazy_1 :: Assertion
unit_prim_lazy_1 =
  let (forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ (,,)
          <$> pfun PrimConst
          `app` bool_ True
          `app` emptyHole
          <*> bool_ True
          `ann` tcon tBool
          <*> primDefs
   in do
        s <- evalFullTest builtinTypes prims Syn e
        s @?= Right r

unit_prim_lazy_2 :: Assertion
unit_prim_lazy_2 =
  let (forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ (,,)
          <$> pfun PrimConst
          `app` bool_ True
          `app` letrec "x" (lvar "x") (tcon tNat) (lvar "x")
          <*> bool_ True
          `ann` tcon tBool
          <*> primDefs
   in do
        s <- evalFullTest builtinTypes prims Syn e
        s @?= Right r

unit_prim_partial_map :: Assertion
unit_prim_partial_map =
  let modName = mkSimpleModuleName "TestModule"
      (forgetMetadata -> e, forgetMetadata -> r, gs, prims) =
        create' $ do
          (mapName, mapDef) <- Examples.map' modName
          (,,,)
            <$> gvar mapName
            `aPP` tcon tChar
            `aPP` tcon tChar
            `app` pfun ToUpper
            `app` list_
              [ char 'a'
              , char 'b'
              , char 'c'
              ]
            <*> list_
              [ char 'A'
              , char 'B'
              , char 'C'
              ]
            `ann` (tcon tList `tapp` tcon tChar)
            <*> pure (M.singleton mapName mapDef)
            <*> primDefs
   in do
        s <- evalFullTest builtinTypes (gs <> prims) Syn e
        s @?= Right r

-- TODO: enable when have EvalFullRequest with interp
---- Test that handleEvalFullRequest will reduce imported terms
-- unit_eval_full_modules :: Assertion
-- unit_eval_full_modules =
--  let test = do
--        builtinModule' <- builtinModule
--        primitiveModule' <- primitiveModule
--        importModules [primitiveModule', builtinModule']
--        foo <- pfun ToUpper `app` char 'a'
--        resp <-
--          readerToState
--            $ handleEvalFullRequest
--              EvalFullReq
--                { evalFullReqExpr = foo
--                , evalFullCxtDir = Chk
--                , evalFullMaxSteps = 2
--                , evalFullOptions = UnderBinders
--                }
--        expect <- char 'A'
--        pure $ case resp of
--          EvalFullRespTimedOut _ -> assertFailure "EvalFull timed out"
--          EvalFullRespNormal e -> e ~= expect
--      a = newEmptyApp
--   in runAppTestM a test <&> fst >>= \case
--        Left err -> assertFailure $ show err
--        Right assertion -> assertion

---- TODO: enable when have EvalFullRequest with interp
---- Test that handleEvalFullRequest will reduce case analysis of imported types
-- unit_eval_full_modules_scrutinize_imported_type :: Assertion
-- unit_eval_full_modules_scrutinize_imported_type =
--  let test = do
--        m' <- m
--        importModules [m']
--        foo <-
--          case_
--            (con0 cTrue `ann` tcon tBool)
--            [branch cTrue [] $ con0 cFalse, branch cFalse [] $ con0 cTrue]
--        resp <-
--          readerToState
--            $ handleEvalFullRequest
--            $ EvalFullReq
--              { evalFullReqExpr = foo
--              , evalFullCxtDir = Chk
--              , evalFullMaxSteps = 2
--              , evalFullOptions = UnderBinders
--              }
--        expect <- con0 cFalse
--        pure $ case resp of
--          EvalFullRespTimedOut _ -> assertFailure "EvalFull timed out"
--          EvalFullRespNormal e -> e ~= expect
--      a = newEmptyApp
--   in runAppTestM a test <&> fst >>= \case
--        Left err -> assertFailure $ show err
--        Right assertion -> assertion
--  where
--    m = do
--      boolDef' <- generateTypeDefIDs $ TypeDefAST boolDef
--      pure
--        $ Module
--          { moduleName = qualifiedModule tBool
--          , moduleTypes = Map.singleton (baseName tBool) boolDef'
--          , moduleDefs = mempty
--          }

unit_wildcard :: Assertion
unit_wildcard =
  let loop = letrec "x" (lvar "x") (tcon tNat) (lvar "x")
      eTerm = create1 $ caseFB_ loop [] (con0 cTrue)
      expectTerm = forgetMetadata $ create' $ con0 cTrue
      eDiverge = create1 $ caseFB_ loop [branch cZero [] $ con0 cFalse] (con0 cTrue)
   in do
        s <- evalFullTest mempty mempty Syn eTerm
        s @?= Right expectTerm
        t <- evalFullTest' (MicroSec 10_000) mempty mempty Syn eDiverge
        t @?= Left Timeout

unit_case_prim :: Assertion
unit_case_prim =
  let e1 = create1 $ caseFB_ (char 'a') [] (con0 cTrue)
      expect1 = create1 $ con0 cTrue
      e2 = create1 $ caseFB_ (char 'a') [branchPrim (PrimChar 'a') $ con0 cFalse] (con0 cTrue)
      expect2 = create1 $ con0 cFalse
      e3 =
        create1
          $ caseFB_
            (char 'b')
            [ branchPrim (PrimChar 'a') $ con0 cTrue
            , branchPrim (PrimChar 'b') $ con0 cFalse
            ]
            (con0 cTrue)
      expect3 = create1 $ con0 cFalse
      e4 =
        create1
          $ caseFB_
            ( (lam "x" (lvar "x") `ann` (tcon tChar `tfun` tcon tChar))
                `app` char 'a'
            )
            [branchPrim (PrimChar 'a') $ con0 cFalse]
            (con0 cTrue)
      expect4 = create1 $ con0 cFalse
   in do
        s1 <- evalFullTest mempty mempty Syn e1
        s1 @?= Right expect1
        s2 <- evalFullTest mempty mempty Syn e2
        s2 @?= Right expect2
        s3 <- evalFullTest mempty mempty Syn e3
        s3 @?= Right expect3
        s4 <- evalFullTest mempty mempty Syn e4
        s4 @?= Right expect4

-- Taking the head of an infinite list works
-- (this tests our interpreter is lazy enough)
unit_lazy_head :: Assertion
unit_lazy_head =
  let hd = lAM "a" $ lam "xs" $ caseFB_ (lvar "xs") [branch cCons [("y", Nothing), ("ys", Nothing)] $ lvar "y"] emptyHole
      hdTy = tforall "a" ktype $ (tcon tList `tapp` tvar "a") `tfun` tvar "a"
      repTrue = letrec "r" (con cCons [con0 cTrue, lvar "r"]) (tcon tList `tapp` tcon tBool) (lvar "r")
      e = create1 $ (hd `ann` hdTy) `aPP` tcon tBool `app` repTrue
      expect = create1 $ con0 cTrue `ann` tcon tBool
   in do
        s <- evalFullTest builtinTypes mempty Syn e
        s @?= Right expect

-- * Utilities

evalFullTest' :: Timeout -> TypeDefMap -> DefMap -> Dir -> Expr' () () () -> IO (Either InterpError (Expr' () () ()))
evalFullTest' t tydefs = interp t tydefs . mkGlobalEnv

evalFullTest :: TypeDefMap -> DefMap -> Dir -> Expr' () () () -> IO (Either InterpError (Expr' () () ()))
evalFullTest = evalFullTest' (MicroSec (-1)) -- negative time means wait forever

unaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> Assertion
unaryPrimTest f x y =
  let (forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ (,,)
          <$> pfun f
          `app` x
          <*> y
          <*> primDefs
   in do
        s <- evalFullTest mempty prims Syn e
        s @?= Right r
binaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> S Expr -> Assertion
binaryPrimTest f x y z =
  let (forgetMetadata -> e, forgetMetadata -> r, prims) =
        create'
          $ (,,)
          <$> pfun f
          `app` x
          `app` y
          <*> z
          <*> primDefs
   in do
        s <- evalFullTest mempty prims Syn e
        s @?= Right r

create1 :: S (Expr' a b c) -> Expr' () () ()
create1 = forgetMetadata . create'

create2 :: S (Expr' a1 b1 c1, Expr' a2 b2 c2) -> (Expr' () () (), Expr' () () ())
create2 = bimap forgetMetadata forgetMetadata . create'

create3 ::
  S (Expr' a1 b1 c1, Expr' a2 b2 c2, Expr' a3 b3 c3) ->
  (Expr' () () (), Expr' () () (), Expr' () () ())
create3 = (\(x, y, z) -> (forgetMetadata x, forgetMetadata y, forgetMetadata z)) . create'
