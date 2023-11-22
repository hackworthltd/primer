{-# LANGUAGE ViewPatterns #-}
module Tests.EvalFullInterp where

import Foreword hiding (unlines)

import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.String (unlines)
import Hedgehog hiding (Property, Var, check, property, test, withDiscards, withTests)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (LabelName (unLabelName))
import Hedgehog.Range qualified as Range
import Optics
import Primer.App (
  EvalFullReq (EvalFullReq, evalFullCxtDir, evalFullMaxSteps, evalFullOptions, evalFullReqExpr),
  EvalFullResp (EvalFullRespNormal, EvalFullRespTimedOut),
  handleEvalFullRequest,
  importModules,
  newEmptyApp,
 )
import Primer.Builtins (
  boolDef,
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
  tPair, cNil,
 )
import Primer.Builtins.DSL (boolAnn, bool_, list_, nat)
import Primer.Core
import Primer.Core.DSL
import Primer.Core.Utils (
  exprIDs,
  forgetMetadata,
  forgetTypeMetadata,
  generateIDs,
 )
import Primer.Def (DefMap, ASTDef (ASTDef), Def (DefAST))
import Primer.Eval
import Primer.EvalFullStep
import Primer.Examples qualified as Examples (
  even,
  map,
  map',
  odd,
 )
import Primer.Gen.Core.Typed (WT, forAllT, genChk, isolateWT, propertyWT)
import Primer.Log (runPureLogT)
import Primer.Module (
  Module (Module, moduleDefs, moduleName, moduleTypes),
  builtinModule,
  builtinTypes,
  moduleDefsQualified,
  primitiveModule,
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
import Primer.Test.App (
  runAppTestM,
 )
import Primer.Test.Expected (
  Expected (defMap, expectedResult, expr, maxID),
  mapEven,
 )
import Primer.Test.TestM (
  evalTestM,
 )
import Primer.Test.Util (
  assertNoSevereLogs,
  failWhenSevereLogs,
  primDefs,
  testNoSevereLogs,
  zeroIDs,
 )
import Primer.TypeDef (TypeDef (..), TypeDefMap, generateTypeDefIDs)
import Primer.Typecheck (
  SmartHoles (NoSmartHoles),
  check,
  extendGlobalCxt,
  typeDefs,
 )
import Protolude.Partial (fromJust)
import Tasty (
  Property,
  property,
  withDiscards,
  withTests,
 )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import Tests.Action.Prog (readerToState)
import Tests.Eval.Utils (genDirTm, hasHoles, hasTypeLets, testModules, (~=))
import Tests.Gen.Core.Typed (checkTest)
import Tests.Typecheck (runTypecheckTestM, runTypecheckTestMWithPrims)
import Primer.EvalFullInterp (interp)

-- TODO: can I integrate with existing tests?
-- The tests here are copy-pasted from stepwise test
--unit_1 :: Assertion
--unit_1 =
--  let (e, maxID) = create emptyHole
--   in do
--        s <- evalFullTest maxID mempty mempty 0 Syn e
--        s <~==> Left (TimedOut e)

unit_2 :: Assertion
unit_2 =
  let (e, maxID) = first forgetMetadata $ create emptyHole -- TODO: so ugly
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= e

-- Check we don't have shadowing issues in types
unit_3 :: Assertion
unit_3 =
  let ((expr, expected), maxID) = first (bimap forgetMetadata forgetMetadata) $ create $ do -- TODO: so ugly!
        -- NB: added a @letType b = Bool@ wrapper
        e <- letType "b" (tcon tBool) $ letType "a" (tvar "b") $ emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "a" `tapp` tforall "a" ktype (tvar "a") `tapp` tforall "b" ktype (tcon' ["M"] "S" `tapp` tvar "a" `tapp` tvar "b"))
        expect <- emptyHole `ann` (tcon' ["M"] "T" `tapp` tcon tBool `tapp` tforall "a" ktype (tvar "a") `tapp` tforall "b" ktype (tcon' ["M"] "S" `tapp` tcon tBool `tapp` tvar "b"))
        pure (e, expect)
   in do
        s <- evalFullTest mempty mempty Syn expr
        s @?= expected

-- Check we don't have shadowing issues in terms
unit_4 :: Assertion
unit_4 =
  let ((expr, expected), maxID) = first (bimap forgetMetadata forgetMetadata) $ create $ do
        -- NB: added a @let b = True : Bool@ wrapper
        e <- let_ "b" (con0 cTrue `ann` tcon tBool) $ let_ "a" (lvar "b") $ con' ["M"] "C" [lvar "a", lam "a" (lvar "a"), lam "b" (con' ["M"] "D" [lvar "a", lvar "b"])]
        expect <- con' ["M"] "C" [con0 cTrue, lam "a" (lvar "a"), lam "b" (con' ["M"] "D" [con0 cTrue, lvar "b"])]
        pure (e, expect)
   in do
        s <- evalFullTest mempty mempty Syn expr
        s @?= expected

-- NB: can't work with interp, as not terminate!
---- This test is slightly unfortunate.
---- Writing [_] for embeddings we don't reduce [ e ] : T (and I'm not
---- sure if we should). This leads to the annotation in the output.
---- See https://github.com/hackworthltd/primer/issues/12
--unit_5 :: Assertion
--unit_5 =
--  let ((e, expt), maxID) = first (bimap forgetMetadata forgetMetadata) $ create $ do
--        a <- letrec "x" (lvar "x") (tcon tBool) (lvar "x")
--        b <- letrec "x" (lvar "x") (tcon tBool) (lvar "x") `ann` tcon tBool
--        pure (a, b)
--   in do
--        s <- evalFullTest mempty mempty Syn e
--        s <~==> Left (TimedOut expt)

unit_6 :: Assertion
unit_6 =
  let ((e, expt), maxID) = first (bimap forgetMetadata forgetMetadata) $ create $ do
        tr <- con0 cTrue
        an <- ann (pure tr) (tcon tBool)
        pure (an, tr)
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= e
        t <- evalFullTest mempty mempty Chk e
        t @?= expt

-- TODO: do we want to expand
--   (λ x. t) : ?
-- to
--   (λ x. t) : ? -> ?
-- and thus have an infinite derivation for
--   ((λ x . x x) : ?) (λ x. x x)
-- Currently we don't, so this is a stuck term
unit_7 :: Assertion
unit_7 =
  let (e, maxID) = first forgetMetadata $ create $ do
        let l = lam "x" $ lvar "x" `app` lvar "x"
        (l `ann` tEmptyHole) `app` l
   in do
        s <- evalFullTest mempty mempty Syn e
        s @?= e

unit_8 :: Assertion
unit_8 =
  let n = 10
      e = mapEven n
   in do
        s <- evalFullTest builtinTypes (defMap e) Syn (forgetMetadata $ expr e)
        s @?= forgetMetadata (expectedResult e)

-- A worker/wrapper'd map
unit_9 :: Assertion
unit_9 =
  let n = 10
      modName = mkSimpleModuleName "TestModule"
      ((globals, e, expected), maxID) = first (bimap forgetMetadata forgetMetadata) $ create $ do
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
        s @?= expected

-- A case redex must have an scrutinee which is an annotated constructor.
-- Plain constructors are not well-typed here, for bidirectionality reasons,
-- although they just fail to reduce rather than the evaluator throwing a type error.
unit_10 :: Assertion
unit_10 =
  let ((forgetMetadata -> s,forgetMetadata -> t,forgetMetadata -> expected), maxID) = create $ do
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
        s' @?= expected
        t' <- evalFullTest builtinTypes mempty Syn t
        t' @?= t

unit_11 :: Assertion
unit_11 =
  let modName = mkSimpleModuleName "TestModule"
      ((globals, forgetMetadata -> e,forgetMetadata -> expected), maxID) = create $ do
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
        s @?= expected

unit_12 :: Assertion
unit_12 =
  let ((forgetMetadata -> e,forgetMetadata -> expected), maxID) = create $ do
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
        s @?= expected

unit_13 :: Assertion
unit_13 =
  let ((forgetMetadata -> e,forgetMetadata -> expected), maxID) = create $ do
        expr <- (lam "x" (con' ["M"] "C" [lvar "x", let_ "x" (con0 cTrue) (lvar "x"), lvar "x"]) `ann` (tcon tNat `tfun` tcon tBool)) `app` con0 cZero
        expect <- con' ["M"] "C" [con0 cZero, con0 cTrue, con0 cZero] `ann` tcon tBool
        pure (expr, expect)
   in do
        s <- evalFullTest builtinTypes mempty Syn e
        s @?= expected

unit_14 :: Assertion
unit_14 =
  let ((forgetMetadata ->e,forgetMetadata -> expected), maxID) = create $ do
        expr <- (lam "x" (lam "x" $ lvar "x") `ann` (tcon tBool `tfun` (tcon tNat `tfun` tcon tNat))) `app` con0 cTrue `app` con0 cZero
        expect <- con0 cZero `ann` tcon tNat
        pure (expr, expect)
   in do
        s <- evalFullTest  builtinTypes mempty Syn e
        s @?= expected

-- TODO: revisit this comment!
-- TODO: revisit this test -- does it do anything useful since we cannot view each step?
-- Sometimes we need to rename a binder in order to push a let past it
--   let x = y in λy.C x y
--   let x = y in λz. let y = z in C x y
--   λz. let x = y in let y = z in C x y
--   λz. C (let x = y in x) (let y = z in y)
--   λz. C y (let y = z in y)
--   λz. C y z
unit_15 :: Assertion
unit_15 =
  let ((forgetMetadata -> expr,forgetMetadata -> expected), maxID) = create $ do
        let l = let_ "x" (lvar "y")
        let c a b = con' ["M"] "C" [a, b]
        -- NB/TODO: I have added a lam y wrapper
        e0 <- lam "y" $ l $ lam "y" $ c (lvar "x") (lvar "y")
        let y' = "a40"
        {-
        let rny = let_ "y" (lvar y')
        e1 <- l $ lam y' $ rny $ c (lvar "x") (lvar "y")
        e2 <- lam y' $ l $ rny $ c (lvar "x") (lvar "y")
        e3 <- lam y' $ c (l $ lvar "x") (rny $ lvar "y")
        e4 <- lam y' $ c (lvar "y") (rny $ lvar "y")
        -}
        e5 <- lam "y" $ lam y' $ c (lvar "y") (lvar y')
        pure (e0, e5)
   in do
        s <- evalFullTest builtinTypes mempty Syn expr
        s @?= expected
--
--unit_map_hole :: Assertion
--unit_map_hole =
--  let n = 3
--      modName = mkSimpleModuleName "TestModule"
--      ((globals, expr, expected), maxID) = create $ do
--        (mapName, mapDef) <- Examples.map modName
--        let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
--        e <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` emptyHole `app` lst
--        let globs = [(mapName, mapDef)]
--        expect <- list_ (take n $ ((emptyHole `ann` (tcon tNat `tfun` tcon tBool)) `app`) <$> iterate (con1 cSucc) (con0 cZero)) `ann` (tcon tList `tapp` tcon tBool)
--        pure (M.fromList globs, e, expect)
--   in do
--        sO <- evalFullTest maxID builtinTypes globals 200 Syn expr
--        sO <~==> Right expected
--        sCG <- evalFullTestClosed GroupedLets maxID builtinTypes globals 200 Syn expr
--        sCG <~==> Right expected
--        sCS <- evalFullTestClosed SingleLets maxID builtinTypes globals 300 Syn expr
--        sCS <~==> Right expected
--
--unit_hole_ann_case :: Assertion
--unit_hole_ann_case =
--  let (tm, maxID) = create $ hole $ ann (case_ emptyHole []) (tcon tBool)
--   in do
--        t <- evalFullTest maxID builtinTypes mempty 1 Chk tm
--        t @?= Right tm
--
---- Check we don't have variable capture in
---- let x = y in case ? of C x -> x ; D y -> x
--unit_case_let_capture :: Assertion
--unit_case_let_capture =
--  let ((expr, steps, expected), maxID) = create $ do
--        let l = let_ "x" (lvar "y")
--        let w = "a66"
--        let z = "a69"
--        let rnx = let_ "x" (lvar w)
--        let rny = let_ "y" (lvar z)
--        e0 <-
--          l
--            $ case_
--              emptyHole
--              [ branch' (["M"], "C") [("x", Nothing)] (lvar "x")
--              , branch' (["M"], "D") [("y", Nothing)] (lvar "x")
--              ]
--        e1 <-
--          l
--            $ case_
--              emptyHole
--              [ branch' (["M"], "C") [(w, Nothing)] (rnx $ lvar "x")
--              , branch' (["M"], "D") [("y", Nothing)] (lvar "x")
--              ]
--        e2 <-
--          l
--            $ case_
--              emptyHole
--              [ branch' (["M"], "C") [(w, Nothing)] (rnx $ lvar "x")
--              , branch' (["M"], "D") [(z, Nothing)] (rny $ lvar "x")
--              ]
--        e3 <-
--          case_
--            emptyHole
--            [ branch' (["M"], "C") [(w, Nothing)] (rnx $ lvar "x")
--            , branch' (["M"], "D") [(z, Nothing)] (l $ rny $ lvar "x")
--            ]
--        e4 <-
--          case_
--            emptyHole
--            [ branch' (["M"], "C") [(w, Nothing)] (lvar w)
--            , branch' (["M"], "D") [(z, Nothing)] (l $ rny $ lvar "x")
--            ]
--        e5 <-
--          case_
--            emptyHole
--            [ branch' (["M"], "C") [(w, Nothing)] (lvar w)
--            , branch' (["M"], "D") [(z, Nothing)] (l $ lvar "x")
--            ]
--        e6 <-
--          case_
--            emptyHole
--            [ branch' (["M"], "C") [(w, Nothing)] (lvar w)
--            , branch' (["M"], "D") [(z, Nothing)] (lvar "y")
--            ]
--        pure (e0, [e0, e1, e2, e3, e4, e5, e6], e6)
--   in do
--        si <- traverse (\i -> evalFullTest maxID builtinTypes mempty i Syn expr) [0 .. fromIntegral $ length steps - 1]
--        zipWithM_ (\s e -> s <~==> Left (TimedOut e)) si steps
--        s <- evalFullTest maxID builtinTypes mempty (fromIntegral $ length steps) Syn expr
--        s <~==> Right expected
--
---- We must evaluate inside the body of a let before the binding:
---- consider @let x = ((λy.t : A -> B) r) in letrec xs = s[x] : S in xs@
---- the two possible reductions are to inline the @letrec@s or to reduce the beta.
---- We should do the @letrec@ first.
--unit_letrec_body_first :: Assertion
--unit_letrec_body_first =
--  let lx = let_ "x" ((lam "x" (lvar "x") `ann` (tcon tBool `tfun` tcon tBool)) `app` con0 cTrue)
--      lxs =
--        letrec
--          "xs"
--          (con cCons [lvar "x", lvar "xs"])
--          (tcon tList `tapp` tEmptyHole)
--      (expr, maxID) = create $ lx $ lxs (lvar "xs")
--      expected1 = create' $ lx $ lxs (con cCons [lvar "x", lvar "xs"]) `ann` (tcon tList `tapp` tEmptyHole)
--      expected2 = create' $ lx (lxs (con cCons [lvar "x", lvar "xs"])) `ann` (tcon tList `tapp` tEmptyHole)
--      expected3 = create' $ con cCons [lx $ lvar "x", lx $ lxs $ lvar "xs"] `ann` (tcon tList `tapp` tEmptyHole)
--   in do
--        e1 <- evalFullTest maxID builtinTypes mempty 1 Syn expr
--        e1 <~==> Left (TimedOut expected1)
--        e2 <- evalFullTest maxID builtinTypes mempty 2 Syn expr
--        e2 <~==> Left (TimedOut expected2)
--        e3 <- evalFullTest maxID builtinTypes mempty 3 Syn expr
--        e3 <~==> Left (TimedOut expected3)
--
---- tlet x = C in D x x
----   ==>
---- (tlet x = C in D x) (tlet x = C in x)
----   ==>
---- D (tlet x = C in x) (tlet x = C in x)
----   ==>
---- D C (tlet x = C in x)
----   ==>
---- D C C
--unit_tlet :: Assertion
--unit_tlet =
--  let ((expr, expected), maxID) = create $ do
--        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tvar "x" `tapp` tvar "x")
--        e1 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tvar "x") `tapp` tlet "x" (tcon' ["M"] "C") (tvar "x")
--        e2 <- ann emptyHole $ tcon' ["M"] "D" `tapp` tlet "x" (tcon' ["M"] "C") (tvar "x") `tapp` tlet "x" (tcon' ["M"] "C") (tvar "x")
--        e3 <- ann emptyHole $ tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tlet "x" (tcon' ["M"] "C") (tvar "x")
--        e4 <- ann emptyHole $ tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tcon' ["M"] "C"
--        pure (e0, map (Left . TimedOut) [e0, e1, e2, e3, e4] ++ [Right e4])
--      test (n, expect) = do
--        r <- evalFullTest maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- tlet x = C in ty ==> ty  when x not occur free in ty
--unit_tlet_elide :: Assertion
--unit_tlet_elide = do
--  let ((expr, expected), maxID) = create $ do
--        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D")
--        e1 <- ann emptyHole $ tcon' ["M"] "D"
--        pure (e0, map (Left . TimedOut) [e0, e1] ++ [Right e1])
--      test (n, expect) = do
--        r <- evalFullTest maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- tlet x = x in x
---- x
--unit_tlet_self_capture :: Assertion
--unit_tlet_self_capture = do
--  let ((expr, expected), maxID) = create $ do
--        e0 <- ann emptyHole $ tlet "x" (tvar "x") $ tvar "x"
--        e1 <- ann emptyHole $ tvar "x"
--        pure (e0, map (Left . TimedOut) [e0, e1] ++ [Right e1])
--      test (n, expect) = do
--        r <- evalFullTest maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- When doing closed eval (i.e. don't go under binders), pushing a @let@
---- through a binder is not considered to be "under" that binder, else
---- @(let x=t1 in λy.t2 : S -> T) t3@ would be stuck.
--unit_closed_let_beta :: Assertion
--unit_closed_let_beta =
--  let ((expr, expected), maxID) = create $ do
--        e0 <-
--          let_
--            "x"
--            (con0 cFalse `ann` tcon tBool)
--            ( lam "y" (con cCons [lvar "x", lvar "y"])
--                `ann` (tcon tBool `tfun` (tcon tList `tapp` tcon tBool))
--            )
--            `app` con0 cTrue
--        e1 <-
--          let_
--            "x"
--            (con0 cFalse `ann` tcon tBool)
--            ( lam
--                "y"
--                (con cCons [lvar "x", lvar "y"])
--            )
--            `ann` (tcon tBool `tfun` (tcon tList `tapp` tcon tBool))
--            `app` con0 cTrue
--        e2 <-
--          lam
--            "y"
--            ( let_
--                "x"
--                (con0 cFalse `ann` tcon tBool)
--                (con cCons [lvar "x", lvar "y"])
--            )
--            `ann` (tcon tBool `tfun` (tcon tList `tapp` tcon tBool))
--            `app` con0 cTrue
--        e3 <-
--          let_
--            "y"
--            (con0 cTrue `ann` tcon tBool)
--            ( let_
--                "x"
--                (con0 cFalse `ann` tcon tBool)
--                (con cCons [lvar "x", lvar "y"])
--            )
--            `ann` (tcon tList `tapp` tcon tBool)
--        e4 <-
--          con
--            cCons
--            [ let_
--                "x"
--                (con0 cFalse `ann` tcon tBool)
--                (lvar "x")
--            , let_
--                "y"
--                (con0 cTrue `ann` tcon tBool)
--                (lvar "y")
--            ]
--            `ann` (tcon tList `tapp` tcon tBool)
--        e5 <-
--          con
--            cCons
--            [ con0 cFalse `ann` tcon tBool
--            , let_
--                "y"
--                (con0 cTrue `ann` tcon tBool)
--                (lvar "y")
--            ]
--            `ann` (tcon tList `tapp` tcon tBool)
--        e6 <-
--          con
--            cCons
--            [ con0 cFalse
--            , let_
--                "y"
--                (con0 cTrue `ann` tcon tBool)
--                (lvar "y")
--            ]
--            `ann` (tcon tList `tapp` tcon tBool)
--        e7 <-
--          con
--            cCons
--            [ con0 cFalse
--            , con0 cTrue `ann` tcon tBool
--            ]
--            `ann` (tcon tList `tapp` tcon tBool)
--        e8 <-
--          con
--            cCons
--            [ con0 cFalse
--            , con0 cTrue
--            ]
--            `ann` (tcon tList `tapp` tcon tBool)
--        pure (e0, map (Left . TimedOut) [e0, e1, e2, e3, e4, e5, e6, e7, e8] ++ [Right e8])
--      test (n, expect) = do
--        r <- evalFullTestClosed GroupedLets maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- Closed eval and handling groups of @let@s singlely work together
--unit_closed_single_lets :: Assertion
--unit_closed_single_lets =
--  let ((expr, expected), maxID) = create $ do
--        e0 <-
--          let_ "x" (con0 cFalse)
--            $ let_ "y" (con0 cTrue)
--            $ con
--              cMakePair
--              [ lvar "x"
--              , lvar "y"
--              ]
--        e1 <-
--          let_ "x" (con0 cFalse)
--            $ con
--              cMakePair
--              [ lvar "x"
--              , let_ "y" (con0 cTrue) $ lvar "y"
--              ]
--        e2 <-
--          con
--            cMakePair
--            [ let_ "x" (con0 cFalse) $ lvar "x"
--            , let_ "y" (con0 cTrue) $ lvar "y"
--            ]
--        e3 <-
--          con
--            cMakePair
--            [ con0 cFalse
--            , let_ "y" (con0 cTrue) $ lvar "y"
--            ]
--        e4 <-
--          con
--            cMakePair
--            [ con0 cFalse
--            , con0 cTrue
--            ]
--        pure (e0, map (Left . TimedOut) [e0, e1, e2, e3, e4] ++ [Right e4])
--      test (n, expect) = do
--        r <- evalFullTestClosed SingleLets maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- One reason for not evaluating under binders is to avoid a size blowup when
---- evaluating a recursive definition. For example, the unsaturated
---- `map @Bool @Bool not` would keep unrolling the recursive mentions of `map`.
---- (If it were applied to a concrete list, the beta redexes would be reduced instead.)
---- Since top-level definitions and recursive lets are essentially the same, one may
---- worry that we have the same issue with @letrec@. This test shows that closed eval
---- handles that case also.
--unit_closed_letrec_binder :: Assertion
--unit_closed_letrec_binder =
--  let ((expr, expected), maxID) = create $ do
--        e0 <-
--          letrec "x" (list_ [lvar "x", lvar "x"]) (tcon tBool)
--            $ lam "y"
--            $ lvar "x"
--        e1 <-
--          lam "y"
--            $ letrec "x" (list_ [lvar "x", lvar "x"]) (tcon tBool)
--            $ lvar "x"
--        pure (e0, map (Left . TimedOut) [e0, e1] ++ [Right e1])
--      test (n, expect) = do
--        r <- evalFullTestClosed GroupedLets maxID mempty mempty n Syn expr
--        r <~==> expect
--   in mapM_ test (zip [0 ..] expected)
--
---- closed eval stops at binders
--unit_closed_binders :: Assertion
--unit_closed_binders = do
--  let isNormalIffClosed e = do
--        let (e', i) = create e
--        evalFullTestClosed GroupedLets i mempty mempty 1 Syn e' >>= \case
--          Left (TimedOut _) -> assertFailure $ "not normal form, for closed eval, grouped lets: " <> show e'
--          Right _ -> pure ()
--        evalFullTestClosed SingleLets i mempty mempty 1 Syn e' >>= \case
--          Left (TimedOut _) -> assertFailure $ "not normal form, for closed eval, single lets: " <> show e'
--          Right _ -> pure ()
--        evalFullTest i mempty mempty 1 Syn e' >>= \case
--          Left (TimedOut _) -> pure ()
--          Right _ -> assertFailure $ "unexpectedly a normal form, for open eval: " <> show e'
--      r = let_ "x" emptyHole $ lvar "x"
--  isNormalIffClosed $ lam "x" r
--  isNormalIffClosed $ lAM "a" r
--  isNormalIffClosed $ case_ emptyHole [branch cTrue [("x", Nothing)] r]
--  -- For consistency, we also do not reduce inside case branches even if they do not bind
--  isNormalIffClosed $ case_ emptyHole [branch cTrue [] r]
--
---- closed eval still pushes lets through binders
--unit_closed_subst :: Assertion
--unit_closed_subst = do
--  let isReducible e = do
--        let (e', i) = create e
--        evalFullTestClosed GroupedLets i mempty mempty 1 Syn e' >>= \case
--          Left (TimedOut _) -> pure ()
--          Right _ -> assertFailure $ "unexpectedly a normal form: " <> show e'
--      l = let_ "x" emptyHole
--      v = lvar "x"
--  isReducible $ l $ lam "y" v
--  isReducible $ l $ lAM "a" v
--  isReducible $ l $ case_ emptyHole [branch cTrue [("x", Nothing)] v]
--  isReducible $ l $ case_ emptyHole [branch cTrue [] v]
--
---- For (closed, hole free) terms of base types, open and closed evaluation
---- agree.  We require hole-free-ness, as holes create stuck terms similar to
---- free variables. Note that we get the same reduction sequence, not only that
---- they reduce to the same value.
--tasty_open_closed_agree_base_types :: Property
--tasty_open_closed_agree_base_types = withDiscards 1000
--  $ propertyWT testModules
--  $ do
--    let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = False}
--    let optsR = RunRedexOptions{pushAndElide = True}
--    ty <- forAllT $ Gen.element @[] [tBool, tNat, tInt]
--    tm' <- forAllT $ genChk $ TCon () ty
--    tm <- generateIDs tm'
--    when (hasHoles tm) discard
--    tds <- asks typeDefs
--    let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
--    let reductionSequence closed expr n =
--          (expr :)
--            <$> if n <= (0 :: Integer)
--              then pure []
--              else
--                evalFull @EvalLog closed optsV optsR tds globs 1 Chk expr >>= \case
--                  Left (TimedOut expr') -> reductionSequence closed expr' (n - 1)
--                  Right _ -> pure []
--    (openSeq, openLogs) <- lift $ isolateWT $ runPureLogT $ reductionSequence UnderBinders tm 100
--    testNoSevereLogs openLogs
--    (closedSeq, closedLogs) <- lift $ isolateWT $ runPureLogT $ reductionSequence StopAtBinders tm 100
--    testNoSevereLogs closedLogs
--    openSeq === closedSeq
--
---- TODO: examples with holes
--
---- TODO: most of these property tests could benefit from generating an
---- arbitrary context first.
---- See https://github.com/hackworthltd/primer/issues/50
--
---- | Resuming evaluation is the same as running it for longer in the first place
--tasty_resume :: Property
--tasty_resume = withDiscards 2000
--  $ propertyWT testModules
--  $ do
--    (dir, t, _) <- genDirTm
--    testModules' <- sequence testModules
--    resumeTest testModules' dir t
--
---- A helper for tasty_resume, and tasty_resume_regression
--resumeTest :: [Module] -> Dir -> Expr -> PropertyT WT ()
--resumeTest mods dir t = do
--  let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = False}
--  let optsR = RunRedexOptions{pushAndElide = True}
--  let globs = foldMap' moduleDefsQualified mods
--  tds <- asks typeDefs
--  n <- forAllT $ Gen.integral $ Range.linear 2 1000 -- Arbitrary limit here
--  closed <- forAllT $ Gen.frequency [(10, pure UnderBinders), (1, pure StopAtBinders)]
--  -- NB: We need to run this first reduction in an isolated context
--  -- as we need to avoid it changing the fresh-name-generator state
--  -- for the next run (sMid and sTotal). This is because reduction may need
--  -- to create fresh names, and we want to test "reducing n+m steps" is
--  -- exactly the same as "reducing n steps and then further reducing m
--  -- steps" (including generated names). (A happy consequence of this is that
--  -- it is precisely the same including ids in metadata.)
--  ((stepsFinal', sFinal), logs) <- lift $ isolateWT $ runPureLogT $ evalFullStepCount @EvalLog closed optsV optsR tds globs n dir t
--  testNoSevereLogs logs
--  when (stepsFinal' < 2) discard
--  let stepsFinal = case sFinal of Left _ -> stepsFinal'; Right _ -> 1 + stepsFinal'
--  m <- forAllT $ Gen.integral $ Range.constant 1 (stepsFinal - 1)
--  (stepsMid, sMid') <- failWhenSevereLogs $ evalFullStepCount @EvalLog closed optsV optsR tds globs m dir t
--  stepsMid === m
--  sMid <- case sMid' of
--    Left (TimedOut e) -> pure e
--    -- This should never happen: we know we are not taking enough steps to
--    -- hit a normal form (as m < stepsFinal)
--    Right e -> assert False >> pure e
--  (stepsTotal, sTotal) <- failWhenSevereLogs $ evalFullStepCount @EvalLog closed optsV optsR tds globs (stepsFinal - m) dir sMid
--  stepsMid + stepsTotal === stepsFinal'
--  sFinal === sTotal
--
---- A pseudo-unit regression test: when reduction needs to create fresh names,
---- the two reduction attempts in resumeTest should not interfere with each
---- other's names, else we will get occasional failures in that property test.
--tasty_resume_regression :: Property
--tasty_resume_regression = propertyWT [] $ do
--  -- This indeed requires fresh names when reducing (see unit_type_preservation_rename_LAM_regression)
--  t <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" $ lvar "b"))
--  resumeTest mempty Chk t
--
---- A regression test: previously EvalFull would rename to avoid variable
---- capture, but would use let instead of lettype for type abstractions ("big
---- lambdas"). (I.e. we changed 'λx.e' into 'λy.let x=y in e' and also did the
---- same for 'Λa.e' into 'Λb.let a=b in e', instead of 'Λb.lettype a=b in e'!)
---- This would lead to sporadic failures in tasty_type_preservation
---- ("WrongSortVariable").
--unit_type_preservation_rename_LAM_regression :: Assertion
--unit_type_preservation_rename_LAM_regression =
--  let ((expr, expected), maxID) = create $ do
--        e <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" $ lvar "b"))
--        expect <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a14" (letType "a" (tvar "a14") $ lvar "b"))) -- NB: fragile name a14
--        pure (e, expect)
--   in do
--        s <- evalFullTest maxID mempty mempty 1 Chk expr
--        s <~==> Left (TimedOut expected)
--
---- Previously EvalFull reducing a case expression could result in variable
---- capture. We would reduce 'λx. case C _ x of C x y -> _'
---- to (eliding annotations) 'λx. let x = _ in let y = x in _', where the
---- 'let x' has captured the reference in the 'let y = x'
--unit_type_preservation_case_regression_tm :: Assertion
--unit_type_preservation_case_regression_tm =
--  let ((expr, expected1, expected2), maxID) = create $ do
--        e <-
--          lam "x"
--            $ case_
--              ( con cMakePair [emptyHole, lvar "x"]
--                  `ann` ((tcon tPair `tapp` tcon tNat) `tapp` tcon tBool)
--              )
--              [branch cMakePair [("x", Nothing), ("y", Nothing)] emptyHole]
--        let x' = "a46" -- NB fragile name
--        expect1 <-
--          lam "x"
--            $ case_
--              ( con cMakePair [emptyHole, lvar "x"]
--                  `ann` ((tcon tPair `tapp` tcon tNat) `tapp` tcon tBool)
--              )
--              [branch cMakePair [(x', Nothing), ("y", Nothing)] $ let_ "x" (lvar x') emptyHole]
--        expect2 <-
--          lam "x"
--            $ let_ x' (emptyHole `ann` tlet "a" (tcon tNat) (tvar "a"))
--            $ let_ "y" (lvar "x" `ann` tlet "b" (tcon tBool) (tvar "b"))
--            $ let_ "x" (lvar x') emptyHole
--        pure (e, expect1, expect2)
--   in do
--        s1 <- evalFullTest maxID builtinTypes mempty 1 Chk expr
--        s1 <~==> Left (TimedOut expected1)
--        s2 <- evalFullTest maxID builtinTypes mempty 2 Chk expr
--        s2 <~==> Left (TimedOut expected2)
--
---- A regression test for the same issue as
---- unit_type_preservation_case_regression_tm, except for reducing case
---- expressions with annotated scruitinees, and emphasizing that capture may
---- happen of variables appearing in the annotation we add to the let
---- bindings. We previously would reduce
---- 'Λx. case MkPair _ _ : Pair _ x of MkPair x y -> _'
---- to 'Λx. let x = _ in let y = _ : x in _', where the 'let x' has captured
---- the reference in (the annotation on the) 'let y = _ : x'
--unit_type_preservation_case_regression_ty :: Assertion
--unit_type_preservation_case_regression_ty =
--  let ((expr, expected1, expected2), maxID) = create $ do
--        e <-
--          lAM "x"
--            $ case_
--              ( con cMakePair [emptyHole, emptyHole]
--                  `ann` (tcon tPair `tapp` tEmptyHole `tapp` tvar "x")
--              )
--              [branch cMakePair [("x", Nothing), ("y", Nothing)] emptyHole]
--        let x' = "a46" -- NB fragile name
--        expect1 <-
--          lAM "x"
--            $ case_
--              ( con cMakePair [emptyHole, emptyHole]
--                  `ann` (tcon tPair `tapp` tEmptyHole `tapp` tvar "x")
--              )
--              [branch cMakePair [(x', Nothing), ("y", Nothing)] $ let_ "x" (lvar x') emptyHole]
--        expect2 <-
--          lAM "x"
--            $ let_ x' (emptyHole `ann` tlet "a" tEmptyHole (tvar "a"))
--            $ let_ "y" (emptyHole `ann` tlet "b" (tvar "x") (tvar "b"))
--            $ let_ "x" (lvar x') emptyHole
--        pure (e, expect1, expect2)
--   in do
--        s1 <- evalFullTest maxID builtinTypes mempty 1 Chk expr
--        s1 <~==> Left (TimedOut expected1)
--        s2 <- evalFullTest maxID builtinTypes mempty 2 Chk expr
--        s2 <~==> Left (TimedOut expected2)
--
---- Previously EvalFull reducing a BETA expression could result in variable
---- capture. We would reduce (Λa.t : ∀b.T) S to
---- let b = S in (let a = S in t) : T
---- The outer let binding could capture references within S or t.
--unit_type_preservation_BETA_regression :: Assertion
--unit_type_preservation_BETA_regression =
--  let (((exprA, expectedAs), (exprB, expectedBs)), maxID) = create $ do
--        -- The 'A' sequence previously captured in the type "S" above
--        -- Λb x. (Λa λc (? : a) : ∀b.(Nat -> b)) @(b Bool) x
--        eA <-
--          lAM "b"
--            $ lam "x"
--            $ ( lAM "a" (lam "c" $ emptyHole `ann` tvar "a")
--                  `ann` tforall "b" ktype (tcon tNat `tfun` tvar "b")
--              )
--            `aPP` (tvar "b" `tapp` tcon tBool)
--            `app` lvar "x"
--        -- Do the BETA step
--        -- Λb x. ((lettype a = b Bool in λc (? : a)) : (let b = b Bool in Nat -> b)) x
--        expectA1 <-
--          lAM "b"
--            $ lam "x"
--            $ ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
--                  `ann` tlet "b" (tvar "b" `tapp` tcon tBool) (tcon tNat `tfun` tvar "b")
--              )
--            `app` lvar "x"
--        -- NB: the point of the ... `app` lvar x is to make the annotated term be in SYN position
--        -- so we reduce the type, rather than taking an upsilon step
--        -- Push the let b
--        -- Λb. λx. ((lettype a = b Bool in λc (? : a)) : (Nat -> (let b = b Bool in b))) x
--        expectA2 <-
--          lAM "b"
--            $ lam "x"
--            $ ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
--                  `ann` (tcon tNat `tfun` tlet "b" (tvar "b" `tapp` tcon tBool) (tvar "b"))
--              )
--            `app` lvar "x"
--        -- Inline the let
--        -- Λb. λx. ((lettype a = b Bool in λc (? : a)) : (Nat -> b Bool)) x
--        expectA3 <-
--          lAM "b"
--            $ lam "x"
--            $ ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
--                  `ann` (tcon tNat `tfun` (tvar "b" `tapp` tcon tBool))
--              )
--            `app` lvar "x"
--        -- Push the let
--        -- Λb. λx. (λc (lettype a = b Bool in (? : a)) : (Nat -> b Bool)) x
--        expectA4 <-
--          lAM "b"
--            $ lam "x"
--            $ ( lam "c" (letType "a" (tvar "b" `tapp` tcon tBool) (emptyHole `ann` tvar "a"))
--                  `ann` (tcon tNat `tfun` (tvar "b" `tapp` tcon tBool))
--              )
--            `app` lvar "x"
--        -- Do the beta step
--        -- Λb. λx. (let c = (x : Nat) in (lettype a = b Bool in (? : a)) : (b Bool))
--        expectA5 <-
--          lAM "b"
--            $ lam "x"
--            $ let_ "c" (lvar "x" `ann` tcon tNat) (letType "a" (tvar "b" `tapp` tcon tBool) (emptyHole `ann` tvar "a"))
--            `ann` (tvar "b" `tapp` tcon tBool)
--        -- Elide a pointless let
--        -- Λb. λx. ((lettype a = b Bool in (? : a)) : (b Bool))
--        expectA6 <-
--          lAM "b"
--            $ lam "x"
--            $ letType "a" (tvar "b" `tapp` tcon tBool) (emptyHole `ann` tvar "a")
--            `ann` (tvar "b" `tapp` tcon tBool)
--        -- Push the lets, eliding those that are redundant
--        -- Λb. λx. ((? : lettype a = b Bool in a) : (b Bool))
--        expectA7 <-
--          lAM "b"
--            $ lam "x"
--            $ emptyHole
--            `ann` tlet "a" (tvar "b" `tapp` tcon tBool) (tvar "a")
--            `ann` (tvar "b" `tapp` tcon tBool)
--        -- Inline a let
--        -- Λb. λx. ((? : b Bool) : (b Bool))
--        expectA8 <-
--          lAM "b"
--            $ lam "x"
--            $ emptyHole
--            `ann` (tvar "b" `tapp` tcon tBool)
--            `ann` (tvar "b" `tapp` tcon tBool)
--        -- The 'B' sequence previously captured in the term "t" above
--        -- Λb. (Λa (foo @(b Bool) : ∀b.Nat) @Char
--        eB <-
--          lAM "b"
--            $ ( lAM "a" (gvar foo `aPP` (tvar "b" `tapp` tcon tBool))
--                  `ann` tforall "b" ktype (tcon tNat)
--              )
--            `aPP` tcon tChar
--        -- BETA step
--        -- Λb. (lettype a = Char in foo @(b Bool)) : (let b = Char in Nat)
--        expectB1 <-
--          lAM "b"
--            $ letType "a" (tcon tChar) (gvar foo `aPP` (tvar "b" `tapp` tcon tBool))
--            `ann` tlet "b" (tcon tChar) (tcon tNat)
--        -- Drop annotation, elide lettype
--        -- Λb. foo @(b Bool)
--        expectB3 <- lAM "b" $ gvar foo `aPP` (tvar "b" `tapp` tcon tBool)
--        -- Note that the reduction of eA and eB take slightly
--        -- different paths: we do not remove the annotation in eA
--        -- because it has an occurrence of a type variable and is thus
--        -- not "concrete"
--        pure
--          (
--            ( eA
--            ,
--              [ (1, expectA1)
--              , (2, expectA2)
--              , (3, expectA3)
--              , (4, expectA4)
--              , (5, expectA5)
--              , (6, expectA6)
--              , (7, expectA7)
--              , (8, expectA8)
--              ]
--            )
--          , (eB, [(1, expectB1), (3, expectB3)])
--          )
--      sA n = evalFullTest maxID builtinTypes mempty n Chk exprA
--      sB n = evalFullTest maxID builtinTypes mempty n Chk exprB
--      tyA = TForall () "c" (KFun () (KType ()) (KType ())) $ TFun () (TCon () tNat) (TApp () (TVar () "c") (TCon () tBool))
--      tyB = TForall () "c" (KFun () (KType ()) (KType ())) $ TCon () tNat
--      foo = qualifyName (ModuleName ["M"]) "foo"
--      fooTy = TForall () "d" (KType ()) $ TCon () tNat
--      tmp ty e = case runTypecheckTestMWithPrims NoSmartHoles
--        $ local (extendGlobalCxt [(foo, fooTy)])
--        $ check ty e of
--        Left err -> assertFailure $ show err
--        Right _ -> pure ()
--   in do
--        tmp tyA exprA
--        for_ expectedAs $ \(n, e) -> sA n >>= (<~==> Left (TimedOut e))
--        tmp tyA $ snd $ NE.last expectedAs
--        tmp tyB exprB
--        for_ expectedBs $ \(n, e) -> sB n >>= (<~==> Left (TimedOut e))
--        tmp tyB $ snd $ NE.last expectedBs
--
---- Previously EvalFull reducing a let expression could result in variable
---- capture. We would reduce 'Λx. let x = _ :: x in x'
---- to (eliding annotations) 'Λx. let x = _ :: x in _ :: x', where the
---- 'let x' has captured the reference to the x in the bound term.
---- This causes the term to become ill-sorted.
---- Similarly, we reduce 'λx. let x = x in x' to itself, due to the same capture.
---- (This was before we changed to "pushing down lets")
--unit_let_self_capture :: Assertion
--unit_let_self_capture =
--  let ( ( expr1
--          , ty1
--          , expr2
--          , expected2
--          , expr3
--          , expected3a
--          , expected3b
--          , expr4
--          , expected4a
--          , expected4b
--          )
--        , maxID
--        ) = create $ do
--          e1 <- lAM "x" $ let_ "x" (emptyHole `ann` tvar "x") (lvar "x")
--          let t1 = TForall () "a" (KType ()) $ TVar () "a"
--          e2 <- lam "x" $ let_ "x" (lvar "x") (lvar "x")
--          expect2 <- lam "x" $ lvar "x"
--          e3 <- lAM "x" $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")
--          expect3a <- lAM "x" $ emptyHole `ann` tlet "x" (tvar "x") (tvar "x")
--          expect3b <- lAM "x" $ emptyHole `ann` tvar "x"
--          -- We do not need to do anything special for letrec
--          e4 <- lAM "a" $ lam "f" $ lam "x" $ letrec "x" (lvar "f" `app` lvar "x") (tvar "a") (lvar "x")
--          expect4a <-
--            lAM "a"
--              $ lam "f"
--              $ lam "x"
--              $ letrec "x" (lvar "f" `app` lvar "x") (tvar "a") (lvar "f" `app` lvar "x")
--              `ann` tvar "a"
--          expect4b <-
--            lAM "a"
--              $ lam "f"
--              $ lam "x"
--              $ (lvar "f" `app` letrec "x" (lvar "f" `app` lvar "x") (tvar "a") (lvar "x"))
--              `ann` tvar "a"
--          pure
--            ( e1
--            , t1
--            , e2
--            , expect2
--            , e3
--            , expect3a
--            , expect3b
--            , e4
--            , expect4a
--            , expect4b
--            )
--      s1 n = evalFullTest maxID mempty mempty n Chk expr1
--      s2 n = evalFullTest maxID mempty mempty n Chk expr2
--      s3 n = evalFullTest maxID mempty mempty n Chk expr3
--      s4 n = evalFullTest maxID mempty mempty n Chk expr4
--      typePres ty f = do
--        (timeout, term) <- spanM isLeft $ f <$> [0 ..]
--        forM_ (timeout <> [fst $ fromJust term]) $ \e ->
--          let e' = case e of
--                Left (TimedOut e'') -> e''
--                Right e'' -> e''
--           in case runTypecheckTestM NoSmartHoles $ check ty e' of
--                Left err -> assertFailure $ show err
--                Right _ -> pure ()
--   in do
--        typePres ty1 s1
--        s2 1 >>= (<~==> Left (TimedOut expected2))
--        s2 2 >>= (<~==> Right expected2)
--        s3 1 >>= (<~==> Left (TimedOut expected3a))
--        s3 2 >>= (<~==> Left (TimedOut expected3b))
--        s3 3 >>= (<~==> Right expected3b)
--        s4 1 >>= (<~==> Left (TimedOut expected4a))
--        s4 2 >>= (<~==> Left (TimedOut expected4b))
--
---- | @spanM p mxs@ returns a tuple where the first component is the
---- values coming from the longest prefix of @mxs@ all of which satisfy
---- @p@, and the second component is the rest of @mxs@. It only runs
---- the necessary actions from @mxs@: those giving the prefix of
---- elements satisfying the predicate, and also the first action giving
---- an element that fails the predicate. (Thus the second component has
---- one element and a list of actions.)
----
---- Compare 'Data.List.span p =<< sequence mxs'.
--spanM :: Monad m => (a -> Bool) -> [m a] -> m ([a], Maybe (a, [m a]))
--spanM _ [] = pure ([], Nothing)
--spanM f (x : xs) = do
--  x' <- x
--  if f x'
--    then first (x' :) <$> spanM f xs
--    else pure ([], Just (x', xs))
--
---- We previously had a bug where we would refuse to inline a let if it would "self-capture"
---- (e.g.  λx. let x=f x in C x x : we cannot inline one occurrence of this non-recursive let
---- since 'f x' refers to the lambda bound variable, but would be captured by the 'let x');
---- however, we did not check this condition when deciding (for capturing reasons) to
---- consider an inner let  (e.g. let x = y in let y = _ in t : we cannot inline the outer
---- let in 't' as the inner let would capture the 'y', so we decide to inline the inner
---- let first). Thus we would mess up an example like
---- Λy. let x = ?:y in let y = _:y in y x
---- reducing it to
---- Λy. let x = ?:y in let y = _:y in (_:y) x
---- (This was before we changed to "pushing down lets")
--unit_regression_self_capture_let_let :: Assertion
--unit_regression_self_capture_let_let = do
--  let e =
--        lAM "y"
--          $ let_ "x" (emptyHole `ann` tvar "y")
--          $ let_ "y" (emptyHole `ann` tvar "y")
--          $ lvar "y"
--          `app` lvar "x"
--      f =
--        lAM "y"
--          $ let_
--            "y"
--            (emptyHole `ann` tvar "y")
--            (lvar "y")
--          `app` let_
--            "x"
--            (emptyHole `ann` tvar "y")
--            (lvar "x")
--      g =
--        lAM "y"
--          $ (emptyHole `ann` tvar "y")
--          `app` (emptyHole `ann` tvar "y")
--      (e', i) = create e
--      ev n = evalFullTest i mempty mempty n Chk e'
--      x ~ y = x >>= (<~==> Left (TimedOut (create' y)))
--  ev 0 ~ e
--  ev 1 ~ f
--  ev 3 ~ g
--
---- | Evaluation preserves types
---- (assuming we don't end with a 'LetType' in the term, as the typechecker
---- cannot currently deal with those)
--tasty_type_preservation :: Property
--tasty_type_preservation = withTests 1000
--  $ withDiscards 2000
--  $ propertyWT testModules
--  $ do
--    let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = False}
--    let optsR = RunRedexOptions{pushAndElide = True}
--    let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
--    tds <- asks typeDefs
--    (dir, t, ty) <- genDirTm
--    let test msg e = do
--          annotateShow $ unLabelName msg
--          annotateShow e
--          s <- case e of
--            Left (TimedOut s') -> label (msg <> "TimedOut") >> pure s'
--            Right s' -> label (msg <> "NF") >> pure s'
--          if hasTypeLets s
--            then label (msg <> "skipped due to LetType") >> success
--            else do
--              annotateShow s
--              s' <- checkTest ty s
--              forgetMetadata s === forgetMetadata s' -- check no smart holes happened
--    maxSteps <- forAllT $ Gen.integral $ Range.linear 1 1000 -- Arbitrary limit here
--    closed <- forAllT $ Gen.frequency [(10, pure UnderBinders), (1, pure StopAtBinders)]
--    (steps, s) <- failWhenSevereLogs $ evalFullStepCount @EvalLog closed optsV optsR tds globs maxSteps dir t
--    annotateShow steps
--    annotateShow s
--    -- s is often reduced to normal form
--    test "long " s
--    -- also test an intermediate point
--    if steps <= 1
--      then label "generated a normal form"
--      else do
--        midSteps <- forAllT $ Gen.integral $ Range.linear 1 (steps - 1)
--        (_, s') <- failWhenSevereLogs $ evalFullStepCount @EvalLog closed optsV optsR tds globs midSteps dir t
--        test "mid " s'
--
---- Unsaturated primitives are stuck terms
--unit_prim_stuck :: Assertion
--unit_prim_stuck =
--  let ((f, prims), maxID) = create $ (,) <$> pfun ToUpper <*> primDefs
--   in do
--        s <- evalFullTest maxID mempty prims 1 Syn f
--        s <~==> Right f
--
--unit_prim_toUpper :: Assertion
--unit_prim_toUpper =
--  unaryPrimTest
--    ToUpper
--    (char 'a')
--    (char 'A')
--
--unit_prim_isSpace_1 :: Assertion
--unit_prim_isSpace_1 =
--  unaryPrimTest
--    IsSpace
--    (char '\n')
--    (boolAnn True)
--
--unit_prim_isSpace_2 :: Assertion
--unit_prim_isSpace_2 =
--  unaryPrimTest
--    IsSpace
--    (char 'a')
--    (boolAnn False)
--
--tasty_prim_hex_nat :: Property
--tasty_prim_hex_nat = withTests 20 . property $ do
--  n <- forAllT $ Gen.integral $ Range.constant 0 50
--  let ne = nat n
--      ((e, r, prims), maxID) =
--        create
--          $ if n <= 15
--            then
--              (,,)
--                <$> case_
--                  ( pfun NatToHex
--                      `app` ne
--                  )
--                  [ branch
--                      cNothing
--                      []
--                      (con cNothing [])
--                  , branch
--                      cJust
--                      [("x", Nothing)]
--                      ( pfun HexToNat
--                          `app` lvar "x"
--                      )
--                  ]
--                <*> con cJust [ne]
--                `ann` (tcon tMaybe `tapp` tcon tNat)
--                <*> primDefs
--            else
--              (,,)
--                <$> pfun NatToHex
--                `app` ne
--                <*> con cNothing []
--                `ann` (tcon tMaybe `tapp` tcon tChar)
--                <*> primDefs
--  s <- evalFullTasty maxID builtinTypes prims 7 Syn e
--  over evalResultExpr zeroIDs s === Right (zeroIDs r)
--
--unit_prim_char_eq_1 :: Assertion
--unit_prim_char_eq_1 =
--  binaryPrimTest
--    EqChar
--    (char 'a')
--    (char 'a')
--    (con0 cTrue `ann` tcon tBool)
--
--unit_prim_char_eq_2 :: Assertion
--unit_prim_char_eq_2 =
--  binaryPrimTest
--    EqChar
--    (char 'a')
--    (char 'A')
--    (con0 cFalse `ann` tcon tBool)
--
--unit_prim_char_partial :: Assertion
--unit_prim_char_partial =
--  let ((e, prims), maxID) =
--        create
--          $ (,)
--          <$> pfun EqChar
--          `app` char 'a'
--          <*> primDefs
--   in do
--        s <- evalFullTest maxID mempty prims 1 Syn e
--        s <~==> Right e
--
--unit_prim_int_add :: Assertion
--unit_prim_int_add =
--  binaryPrimTest
--    IntAdd
--    (int 2)
--    (int 2)
--    (int 4)
--
--unit_prim_int_add_big :: Assertion
--unit_prim_int_add_big =
--  binaryPrimTest
--    IntAdd
--    (int big)
--    (int big)
--    (int (2 * big :: Integer))
--  where
--    big = fromIntegral (maxBound :: Word64)
--
--unit_prim_int_sub :: Assertion
--unit_prim_int_sub =
--  binaryPrimTest
--    IntMinus
--    (int 5)
--    (int 3)
--    (int 2)
--
--unit_prim_int_sub_negative :: Assertion
--unit_prim_int_sub_negative =
--  binaryPrimTest
--    IntMinus
--    (int 3)
--    (int 5)
--    (int (-2))
--
--unit_prim_int_mul :: Assertion
--unit_prim_int_mul =
--  binaryPrimTest
--    IntMul
--    (int 3)
--    (int 2)
--    (int 6)
--
--unit_prim_int_quotient :: Assertion
--unit_prim_int_quotient =
--  binaryPrimTest
--    IntQuotient
--    (int 7)
--    (int 3)
--    (con cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_quotient_negative :: Assertion
--unit_prim_int_quotient_negative =
--  binaryPrimTest
--    IntQuotient
--    (int (-7))
--    (int 3)
--    (con cJust [int (-3)] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_quotient_zero :: Assertion
--unit_prim_int_quotient_zero =
--  binaryPrimTest
--    IntQuotient
--    (int (-7))
--    (int 0)
--    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_remainder :: Assertion
--unit_prim_int_remainder =
--  binaryPrimTest
--    IntRemainder
--    (int 7)
--    (int 3)
--    (con cJust [int 1] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_remainder_negative_1 :: Assertion
--unit_prim_int_remainder_negative_1 =
--  binaryPrimTest
--    IntRemainder
--    (int (-7))
--    (int (-3))
--    (con cJust [int (-1)] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_remainder_negative_2 :: Assertion
--unit_prim_int_remainder_negative_2 =
--  binaryPrimTest
--    IntRemainder
--    (int (-7))
--    (int 3)
--    (con cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_remainder_negative_3 :: Assertion
--unit_prim_int_remainder_negative_3 =
--  binaryPrimTest
--    IntRemainder
--    (int 7)
--    (int (-3))
--    (con cJust [int (-2)] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_remainder_zero :: Assertion
--unit_prim_int_remainder_zero =
--  binaryPrimTest
--    IntRemainder
--    (int 7)
--    (int 0)
--    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))
--
--unit_prim_int_quot :: Assertion
--unit_prim_int_quot =
--  binaryPrimTest
--    IntQuot
--    (int 7)
--    (int 3)
--    (int 2)
--
--unit_prim_int_quot_negative :: Assertion
--unit_prim_int_quot_negative =
--  binaryPrimTest
--    IntQuot
--    (int (-7))
--    (int 3)
--    (int (-3))
--
--unit_prim_int_quot_zero :: Assertion
--unit_prim_int_quot_zero =
--  binaryPrimTest
--    IntQuot
--    (int (-7))
--    (int 0)
--    (int 0)
--
--unit_prim_int_rem :: Assertion
--unit_prim_int_rem =
--  binaryPrimTest
--    IntRem
--    (int 7)
--    (int 3)
--    (int 1)
--
--unit_prim_int_rem_negative_1 :: Assertion
--unit_prim_int_rem_negative_1 =
--  binaryPrimTest
--    IntRem
--    (int (-7))
--    (int (-3))
--    (int (-1))
--
--unit_prim_int_rem_negative_2 :: Assertion
--unit_prim_int_rem_negative_2 =
--  binaryPrimTest
--    IntRem
--    (int (-7))
--    (int 3)
--    (int 2)
--
--unit_prim_int_rem_negative_3 :: Assertion
--unit_prim_int_rem_negative_3 =
--  binaryPrimTest
--    IntRem
--    (int 7)
--    (int (-3))
--    (int (-2))
--
--unit_prim_int_rem_zero :: Assertion
--unit_prim_int_rem_zero =
--  binaryPrimTest
--    IntRem
--    (int 7)
--    (int 0)
--    (int 7)
--
--unit_prim_int_eq_1 :: Assertion
--unit_prim_int_eq_1 =
--  binaryPrimTest
--    IntEq
--    (int 2)
--    (int 2)
--    (boolAnn True)
--
--unit_prim_int_eq_2 :: Assertion
--unit_prim_int_eq_2 =
--  binaryPrimTest
--    IntEq
--    (int 2)
--    (int 1)
--    (boolAnn False)
--
--unit_prim_int_neq_1 :: Assertion
--unit_prim_int_neq_1 =
--  binaryPrimTest
--    IntNeq
--    (int 2)
--    (int 2)
--    (boolAnn False)
--
--unit_prim_int_neq_2 :: Assertion
--unit_prim_int_neq_2 =
--  binaryPrimTest
--    IntNeq
--    (int 2)
--    (int 1)
--    (boolAnn True)
--
--unit_prim_int_less_than_1 :: Assertion
--unit_prim_int_less_than_1 =
--  binaryPrimTest
--    IntLT
--    (int 1)
--    (int 2)
--    (boolAnn True)
--
--unit_prim_int_less_than_2 :: Assertion
--unit_prim_int_less_than_2 =
--  binaryPrimTest
--    IntLT
--    (int 1)
--    (int 1)
--    (boolAnn False)
--
--unit_prim_int_less_than_or_equal_1 :: Assertion
--unit_prim_int_less_than_or_equal_1 =
--  binaryPrimTest
--    IntLTE
--    (int 1)
--    (int 2)
--    (boolAnn True)
--
--unit_prim_int_less_than_or_equal_2 :: Assertion
--unit_prim_int_less_than_or_equal_2 =
--  binaryPrimTest
--    IntLTE
--    (int 1)
--    (int 1)
--    (boolAnn True)
--
--unit_prim_int_less_than_or_equal_3 :: Assertion
--unit_prim_int_less_than_or_equal_3 =
--  binaryPrimTest
--    IntLTE
--    (int 2)
--    (int 1)
--    (boolAnn False)
--
--unit_prim_int_greater_than_1 :: Assertion
--unit_prim_int_greater_than_1 =
--  binaryPrimTest
--    IntGT
--    (int 2)
--    (int 1)
--    (boolAnn True)
--
--unit_prim_int_greater_than_2 :: Assertion
--unit_prim_int_greater_than_2 =
--  binaryPrimTest
--    IntGT
--    (int 1)
--    (int 1)
--    (boolAnn False)
--
--unit_prim_int_greater_than_or_equal_1 :: Assertion
--unit_prim_int_greater_than_or_equal_1 =
--  binaryPrimTest
--    IntGTE
--    (int 1)
--    (int 2)
--    (boolAnn False)
--
--unit_prim_int_greater_than_or_equal_2 :: Assertion
--unit_prim_int_greater_than_or_equal_2 =
--  binaryPrimTest
--    IntGTE
--    (int 1)
--    (int 1)
--    (boolAnn True)
--
--unit_prim_int_greater_than_or_equal_3 :: Assertion
--unit_prim_int_greater_than_or_equal_3 =
--  binaryPrimTest
--    IntGTE
--    (int 2)
--    (int 1)
--    (boolAnn True)
--
--unit_prim_int_toNat :: Assertion
--unit_prim_int_toNat =
--  unaryPrimTest
--    IntToNat
--    (int 0)
--    (con cJust [nat 0] `ann` (tcon tMaybe `tapp` tcon tNat))
--
--unit_prim_int_toNat_negative :: Assertion
--unit_prim_int_toNat_negative =
--  unaryPrimTest
--    IntToNat
--    (int (-1))
--    (con cNothing [] `ann` (tcon tMaybe `tapp` tcon tNat))
--
--unit_prim_int_fromNat :: Assertion
--unit_prim_int_fromNat =
--  unaryPrimTest
--    IntFromNat
--    (nat 4)
--    (int 4)
--
--unit_prim_ann :: Assertion
--unit_prim_ann =
--  let ((e, r, prims), maxID) =
--        create
--          $ (,,)
--          <$> ( pfun ToUpper
--                  `ann` (tcon tChar `tfun` tcon tChar)
--              )
--          `app` (char 'a' `ann` tcon tChar)
--          <*> char 'A'
--          <*> primDefs
--   in do
--        s <- evalFullTest maxID builtinTypes prims 2 Syn e
--        s <~==> Right r
--
--unit_prim_lazy_1 :: Assertion
--unit_prim_lazy_1 =
--  let ((e, r, prims), maxID) =
--        create
--          $ (,,)
--          <$> pfun PrimConst
--          `app` bool_ True
--          `app` emptyHole
--          <*> bool_ True
--          `ann` tcon tBool
--          <*> primDefs
--   in do
--        s <- evalFullTest maxID builtinTypes prims 2 Syn e
--        s <~==> Right r
--
--unit_prim_lazy_2 :: Assertion
--unit_prim_lazy_2 =
--  let ((e, r, prims), maxID) =
--        create
--          $ (,,)
--          <$> pfun PrimConst
--          `app` bool_ True
--          `app` letrec "x" (lvar "x") (tcon tNat) (lvar "x")
--          <*> bool_ True
--          `ann` tcon tBool
--          <*> primDefs
--   in do
--        s <- evalFullTest maxID builtinTypes prims 2 Syn e
--        s <~==> Right r
--
--unit_prim_partial_map :: Assertion
--unit_prim_partial_map =
--  let modName = mkSimpleModuleName "TestModule"
--      ((e, r, gs, prims), maxID) =
--        create $ do
--          (mapName, mapDef) <- Examples.map' modName
--          (,,,)
--            <$> gvar mapName
--            `aPP` tcon tChar
--            `aPP` tcon tChar
--            `app` pfun ToUpper
--            `app` list_
--              [ char 'a'
--              , char 'b'
--              , char 'c'
--              ]
--            <*> list_
--              [ char 'A'
--              , char 'B'
--              , char 'C'
--              ]
--            `ann` (tcon tList `tapp` tcon tChar)
--            <*> pure (M.singleton mapName mapDef)
--            <*> primDefs
--   in do
--        s <- evalFullTestExactSteps maxID builtinTypes (gs <> prims) 91 Syn e
--        s ~== r
--
---- Test that handleEvalFullRequest will reduce imported terms
--unit_eval_full_modules :: Assertion
--unit_eval_full_modules =
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
--
---- Test that handleEvalFullRequest will reduce case analysis of imported types
--unit_eval_full_modules_scrutinize_imported_type :: Assertion
--unit_eval_full_modules_scrutinize_imported_type =
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
--
---- Test that evaluation does not duplicate node IDs
--tasty_unique_ids :: Property
--tasty_unique_ids = withTests 1000
--  $ withDiscards 2000
--  $ propertyWT testModules
--  $ do
--    let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = False}
--    let optsR = RunRedexOptions{pushAndElide = True}
--    let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
--    tds <- asks typeDefs
--    (dir, t1, _) <- genDirTm
--    closed <- forAllT $ Gen.frequency [(10, pure UnderBinders), (1, pure StopAtBinders)]
--    let go n t
--          | n == (0 :: Int) = pure ()
--          | otherwise = do
--              t' <- failWhenSevereLogs $ evalFull @EvalLog closed optsV optsR tds globs 1 dir t
--              case t' of
--                Left (TimedOut e) -> uniqueIDs e >> go (n - 1) e
--                Right e -> uniqueIDs e
--    go 20 t1 -- we need some bound since not all terms terminate
--  where
--    uniqueIDs e =
--      let ids = e ^.. exprIDs
--       in ids === ordNub ids
--
--unit_wildcard :: Assertion
--unit_wildcard =
--  let loop = letrec "x" (lvar "x") (tcon tNat) (lvar "x")
--      (eTerm, maxIDTerm) = create $ caseFB_ loop [] (con0 cTrue)
--      expectTerm = create' $ con0 cTrue
--      (eDiverge, maxIDDiverge) = create $ caseFB_ loop [branch cZero [] $ con0 cFalse] (con0 cTrue)
--      expectDiverge =
--        create'
--          $ caseFB_
--            ( letrec "x" (lvar "x") (tcon tNat) (lvar "x")
--                `ann` tcon tNat
--            )
--            [branch cZero [] $ con0 cFalse]
--            (con0 cTrue)
--   in do
--        s <- evalFullTest maxIDTerm mempty mempty 2 Syn eTerm
--        s <~==> Right expectTerm
--        t <- evalFullTest maxIDDiverge mempty mempty 5 Syn eDiverge
--        t <~==> Left (TimedOut expectDiverge)
--
--unit_case_prim :: Assertion
--unit_case_prim =
--  let (e1, maxID1) = create $ caseFB_ (char 'a') [] (con0 cTrue)
--      expect1 = create' $ con0 cTrue
--      (e2, maxID2) = create $ caseFB_ (char 'a') [branchPrim (PrimChar 'a') $ con0 cFalse] (con0 cTrue)
--      expect2 = create' $ con0 cFalse
--      (e3, maxID3) =
--        create
--          $ caseFB_
--            (char 'b')
--            [ branchPrim (PrimChar 'a') $ con0 cTrue
--            , branchPrim (PrimChar 'b') $ con0 cFalse
--            ]
--            (con0 cTrue)
--      expect3 = create' $ con0 cFalse
--      (e4, maxID4) =
--        create
--          $ caseFB_
--            ( (lam "x" (lvar "x") `ann` (tcon tChar `tfun` tcon tChar))
--                `app` char 'a'
--            )
--            [branchPrim (PrimChar 'a') $ con0 cFalse]
--            (con0 cTrue)
--      expect4 = create' $ con0 cFalse
--   in do
--        s1 <- evalFullTest maxID1 mempty mempty 2 Syn e1
--        s1 <~==> Right expect1
--        s2 <- evalFullTest maxID2 mempty mempty 2 Syn e2
--        s2 <~==> Right expect2
--        s3 <- evalFullTest maxID3 mempty mempty 2 Syn e3
--        s3 <~==> Right expect3
--        s4 <- evalFullTest maxID4 mempty mempty 6 Syn e4
--        s4 <~==> Right expect4

-- * Utilities

{-
evalFullTest' ::
  HasCallStack =>
  ViewRedexOptions ->
  ID ->
  TypeDefMap ->
  DefMap ->
  TerminationBound ->
  Dir ->
  Expr ->
  IO (Either EvalFullError Expr)
evalFullTest' optsV id_ tydefs globals n d e = do
  let optsN = UnderBinders
  let optsR = RunRedexOptions{pushAndElide = True}
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalLog optsN optsV optsR tydefs globals n d e
  assertNoSevereLogs logs
  distinctIDs r
  pure r
-}

evalFullTest :: HasCallStack => TypeDefMap -> DefMap -> Dir -> Expr' () () () -> IO (Expr' () () ())
-- TODO: deal with primitives
evalFullTest tydefs defs dir = pure . interp tydefs (M.fromList $ mapMaybe (\(f,d) -> case d of
      DefAST (ASTDef tm ty) -> Just (Left f,Ann () (forgetMetadata tm) (forgetTypeMetadata ty))
      _ -> Nothing)
      $ M.assocs defs, mempty) dir

{-
evalFullTestAvoidShadowing :: HasCallStack => ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> IO (Either EvalFullError Expr)
evalFullTestAvoidShadowing = evalFullTest' ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = True}

evalFullTestExactSteps :: HasCallStack => ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> IO Expr
evalFullTestExactSteps id_ tydefs globals n d e = do
  s <- evalFullTest id_ tydefs globals (n - 1) d e
  case s of
    Right s' -> assertFailure $ "Unexpectedly reached normal form: " <> show s'
    Left _ -> do
      t <- evalFullTest id_ tydefs globals n d e
      case t of
        Left t' -> assertFailure $ "Unexpected timeout: " <> show t'
        Right t' -> pure t'

data GroupedLets = GroupedLets | SingleLets

evalFullTestClosed :: GroupedLets -> ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> IO (Either EvalFullError Expr)
evalFullTestClosed gl id_ tydefs globals n d e = do
  let optsN = StopAtBinders
  let gl' = case gl of
        GroupedLets -> True
        SingleLets -> False
  let optsV = ViewRedexOptions{groupedLets = gl', aggressiveElision = True, avoidShadowing = False}
  let optsR = RunRedexOptions{pushAndElide = True}
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalLog optsN optsV optsR tydefs globals n d e
  assertNoSevereLogs logs
  distinctIDs r
  pure r

evalFullTasty :: MonadTest m => ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> m (Either EvalFullError Expr)
evalFullTasty id_ tydefs globals n d e = do
  let optsN = UnderBinders
  let optsV = ViewRedexOptions{groupedLets = True, aggressiveElision = True, avoidShadowing = False}
  let optsR = RunRedexOptions{pushAndElide = True}
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalLog optsN optsV optsR tydefs globals n d e
  testNoSevereLogs logs
  let ids = r ^.. evalResultExpr % exprIDs
  ids === ordNub ids
  pure r

unaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> Assertion
unaryPrimTest f x y =
  let ((e, r, prims), maxID) =
        create
          $ (,,)
          <$> pfun f
          `app` x
          <*> y
          <*> primDefs
   in do
        s <- evalFullTest maxID mempty prims 2 Syn e
        s <~==> Right r
binaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> S Expr -> Assertion
binaryPrimTest f x y z =
  let ((e, r, prims), maxID) =
        create
          $ (,,)
          <$> pfun f
          `app` x
          `app` y
          <*> z
          <*> primDefs
   in do
        s <- evalFullTest maxID mempty prims 2 Syn e
        s <~==> Right r

evalResultExpr :: Traversal' (Either EvalFullError Expr) Expr
evalResultExpr = _Left % timedOut `adjoin` _Right
  where
    timedOut = prism TimedOut (Right . \case TimedOut e -> e)

(<~==>) :: HasCallStack => Either EvalFullError Expr -> Either EvalFullError Expr -> Assertion
x <~==> y = on (@?=) (over evalResultExpr zeroIDs) x y

(~==) :: HasCallStack => Expr -> Expr -> Assertion
x ~== y = on (@?=) zeroIDs x y

distinctIDs :: Either EvalFullError Expr -> Assertion
distinctIDs e =
  let ids = e ^.. evalResultExpr % exprIDs
      nIds = length ids
      uniqIDs = S.fromList ids
      nDistinct = S.size uniqIDs
   in assertBool
        ( unlines
            [ "Failure: non-distinct ids; had " ++ show nIds ++ " ids, but only " ++ show nDistinct ++ " unique ones"
            , "The duplicates were"
            , show $ ids \\ S.toList uniqIDs
            ]
        )
        (nIds == nDistinct)
-}
