module Tests.EvalFull where

import Foreword hiding (unlines)

import Data.Generics.Uniplate.Data (universe)
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
  EvalFullReq (EvalFullReq, evalFullCxtDir, evalFullMaxSteps, evalFullReqExpr),
  EvalFullResp (EvalFullRespNormal, EvalFullRespTimedOut),
  appIdCounter,
  handleEvalFullRequest,
  importModules,
  newEmptyApp,
 )
import Primer.Builtins (
  boolDef,
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
  exprIDs,
  forgetMetadata,
 )
import Primer.Def (DefMap)
import Primer.EvalFull
import Primer.Examples qualified as Examples (
  even,
  map',
  odd,
 )
import Primer.Gen.Core.Typed (WT, forAllT, isolateWT, propertyWT)
import Primer.Log (runPureLogT)
import Primer.Module (
  Module (Module, moduleDefs, moduleName, moduleTypes),
  builtinModule,
  builtinTypes,
  moduleDefsQualified,
  moduleTypesQualified,
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
    ToUpper
  ),
  tChar,
  tInt,
 )
import Primer.Primitives.DSL (pfun)
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
import Primer.TypeDef (TypeDef (..), TypeDefMap)
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
import Tests.Action.Prog (runAppTestM)
import Tests.Eval.Utils (genDirTm, testModules, (~=))
import Tests.Gen.Core.Typed (checkTest)
import Tests.Typecheck (expectTypedWithPrims, runTypecheckTestM, runTypecheckTestMWithPrims)

unit_1 :: Assertion
unit_1 =
  let (e, maxID) = create emptyHole
   in do
        s <- evalFullTest maxID mempty mempty 0 Syn e
        s <~==> Left (TimedOut e)

unit_2 :: Assertion
unit_2 =
  let (e, maxID) = create emptyHole
   in do
        s <- evalFullTest maxID mempty mempty 1 Syn e
        s <~==> Right e

-- Check we don't have shadowing issues in types
unit_3 :: Assertion
unit_3 =
  let ((expr, expected), maxID) = create $ do
        e <- letType "a" (tvar "b") $ emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "a" `tapp` tforall "a" KType (tvar "a") `tapp` tforall "b" KType (tcon' ["M"] "S" `tapp` tvar "a" `tapp` tvar "b"))
        let b' = "a33" -- NB: fragile name a33
        expect <- emptyHole `ann` (tcon' ["M"] "T" `tapp` tvar "b" `tapp` tforall "a" KType (tvar "a") `tapp` tforall b' KType (tcon' ["M"] "S" `tapp` tvar "b" `tapp` tvar b'))
        pure (e, expect)
   in do
        s <- evalFullTest maxID mempty mempty 7 Syn expr
        s <~==> Right expected

-- Check we don't have shadowing issues in terms
unit_4 :: Assertion
unit_4 =
  let ((expr, expected), maxID) = create $ do
        e <- let_ "a" (lvar "b") $ con' ["M"] "C" [lvar "a", lam "a" (lvar "a"), lam "b" (con' ["M"] "D" [lvar "a", lvar "b"])]
        let b' = "a19" -- NB: fragile name
        expect <- con' ["M"] "C" [lvar "b", lam "a" (lvar "a"), lam b' (con' ["M"] "D" [lvar "b", lvar b'])]
        pure (e, expect)
   in do
        s <- evalFullTest maxID mempty mempty 7 Syn expr
        s <~==> Right expected

-- This test is slightly unfortunate for two reasons
-- First, maybe we should do upsilon redexes more aggressively, to avoid the
-- inner annotation in the output
-- Second, writing [_] for embeddings we don't reduce [ e ] : T (and I'm not
-- sure if we should). This leads to the outer annotation in the output.
-- See https://github.com/hackworthltd/primer/issues/12
unit_5 :: Assertion
unit_5 =
  let ((e, expt), maxID) = create $ do
        a <- letrec "x" (lvar "x") (tcon tBool) (lvar "x")
        b <- letrec "x" (lvar "x") (tcon tBool) (lvar "x" `ann` tcon tBool) `ann` tcon tBool
        pure (a, b)
   in do
        s <- evalFullTest maxID mempty mempty 100 Syn e
        s <~==> Left (TimedOut expt)

unit_6 :: Assertion
unit_6 =
  let ((e, expt), maxID) = create $ do
        tr <- con0 cTrue
        an <- ann (pure tr) (tcon tBool)
        pure (an, tr)
   in do
        s <- evalFullTest maxID mempty mempty 1 Syn e
        s <~==> Right e
        t <- evalFullTest maxID mempty mempty 2 Chk e
        t <~==> Right expt

-- TODO: do we want to expand
--   (λ x. t) : ?
-- to
--   (λ x. t) : ? -> ?
-- and thus have an infinite derivation for
--   ((λ x . x x) : ?) (λ x. x x)
-- Currently we don't, so this is a stuck term
unit_7 :: Assertion
unit_7 =
  let (e, maxID) = create $ do
        let l = lam "x" $ lvar "x" `app` lvar "x"
        (l `ann` tEmptyHole) `app` l
   in do
        -- in evalFullTest maxID mempty mempty 100 Syn e <~==> Left (TimedOut e)
        s <- evalFullTest maxID mempty mempty 100 Syn e
        s <~==> Right e

unit_8 :: Assertion
unit_8 =
  let n = 10
      e = mapEven n
   in do
        evalFullTest (maxID e) builtinTypes (defMap e) 500 Syn (expr e) >>= \case
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        s <- evalFullTest (maxID e) builtinTypes (defMap e) 1000 Syn (expr e)
        s <~==> Right (expectedResult e)

-- A worker/wrapper'd map
unit_9 :: Assertion
unit_9 =
  let n = 10
      modName = mkSimpleModuleName "TestModule"
      ((globals, e, expected), maxID) = create $ do
        (mapName, mapDef) <- Examples.map' modName
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let lst = list_ $ take n $ iterate (con1 cSucc) (con0 cZero)
        expr <- gvar mapName `aPP` tcon tNat `aPP` tcon tBool `app` gvar evenName `app` lst
        let globs = [(mapName, mapDef), (evenName, evenDef), (oddName, oddDef)]
        expect <- list_ (take n $ cycle [con0 cTrue, con0 cFalse]) `ann` (tcon tList `tapp` tcon tBool)
        pure (globs, expr, expect)
   in do
        evalFullTest maxID builtinTypes (M.fromList globals) 500 Syn e >>= \case
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        s <- evalFullTest maxID builtinTypes (M.fromList globals) 1000 Syn e
        s <~==> Right expected

-- A case redex must have an scrutinee which is an annotated constructor.
-- Plain constructors are not well-typed here, for bidirectionality reasons,
-- although they just fail to reduce rather than the evaluator throwing a type error.
unit_10 :: Assertion
unit_10 =
  let ((s, t, expected), maxID) = create $ do
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
        s' <- evalFullTest maxID builtinTypes mempty 2 Syn s
        s' <~==> Right expected
        t' <- evalFullTest maxID builtinTypes mempty 1 Syn t
        t' <~==> Right t

-- This example shows that when we are under even a 'let' all we can do is
-- substitute, otherwise we may go down a rabbit hole!
unit_11 :: Assertion
unit_11 =
  let modName = mkSimpleModuleName "TestModule"
      ((globals, e, expected), maxID) = create $ do
        (evenName, evenDef) <- Examples.even modName
        (oddName, oddDef) <- Examples.odd modName
        let ty = tcon tNat `tfun` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
        let expr1 =
              let_ "x" (con0 cZero) $
                lam "n" (con cMakePair [(gvar evenName `app` lvar "n"), lvar "x"])
                  `ann` ty
        expr <- expr1 `app` con0 cZero
        let globs = [(evenName, evenDef), (oddName, oddDef)]
        expect <-
          (con cMakePair [con0 cTrue, con0 cZero])
            `ann` (tcon tPair `tapp` tcon tBool `tapp` tcon tNat)
        pure (globs, expr, expect)
   in do
        evalFullTest maxID builtinTypes (M.fromList globals) 10 Syn e >>= \case
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        s <- evalFullTest maxID builtinTypes (M.fromList globals) 20 Syn e
        s <~==> Right expected

-- This example shows that we may only substitute the top-most let otherwise we
-- may go down a rabbit hole unrolling a letrec and not doing enough
-- computation to see the recursion should terminate
unit_12 :: Assertion
unit_12 =
  let ((e, expected), maxID) = create $ do
        -- 'f' is a bit silly here, but could just as well be a definition of 'even'
        let f =
              lam "x" $
                case_
                  (lvar "x")
                  [ branch cZero [] $ con0 cTrue
                  , branch cSucc [("i", Nothing)] $ lvar "f" `app` lvar "i"
                  ]
        expr <- let_ "n" (con0 cZero) $ letrec "f" f (tcon tNat `tfun` tcon tBool) $ lvar "f" `app` lvar "n"
        expect <- con0 cTrue `ann` tcon tBool
        pure (expr, expect)
   in do
        s <- evalFullTest maxID builtinTypes mempty 15 Syn e
        s <~==> Right expected

unit_13 :: Assertion
unit_13 =
  let ((e, expected), maxID) = create $ do
        expr <- (lam "x" (con' ["M"] "C" [lvar "x", let_ "x" (con0 cTrue) (lvar "x"), lvar "x"]) `ann` (tcon tNat `tfun` tcon tBool)) `app` con0 cZero
        expect <- (con' ["M"] "C" [con0 cZero, con0 cTrue, con0 cZero]) `ann` tcon tBool
        pure (expr, expect)
   in do
        s <- evalFullTest maxID builtinTypes mempty 15 Syn e
        s <~==> Right expected

unit_14 :: Assertion
unit_14 =
  let ((e, expected), maxID) = create $ do
        expr <- (lam "x" (lam "x" $ lvar "x") `ann` (tcon tBool `tfun` (tcon tNat `tfun` tcon tNat))) `app` con0 cTrue `app` con0 cZero
        expect <- con0 cZero `ann` tcon tNat
        pure (expr, expect)
   in do
        s <- evalFullTest maxID builtinTypes mempty 15 Syn e
        s <~==> Right expected

-- Need to swap to substituting an inner let, when it (could have) arises from
-- a renaming to unblock the outer let.
-- i.e. when trying to reduce the let x:
--   let x = y in λy.C x y
--   let x = y in λz. let y = z in C x y
--   let x = y in λz. let y = z in C x z
--   let x = y in λz. C x z
--   let x = y in λz. C y z
--                λz. C y z
unit_15 :: Assertion
unit_15 =
  let ((expr, steps, expected), maxID) = create $ do
        let l = let_ "x" (lvar "y")
        let c a b = con' ["M"] "C" [lvar a, lvar b]
        e0 <- l $ lam "y" $ c "x" "y"
        let y' = "a38"
        e1 <- l $ lam y' $ let_ "y" (lvar y') $ c "x" "y"
        e2 <- l $ lam y' $ let_ "y" (lvar y') $ c "x" y'
        e3 <- l $ lam y' $ c "x" y'
        e4 <- l $ lam y' $ c "y" y'
        e5 <- lam y' $ c "y" y'
        pure (e0, [e0, e1, e2, e3, e4, e5], e5)
   in do
        si <- traverse (\i -> evalFullTest maxID builtinTypes mempty i Syn expr) [0 .. fromIntegral $ length steps - 1]
        zipWithM_ (\s e -> s <~==> Left (TimedOut e)) si steps
        s <- evalFullTest maxID builtinTypes mempty (fromIntegral $ length steps) Syn expr
        s <~==> Right expected

unit_hole_ann_case :: Assertion
unit_hole_ann_case =
  let (tm, maxID) = create $ hole $ ann (case_ emptyHole []) (tcon tBool)
   in do
        t <- evalFullTest maxID builtinTypes mempty 1 Chk tm
        t @?= Right tm

-- tlet x = C in D x x
--   ==>
-- tlet x = C in D C x
--   ==>
-- tlet x = C in D C C
--   ==>
-- D C C
unit_tlet :: Assertion
unit_tlet =
  let ((expr, expected), maxID) = create $ do
        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tvar "x" `tapp` tvar "x")
        e1 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tvar "x")
        e2 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tcon' ["M"] "C")
        e3 <- ann emptyHole $ tcon' ["M"] "D" `tapp` tcon' ["M"] "C" `tapp` tcon' ["M"] "C"
        pure (e0, map (Left . TimedOut) [e0, e1, e2, e3] ++ [Right e3])
      test (n, expect) = do
        r <- evalFullTest maxID mempty mempty n Syn expr
        r <~==> expect
   in mapM_ test (zip [0 ..] expected)

-- tlet x = C in ty ==> ty  when x not occur free in ty
unit_tlet_elide :: Assertion
unit_tlet_elide = do
  let ((expr, expected), maxID) = create $ do
        e0 <- ann emptyHole $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D")
        e1 <- ann emptyHole $ tcon' ["M"] "D"
        pure (e0, map (Left . TimedOut) [e0, e1] ++ [Right e1])
      test (n, expect) = do
        r <- evalFullTest maxID mempty mempty n Syn expr
        r <~==> expect
   in mapM_ test (zip [0 ..] expected)

-- tlet x = x in x
--   ==>
-- tlet y = x in let x = y in x
--   ==>
-- tlet y = x in let x = y in y
--   ==>
-- tlet y = x in y
--   ==>
-- tlet y = x in x
--   ==>
-- x
unit_tlet_self_capture :: Assertion
unit_tlet_self_capture = do
  let y = "a32"
      ((expr, expected), maxID) = create $ do
        e0 <- ann emptyHole $ tlet "x" (tvar "x") $ tvar "x"
        e1 <- ann emptyHole $ tlet y (tvar "x") $ tlet "x" (tvar y) $ tvar "x"
        e2 <- ann emptyHole $ tlet y (tvar "x") $ tlet "x" (tvar y) $ tvar y
        e3 <- ann emptyHole $ tlet y (tvar "x") $ tvar y
        e4 <- ann emptyHole $ tlet y (tvar "x") $ tvar "x"
        e5 <- ann emptyHole $ tvar "x"
        pure (e0, map (Left . TimedOut) [e0, e1, e2, e3, e4, e5] ++ [Right e5])
      test (n, expect) = do
        r <- evalFullTest maxID mempty mempty n Syn expr
        r <~==> expect
   in mapM_ test (zip [0 ..] expected)

-- TODO: examples with holes

-- TODO: most of these property tests could benefit from generating an
-- arbitrary context first.
-- See https://github.com/hackworthltd/primer/issues/50

-- | Resuming evaluation is the same as running it for longer in the first place
tasty_resume :: Property
tasty_resume = withDiscards 2000 $
  propertyWT testModules $ do
    (dir, t, _) <- genDirTm
    resumeTest testModules dir t

-- A helper for tasty_resume, and tasty_resume_regression
resumeTest :: [Module] -> Dir -> Expr -> PropertyT WT ()
resumeTest mods dir t = do
  let globs = foldMap' moduleDefsQualified mods
  tds <- asks typeDefs
  n <- forAllT $ Gen.integral $ Range.linear 2 1000 -- Arbitrary limit here
  -- NB: We need to run this first reduction in an isolated context
  -- as we need to avoid it changing the fresh-name-generator state
  -- for the next run (sMid and sTotal). This is because reduction may need
  -- to create fresh names, and we want to test "reducing n+m steps" is
  -- exactly the same as "reducing n steps and then further reducing m
  -- steps" (including generated names). (A happy consequence of this is that
  -- it is precisely the same including ids in metadata.)
  ((stepsFinal', sFinal), logs) <- lift $ isolateWT $ runPureLogT $ evalFullStepCount @EvalLog tds globs n dir t
  testNoSevereLogs logs
  when (stepsFinal' < 2) discard
  let stepsFinal = case sFinal of Left _ -> stepsFinal'; Right _ -> 1 + stepsFinal'
  m <- forAllT $ Gen.integral $ Range.constant 1 (stepsFinal - 1)
  (stepsMid, sMid') <- failWhenSevereLogs $ evalFullStepCount @EvalLog tds globs m dir t
  stepsMid === m
  sMid <- case sMid' of
    Left (TimedOut e) -> pure e
    -- This should never happen: we know we are not taking enough steps to
    -- hit a normal form (as m < stepsFinal)
    Right e -> assert False >> pure e
  (stepsTotal, sTotal) <- failWhenSevereLogs $ evalFullStepCount @EvalLog tds globs (stepsFinal - m) dir sMid
  stepsMid + stepsTotal === stepsFinal'
  sFinal === sTotal

-- A pseudo-unit regression test: when reduction needs to create fresh names,
-- the two reduction attempts in resumeTest should not interfere with each
-- other's names, else we will get occasional failures in that property test.
tasty_resume_regression :: Property
tasty_resume_regression = propertyWT [] $ do
  -- This indeed requires fresh names when reducing (see unit_type_preservation_rename_LAM_regression)
  t <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" $ lvar "b"))
  resumeTest mempty Chk t

-- A regression test: previously EvalFull would rename to avoid variable
-- capture, but would use let instead of lettype for type abstractions ("big
-- lambdas"). (I.e. we changed 'λx.e' into 'λy.let x=y in e' and also did the
-- same for 'Λa.e' into 'Λb.let a=b in e', instead of 'Λb.lettype a=b in e'!)
-- This would lead to sporadic failures in tasty_type_preservation
-- ("WrongSortVariable").
unit_type_preservation_rename_LAM_regression :: Assertion
unit_type_preservation_rename_LAM_regression =
  let ((expr, expected), maxID) = create $ do
        e <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" $ lvar "b"))
        expect <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a14" (letType "a" (tvar "a14") $ lvar "b"))) -- NB: fragile name a14
        pure (e, expect)
   in do
        s <- evalFullTest maxID mempty mempty 1 Chk expr
        s <~==> Left (TimedOut expected)

-- Previously EvalFull reducing a case expression could result in variable
-- capture. We would reduce 'λx. case C _ x of C x y -> _'
-- to (eliding annotations) 'λx. let x = _ in let y = x in _', where the
-- 'let x' has captured the reference in the 'let y = x'
unit_type_preservation_case_regression_tm :: Assertion
unit_type_preservation_case_regression_tm =
  let ((expr, expected1, expected2), maxID) = create $ do
        e <-
          lam "x" $
            case_
              (con cMakePair [emptyHole, lvar "x"] `ann` ((tcon tPair `tapp` tcon tNat) `tapp` tcon tBool))
              [branch cMakePair [("x", Nothing), ("y", Nothing)] emptyHole]
        let x' = "a42" -- NB: fragile name
        expect1 <-
          lam "x" $
            case_
              (con cMakePair [emptyHole, lvar "x"] `ann` ((tcon tPair `tapp` tcon tNat) `tapp` tcon tBool))
              [branch cMakePair [(x', Nothing), ("y", Nothing)] $ let_ "x" (lvar x') emptyHole]
        expect2 <-
          lam "x" $
            let_ x' (emptyHole `ann` tcon tNat) $
              let_ "y" (lvar "x" `ann` tcon tBool) $
                let_ "x" (lvar x') emptyHole
        pure (e, expect1, expect2)
   in do
        s1 <- evalFullTest maxID builtinTypes mempty 1 Chk expr
        s1 <~==> Left (TimedOut expected1)
        s2 <- evalFullTest maxID builtinTypes mempty 2 Chk expr
        s2 <~==> Left (TimedOut expected2)

-- A regression test for the same issue as
-- unit_type_preservation_case_regression_tm, except for reducing case
-- expressions with annotated scruitinees, and emphasizing that capture may
-- happen of variables appearing in the annotation we add to the let
-- bindings. We previously would reduce
-- 'Λx. case MkPair _ _ : Pair _ x of MkPair x y -> _'
-- to 'Λx. let x = _ in let y = _ : x in _', where the 'let x' has captured
-- the reference in (the annotation on the) 'let y = _ : x'
unit_type_preservation_case_regression_ty :: Assertion
unit_type_preservation_case_regression_ty =
  let ((expr, expected1, expected2), maxID) = create $ do
        e <-
          lAM "x" $
            case_
              ( (con cMakePair [emptyHole, emptyHole])
                  `ann` (tcon tPair `tapp` tEmptyHole `tapp` tvar "x")
              )
              [branch cMakePair [("x", Nothing), ("y", Nothing)] emptyHole]
        let x' = "a42" -- NB fragile name
        expect1 <-
          lAM "x" $
            case_
              ( (con cMakePair [emptyHole, emptyHole])
                  `ann` (tcon tPair `tapp` tEmptyHole `tapp` tvar "x")
              )
              [branch cMakePair [(x', Nothing), ("y", Nothing)] $ let_ "x" (lvar x') emptyHole]
        expect2 <-
          lAM "x" $
            let_ x' (emptyHole `ann` tEmptyHole) $
              let_ "y" (emptyHole `ann` tvar "x") $
                let_ "x" (lvar x') emptyHole
        pure (e, expect1, expect2)
   in do
        s1 <- evalFullTest maxID builtinTypes mempty 1 Chk expr
        s1 <~==> Left (TimedOut expected1)
        s2 <- evalFullTest maxID builtinTypes mempty 2 Chk expr
        s2 <~==> Left (TimedOut expected2)

-- Previously EvalFull reducing a BETA expression could result in variable
-- capture. We would reduce (Λa.t : ∀b.T) S to
-- let b = S in (let a = S in t) : T
-- The outer let binding could capture references within S or t.
unit_type_preservation_BETA_regression :: Assertion
unit_type_preservation_BETA_regression =
  let (((exprA, expectedAs), (exprB, expectedBs)), maxID) = create $ do
        -- The 'A' sequence previously captured in the type "S" above
        -- Λb x. (Λa λc (_ : a) : ∀b.(Nat -> b)) @(b -> Bool) x
        eA <-
          lAM "b" $
            lam "x" $
              ( lAM "a" (lam "c" $ emptyHole `ann` tvar "a")
                  `ann` tforall "b" KType (tcon tNat `tfun` tvar "b")
              )
                `aPP` (tvar "b" `tapp` tcon tBool)
                `app` lvar "x"
        -- Do the BETA step
        -- Λb x. ((lettype a = b Bool in λc (_ : a)) : (let b = b Bool in Nat -> b)) x
        expectA1 <-
          lAM "b" $
            lam "x" $
              ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
                  `ann` tlet "b" (tvar "b" `tapp` tcon tBool) (tcon tNat `tfun` tvar "b")
              )
                `app` lvar "x"
        -- NB: the point of the ... `app` lvar x is to make the annotated term be in SYN position
        -- so we reduce the type, rather than taking an upsilon step
        -- Rename the let b
        -- Λb. λx. ((lettype a = b Bool in λc (_ : a)) : (let c = b Bool in let b = c in Nat -> b)) x
        let b' = "a132"
        expectA2 <-
          lAM "b" $
            lam "x" $
              ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
                  `ann` tlet b' (tvar "b" `tapp` tcon tBool) (tlet "b" (tvar b') $ tcon tNat `tfun` tvar "b")
              )
                `app` lvar "x"
        -- Resolve the renaming
        -- Λb. λx. ((lettype a = b Bool in λc (_ : a)) : (let c = b Bool in Nat -> c)) x
        expectA4 <-
          lAM "b" $
            lam "x" $
              ( letType "a" (tvar "b" `tapp` tcon tBool) (lam "c" $ emptyHole `ann` tvar "a")
                  `ann` tlet b' (tvar "b" `tapp` tcon tBool) (tcon tNat `tfun` tvar b')
              )
                `app` lvar "x"
        -- Resolve all the letTypes
        -- Λb. λx. ((λc (_ : b Bool)) : (Nat -> b Bool)) x
        expectA8 <-
          lAM "b" $
            lam "x" $
              ( lam "c" (emptyHole `ann` (tvar "b" `tapp` tcon tBool))
                  `ann` (tcon tNat `tfun` (tvar "b" `tapp` tcon tBool))
              )
                `app` lvar "x"
        -- The 'B' sequence previously captured in the term "t" above
        -- Λb. (Λa (foo @(b Bool) : ∀b.Nat) @Char
        eB <-
          lAM "b" $
            ( lAM "a" (gvar foo `aPP` (tvar "b" `tapp` tcon tBool))
                `ann` tforall "b" KType (tcon tNat)
            )
              `aPP` tcon tChar
        -- BETA step
        -- Λb. (lettype a = Char in foo @(b Bool)) : (let b = Char in Nat)
        expectB1 <-
          lAM "b" $
            letType "a" (tcon tChar) (gvar foo `aPP` (tvar "b" `tapp` tcon tBool))
              `ann` tlet "b" (tcon tChar) (tcon tNat)
        -- Drop annotation and elide lettype
        -- Λb. foo @(b Bool)
        expectB3 <- lAM "b" $ gvar foo `aPP` (tvar "b" `tapp` tcon tBool)
        -- Note that the reduction of eA and eB take slightly
        -- different paths: we do not remove the annotation in eA
        -- because it has an occurrence of a type variable and is thus
        -- not "concrete"
        pure
          ( (eA, [(1, expectA1), (2, expectA2), (4, expectA4), (8, expectA8)])
          , (eB, [(1, expectB1), (3, expectB3)])
          )
      sA n = evalFullTest maxID builtinTypes mempty n Chk exprA
      sB n = evalFullTest maxID builtinTypes mempty n Chk exprB
      tyA = TForall () "c" (KFun KType KType) $ TFun () (TCon () tNat) (TApp () (TVar () "c") (TCon () tBool))
      tyB = TForall () "c" (KFun KType KType) $ TCon () tNat
      foo = qualifyName (ModuleName ["M"]) "foo"
      fooTy = TForall () "d" KType $ TCon () tNat
      tmp ty e = case runTypecheckTestMWithPrims NoSmartHoles $
        local (extendGlobalCxt [(foo, fooTy)]) $
          check ty e of
        Left err -> assertFailure $ show err
        Right _ -> pure ()
   in do
        tmp tyA exprA
        for_ expectedAs $ \(n, e) -> sA n >>= (<~==> Left (TimedOut e))
        tmp tyA $ snd $ NE.last expectedAs
        tmp tyB exprB
        for_ expectedBs $ \(n, e) -> sB n >>= (<~==> Left (TimedOut e))
        tmp tyB $ snd $ NE.last expectedBs

-- Previously EvalFull reducing a let expression could result in variable
-- capture. We would reduce 'Λx. let x = _ :: x in x'
-- to (eliding annotations) 'Λx. let x = _ :: x in _ :: x', where the
-- 'let x' has captured the reference to the x in the bound term.
-- This causes the term to become ill-sorted.
-- Similarly, we reduce 'λx. let x = x in x' to itself, due to the same capture.
unit_let_self_capture :: Assertion
unit_let_self_capture =
  let ( ( expr1
          , ty1
          , expr2
          , expected2a
          , expected2b
          , expr3
          , expected3a
          , expected3b
          , expr4
          , expected4a
          , expected4b
          )
        , maxID
        ) = create $ do
          e1 <- lAM "x" $ let_ "x" (emptyHole `ann` tvar "x") (lvar "x")
          let t1 = TForall () "a" KType $ TVar () "a"
          e2 <- lam "x" $ let_ "x" (lvar "x") (lvar "x")
          expect2a <- lam "x" $ let_ "a76" (lvar "x") (let_ "x" (lvar "a76") (lvar "x"))
          expect2b <- lam "x" $ lvar "x"
          e3 <- lAM "x" $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")
          expect3a <- lAM "x" $ letType "a76" (tvar "x") (letType "x" (tvar "a76") (emptyHole `ann` tvar "x"))
          expect3b <- lAM "x" $ emptyHole `ann` tvar "x"
          -- We do not need to do anything special for letrec
          e4 <- lAM "a" $ lam "f" $ lam "x" $ letrec "x" (lvar "f" `app` lvar "x") (tvar "a") (lvar "x")
          expect4a <-
            lAM "a" $
              lam "f" $
                lam "x" $
                  letrec "x" (lvar "f" `app` lvar "x") (tvar "a") $
                    letrec "x" (lvar "f" `app` lvar "x") (tvar "a") ((lvar "f" `app` lvar "x") `ann` tvar "a")
          expect4b <-
            lAM "a" $
              lam "f" $
                lam "x" $
                  letrec
                    "x"
                    (lvar "f" `app` lvar "x")
                    (tvar "a")
                    ((lvar "f" `app` lvar "x") `ann` tvar "a")
          pure
            ( e1
            , t1
            , e2
            , expect2a
            , expect2b
            , e3
            , expect3a
            , expect3b
            , e4
            , expect4a
            , expect4b
            )
      s1 n = evalFullTest maxID mempty mempty n Chk expr1
      s2 n = evalFullTest maxID mempty mempty n Chk expr2
      s3 n = evalFullTest maxID mempty mempty n Chk expr3
      s4 n = evalFullTest maxID mempty mempty n Chk expr4
      typePres ty f = do
        (timeout, term) <- spanM isLeft $ f <$> [0 ..]
        forM_ (timeout <> [fst $ fromJust term]) $ \e ->
          let e' = case e of
                Left (TimedOut e'') -> e''
                Right e'' -> e''
           in case runTypecheckTestM NoSmartHoles $ check ty e' of
                Left err -> assertFailure $ show err
                Right _ -> pure ()
   in do
        typePres ty1 s1
        s2 1 >>= (<~==> Left (TimedOut expected2a))
        s2 5 >>= (<~==> Left (TimedOut expected2b))
        s2 6 >>= (<~==> Right expected2b)
        s3 1 >>= (<~==> Left (TimedOut expected3a))
        s3 5 >>= (<~==> Left (TimedOut expected3b))
        s3 6 >>= (<~==> Right expected3b)
        s4 1 >>= (<~==> Left (TimedOut expected4a))
        s4 2 >>= (<~==> Left (TimedOut expected4b))

-- | @spanM p mxs@ returns a tuple where the first component is the
-- values coming from the longest prefix of @mxs@ all of which satisfy
-- @p@, and the second component is the rest of @mxs@. It only runs
-- the necessary actions from @mxs@: those giving the prefix of
-- elements satisfying the predicate, and also the first action giving
-- an element that fails the predicate. (Thus the second component has
-- one element and a list of actions.)
--
-- Compare 'Data.List.span p =<< sequence mxs'.
spanM :: Monad m => (a -> Bool) -> [m a] -> m ([a], Maybe (a, [m a]))
spanM _ [] = pure ([], Nothing)
spanM f (x : xs) = do
  x' <- x
  if f x'
    then first (x' :) <$> spanM f xs
    else pure ([], Just (x', xs))

-- We previously had a bug where we would refuse to inline a let if it would "self-capture"
-- (e.g.  λx. let x=f x in C x x : we cannot inline one occurrence of this non-recursive let
-- since 'f x' refers to the lambda bound variable, but would be captured by the 'let x');
-- however, we did not check this condition when deciding (for capturing reasons) to
-- consider an inner let  (e.g. let x = y in let y = _ in t : we cannot inline the outer
-- let in 't' as the inner let would capture the 'y', so we decide to inline the inner
-- let first). Thus we would mess up an example like
-- Λy. let x = ?:y in let y = _:y in y x
-- reducing it to
-- Λy. let x = ?:y in let y = _:y in (_:y) x
unit_regression_self_capture_let_let :: Assertion
unit_regression_self_capture_let_let = do
  let e =
        lAM "y" $
          let_ "x" (emptyHole `ann` tvar "y") $
            let_ "y" (emptyHole `ann` tvar "y") $
              lvar "y" `app` lvar "x"
      z = "a12"
      f =
        lAM "y" $
          let_ "x" (emptyHole `ann` tvar "y") $
            let_ z (emptyHole `ann` tvar "y") $
              let_ "y" (lvar z) $
                lvar "y" `app` lvar "x"
      (e', i) = create e
      ev n = evalFullTest i mempty mempty n Chk e'
      x ~ y = x >>= (<~==> Left (TimedOut (create' y)))
  ev 0 ~ e
  ev 1 ~ f

-- | Evaluation preserves types
-- (assuming we don't end with a 'LetType' in the term, as the typechecker
-- cannot currently deal with those)
tasty_type_preservation :: Property
tasty_type_preservation = withTests 1000 $
  withDiscards 2000 $
    propertyWT testModules $ do
      let globs = foldMap' moduleDefsQualified testModules
      tds <- asks typeDefs
      (dir, t, ty) <- genDirTm
      let test msg e = do
            annotateShow $ unLabelName msg
            annotateShow e
            s <- case e of
              Left (TimedOut s') -> label (msg <> "TimedOut") >> pure s'
              Right s' -> label (msg <> "NF") >> pure s'
            if null [() | LetType{} <- universe s]
              then do
                annotateShow s
                s' <- checkTest ty s
                forgetMetadata s === forgetMetadata s' -- check no smart holes happened
              else label (msg <> "skipped due to LetType") >> success
      maxSteps <- forAllT $ Gen.integral $ Range.linear 1 1000 -- Arbitrary limit here
      (steps, s) <- failWhenSevereLogs $ evalFullStepCount @EvalLog tds globs maxSteps dir t
      annotateShow steps
      annotateShow s
      -- s is often reduced to normal form
      test "long " s
      -- also test an intermediate point
      if steps <= 1
        then label "generated a normal form"
        else do
          midSteps <- forAllT $ Gen.integral $ Range.linear 1 (steps - 1)
          (_, s') <- failWhenSevereLogs $ evalFullStepCount @EvalLog tds globs midSteps dir t
          test "mid " s'

-- Unsaturated primitives are stuck terms
unit_prim_stuck :: Assertion
unit_prim_stuck =
  let (f, maxID) = create $ pfun ToUpper
   in do
        s <- evalFullTest maxID mempty primDefs 1 Syn f
        s <~==> Right f

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
      ((e, r), maxID) =
        create $
          if n <= 15
            then
              (,)
                <$> case_
                  ( pfun NatToHex
                      `app` ne
                  )
                  [ branch
                      cNothing
                      []
                      (con0 cNothing)
                  , branch
                      cJust
                      [("x", Nothing)]
                      ( pfun HexToNat
                          `app` lvar "x"
                      )
                  ]
                <*> con1 cJust ne
                `ann` (tcon tMaybe `tapp` tcon tNat)
            else
              (,)
                <$> pfun NatToHex
                `app` ne
                <*> con0 cNothing
                `ann` (tcon tMaybe `tapp` tcon tChar)
  s <- evalFullTasty maxID builtinTypes primDefs 7 Syn e
  over evalResultExpr zeroIDs s === Right (zeroIDs r)

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
  let (e, maxID) =
        create $
          pfun EqChar
            `app` char 'a'
   in do
        s <- evalFullTest maxID mempty primDefs 1 Syn e
        s <~==> Right e

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
    (conSat cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_quotient_negative :: Assertion
unit_prim_int_quotient_negative =
  binaryPrimTest
    IntQuotient
    (int (-7))
    (int 3)
    (conSat cJust [int (-3)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_quotient_zero :: Assertion
unit_prim_int_quotient_zero =
  binaryPrimTest
    IntQuotient
    (int (-7))
    (int 0)
    (conSat cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder :: Assertion
unit_prim_int_remainder =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int 3)
    (conSat cJust [int 1] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_1 :: Assertion
unit_prim_int_remainder_negative_1 =
  binaryPrimTest
    IntRemainder
    (int (-7))
    (int (-3))
    (conSat cJust [int (-1)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_2 :: Assertion
unit_prim_int_remainder_negative_2 =
  binaryPrimTest
    IntRemainder
    (int (-7))
    (int 3)
    (conSat cJust [int 2] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_negative_3 :: Assertion
unit_prim_int_remainder_negative_3 =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int (-3))
    (conSat cJust [int (-2)] `ann` (tcon tMaybe `tapp` tcon tInt))

unit_prim_int_remainder_zero :: Assertion
unit_prim_int_remainder_zero =
  binaryPrimTest
    IntRemainder
    (int 7)
    (int 0)
    (conSat cNothing [] `ann` (tcon tMaybe `tapp` tcon tInt))

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
    (conSat cJust [nat 0] `ann` (tcon tMaybe `tapp` tcon tNat))

unit_prim_int_toNat_negative :: Assertion
unit_prim_int_toNat_negative =
  unaryPrimTest
    IntToNat
    (int (-1))
    (conSat cNothing [] `ann` (tcon tMaybe `tapp` tcon tNat))

unit_prim_int_fromNat :: Assertion
unit_prim_int_fromNat =
  unaryPrimTest
    IntFromNat
    (nat 4)
    (int 4)

unit_prim_ann :: Assertion
unit_prim_ann =
  let ((e, r), maxID) =
        create $
          (,)
            <$> ( pfun ToUpper
                    `ann` (tcon tChar `tfun` tcon tChar)
                )
            `app` (char 'a' `ann` tcon tChar)
            <*> char 'A'
   in do
        s <- evalFullTest maxID builtinTypes primDefs 2 Syn e
        s <~==> Right r

unit_prim_partial_map :: Assertion
unit_prim_partial_map =
  let modName = mkSimpleModuleName "TestModule"
      ((e, r, gs), maxID) =
        create $ do
          (mapName, mapDef) <- Examples.map' modName
          (,,)
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
   in do
        s <- evalFullTest maxID builtinTypes (gs <> primDefs) 67 Syn e
        s <~==> Right r

-- Test that handleEvalFullRequest will reduce imported terms
unit_eval_full_modules :: Assertion
unit_eval_full_modules =
  let test = do
        importModules [primitiveModule, builtinModule]
        foo <- pfun ToUpper `app` char 'a'
        resp <-
          handleEvalFullRequest
            EvalFullReq
              { evalFullReqExpr = foo
              , evalFullCxtDir = Chk
              , evalFullMaxSteps = 2
              }
        expect <- char 'A'
        pure $ case resp of
          EvalFullRespTimedOut _ -> assertFailure "EvalFull timed out"
          EvalFullRespNormal e -> e ~= expect
      a = newEmptyApp
   in runAppTestM (appIdCounter a) a test <&> fst >>= \case
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Test that handleEvalFullRequest will reduce case analysis of imported types
unit_eval_full_modules_scrutinize_imported_type :: Assertion
unit_eval_full_modules_scrutinize_imported_type =
  let test = do
        importModules [m]
        foo <- case_ (con0 cTrue `ann` tcon tBool) [branch cTrue [] $ con0 cFalse, branch cFalse [] $ con0 cTrue]
        resp <-
          handleEvalFullRequest
            EvalFullReq{evalFullReqExpr = foo, evalFullCxtDir = Chk, evalFullMaxSteps = 2}
        expect <- con0 cFalse
        pure $ case resp of
          EvalFullRespTimedOut _ -> assertFailure "EvalFull timed out"
          EvalFullRespNormal e -> e ~= expect
      a = newEmptyApp
   in runAppTestM (appIdCounter a) a test <&> fst >>= \case
        Left err -> assertFailure $ show err
        Right assertion -> assertion
  where
    m =
      Module
        { moduleName = qualifiedModule tBool
        , moduleTypes = Map.singleton (baseName tBool) (TypeDefAST boolDef)
        , moduleDefs = mempty
        }

-- Test that evaluation does not duplicate node IDs
tasty_unique_ids :: Property
tasty_unique_ids = withTests 1000 $
  withDiscards 2000 $
    propertyWT testModules $ do
      let globs = foldMap' moduleDefsQualified testModules
      tds <- asks typeDefs
      (dir, t1, _) <- genDirTm
      let go n t
            | n == (0 :: Int) = pure ()
            | otherwise = do
                t' <- failWhenSevereLogs $ evalFull @EvalLog tds globs 1 dir t
                case t' of
                  Left (TimedOut e) -> uniqueIDs e >> go (n - 1) e
                  Right e -> uniqueIDs e
      go 20 t1 -- we need some bound since not all terms terminate
  where
    uniqueIDs e =
      let ids = e ^.. exprIDs
       in ids === ordNub ids

-- * Utilities

evalFullTest :: ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> IO (Either EvalFullError Expr)
evalFullTest id_ tydefs globals n d e = do
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalLog tydefs globals n d e
  assertNoSevereLogs logs
  distinctIDs r
  pure r

evalFullTasty :: MonadTest m => ID -> TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> m (Either EvalFullError Expr)
evalFullTasty id_ tydefs globals n d e = do
  let (r, logs) = evalTestM id_ $ runPureLogT $ evalFull @EvalLog tydefs globals n d e
  testNoSevereLogs logs
  let ids = r ^.. evalResultExpr % exprIDs
  ids === ordNub ids
  pure r

unaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> Assertion
unaryPrimTest f x y =
  let ((e, r), maxID) =
        create $
          (,)
            <$> pfun f
            `app` x
            <*> y
   in do
        s <- evalFullTest maxID mempty primDefs 2 Syn e
        s <~==> Right r
binaryPrimTest :: HasCallStack => PrimDef -> S Expr -> S Expr -> S Expr -> Assertion
binaryPrimTest f x y z =
  let ((e, r), maxID) =
        create $
          (,)
            <$> pfun f
            `app` x
            `app` y
            <*> z
   in do
        s <- evalFullTest maxID mempty primDefs 2 Syn e
        s <~==> Right r

evalResultExpr :: Traversal' (Either EvalFullError Expr) Expr
evalResultExpr = _Left % timedOut `adjoin` _Right
  where
    timedOut = prism TimedOut (Right . \case TimedOut e -> e)

(<~==>) :: HasCallStack => Either EvalFullError Expr -> Either EvalFullError Expr -> Assertion
x <~==> y = on (@?=) (over evalResultExpr zeroIDs) x y

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
