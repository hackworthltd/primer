module Tests.EvalFull where

import Foreword hiding (unlines)

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Generics.Uniplate.Data (universe)
import Data.List ((\\))
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (unlines)
import Gen.Core.Typed (WT, forAllT, genChk, genSyn, genWTType, isolateWT, propertyWT)
import Hedgehog hiding (Var, test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Optics
import Primer.App (defaultTypeDefs)
import Primer.Core
import Primer.Core.DSL
import Primer.Core.Utils (forgetIDs, generateIDs, generateTypeIDs)
import Primer.EvalFull
import Primer.Name
import Primer.Typecheck (
  SmartHoles (NoSmartHoles),
  buildTypingContext,
  globalCxt,
  mkTypeDefMap,
  typeDefs,
 )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import TestM
import TestUtils (withPrimDefs)
import Tests.Gen.Core.Typed (checkTest)

unit_1 :: Assertion
unit_1 =
  let (e, maxID) = create emptyHole
      s = evalFullTest maxID mempty mempty 0 Syn e
   in do
        distinctIDs s
        s <~==> Left (TimedOut e)

unit_2 :: Assertion
unit_2 =
  let (e, maxID) = create emptyHole
      s = evalFullTest maxID mempty mempty 1 Syn e
   in do
        distinctIDs s
        s <~==> Right e

-- Check we don't have shadowing issues in types
unit_3 :: Assertion
unit_3 =
  let ((expr, expected), maxID) = create $ do
        e <- letType "a" (tvar "b") $ emptyHole `ann` (tcon "T" `tapp` tvar "a" `tapp` tforall "a" KType (tvar "a") `tapp` tforall "b" KType (tcon "S" `tapp` tvar "a" `tapp` tvar "b"))
        let b' = "a33" -- NB: fragile name a33
        expect <- emptyHole `ann` (tcon "T" `tapp` tvar "b" `tapp` tforall "a" KType (tvar "a") `tapp` tforall b' KType (tcon "S" `tapp` tvar "b" `tapp` tvar b'))
        pure (e, expect)
      s = evalFullTest maxID mempty mempty 5 Syn expr
   in do
        distinctIDs s
        s <~==> Right expected

-- Check we don't have shadowing issues in terms
unit_4 :: Assertion
unit_4 =
  let ((expr, expected), maxID) = create $ do
        e <- let_ "a" (var "b") $ con "C" `app` var "a" `app` lam "a" (var "a") `app` lam "b" (con "D" `app` var "a" `app` var "b")
        let b' = "a29" -- NB: fragile name a29
        expect <- con "C" `app` var "b" `app` lam "a" (var "a") `app` lam b' (con "D" `app` var "b" `app` var b')
        pure (e, expect)
      s = evalFullTest maxID mempty mempty 7 Syn expr
   in do
        distinctIDs s
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
        a <- letrec "x" (var "x") (tcon "Bool") (var "x")
        b <- letrec "x" (var "x") (tcon "Bool") (var "x" `ann` tcon "Bool") `ann` tcon "Bool"
        pure (a, b)
      s = evalFullTest maxID mempty mempty 100 Syn e
   in do
        distinctIDs s
        s <~==> Left (TimedOut expt)

unit_6 :: Assertion
unit_6 =
  let ((e, expt), maxID) = create $ do
        tr <- con "True"
        an <- ann (pure tr) (tcon "Bool")
        pure (an, tr)
      s = evalFullTest maxID mempty mempty 1 Syn e
      t = evalFullTest maxID mempty mempty 2 Chk e
   in do
        distinctIDs s
        s <~==> Right e
        distinctIDs t
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
        let l = lam "x" $ var "x" `app` var "x"
        (l `ann` tEmptyHole) `app` l
   in -- in evalFullTest maxID mempty mempty 100 Syn e <~==> Left (TimedOut e)
      evalFullTest maxID mempty mempty 100 Syn e <~==> Right e

unit_8 :: Assertion
unit_8 =
  let n = 10
      ((globals, e, expected), maxID) = create $ do
        mapID <- fresh
        evenID <- fresh
        oddID <- fresh
        mapTy <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` tvar "b") `tfun` ((tcon "List" `tapp` tvar "a") `tfun` (tcon "List" `tapp` tvar "b"))
        map_ <-
          lAM "a" $
            lAM "b" $
              lam "f" $
                lam "xs" $
                  case_
                    (var "xs")
                    [ branch "Nil" [] $ con "Nil" `aPP` tvar "b"
                    , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ con "Cons" `aPP` tvar "b" `app` (var "f" `app` var "y") `app` (global mapID `aPP` tvar "a" `aPP` tvar "b" `app` var "f" `app` var "ys")
                    ]
        -- even and odd have almost the same type, but their types contain different IDs
        let evenOddTy = tcon "Nat" `tfun` tcon "Bool"
        evenTy <- evenOddTy
        oddTy <- evenOddTy
        isEven <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "True", branch "Succ" [("n", Nothing)] $ global oddID `app` var "n"]
        isOdd <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "False", branch "Succ" [("n", Nothing)] $ global evenID `app` var "n"]
        let mkList t = foldr (\x xs -> con "Cons" `aPP` t `app` x `app` xs) (con "Nil" `aPP` t)
        let lst = mkList (tcon "Nat") $ take n $ iterate (con "Succ" `app`) (con "Zero")
        expr <- global mapID `aPP` tcon "Nat" `aPP` tcon "Bool" `app` global evenID `app` lst
        let mapDef = DefAST $ ASTDef mapID "map" map_ mapTy
        let evenDef = DefAST $ ASTDef evenID "even" isEven evenTy
        let oddDef = DefAST $ ASTDef oddID "odd" isOdd oddTy
        let globs = [(mapID, mapDef), (evenID, evenDef), (oddID, oddDef)]
        expect <- mkList (tcon "Bool") (take n $ cycle [con "True", con "False"]) `ann` (tcon "List" `tapp` tcon "Bool")
        pure (globs, expr, expect)
   in do
        case evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 500 Syn e of
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 1000 Syn e
        distinctIDs s
        s <~==> Right expected

-- A worker/wrapper'd map
unit_9 :: Assertion
unit_9 =
  let n = 10
      ((globals, e, expected), maxID) = create $ do
        mapID <- fresh
        evenID <- fresh
        oddID <- fresh
        mapTy <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` tvar "b") `tfun` ((tcon "List" `tapp` tvar "a") `tfun` (tcon "List" `tapp` tvar "b"))
        let worker =
              lam "xs" $
                case_
                  (var "xs")
                  [ branch "Nil" [] $ con "Nil" `aPP` tvar "b"
                  , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ con "Cons" `aPP` tvar "b" `app` (var "f" `app` var "y") `app` (var "go" `app` var "ys")
                  ]
        map_ <- lAM "a" $ lAM "b" $ lam "f" $ letrec "go" worker ((tcon "List" `tapp` tvar "a") `tfun` (tcon "List" `tapp` tvar "b")) $ var "go"
        -- even and odd have almost the same type, but their types contain different IDs
        let evenOddTy = tcon "Nat" `tfun` tcon "Bool"
        evenTy <- evenOddTy
        oddTy <- evenOddTy
        isEven <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "True", branch "Succ" [("n", Nothing)] $ global oddID `app` var "n"]
        isOdd <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "False", branch "Succ" [("n", Nothing)] $ global evenID `app` var "n"]
        let mkList t = foldr (\x xs -> con "Cons" `aPP` t `app` x `app` xs) (con "Nil" `aPP` t)
        let lst = mkList (tcon "Nat") $ take n $ iterate (con "Succ" `app`) (con "Zero")
        expr <- global mapID `aPP` tcon "Nat" `aPP` tcon "Bool" `app` global evenID `app` lst
        let mapDef = DefAST $ ASTDef mapID "map" map_ mapTy
        let evenDef = DefAST $ ASTDef evenID "even" isEven evenTy
        let oddDef = DefAST $ ASTDef oddID "odd" isOdd oddTy
        let globs = [(mapID, mapDef), (evenID, evenDef), (oddID, oddDef)]
        expect <- mkList (tcon "Bool") (take n $ cycle [con "True", con "False"]) `ann` (tcon "List" `tapp` tcon "Bool")
        pure (globs, expr, expect)
   in do
        case evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 500 Syn e of
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 1000 Syn e
        distinctIDs s
        s <~==> Right expected

-- Check that we handle constructors-are-synthesisable well
-- NB: annotated scrutinees are common, e.g. (λx.case x of ... : S -> T) s
--     but plain constructors should be supported also, as we let users write
--     construtors in synthesisable position
unit_10 :: Assertion
unit_10 =
  let ((s, t, expected), maxID) = create $ do
        annCase <-
          case_
            (con "Zero" `ann` tcon "Nat")
            [ branch "Zero" [] $ con "True"
            , branch "Succ" [("n", Nothing)] $ con "False"
            ]
        noannCase <-
          case_
            (con "Zero")
            [ branch "Zero" [] $ con "True"
            , branch "Succ" [("n", Nothing)] $ con "False"
            ]
        expect <- con "True"
        pure (annCase, noannCase, expect)
   in do
        let s' = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 2 Syn s
            t' = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 2 Syn t
        distinctIDs s'
        s' <~==> Right expected
        distinctIDs t'
        t' <~==> Right expected

-- This example shows that when we are under even a 'let' all we can do is
-- substitute, otherwise we may go down a rabbit hole!
unit_11 :: Assertion
unit_11 =
  let ((globals, e, expected), maxID) = create $ do
        evenID <- fresh
        oddID <- fresh
        -- even and odd have almost the same type, but their types contain different IDs
        let evenOddTy = tcon "Nat" `tfun` tcon "Bool"
        evenTy <- evenOddTy
        oddTy <- evenOddTy
        isEven <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "True", branch "Succ" [("n", Nothing)] $ global oddID `app` var "n"]
        isOdd <- lam "x" $ case_ (var "x") [branch "Zero" [] $ con "False", branch "Succ" [("n", Nothing)] $ global evenID `app` var "n"]
        let ty = tcon "Nat" `tfun` (tcon "Pair" `tapp` tcon "Bool" `tapp` tcon "Nat")
        let expr1 =
              let_ "x" (con "Zero") $
                lam "n" (con "MakePair" `aPP` tcon "Bool" `aPP` tcon "Nat" `app` (global evenID `app` var "n") `app` var "x")
                  `ann` ty
        expr <- expr1 `app` con "Zero"
        let evenDef = DefAST $ ASTDef evenID "even" isEven evenTy
        let oddDef = DefAST $ ASTDef oddID "odd" isOdd oddTy
        let globs = [(evenID, evenDef), (oddID, oddDef)]
        expect <-
          (con "MakePair" `aPP` tcon "Bool" `aPP` tcon "Nat" `app` con "True" `app` con "Zero")
            `ann` (tcon "Pair" `tapp` tcon "Bool" `tapp` tcon "Nat")
        pure (globs, expr, expect)
   in do
        case evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 10 Syn e of
          Left (TimedOut _) -> pure ()
          x -> assertFailure $ show x
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) (M.fromList globals) 20 Syn e
        distinctIDs s
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
                  (var "x")
                  [ branch "Zero" [] $ con "True"
                  , branch "Succ" [("i", Nothing)] $ var "f" `app` var "i"
                  ]
        expr <- let_ "n" (con "Zero") $ letrec "f" f (tcon "Nat" `tfun` tcon "Bool") $ var "f" `app` var "n"
        expect <- con "True" `ann` tcon "Bool"
        pure (expr, expect)
   in do
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 15 Syn e
        distinctIDs s
        s <~==> Right expected

unit_13 :: Assertion
unit_13 =
  let ((e, expected), maxID) = create $ do
        expr <- (lam "x" (con "C" `app` var "x" `app` let_ "x" (con "True") (var "x") `app` var "x") `ann` (tcon "Nat" `tfun` tcon "Bool")) `app` con "Zero"
        expect <- (con "C" `app` con "Zero" `app` con "True" `app` con "Zero") `ann` tcon "Bool"
        pure (expr, expect)
   in do
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 15 Syn e
        distinctIDs s
        s <~==> Right expected

unit_14 :: Assertion
unit_14 =
  let ((e, expected), maxID) = create $ do
        expr <- (lam "x" (lam "x" $ var "x") `ann` (tcon "Bool" `tfun` (tcon "Nat" `tfun` tcon "Nat"))) `app` con "True" `app` con "Zero"
        expect <- con "Zero" `ann` tcon "Nat"
        pure (expr, expect)
   in do
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 15 Syn e
        distinctIDs s
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
        let l = let_ "x" (var "y")
        let c a b = con "C" `app` var a `app` var b
        e0 <- l $ lam "y" $ c "x" "y"
        let y' = "a50" -- NB: fragile name "a50"
        e1 <- l $ lam y' $ let_ "y" (var y') $ c "x" "y"
        e2 <- l $ lam y' $ let_ "y" (var y') $ c "x" y'
        e3 <- l $ lam y' $ c "x" y'
        e4 <- l $ lam y' $ c "y" y'
        e5 <- lam y' $ c "y" y'
        pure (e0, [e0, e1, e2, e3, e4, e5], e5)
   in do
        let si = map (\i -> evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty i Syn expr) [0 .. fromIntegral $ length steps - 1]
            f s e = do
              distinctIDs s
              s <~==> Left (TimedOut e)
        zipWithM_ f si steps
        let s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty (fromIntegral $ length steps) Syn expr
        distinctIDs s
        s <~==> Right expected

unit_hole_ann_case :: Assertion
unit_hole_ann_case =
  let (tm, maxID) = create $ hole $ ann (case_ emptyHole []) (tcon "Bool")
   in evalFullTest maxID (mkTypeDefMap defaultTypeDefs) mempty 1 Chk tm @?= Right tm

-- TODO: examples with holes

-- TODO: most of these property tests could benefit from generating an
-- arbitrary context first.
-- See https://github.com/hackworthltd/primer/issues/50

-- | Resuming evaluation is the same as running it for longer in the first place
hprop_resume :: Property
hprop_resume = withDiscards 2000 $
  propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
    (dir, t, _, globs) <- genDirTmGlobs
    resumeTest globs dir t

-- A helper for hprop_resume, and hprop_resume_regression
resumeTest :: Map ID Def -> Dir -> Expr -> PropertyT WT ()
resumeTest globs dir t = do
  tds <- asks typeDefs
  n <- forAllT $ Gen.integral $ Range.linear 2 1000 -- Arbitrary limit here
  -- NB: We need to run this first reduction in an isolated context
  -- as we need to avoid it changing the fresh-name-generator state
  -- for the next run (sMid and sTotal). This is because reduction may need
  -- to create fresh names, and we want to test "reducing n+m steps" is
  -- exactly the same as "reducing n steps and then further reducing m
  -- steps" (including generated names). (A happy consequence of this is that
  -- it is precisely the same including ids in metadata.)
  (stepsFinal', sFinal) <- lift $ isolateWT $ evalFullStepCount tds globs n dir t
  when (stepsFinal' < 2) discard
  let stepsFinal = case sFinal of Left _ -> stepsFinal'; Right _ -> 1 + stepsFinal'
  m <- forAllT $ Gen.integral $ Range.constant 1 (stepsFinal - 1)
  (stepsMid, sMid') <- evalFullStepCount tds globs m dir t
  stepsMid === m
  sMid <- case sMid' of
    Left (TimedOut e) -> pure e
    -- This should never happen: we know we are not taking enough steps to
    -- hit a normal form (as m < stepsFinal)
    Right e -> assert False >> pure e
  (stepsTotal, sTotal) <- evalFullStepCount tds globs (stepsFinal - m) dir sMid
  stepsMid + stepsTotal === stepsFinal'
  sFinal === sTotal

-- A pseudo-unit regression test: when reduction needs to create fresh names,
-- the two reduction attempts in resumeTest should not interfere with each
-- other's names, else we will get occasional failures in that property test.
hprop_resume_regression :: Property
hprop_resume_regression = propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
  -- This indeed requires fresh names when reducing (see unit_type_preservation_rename_LAM_regression)
  t <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" emptyHole))
  resumeTest mempty Chk t

-- A regression test: previously EvalFull would rename to avoid variable
-- capture, but would use let instead of lettype for type abstractions ("big
-- lambdas"). (I.e. we changed 'λx.e' into 'λy.let x=y in e' and also did the
-- same for 'Λa.e' into 'Λb.let a=b in e', instead of 'Λb.lettype a=b in e'!)
-- This would lead to sporadic failures in hprop_type_preservation
-- ("WrongSortVariable").
unit_type_preservation_rename_LAM_regression :: Assertion
unit_type_preservation_rename_LAM_regression =
  let ((expr, expected), maxID) = create $ do
        e <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a" emptyHole))
        -- We may expect the following, but our evaluator doesn't notice that
        -- the letrec never engenders a substitution.
        --   expect <- lAM "a" (lAM "a" emptyHole)
        -- and out of an abundance of caution we rename the potentially-capturing inner lambda
        expect <- lAM "a" (letrec "b" emptyHole (tvar "a") (lAM "a14" (letType "a" (tvar "a14") emptyHole))) -- NB: fragile name a14
        pure (e, expect)
      s = evalFullTest maxID mempty mempty 1 Chk expr
   in do
        distinctIDs s
        s <~==> Left (TimedOut expected)

-- | Evaluation preserves types
-- (assuming we don't end with a 'LetType' in the term, as the typechecker
-- cannot currently deal with those)
hprop_type_preservation :: Property
hprop_type_preservation = withTests 1000 $
  withDiscards 2000 $
    propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
      tds <- asks typeDefs
      (dir, t, ty, globs) <- genDirTmGlobs
      let test msg e = do
            s <- case e of
              Left (TimedOut s') -> label (msg <> "TimedOut") >> pure s'
              Right s' -> label (msg <> "NF") >> pure s'
            if null [() | LetType{} <- universe s]
              then do
                annotateShow s
                s' <- checkTest ty s
                forgetIDs s === forgetIDs s' -- check no smart holes happened
              else label (msg <> "skipped due to LetType") >> success
      maxSteps <- forAllT $ Gen.integral $ Range.linear 1 1000 -- Arbitrary limit here
      (steps, s) <- evalFullStepCount tds globs maxSteps dir t
      -- s is often reduced to normal form
      test "long " s
      -- also test an intermediate point
      if steps <= 1
        then label "generated a normal form"
        else do
          midSteps <- forAllT $ Gen.integral $ Range.linear 1 (steps - 1)
          (_, s') <- evalFullStepCount tds globs midSteps dir t
          test "mid " s'

unit_prim_toUpper :: Assertion
unit_prim_toUpper =
  unaryPrimTest
    "toUpper"
    (char 'a')
    (char 'A')

unit_prim_isSpace_1 :: Assertion
unit_prim_isSpace_1 =
  unaryPrimTest
    "isSpace"
    (char '\n')
    (bool_ True)

unit_prim_isSpace_2 :: Assertion
unit_prim_isSpace_2 =
  unaryPrimTest
    "isSpace"
    (char 'a')
    (bool_ False)

hprop_prim_hex_nat :: Property
hprop_prim_hex_nat = withTests 20 . property $ do
  n <- forAllT $ Gen.integral $ Range.constant 0 50
  let ne = nat n
      ((e, r, gs), maxID) =
        if n <= 15
          then create . withPrimDefs $ \defs globals ->
            (,,)
              <$> case_
                ( global (defs ! "natToHex")
                    `app` ne
                )
                [ branch
                    "Nothing"
                    []
                    (con "Nothing")
                , branch
                    "Just"
                    [("x", Nothing)]
                    ( global (defs ! "hexToNat")
                        `app` var "x"
                    )
                ]
              <*> (con "Just" `aPP` tcon "Nat")
                `app` ne
              <*> pure (DefPrim <$> globals)
          else create . withPrimDefs $ \defs globals ->
            (,,)
              <$> global (defs ! "natToHex")
                `app` ne
              <*> con "Nothing"
                `aPP` tcon "Char"
              <*> pure (DefPrim <$> globals)
      s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) gs 7 Syn e
  set _ids' 0 s === set _ids' 0 (Right r)

unit_prim_char_eq_1 :: Assertion
unit_prim_char_eq_1 =
  binaryPrimTest
    "eqChar"
    (char 'a')
    (char 'a')
    (con "True")

unit_prim_char_eq_2 :: Assertion
unit_prim_char_eq_2 =
  binaryPrimTest
    "eqChar"
    (char 'a')
    (char 'A')
    (con "False")

unit_prim_char_partial :: Assertion
unit_prim_char_partial =
  let ((e, gs), maxID) =
        create . withPrimDefs $ \defs globals ->
          (,)
            <$> global (defs ! "eqChar")
              `app` char 'a'
            <*> pure (DefPrim <$> globals)
      s = evalFullTest maxID mempty gs 1 Syn e
   in do
        distinctIDs s
        s <~==> Right e

unit_prim_int_add :: Assertion
unit_prim_int_add =
  binaryPrimTest
    "Int.+"
    (int 2)
    (int 2)
    (int 4)

unit_prim_int_add_big :: Assertion
unit_prim_int_add_big =
  binaryPrimTest
    "Int.+"
    (int big)
    (int big)
    (int (2 * big :: Integer))
  where
    big = fromIntegral (maxBound :: Word64)

unit_prim_int_sub :: Assertion
unit_prim_int_sub =
  binaryPrimTest
    "Int.-"
    (int 5)
    (int 3)
    (int 2)

unit_prim_int_sub_negative :: Assertion
unit_prim_int_sub_negative =
  binaryPrimTest
    "Int.-"
    (int 3)
    (int 5)
    (int (-2))

unit_prim_int_mul :: Assertion
unit_prim_int_mul =
  binaryPrimTest
    "Int.×"
    (int 3)
    (int 2)
    (int 6)

unit_prim_int_quotient :: Assertion
unit_prim_int_quotient =
  binaryPrimTest
    "Int.quotient"
    (int 7)
    (int 3)
    (con "Just" `aPP` tcon "Int" `app` int 2)

unit_prim_int_quotient_negative :: Assertion
unit_prim_int_quotient_negative =
  binaryPrimTest
    "Int.quotient"
    (int (-7))
    (int 3)
    (con "Just" `aPP` tcon "Int" `app` int (-3))

unit_prim_int_quotient_zero :: Assertion
unit_prim_int_quotient_zero =
  binaryPrimTest
    "Int.quotient"
    (int (-7))
    (int 0)
    (con "Nothing" `aPP` tcon "Int")

unit_prim_int_remainder :: Assertion
unit_prim_int_remainder =
  binaryPrimTest
    "Int.remainder"
    (int 7)
    (int 3)
    (con "Just" `aPP` tcon "Int" `app` int 1)

unit_prim_int_remainder_negative_1 :: Assertion
unit_prim_int_remainder_negative_1 =
  binaryPrimTest
    "Int.remainder"
    (int (-7))
    (int (-3))
    (con "Just" `aPP` tcon "Int" `app` int (-1))

unit_prim_int_remainder_negative_2 :: Assertion
unit_prim_int_remainder_negative_2 =
  binaryPrimTest
    "Int.remainder"
    (int (-7))
    (int 3)
    (con "Just" `aPP` tcon "Int" `app` int 2)

unit_prim_int_remainder_negative_3 :: Assertion
unit_prim_int_remainder_negative_3 =
  binaryPrimTest
    "Int.remainder"
    (int 7)
    (int (-3))
    (con "Just" `aPP` tcon "Int" `app` int (-2))

unit_prim_int_remainder_zero :: Assertion
unit_prim_int_remainder_zero =
  binaryPrimTest
    "Int.remainder"
    (int 7)
    (int 0)
    (con "Nothing" `aPP` tcon "Int")

unit_prim_int_quot :: Assertion
unit_prim_int_quot =
  binaryPrimTest
    "Int.quot"
    (int 7)
    (int 3)
    (int 2)

unit_prim_int_quot_negative :: Assertion
unit_prim_int_quot_negative =
  binaryPrimTest
    "Int.quot"
    (int (-7))
    (int 3)
    (int (-3))

unit_prim_int_quot_zero :: Assertion
unit_prim_int_quot_zero =
  binaryPrimTest
    "Int.quot"
    (int (-7))
    (int 0)
    (int 0)

unit_prim_int_rem :: Assertion
unit_prim_int_rem =
  binaryPrimTest
    "Int.rem"
    (int 7)
    (int 3)
    (int 1)

unit_prim_int_rem_negative_1 :: Assertion
unit_prim_int_rem_negative_1 =
  binaryPrimTest
    "Int.rem"
    (int (-7))
    (int (-3))
    (int (-1))

unit_prim_int_rem_negative_2 :: Assertion
unit_prim_int_rem_negative_2 =
  binaryPrimTest
    "Int.rem"
    (int (-7))
    (int 3)
    (int 2)

unit_prim_int_rem_negative_3 :: Assertion
unit_prim_int_rem_negative_3 =
  binaryPrimTest
    "Int.rem"
    (int 7)
    (int (-3))
    (int (-2))

unit_prim_int_rem_zero :: Assertion
unit_prim_int_rem_zero =
  binaryPrimTest
    "Int.rem"
    (int 7)
    (int 0)
    (int 7)

unit_prim_int_eq_1 :: Assertion
unit_prim_int_eq_1 =
  binaryPrimTest
    "Int.="
    (int 2)
    (int 2)
    (bool_ True)

unit_prim_int_eq_2 :: Assertion
unit_prim_int_eq_2 =
  binaryPrimTest
    "Int.="
    (int 2)
    (int 1)
    (bool_ False)

unit_prim_int_neq_1 :: Assertion
unit_prim_int_neq_1 =
  binaryPrimTest
    "Int.≠"
    (int 2)
    (int 2)
    (bool_ False)

unit_prim_int_neq_2 :: Assertion
unit_prim_int_neq_2 =
  binaryPrimTest
    "Int.≠"
    (int 2)
    (int 1)
    (bool_ True)

unit_prim_int_less_than_1 :: Assertion
unit_prim_int_less_than_1 =
  binaryPrimTest
    "Int.<"
    (int 1)
    (int 2)
    (bool_ True)

unit_prim_int_less_than_2 :: Assertion
unit_prim_int_less_than_2 =
  binaryPrimTest
    "Int.<"
    (int 1)
    (int 1)
    (bool_ False)

unit_prim_int_less_than_or_equal_1 :: Assertion
unit_prim_int_less_than_or_equal_1 =
  binaryPrimTest
    "Int.≤"
    (int 1)
    (int 2)
    (bool_ True)

unit_prim_int_less_than_or_equal_2 :: Assertion
unit_prim_int_less_than_or_equal_2 =
  binaryPrimTest
    "Int.≤"
    (int 1)
    (int 1)
    (bool_ True)

unit_prim_int_less_than_or_equal_3 :: Assertion
unit_prim_int_less_than_or_equal_3 =
  binaryPrimTest
    "Int.≤"
    (int 2)
    (int 1)
    (bool_ False)

unit_prim_int_greater_than_1 :: Assertion
unit_prim_int_greater_than_1 =
  binaryPrimTest
    "Int.>"
    (int 2)
    (int 1)
    (bool_ True)

unit_prim_int_greater_than_2 :: Assertion
unit_prim_int_greater_than_2 =
  binaryPrimTest
    "Int.>"
    (int 1)
    (int 1)
    (bool_ False)

unit_prim_int_greater_than_or_equal_1 :: Assertion
unit_prim_int_greater_than_or_equal_1 =
  binaryPrimTest
    "Int.≥"
    (int 1)
    (int 2)
    (bool_ False)

unit_prim_int_greater_than_or_equal_2 :: Assertion
unit_prim_int_greater_than_or_equal_2 =
  binaryPrimTest
    "Int.≥"
    (int 1)
    (int 1)
    (bool_ True)

unit_prim_int_greater_than_or_equal_3 :: Assertion
unit_prim_int_greater_than_or_equal_3 =
  binaryPrimTest
    "Int.≥"
    (int 2)
    (int 1)
    (bool_ True)

unit_prim_int_toNat :: Assertion
unit_prim_int_toNat =
  unaryPrimTest
    "Int.toNat"
    (int 0)
    (con "Just" `aPP` tcon "Nat" `app` nat 0)

unit_prim_int_toNat_negative :: Assertion
unit_prim_int_toNat_negative =
  unaryPrimTest
    "Int.toNat"
    (int (-1))
    (con "Nothing" `aPP` tcon "Nat")

unit_prim_int_fromNat :: Assertion
unit_prim_int_fromNat =
  unaryPrimTest
    "Int.fromNat"
    (nat 4)
    (int 4)

unit_prim_ann :: Assertion
unit_prim_ann =
  let ((e, r, gs), maxID) =
        create . withPrimDefs $ \defs globals -> do
          (,,)
            <$> ( global (defs ! "toUpper")
                    `ann` (tcon "Char" `tfun` tcon "Char")
                )
              `app` (char 'a' `ann` tcon "Char")
            <*> char 'A'
            <*> pure (DefPrim <$> globals)
      s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) gs 2 Syn e
   in do
        distinctIDs s
        s <~==> Right r

unit_prim_partial_map :: Assertion
unit_prim_partial_map =
  let ((e, r, gs), maxID) =
        create . withPrimDefs $ \defs globals -> do
          map_ <- mapDef
          (,,)
            <$> global (defID map_)
              `aPP` tcon "Char"
              `aPP` tcon "Char"
              `app` global (defs ! "toUpper")
              `app` list_
                "Char"
                [ char 'a'
                , char 'b'
                , char 'c'
                ]
            <*> list_
              "Char"
              [ char 'A'
              , char 'B'
              , char 'C'
              ]
              `ann` (tcon "List" `tapp` tcon "Char")
            <*> pure (M.singleton (defID map_) map_ <> (DefPrim <$> globals))
      s = evalFullTest maxID (mkTypeDefMap defaultTypeDefs) gs 65 Syn e
   in do
        distinctIDs s
        s <~==> Right r
  where
    mapDef :: MonadFresh ID m => m Def
    mapDef = do
      mapID <- fresh
      mapTy <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` tvar "b") `tfun` ((tcon "List" `tapp` tvar "a") `tfun` (tcon "List" `tapp` tvar "b"))
      let worker =
            lam "xs" $
              case_
                (var "xs")
                [ branch "Nil" [] $ con "Nil" `aPP` tvar "b"
                , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ con "Cons" `aPP` tvar "b" `app` (var "f" `app` var "y") `app` (var "go" `app` var "ys")
                ]
      map_ <- lAM "a" $ lAM "b" $ lam "f" $ letrec "go" worker ((tcon "List" `tapp` tvar "a") `tfun` (tcon "List" `tapp` tvar "b")) $ var "go"
      pure $ DefAST $ ASTDef mapID "map" map_ mapTy

-- * Utilities

evalFullTest :: ID -> M.Map Name TypeDef -> M.Map ID Def -> TerminationBound -> Dir -> Expr -> Either EvalFullError Expr
evalFullTest id_ tydefs globals n d e = evalTestM id_ $ evalFull tydefs globals n d e

unaryPrimTest :: Name -> S Expr -> S Expr -> Assertion
unaryPrimTest f x y =
  let ((e, r, gs), maxID) =
        create . withPrimDefs $ \defs globals ->
          (,,)
            <$> global (defs ! f)
              `app` x
            <*> y
            <*> pure (DefPrim <$> globals)
      s = evalFullTest maxID mempty gs 2 Syn e
   in do
        distinctIDs s
        s <~==> Right r
binaryPrimTest :: Name -> S Expr -> S Expr -> S Expr -> Assertion
binaryPrimTest f x y z =
  let ((e, r, gs), maxID) =
        create . withPrimDefs $ \defs globals ->
          (,,)
            <$> global (defs ! f)
              `app` x
              `app` y
            <*> z
            <*> pure (DefPrim <$> globals)
      s = evalFullTest maxID mempty gs 2 Syn e
   in do
        distinctIDs s
        s <~==> Right r

-- | Generates
--
--  * a term (to be the subject of some evaluation steps)
--
--  * definitions for each global variable (to be the environment of the evaluation steps)
--
-- Also returns
--
--  * whether the term is synthesisable or checkable
--
--  * the type of the term
genDirTmGlobs :: PropertyT WT (Dir, Expr, Type' (), M.Map ID Def)
genDirTmGlobs = do
  dir <- forAllT $ Gen.element [Chk, Syn]
  (t', ty) <- case dir of
    Chk -> do
      ty' <- forAllT $ genWTType KType
      t' <- forAllT $ genChk ty'
      pure (t', ty')
    Syn -> forAllT genSyn
  t <- generateIDs t'
  globTypes <- asks globalCxt
  let genDef i (n, defTy) =
        (\ty' e -> DefAST ASTDef{astDefID = i, astDefName = n, astDefType = ty', astDefExpr = e})
          <$> generateTypeIDs defTy <*> (generateIDs =<< genChk defTy)
  globs <- forAllT $ M.traverseWithKey genDef globTypes
  pure (dir, t, ty, globs)

_ids :: Traversal' Expr ID
_ids = (_exprMeta % _id) `adjoin` (_exprTypeMeta % _id)

_ids' :: Traversal' (Either EvalFullError Expr) ID
_ids' = _Left % timedOut % _ids `adjoin` _Right % _ids
  where
    timedOut = prism TimedOut (Right . \case TimedOut e -> e)

(<~==>) :: Either EvalFullError Expr -> Either EvalFullError Expr -> Assertion
x <~==> y = on (@?=) (set _ids' 0) x y

distinctIDs :: Either EvalFullError Expr -> Assertion
distinctIDs e =
  let ids = e ^.. _ids'
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
