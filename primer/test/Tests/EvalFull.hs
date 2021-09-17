module Tests.EvalFull where

import Control.Monad (when, zipWithM_)
import Control.Monad.Fresh (fresh)
import Control.Monad.Reader (asks)
import Data.Function (on)
import Data.Generics.Uniplate.Data (universe)
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Set as S
import Gen.Core.Typed (WT, forAllT, genChk, genSyn, genWTType, propertyWT)
import Hedgehog hiding (test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Optics
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
   in --in evalFullTest maxID mempty mempty 100 Syn e <~==> Left (TimedOut e)
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
        let list = mkList (tcon "Nat") $ take n $ iterate (con "Succ" `app`) (con "Zero")
        expr <- global mapID `aPP` tcon "Nat" `aPP` tcon "Bool" `app` global evenID `app` list
        let mapDef = Def mapID "map" map_ mapTy
        let evenDef = Def evenID "even" isEven evenTy
        let oddDef = Def oddID "odd" isOdd oddTy
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
        let list = mkList (tcon "Nat") $ take n $ iterate (con "Succ" `app`) (con "Zero")
        expr <- global mapID `aPP` tcon "Nat" `aPP` tcon "Bool" `app` global evenID `app` list
        let mapDef = Def mapID "map" map_ mapTy
        let evenDef = Def evenID "even" isEven evenTy
        let oddDef = Def oddID "odd" isOdd oddTy
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
        let evenDef = Def evenID "even" isEven evenTy
        let oddDef = Def oddID "odd" isOdd oddTy
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
-- See https://github.com/hackworthltd/vonnegut/pull/800

-- | Resuming evaluation is the same as running it for longer in the first place
hprop_resume :: Property
hprop_resume = withDiscards 1000 $
  propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
    tds <- asks typeDefs
    (dir, t, _, globs) <- genDirTmGlobs
    n <- forAllT $ Gen.integral $ Range.linear 2 1000 -- Arbitrary limit here
    (stepsFinal', sFinal) <- evalFullStepCount tds globs n dir t
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
    set _ids' 0 sFinal === set _ids' 0 sTotal

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
            if null [() | LetType {} <- universe s]
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

-- * Utilities

unit :: PropertyT IO () -> Property
unit = withTests 1 . property

evalFullTest :: ID -> M.Map Name TypeDef -> M.Map ID Def -> TerminationBound -> Dir -> Expr -> Either EvalFullError Expr
evalFullTest id_ tydefs globals n d e = evalTestM id_ $ evalFull tydefs globals n d e

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
        (\ty' e -> Def {defID = i, defName = n, defType = ty', defExpr = e})
          <$> generateTypeIDs defTy <*> (generateIDs =<< genChk defTy)
  globs <- forAllT $ M.traverseWithKey genDef globTypes
  pure (dir, t, ty, globs)

-- | Like '===' but specifically for expressions.
-- Ignores IDs, but remembers metadata (like cached types)
(~==) :: Expr -> Expr -> Assertion
x ~== y = on (@?=) (set _ids 0) x y

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
