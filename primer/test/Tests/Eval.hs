{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Eval where

import Foreword

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics ((^.))
import Primer.App (
  EvalReq (EvalReq, evalReqExpr, evalReqRedex),
  EvalResp (EvalResp, evalRespExpr),
  appIdCounter,
  handleEvalRequest,
  importModules,
  newEmptyApp, ProgError, App,
 )
import Primer.Builtins (
  boolDef,
  cFalse,
  cNil,
  cTrue,
  cZero,
  tBool,
 )
import Primer.Core (
  Expr,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind (KType, KFun),
  Type,
  getID,
  _id, qualifyName, mkSimpleModuleName, unsafeMkGlobalName, unLocalName, LocalName, Type' (TEmptyHole, TCon),
 )
import Primer.Core.DSL
import Primer.Core.Utils (forgetMetadata, forgetTypeMetadata)
import Primer.Def (ASTDef (..), Def (..), DefMap, defPrim)
import Primer.Eval (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  CaseReductionDetail (..),
  EvalDetail (..),
  EvalError (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LetRenameDetail (..),
  LocalVarInlineDetail (..),
  PushAppIntoLetrecDetail (..),
  RHSCaptured (Capture, NoCapture),
  findNodeByID,
  redexes,
  singletonCxtLet,
  singletonCxtLetType,
  singletonCxtLetrec,
  step,
  tryReduceExpr,
  tryReduceType, Cxt(Cxt),Local(LLetrec, LLet, LLetType), SomeLocal (LSome)
 )
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes), builtinModule, primitiveModule)
import Primer.Primitives (PrimDef (EqChar, ToUpper), primitiveGVar, tChar)
import Primer.Primitives.DSL (pfun)
import Primer.TypeDef (TypeDef (..), TypeDefMap,
                       ASTTypeDef (ASTTypeDef, astTypeDefParameters, astTypeDefConstructors,
                                   astTypeDefNameHints),ValCon(ValCon))
import Primer.Zipper (target)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import TestM (evalTestM)
import TestUtils (gvn, primDefs, vcn, PrimerLog, evalTestMNoSevereLogs)
import qualified Tests.Action.Prog
import Tests.Action.Prog (AppTestM)
import Primer.Eval (Dir(Syn), getNonCapturedLocal)

-- * 'tryReduce' tests

-- | A helper for these tests
runTryReduce :: TypeDefMap -> DefMap -> Cxt -> (Expr, ID) -> Either EvalError (Expr, EvalDetail)
runTryReduce tys globals locals (expr, i) = evalTestM i $ runExceptT
  $ tryReduceExpr tys globals locals Syn expr

-- For use in assertions
runTryReduceType :: DefMap -> Cxt -> (Type, ID) -> IO (Either EvalError (Type, EvalDetail))
runTryReduceType globals locals (ty, i) = evalTestMNoSevereLogs @() i $ runExceptT $ tryReduceType globals locals ty

unit_tryReduce_no_redex :: Assertion
unit_tryReduce_no_redex = do
  runTryReduce mempty mempty mempty (create (con cZero)) @?= Left NotRedex

unit_tryReduce_beta :: Assertion
unit_tryReduce_beta = do
  let (input, maxid) =
        create $ do
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con cZero
          app (pure l) (pure a)
      result = runTryReduce mempty mempty mempty (input, maxid)
  case result of
    Left NotRedex -> pure ()
    _ -> assertFailure $ show result

unit_tryReduce_beta_annotation :: Assertion
unit_tryReduce_beta_annotation = do
  let ((lambda, body, arg, input, expectedResult, argType, resultType), maxid) =
        create $ do
          t1 <- tcon' ["M"] "A"
          t2 <- tcon' ["M"] "B"
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con' ["M"] "C"
          i <- app (ann (pure l) (tfun (pure t1) (pure t2))) (pure a)
          r <- ann (let_ "x" (ann (pure a) (pure t1)) (pure x)) (pure t2)
          pure (l, x, a, i, r, t1, t2)
      result = runTryReduce tydefs mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{types = Just (l, r)}) -> do
      expr ~= expectedResult
      detail.before ~= input
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.lambdaID @?= lambda ^. _id
      detail.argID @?= arg ^. _id
      detail.bodyID @?= body ^. _id
      l ~~= argType
      r ~~= resultType
    _ -> assertFailure $ show result

-- We don't reduce @(λx. t : ?) a@, where the annotation
-- is a hole rather than an arrow
unit_tryReduce_beta_annotation_hole :: Assertion
unit_tryReduce_beta_annotation_hole = do
  let (input, maxid) =
        create $ do
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con' ["M"] "C"
          app (ann (pure l) tEmptyHole) (pure a)
      result = runTryReduce tydefs mempty mempty (input, maxid)
  case result of
    Left NotRedex -> pure ()
    _ -> assertFailure $ show result

unit_tryReduce_BETA :: Assertion
unit_tryReduce_BETA = do
  let ((body, lambda, arg, input, expectedResult, k, ty), maxid) =
        create $ do
          b <- aPP (con cNil) (tvar "x")
          l <- lAM "x" (pure b)
          a <- tcon tBool
          let k_ = KFun KType KType
          ty_ <- tEmptyHole
          i <- aPP (pure l `ann` tforall "a" k_ (pure ty_)) (pure a)
          r <- letType "x" (pure a) (pure b) `ann` tlet "a" (pure a) (pure ty_)
          pure (b, l, a, i, r, k_, ty_)
      result = runTryReduce tydefs mempty mempty (input, maxid)
  case result of
    Right (expr, BETAReduction detail) -> do
      expr ~= expectedResult
      detail.before ~= input
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.lambdaID @?= lambda ^. _id
      detail.letID @?= expr ^. _id
      detail.argID @?= arg ^. _id
      detail.bodyID @?= body ^. _id
      detail.types @?= Just (k,ty)
    _ -> assertFailure $ show result

unit_tryReduce_local_term_var :: Assertion
unit_tryReduce_local_term_var = do
  -- We assume we're inside a larger expression (e.g. a let) where the node that binds x has ID 5.
  let ((expr, val), i) = create $ (,) <$> lvar "x" <*> con' ["M"] "C"
      locals = singletonCxtLet @ID 5 "x" val
      result = runTryReduce tydefs mempty locals (expr, i)
  case result of
    Right (expr', LocalVarInline detail) -> do
      expr' ~= val

      detail.letID @?= 5
      detail.varID @?= 0
      detail.bindingName @?= "x"
      detail.valueID @?= 1
      detail.replacementID @?= 2
      detail.isTypeVar @?= False
    _ -> assertFailure $ show result

unit_tryReduce_local_type_var :: Assertion
unit_tryReduce_local_type_var = do
  -- We assume we're inside a larger expression (e.g. a let type) where the node that binds x has ID 5.
  let ((tyvar, val), i) = create $ (,) <$> tvar "x" <*> tcon' ["M"] "C"
      locals = singletonCxtLetType @ID 5 "x" val
  result <- runTryReduceType mempty locals (tyvar, i)
  case result of
    Right (ty, LocalTypeVarInline detail) -> do
      ty ~~= val

      detail.letID @?= 5
      detail.varID @?= 0
      detail.bindingName @?= "x"
      detail.valueID @?= 1
      detail.replacementID @?= 2
      detail.isTypeVar @?= True
    _ -> assertFailure $ show result

unit_tryReduce_global_var :: Assertion
unit_tryReduce_global_var = do
  let f = gvn ["M"] "f"
      ((expr, def), i) = create $ do
        g <- gvar f
        e <- lam "x" (lvar "x")
        t <- tfun (tcon' ["M"] "A") (tcon' ["M"] "B")
        pure (g, ASTDef{astDefExpr = e, astDefType = t})
      globals = Map.singleton f (DefAST def)
      result = runTryReduce tydefs globals mempty (expr, i)
      expectedResult = create' $ ann (lam "x" (lvar "x")) (tfun (tcon' ["M"] "A") (tcon' ["M"] "B"))
  case result of
    Right (expr', GlobalVarInline detail) -> do
      expr' ~= expectedResult

      detail.def @?= def
      detail.var @?= expr
      detail.after @?= expr'
    _ -> assertFailure $ show result

unit_tryReduce_let :: Assertion
unit_tryReduce_let = do
  let (expr, i) = create $ let_ "x" (con' ["M"] "C") (con' ["M"] "D")
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.letID @?= 0
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

-- let x = x in x ==> let y = x in let x = y in x
unit_tryReduce_let_self_capture :: Assertion
unit_tryReduce_let_self_capture = do
  let (expr, i) = create $ let_ "x" (lvar "x") (lvar "x")
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ let_ "a3" (lvar "x") $ let_ "x" (lvar "a3") (lvar "x")
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingNameOld @?= ["x"]
      detail.bindingNameNew @?= ["a3"]
      detail.binderID @?= [0]
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_lettype :: Assertion
unit_tryReduce_lettype = do
  let (expr, i) = create $ letType "x" (tcon' ["M"] "C") (con' ["M"] "D")
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.letID @?= 0
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

-- let type x = x in _ :: x ==> let type y = x in lettype x = y in _ :: x
unit_tryReduce_lettype_self_capture :: Assertion
unit_tryReduce_lettype_self_capture = do
  let (expr, i) = create $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ letType "a5" (tvar "x") $ letType "x" (tvar "a5") (emptyHole `ann` tvar "x")
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingNameOld @?= ["x"]
      detail.bindingNameNew @?= ["a5"]
      detail.binderID @?= [0]
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

-- tlet x = C in ty ==> ty  when x not occur free in ty
unit_tryReduce_tlet_elide :: Assertion
unit_tryReduce_tlet_elide = do
  let (ty, i) = create $ tlet "x" (tcon' ["M"] "C") (tcon' ["M"] "D")
      expectedResult = create' $ tcon' ["M"] "D"
  result <- runTryReduceType mempty mempty (ty, i)
  case result of
    Right (ty', TLetRemoval detail) -> do
      ty' ~~= expectedResult

      detail.before @?= ty
      detail.after ~~= expectedResult
      detail.bindingName @?= "x"
      detail.letID @?= 0
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

-- tlet x = x in x ==> tlet y = x in  tlet x = y in x
unit_tryReduce_tlet_self_capture :: Assertion
unit_tryReduce_tlet_self_capture = do
  let (ty, i) = create $ tlet "x" (tvar "x") (tvar "x")
      n = "a3"
      expectedResult = create' $ tlet n (tvar "x") $ tlet "x" (tvar n) (tvar "x")
  result <- runTryReduceType mempty mempty (ty, i)
  case result of
    Right (ty', TLetRename detail) -> do
      ty' ~~= expectedResult

      detail.before @?= ty
      detail.after ~~= expectedResult
      detail.bindingNameOld @?= "x"
      detail.bindingNameNew @?= unLocalName n
      detail.letID @?= 0
      detail.bindingOccurrences @?= [1]
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_letrec :: Assertion
unit_tryReduce_letrec = do
  let (expr, i) = create $ letrec "x" (con' ["M"] "C") (tcon' ["M"] "T") (con' ["M"] "D")
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.letID @?= 0
      detail.bodyID @?= 3
    _ -> assertFailure $ show result


-- let f = D in (letrec f = λx. x : T in λx. f x) f
--                                                ^
-- doesn't reduce until this f is inlined, because it would be captured by the letrec.
unit_tryReduce_letrec_name_clash :: Assertion
unit_tryReduce_letrec_name_clash = do
  -- We construct the letrec expression, and a fake "let" expression whose ID we can insert into the
  -- locals map. This simulates focusing on the letrec inside the let expression.
  let ((expr, d,t, letd), i) = create $ do
        -- the value bound by the outer let
        d_ <- con' ["M"] "D"
        -- the value bound is of this type
        t_ <- tcon' ["M"] "T"
        -- the application
        e <- app (letrec "f" (lam "x" (lvar "x")) (pure t) (lam "x" (app (lvar "f") (lvar "x")))) (lvar "f")
        -- the outer let
        letd_ <- let_ "f" (pure d_) (pure e)
        pure (e, d_,t_, letd_)
      result = runTryReduce tydefs mempty (singletonCxtLetrec (letd ^. _id) "f" d t) (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_1 :: Assertion
unit_tryReduce_case_1 = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D"), branch' (["M"], "C") [] (con' ["M"] "E")]
      result = runTryReduce tydefs mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "E"
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 1
      detail.ctorName @?= vcn ["M"] "C"
      detail.targetArgIDs @?= []
      detail.branchBindingIDs @?= []
      detail.branchRhsID @?= 4
      detail.letIDs @?= []
    _ -> assertFailure $ show result

unit_tryReduce_case_2 :: Assertion
unit_tryReduce_case_2 = do
  let (expr, i) =
        create $
          case_
            (app (app (app (con' ["M"] "C") (lam "x" (lvar "x"))) (lvar "y")) (lvar "z"))
            [ branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing), ("d", Nothing), ("e", Nothing)] (con' ["M"] "E")
            ]
      result = runTryReduce tydef mempty mempty (expr, i)
      x = unsafeMkGlobalName (["M"],"X")
      y = unsafeMkGlobalName (["M"],"Y")
      z = unsafeMkGlobalName (["M"],"Z")
      tydef = Map.singleton (unsafeMkGlobalName (["M"], "T")) $ TypeDefAST $ ASTTypeDef {
          astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                                     ,ValCon (unsafeMkGlobalName (["M"], "C")) [TCon () x,TCon () y,TCon () z]]
          , astTypeDefNameHints = []
          }
      expectedResult = create' $ let_ "c" (lam "x" (lvar "x") `ann` tcon x)
                                (let_ "d" (lvar "y" `ann` tcon y)
                                (let_ "e" (lvar "z" `ann` tcon z)
                                 (con' ["M"] "E")))
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 4
      detail.ctorName @?= vcn ["M"] "C"
      detail.targetArgIDs @?= [5, 7, 8]
      detail.branchBindingIDs @?= [11, 12, 13]
      detail.branchRhsID @?= 14
      detail.letIDs @?= [21, 18, 15]
    _ -> assertFailure $ show result

unit_tryReduce_case_3 :: Assertion
unit_tryReduce_case_3 = do
  let (expr, i) =
        create $
          case_
            (app (aPP (con' ["M"] "C") (tcon' ["M"] "D")) (con' ["M"] "E"))
            [ branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing)] (con' ["M"] "F")
            ]
      result = runTryReduce tydef mempty mempty (expr, i)
      tydef = Map.singleton (unsafeMkGlobalName (["M"], "T")) $ TypeDefAST $ ASTTypeDef {
          astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                                     ,ValCon (unsafeMkGlobalName (["M"], "C")) [TEmptyHole ()]]
          , astTypeDefNameHints = []
          }
      expectedResult = create' $ let_ "c" (con' ["M"] "E" `ann` tEmptyHole) (con' ["M"] "F")
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 3
      detail.ctorName @?= vcn ["M"] "C"
      detail.targetArgIDs @?= [5]
      detail.branchBindingIDs @?= [8]
      detail.branchRhsID @?= 9
      detail.letIDs @?= [10]
    _ -> assertFailure $ show result

unit_tryReduce_case_name_clash :: Assertion
unit_tryReduce_case_name_clash = do
  let (expr, i) =
        create $
          case_
            (con' ["M"] "C" `app` emptyHole `app` lvar "x")
            [branch' (["M"], "C") [("x", Nothing), ("y", Nothing)] emptyHole]
      result = runTryReduce tydef mempty mempty (expr, i)
      tydef = Map.singleton (unsafeMkGlobalName (["M"], "T")) $ TypeDefAST $ ASTTypeDef {
          astTypeDefParameters = []
          , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "C")) [TEmptyHole (),TEmptyHole ()]]
          , astTypeDefNameHints = []
          }
      expectedResult =
        create' $
          case_
            (con' ["M"] "C" `app` emptyHole `app` lvar "x")
            [branch' (["M"], "C") [("a9", Nothing), ("y", Nothing)] $ let_ "x" (lvar "a9") emptyHole]
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.bindingNameOld @?= ["x","y"]
      detail.bindingNameNew @?= ["a9","y"]
      detail.binderID @?= [6,7]
      detail.bodyID @?= 8
    _ -> assertFailure $ show result

{-
--unit_tryReduce_case_too_many_bindings :: Assertion
--unit_tryReduce_case_too_many_bindings = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "C") [("b", Nothing)] (con' ["M"] "D")]
      result = runTryReduce tydefs mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch

--unit_tryReduce_case_too_few_bindings :: Assertion
--unit_tryReduce_case_too_few_bindings = do
  let (expr, i) = create $ case_ (app (con' ["M"] "B") (lvar "y")) [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce tydefs mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch
-}

unit_tryReduce_case_scrutinee_not_redex :: Assertion
unit_tryReduce_case_scrutinee_not_redex = do
  let (expr, i) = create $ case_ (lvar "x") [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce tydefs mempty mempty (expr, i)
  result @?= Left NotRedex

{-
--unit_tryReduce_case_no_matching_branch :: Assertion
--unit_tryReduce_case_no_matching_branch = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce tydefs mempty mempty (expr, i)
  result @?= Left NoMatchingCaseBranch
-}

unit_tryReduce_prim :: Assertion
unit_tryReduce_prim = do
  let ((expr, expectedResult), i) =
        create $
          (,)
            <$> pfun EqChar
            `app` char 'a'
            `app` char 'a'
            <*> con cTrue
      result = runTryReduce tydefs primDefs mempty (expr, i)
  case result of
    Right (expr', ApplyPrimFun detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expr'
      detail.name @?= primitiveGVar EqChar
      detail.argIDs @?= [3, 4]
    _ -> assertFailure $ show result

unit_tryReduce_prim_fail_unsaturated :: Assertion
unit_tryReduce_prim_fail_unsaturated = do
  let (expr, i) =
        create $
          pfun EqChar
            `app` char 'a'
      result = runTryReduce tydefs primDefs mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_prim_fail_unreduced_args :: Assertion
unit_tryReduce_prim_fail_unreduced_args = do
  let (expr, i) =
        create $
          pfun EqChar
            `app` char 'a'
            `app` (pfun ToUpper `app` char 'a')
      result = runTryReduce tydefs primDefs mempty (expr, i)
  result @?= Left NotRedex

-- One can call the eval-step api endpoint with an expression and ID
-- of one of its nodes where that node is not a redex. This will call
-- step with such data. We should not assume that all eval api calls
-- will actually be redexes.
-- In particular here we test variable occurrences which cannot be
-- reduced by inlining a let.
unit_step_non_redex :: Assertion
unit_step_non_redex = do
  ((r1, s1), (r2, s2)) <- evalTestMNoSevereLogs @() 0 $ do
        e1 <- let_ "x" (con' ["M"] "C") $ lam "x" $ lvar "x"
        e2 <- let_ "x" (con' ["M"] "C" `app` lvar "x") $ lvar "x"
        let i1 = 3
        let i2 = 8 -- NB: e1 has nodes 0,1,2,3; e2 has 4,5,6,7,8
        s1' <- step tydefs mempty e1 Syn i1
        s2' <- step tydefs mempty e2 Syn i2
        pure ((elem i1 $ redexes mempty mempty Syn e1, s1'), (elem i2 $ redexes mempty mempty Syn e2, s2'))
  assertBool "Should not be in 'redexes', as shadowed by a lambda" $ not r1
  assertBool "Should not be in 'redexes', as would self-capture" $ not r2
  case s1 of
    Left NotRedex -> pure ()
    s1' -> assertFailure $ show s1'
  case s2 of
    Left NotRedex -> pure ()
    s2' -> assertFailure $ show s2'

-- * 'findNodeByID' tests

-- Some helpers to aid inference of matching on existentials in do notation
lookupNonCaptured :: LocalName k -> Cxt -> Maybe (ID, SomeLocal)
lookupNonCaptured = runReader . getNonCapturedLocal

lookupNonCapturedLet :: LocalName k -> Cxt -> Maybe (ID, Expr)
lookupNonCapturedLet n c = lookupNonCaptured n c >>= \case
  (i,LSome (LLet _ e)) -> Just (i,e)
  _ -> Nothing

lookupNonCapturedLetType :: LocalName k -> Cxt -> Maybe (ID, Type)
lookupNonCapturedLetType n c = lookupNonCaptured n c >>= \case
  (i,LSome (LLetType _ t)) -> Just (i,t)
  _ -> Nothing

lookupNonCapturedLetrec :: LocalName k -> Cxt -> Maybe (ID, Expr)
lookupNonCapturedLetrec n c = lookupNonCaptured n c >>= \case
  (i,LSome (LLetrec _ e _)) -> Just (i,e)
  _ -> Nothing

lookupCaptured :: LocalName k -> Cxt -> Maybe (ID, SomeLocal)
lookupCaptured n c@(Cxt c') | Nothing <- lookupNonCaptured n c
                            , Just (Just r,i,_) <- Map.lookup (unLocalName n) c'
  = pure (i,r)
                            |otherwise = Nothing

lookupCapturedLetType :: LocalName k -> Cxt -> Maybe (ID, Type)
lookupCapturedLetType n c = lookupCaptured n c >>= \case
  (i,LSome (LLetType _ t)) -> Just (i,t)
  _ -> Nothing

lookupCapturedLetrec :: LocalName k -> Cxt -> Maybe (ID, Expr)
lookupCapturedLetrec n c = lookupCaptured n c >>= \case
  (i,LSome (LLetrec _ e _)) -> Just (i,e)
  _ -> Nothing

unit_findNodeByID_letrec :: Assertion
unit_findNodeByID_letrec = do
  let expr = create' $ letrec "x" (lvar "x") (tcon' ["M"] "T") (lvar "x")
      x = create' $ lvar "x"
      t = create' $ tcon' ["M"] "T"
  case findNodeByID 0 Syn expr of
    Just (Cxt locals, Left (_,z)) -> do
      assertBool "no locals in scope at node 0" $ Map.null locals
      target z ~= expr
    _ -> assertFailure "node 0 not found"
  case findNodeByID 1 Syn expr of
    Just (locals, Left (_,z)) -> do
      target z ~= x
      case lookupNonCapturedLetrec "x" locals of
        Just (0, e) -> e ~= x
        _ -> assertFailure $ show locals
    _ -> assertFailure "node 1 not found"
  case findNodeByID 2 Syn expr of
    Just (Cxt locals, Right z) -> do
      target z ~~= t
      assertBool "no locals in scope at node 2" $ Map.null locals
    _ -> assertFailure "node 2 not found"
  case findNodeByID 3 Syn expr of
    Just (locals, Left (_,z)) -> do
      target z ~= x
      case lookupNonCapturedLetrec "x" locals of
        Just (0 , e) -> e ~= x
        _ -> assertFailure $ show locals
    _ -> assertFailure "node 3 not found"

unit_findNodeByID_1 :: Assertion
unit_findNodeByID_1 = do
  let (x, c, expr) = create' $ do
        -- id 0
        x_ <- lvar "x"
        -- id 1
        c_ <- con' ["M"] "C"
        -- id 2
        e <- let_ "x" (pure c_) (pure x_)
        pure (x_, c_, e)
  case findNodeByID 0 Syn expr of
    Just (locals, Left (_,z)) -> do
      case lookupNonCapturedLet "x" locals of
        Just (i , e) -> do
          i @?= 2
          e ~= c
        _ -> assertFailure $ show locals
      target z ~= x
    _ -> assertFailure "node 0 not found"

  case findNodeByID 1 Syn expr of
    Just (Cxt locals, Left (_,z)) -> do
      assertBool "expected nothing in scope" $ Map.null locals
      target z ~= c
    _ -> assertFailure "node 1 not found"

  case findNodeByID 2 Syn expr of
    Just (Cxt locals, Left (_, z)) -> do
      assertBool "expected nothing in scope" $ Map.null locals
      target z ~= expr
    _ -> assertFailure "node 2 not found"

unit_findNodeByID_2 :: Assertion
unit_findNodeByID_2 = do
  let (x, t, expr) = create' $ do
        -- id 0
        x_ <- tvar "x"
        -- id 1
        t_ <- tcon' ["M"] "T"
        -- id 2
        e <- letType "x" (pure t_) (ann (lvar "y") (pure x_))
        pure (x_, t_, e)
  case findNodeByID 0 Syn expr of
    Just (locals, Right z) -> do
      case lookupNonCapturedLetType "x" locals of
        Just (i, e) -> do
          i @?= 2
          e ~~= t
        _ -> assertFailure $ show locals
      target z ~~= x
    _ -> assertFailure "node 0 not found"

unit_findNodeByID_tlet :: Assertion
unit_findNodeByID_tlet = do
  let (x, t, expr) = create' $ do
        -- id 0
        x_ <- tvar "x"
        -- id 1
        t_ <- tcon' ["M"] "T"
        -- id 2
        e <- ann (lvar "y") (tlet "x" (tcon' ["M"] "T") (pure x_))
        pure (x_, t_, e)
  case findNodeByID 0 Syn  expr of
    Just (locals, Right z) -> do
      case lookupNonCapturedLetType "x" locals of
        Just (i, e) -> do
          i @?= 4
          e ~~= t
        _ -> assertFailure $ show locals
      target z ~~= x
    _ -> assertFailure "node 0 not found"

unit_findNodeByID_scoping_1 :: Assertion
unit_findNodeByID_scoping_1 = do
  let expr = create' $ let_ "x" (con' ["M"] "C") $ lam "x" $ lvar "x"
  case findNodeByID 3 Syn expr of
    _ -> assertFailure "expected failure"
    Just (Cxt locals, Left _) -> assertBool "Expected 'x' not to be in scope" (Map.null locals)
    _ -> assertFailure "Expected to find the lvar 'x'"

unit_findNodeByID_scoping_2 :: Assertion
unit_findNodeByID_scoping_2 = do
  let (bind, expr) = create' $ do
        b <- con' ["M"] "D"
        e <- let_ "x" (con' ["M"] "C") $ let_ "x" (pure b) $ lvar "x"
        pure (b, e)
  case findNodeByID 4 Syn expr of
    Just (locals@(Cxt locals'), Left _)
      | Map.size locals' == 1
      , lookupNonCapturedLet "x" locals == Just (3, bind) ->
          pure ()
    Just (_, Left _) -> assertFailure "Expected to have inner let binding of 'x' reported"
    _ -> assertFailure "Expected to find the lvar 'x'"

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- some intervening binder. This tests findNodeByID (and step, and thus
-- tryReduce), see unit_redexes_let_capture for a test of redexes
unit_findNodeByID_capture :: Assertion
unit_findNodeByID_capture = do
  (expr, varOcc, reduct) <- evalTestMNoSevereLogs @() 0 $ do
        v <- lvar "x"
        e <- letrec "x" (lvar "y") (tcon tBool) $ lam "y" $ pure v
        let r = getID v
        s <- step mempty mempty e Syn r
        pure (e, r, s)
  case findNodeByID varOcc Syn expr of
          Just (locals@(Cxt locals'), Left _)
            | Map.size locals' == 1
            , Just (1, _) <- lookupCapturedLetrec "x" locals ->
                pure ()
          Just (_, Left _) -> assertFailure "Expected let binding of 'x' to be reported as captured-if-inlined"
          _ -> assertFailure "Expected to find the lvar 'x'"
  case reduct of
          Left NotRedex -> pure ()
          e -> assertFailure $ show e

unit_findNodeByID_capture_type :: Assertion
unit_findNodeByID_capture_type = do
  (expr, varOcc, reduct) <- evalTestMNoSevereLogs @() 0 $ do
        v <- tvar "x"
        e <- letType "x" (tvar "y") (emptyHole `ann` tlet "z" (tvar "y") (tforall "y" KType (pure v)))
        let r = getID v
        s <- step mempty mempty e Syn r
        pure (e, r, s)
  case findNodeByID varOcc Syn expr of
          Just (locals@(Cxt locals'), Right _)
            | Map.size locals' == 2
            , Just (1,_) <- lookupCapturedLetType "x" locals
            , Just (5, _) <- lookupCapturedLetType "z" locals ->
                pure ()
          Just (_, Right _) -> assertFailure "Expected lettype binding of 'x' and the tlet binding of 'z' to be reported as captured-if-inlined"
          _ -> assertFailure "Expected to find the lvar 'x'"
  case reduct of
          Left NotRedex -> pure ()
          e -> assertFailure $ show e

-- * 'redexes' tests

-- In these tests we refer to node IDs in the expressions, because it's quite concise.
-- The downside is it's less obvious what each ID represents.
-- But if you read the DSL from left to right, you can work it out.
-- IDs start at 0.
-- e.g.
--
--  0        1    2        3          4    5        6         7
-- lam "y" (app (lam "x" (var "x")) (app (lam "z" (var "z")) (con' ["M"] "C")))

-- | A helper for these tests
redexesOf :: S Expr -> Set ID
redexesOf = foldMap Set.singleton . redexes tydefs mempty Syn . create'

-- | A variation of 'redexesOf' for when the expression tested requires primitives to be in scope.
redexesOfWithPrims :: S Expr -> Set ID
redexesOfWithPrims = foldMap Set.singleton . redexes tydefs primDefs Syn . create'

-- TODO: do we really want to say nothing in scope?
-- will need a typedef for data M.C = M.C ; data M.D = M.D
tydefs :: TypeDefMap
tydefs = c <> d
  where
    c = Map.singleton (unsafeMkGlobalName (["M"], "C")) $ TypeDefAST $ ASTTypeDef {
      astTypeDefParameters = []
      , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "C")) []]
      , astTypeDefNameHints = []
      }
    d = Map.singleton (unsafeMkGlobalName (["M"], "D")) $ TypeDefAST $ ASTTypeDef {
      astTypeDefParameters = []
      , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "D")) []]
      , astTypeDefNameHints = []
      }

unit_redexes_con :: Assertion
unit_redexes_con = redexesOf (con' ["M"] "C") @?= mempty

unit_redexes_lam_1 :: Assertion
unit_redexes_lam_1 = do
  redexesOf (app (lam "x" (lvar "x")) (con' ["M"] "C")) @?= mempty
  redexesOf (app (lam "x" (lvar "x") `ann` (tvar "a" `tfun` tvar "a")) (con' ["M"] "C")) @?= Set.singleton 0

unit_redexes_lam_2 :: Assertion
unit_redexes_lam_2 = do
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (con' ["M"] "C"))) @?= mempty
  redexesOf (lam "y" (app (lam "x" (lvar "x") `ann` (tvar "a" `tfun` tvar "a")) (con' ["M"] "C"))) @?= Set.singleton 1

unit_redexes_lam_3 :: Assertion
unit_redexes_lam_3 = do
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (app (lam "z" (lvar "z")) (con' ["M"] "C"))))
    @?= mempty
  redexesOf (lam "y" (app (lam "x" (lvar "x")`ann` (tvar "a" `tfun` tvar "a"))
                      (app (lam "z" (lvar "z")`ann` (tvar "a" `tfun` tvar "a")) (con' ["M"] "C"))))
    @?= Set.fromList [1, 8]

unit_redexes_lam_4 :: Assertion
unit_redexes_lam_4 = do
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (app (lam "z" (lvar "z")) (con' ["M"] "C"))))
    @?= mempty
  redexesOf (lam "y" (app (lam "x" (lvar "x")`ann` (tvar "a" `tfun` tvar "a"))
                      (app (lam "z" (lvar "z")`ann` (tvar "a" `tfun` tvar "a")) (con' ["M"] "C"))))
    @?= Set.fromList [1, 8]

unit_redexes_LAM_1 :: Assertion
unit_redexes_LAM_1 =
  redexesOf (lAM "a" (con' ["M"] "C")) @?= mempty

unit_redexes_LAM_2 :: Assertion
unit_redexes_LAM_2 = do
  redexesOf (aPP (lAM "a" (con' ["M"] "C")) (tcon' ["M"] "A")) @?= mempty
  redexesOf (aPP (lAM "a" (con' ["M"] "C") `ann` (tforall "a" KType (tcon' ["M"] "C")))
             (tcon' ["M"] "A")) @?= Set.fromList [0]

unit_redexes_LAM_3 :: Assertion
unit_redexes_LAM_3 = do
  redexesOf (lAM "a" (aPP (lAM "b" (con' ["M"] "X")) (tcon' ["M"] "T"))) @?= mempty
  redexesOf (lAM "a" (aPP (lAM "b" (con' ["M"] "X") `ann` (tforall "a" KType (tcon' ["M"] "C")))
                      (tcon' ["M"] "T"))) @?= Set.fromList [1]

unit_redexes_LAM_4 :: Assertion
unit_redexes_LAM_4 = do
  redexesOf (let_ "x" (con' ["M"] "C") (lAM "a" (aPP (lAM "b" (lvar "x")) (tcon' ["M"] "T"))))
    @?= Set.singleton 5
  redexesOf (let_ "x" (con' ["M"] "C") (lAM "a" (aPP (lAM "b" (lvar "x") `ann`
                                                      (tforall "a" KType (tcon' ["M"] "C"))) (tcon' ["M"] "T"))))
    @?= Set.fromList [3, 6]

unit_redexes_let_1 :: Assertion
unit_redexes_let_1 =
  redexesOf (let_ "x" (con' ["M"] "C") (app (lvar "x") (lvar "y")))
    @?= Set.singleton 3

unit_redexes_let_2 :: Assertion
unit_redexes_let_2 =
  redexesOf (let_ "x" (con' ["M"] "C") (lam "x" (app (lvar "x") (lvar "y"))))
    @?= Set.singleton 0

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_let_3 :: Assertion
unit_redexes_let_3 = do
  -- NB we must not say node 3 (the occurrence of the variable) is a redex
  redexesOf (lam "x" $ let_ "x" (lvar "x") (lvar "x")) @?= Set.fromList [1]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- some intervening binder.
unit_redexes_let_capture :: Assertion
unit_redexes_let_capture =
  -- We should maybe rename the lambda, and not inline the variable
  -- TODO: update https://github.com/hackworthltd/primer/issues/509
      redexesOf (let_ "x" (lvar "y") $ lam "y" $ lvar "x") @?= Set.singleton 2

unit_redexes_lettype_capture :: Assertion
unit_redexes_lettype_capture =
  -- We should rename the forall and not inline the variable
  -- TODO: update https://github.com/hackworthltd/primer/issues/509
  redexesOf (letType "x" (tvar "y") (emptyHole `ann` tforall "y" KType (tvar "x"))) @?= Set.singleton 4

unit_redexes_letrec_1 :: Assertion
unit_redexes_letrec_1 =
  redexesOf (letrec "x" (app (con' ["M"] "C") (lvar "x")) (tcon' ["M"] "T") (app (lvar "x") (lvar "y")))
    @?= Set.fromList [3, 6]

unit_redexes_letrec_2 :: Assertion
unit_redexes_letrec_2 =
  redexesOf (letrec "x" (app (con' ["M"] "C") (lvar "x")) (tcon' ["M"] "T") (lvar "y"))
    @?= Set.fromList [0, 3]

-- Test that our self-capture logic does not apply to letrec.
unit_redexes_letrec_3 :: Assertion
unit_redexes_letrec_3 =
  -- If this were a let, we would not be able to substitute, but it is possible for letrec
  redexesOf (lAM "a" $ lam "x" $ letrec "x" (lvar "x") (tvar "a") (lvar "x")) @?= Set.fromList [3, 5]

-- The application could potentially be reduced by pushing the
-- argument inside the letrec, but that is not an reduction rule. Once
-- we inline the letrec enough we would be able to see the beta.
unit_redexes_letrec_app_1 :: Assertion
unit_redexes_letrec_app_1 =
  redexesOf (app (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lam "x" (lvar "e") `ann` (tvar "a" `tfun` tcon' ["M"] "T")))
             (con' ["M"] "D"))
    @?= Set.fromList [6]

-- The application can't be reduced because variables in the argument clash with the letrec
unit_redexes_letrec_app_2 :: Assertion
unit_redexes_letrec_app_2 =
  redexesOf (let_ "e" (con' ["M"] "D") (app (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lam "x" (lvar "e"))) (lvar "e")))
    @?= Set.fromList [7, 8]
-- TODO: this ^ and probably other tests pass, but for the "wrong reason" -- I should
-- audit them and (at the least) add annotations

-- The application could potentially be reduced by pushing the
-- argument inside the letrec, but that is not an reduction rule. Once
-- we inline the letrec enough we would be able to see the beta.
unit_redexes_letrec_APP_1 :: Assertion
unit_redexes_letrec_APP_1 =
  redexesOf (aPP (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T")
                  (lAM "x" (lvar "e") `ann` tforall "a" KType (tcon' ["M"] "T"))) (tcon' ["M"] "D"))
    @?= Set.fromList [6]

unit_redexes_letrec_APP_2 :: Assertion
unit_redexes_letrec_APP_2 =
  redexesOf (letType "e" (tcon' ["M"] "D") (aPP (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lAM "x" (lvar "e"))) (tvar "e")))
    @?= Set.fromList [7, 8]

unit_redexes_lettype_1 :: Assertion
unit_redexes_lettype_1 =
  redexesOf (letType "x" (tcon' ["M"] "T") (con' ["M"] "C")) @?= Set.fromList [0]

unit_redexes_lettype_2 :: Assertion
unit_redexes_lettype_2 =
  redexesOf (letType "x" (tcon' ["M"] "T") (aPP (con' ["M"] "C") (tvar "x"))) @?= Set.fromList [4]

unit_redexes_lettype_3 :: Assertion
unit_redexes_lettype_3 =
  redexesOf (letType "x" (tcon' ["M"] "T") (letrec "y" (con' ["M"] "C") (tvar "x") (lvar "y"))) @?= Set.fromList [4, 5]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_lettype_4 :: Assertion
unit_redexes_lettype_4 = do
  -- NB we must not say node 5 (the occurrence of the variable) is a redex
  redexesOf (lAM "x" $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")) @?= Set.fromList [1]

unit_redexes_tlet_1 :: Assertion
unit_redexes_tlet_1 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tcon' ["M"] "S")) @?= Set.fromList [2]

unit_redexes_tlet_2 :: Assertion
unit_redexes_tlet_2 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tapp (tcon' ["M"] "S") (tvar "x"))) @?= Set.fromList [6]

unit_redexes_tlet_3 :: Assertion
unit_redexes_tlet_3 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tlet "y" (tcon' ["M"] "S") (tapp (tvar "x") (tvar "y")))) @?= Set.fromList [7, 8]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_tlet_4 :: Assertion
unit_redexes_tlet_4 = do
  -- NB we must not say node 5 (the occurrence of the variable) is a redex
  redexesOf (lAM "x" $ emptyHole `ann` tlet "x" (tvar "x") (tvar "x")) @?= Set.fromList [3]

unit_redexes_case_1 :: Assertion
unit_redexes_case_1 =
  redexesOf (case_ (con' ["M"] "C") [branch' (["M"], "C") [] (con' ["M"] "D")])
    @?= Set.singleton 0

-- Same as above, but the scrutinee has an annotation
unit_redexes_case_1_annotated :: Assertion
unit_redexes_case_1_annotated =
  redexesOf (case_ (ann (con' ["M"] "C") (tcon' ["M"] "C")) [branch' (["M"], "C") [] (con' ["M"] "D")])
    @?= Set.singleton 0

unit_redexes_case_2 :: Assertion
unit_redexes_case_2 =
  redexesOf (case_ (lam "x" (lvar "x")) [branch' (["M"], "C") [] (con' ["M"] "D")])
    @?= mempty

-- The case expression can be reduced, as can the variable x in the branch rhs.
unit_redexes_case_3 :: Assertion
unit_redexes_case_3 =
  redexesOf (let_ "x" (con' ["M"] "C") (case_ (con' ["M"] "C") [branch' (["M"], "C") [] (lvar "x")]))
    @?= Set.fromList [2, 4]

-- The variable x in the rhs is bound to the branch pattern, so is no longer reducible.
-- However this means the let is redundant, and can be reduced.
unit_redexes_case_4 :: Assertion
unit_redexes_case_4 =
  redexesOf (let_ "x" (con' ["M"] "C") (case_ (con' ["M"] "C") [branch' (["M"], "C") [("x", Nothing)] (lvar "x")]))
    @?= Set.fromList [0, 2]

-- If scrutinee of a case is a redex itself, we recognise that
unit_redexes_case_5 :: Assertion
unit_redexes_case_5 =
  redexesOf (let_ "x" (con' ["M"] "C") (case_ (lvar "x") [])) @?= Set.fromList [3]

-- The body of a let has the same directionality as the let itself
unit_redexes_let_upsilon :: Assertion
unit_redexes_let_upsilon = do
  let t = tforall "a" KType (tvar "a")
  redexesOf (let_ "x" (lam "x" emptyHole `ann` t) $ lam "x" emptyHole `ann` t) @?= Set.fromList [0]
  redexesOf (lam "x" $ let_ "x" (lam "x" emptyHole `ann` t) $ emptyHole `ann` t) @?= Set.fromList [1,7]
  redexesOf (letType "x" t $ lam "x" emptyHole `ann` t) @?= Set.fromList [0]
  redexesOf (lam "x" $ letType "x" t $ emptyHole `ann` t) @?= Set.fromList [1,4]
  redexesOf (letrec "x" (lam "x" emptyHole `ann` t) t $ lam "x" emptyHole `ann` t) @?= Set.fromList [0,1]
  redexesOf (lam "x" $ letrec "x" (lam "x" emptyHole `ann` t) t $ emptyHole `ann` t) @?= Set.fromList [1,2,9]

unit_redexes_prim_1 :: Assertion
unit_redexes_prim_1 =
  redexesOfWithPrims (pfun EqChar `app` char 'a' `app` char 'b') @?= Set.fromList [0]

unit_redexes_prim_2 :: Assertion
unit_redexes_prim_2 =
  redexesOfWithPrims (pfun EqChar `app` lvar "a" `app` char 'b') @?= Set.empty

unit_redexes_prim_3 :: Assertion
unit_redexes_prim_3 =
  redexesOfWithPrims (pfun EqChar `app` char 'a') @?= Set.empty

unit_redexes_prim_ann :: Assertion
unit_redexes_prim_ann =
  redexesOfWithPrims expr @?= Set.fromList [0,6]
  where
    expr =
      pfun ToUpper
        `ann` (tcon tChar `tfun` tcon tChar)
        `app` (char 'a' `ann` tcon tChar)

-- Test that handleEvalRequest will reduce imported terms
unit_eval_modules :: Assertion
unit_eval_modules =
  let test = do
        importModules [primitiveModule, builtinModule]
        foo <- pfun ToUpper `app` char 'a'
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- char 'A'
        pure $ e ~= expect
      a = newEmptyApp
   in case fst $ runAppTestMNoSevereMsgs (appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Test that handleEvalRequest will reduce case analysis on imported types
unit_eval_modules_scrutinize_imported_type :: Assertion
unit_eval_modules_scrutinize_imported_type =
  let test = do
        importModules [m]
        foo <- case_ (con cTrue) [branch cTrue [] $ con cFalse, branch cFalse [] $ con cTrue]
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- con cFalse
        pure $ e ~= expect
      a = newEmptyApp
   in case fst $ runAppTestMNoSevereMsgs (appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion
  where
    m =
      Module
        { moduleName = qualifiedModule tBool
        , moduleTypes = Map.singleton (baseName tBool) (TypeDefAST boolDef)
        , moduleDefs = mempty
        }

-- * Misc helpers

-- | Like '@?=' but specifically for expressions.
-- Ignores IDs and metadata.
(~=) :: HasCallStack => Expr -> Expr -> Assertion
x ~= y = forgetMetadata x @?= forgetMetadata y

-- | Like '~=' but for types.
(~~=) :: HasCallStack => Type -> Type -> Assertion
x ~~= y = forgetTypeMetadata x @?= forgetTypeMetadata y

runAppTestMNoSevereMsgs :: ID -> App -> AppTestM PrimerLog Assertion -> (Either ProgError Assertion, App)
runAppTestMNoSevereMsgs = Tests.Action.Prog.runAppTestMNoSevereMsgs
