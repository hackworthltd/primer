{-# LANGUAGE TupleSections #-}

module Tests.Eval where

import Foreword

import Data.Map ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics (over, (^.))
import Primer.App (
  App (appIdCounter, appProg),
  EvalReq (EvalReq, evalReqExpr, evalReqRedex),
  EvalResp (EvalResp, evalRespExpr),
  Prog (progModule),
  boolDef,
  handleEvalRequest,
  importModules,
  newEmptyApp,
 )
import Primer.Core (
  ASTDef (..),
  Def (..),
  Expr,
  Expr',
  ID (ID),
  Type,
  Type',
  TypeDef (TypeDefAST),
  defID,
  getID,
  _exprMeta,
  _exprTypeMeta,
  _id,
  _typeMeta,
 )
import Primer.Core.DSL
import Primer.Eval (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  CaseReductionDetail (..),
  EvalDetail (..),
  EvalError (..),
  GlobalVarInlineDetail (..),
  Globals,
  LetRemovalDetail (..),
  LocalVarInlineDetail (..),
  Locals,
  PushAppIntoLetrecDetail (..),
  findNodeByID,
  redexes,
  tryReduceExpr,
  tryReduceType,
 )
import Primer.Module (Module (Module, moduleDefs, moduleTypes))
import Primer.Name (Name)
import Primer.Zipper (target)
import Protolude.Partial (fromJust)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import TestM (evalTestM)
import TestUtils (withPrimDefs)
import Tests.Action.Prog (defaultPrimsProg, findGlobalByName, runAppTestM)

-- * 'tryReduce' tests

-- | A helper for these tests
runTryReduce :: Globals -> Locals -> (Expr, ID) -> Either EvalError (Expr, EvalDetail)
runTryReduce globals locals (expr, i) = evalTestM i $ runExceptT $ tryReduceExpr globals locals expr

runTryReduceType :: Globals -> Locals -> (Type, ID) -> Either EvalError (Type, EvalDetail)
runTryReduceType globals locals (ty, i) = evalTestM i $ runExceptT $ tryReduceType globals locals ty

unit_tryReduce_no_redex :: Assertion
unit_tryReduce_no_redex = do
  runTryReduce mempty mempty (create (con "Zero")) @?= Left NotRedex

unit_tryReduce_beta :: Assertion
unit_tryReduce_beta = do
  let ((lambda, body, arg, input, expectedResult), maxid) =
        create $ do
          x <- var "x"
          l <- lam "x" (pure x)
          a <- con "Zero"
          i <- app (pure l) (pure a)
          r <- let_ "x" (pure a) (pure x)
          pure (l, x, a, i, r)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail) -> do
      expr ~= expectedResult
      betaBefore detail ~= input
      betaAfter detail ~= expectedResult
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaLetID detail @?= expr ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      betaTypes detail @?= Nothing
    _ -> assertFailure $ show result

unit_tryReduce_beta_annotation :: Assertion
unit_tryReduce_beta_annotation = do
  let ((lambda, body, arg, input, expectedResult, argType, resultType), maxid) =
        create $ do
          t1 <- tcon "A"
          t2 <- tcon "B"
          x <- var "x"
          l <- lam "x" (pure x)
          a <- con "C"
          i <- app (ann (pure l) (tfun (pure t1) (pure t2))) (pure a)
          r <- ann (let_ "x" (ann (pure a) (pure t1)) (pure x)) (pure t2)
          pure (l, x, a, i, r, t1, t2)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{betaTypes = Just (l, r)}) -> do
      expr ~= expectedResult
      betaBefore detail ~= input
      betaAfter detail ~= expectedResult
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      l ~~= argType
      r ~~= resultType
    _ -> assertFailure $ show result

unit_tryReduce_beta_annotation_hole :: Assertion
unit_tryReduce_beta_annotation_hole = do
  let ((lambda, body, arg, input, expectedResult, argType, resultType), maxid) =
        create $ do
          t1 <- tEmptyHole
          t2 <- tEmptyHole
          x <- var "x"
          l <- lam "x" (pure x)
          a <- con "C"
          i <- app (ann (pure l) tEmptyHole) (pure a)
          r <- hole (let_ "x" (hole (pure a)) (pure x))
          pure (l, x, a, i, r, t1, t2)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{betaTypes = Just (l, r)}) -> do
      expr ~= expectedResult
      betaBefore detail ~= input
      betaAfter detail ~= expectedResult
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      l ~~= argType
      r ~~= resultType
    _ -> assertFailure $ show result

-- ((\x. \y. x) C) D ==> (let x = C in \y. x) D
unit_tryReduce_beta_nested :: Assertion
unit_tryReduce_beta_nested = do
  let ((lambda, body, arg, input, expectedResult), maxid) =
        create $ do
          e <- lam "y" (var "x")
          l <- lam "x" (pure e)
          a <- con "C"
          i <- app (app (pure l) (pure a)) (con "D")
          r <- app (let_ "x" (pure a) (pure e)) (con "D")
          pure (l, e, a, i, r)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail) -> do
      expr ~= expectedResult
      betaBefore detail ~= fst (create (app (lam "x" (lam "y" (var "x"))) (con "C")))
      betaAfter detail ~= fst (create (let_ "x" (con "C") (lam "y" (var "x"))))
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      betaTypes detail @?= Nothing
    _ -> assertFailure $ show result

unit_tryReduce_beta_annotation_nested :: Assertion
unit_tryReduce_beta_annotation_nested = do
  let ((lambda, body, arg, input, expectedResult, argType, resultType), maxid) =
        create $ do
          t1 <- tcon "A"
          t2 <- tcon "B"
          x <- var "x"
          l <- lam "x" (pure x)
          a <- con "C"
          i <- app (app (ann (pure l) (tfun (pure t1) (pure t2))) (pure a)) (con "D")
          r <- app (ann (let_ "x" (ann (pure a) (pure t1)) (pure x)) (pure t2)) (con "D")
          pure (l, x, a, i, r, t1, t2)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{betaTypes = Just (l, r)}) -> do
      expr ~= expectedResult
      betaBefore detail ~= fst (create (app (ann (lam "x" (var "x")) (tfun (tcon "A") (tcon "B"))) (con "C")))
      betaAfter detail ~= fst (create (ann (let_ "x" (ann (con "C") (tcon "A")) (var "x")) (tcon "B")))
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      l ~~= argType
      r ~~= resultType
    _ -> assertFailure $ show result

-- This test looks at the case where we are reducing a lambda applicatin by constructing a let, and
-- the name bound by the lambda is already used in the argument of the application. We therefore
-- need to change the name of the let and then rename any references to the old name in the lambda
-- body. We also need to avoid choosing a name that would clash with any bound in the body of the
-- lambda.
--     let x = C in (\x. \x0. x) x
-- ==> let x = C in (let x1 = x in \x0. x1)   [reduce middle λ]
-- ==> let x = C in (let x1 = x in \x0. x)    [inline x1]
-- ==> let x = C in \x0. x                    [remove redundant let]
unit_tryReduce_beta_name_clash :: Assertion
unit_tryReduce_beta_name_clash = do
  let ((c, lambda, body, arg, input, expectedResult), maxid) =
        create $ do
          c_ <- con "C"
          e <- lam "x0" (var "x")
          l <- lam "x" (pure e)
          a <- var "x"
          i <- app (pure l) (pure a)
          r <- let_ "x1" (pure a) (lam "x0" (var "x1"))
          pure (c_, l, e, a, i, r)
      result = runTryReduce mempty (Map.singleton "x" (0, Left c)) (input, maxid)
  case result of
    Right (expr, BetaReduction detail) -> do
      expr ~= expectedResult
      betaBefore detail ~= input
      betaAfter detail ~= expectedResult
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaLetID detail @?= expr ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      betaTypes detail @?= Nothing
    _ -> assertFailure $ show result

unit_tryReduce_BETA :: Assertion
unit_tryReduce_BETA = do
  let ((body, lambda, arg, input, expectedResult), maxid) =
        create $ do
          b <- aPP (con "Nil") (tvar "x")
          l <- lAM "x" (pure b)
          a <- tcon "Bool"
          i <- aPP (pure l) (pure a)
          r <- letType "x" (pure a) (pure b)
          pure (b, l, a, i, r)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BETAReduction detail) -> do
      expr ~= expectedResult
      betaBefore detail ~= input
      betaAfter detail ~= expectedResult
      betaBindingName detail @?= "x"
      betaLambdaID detail @?= lambda ^. _id
      betaLetID detail @?= expr ^. _id
      betaArgID detail @?= arg ^. _id
      betaBodyID detail @?= body ^. _id
      betaTypes detail @?= Nothing
    _ -> assertFailure $ show result

unit_tryReduce_local_term_var :: Assertion
unit_tryReduce_local_term_var = do
  -- We assume we're inside a larger expression (e.g. a let) where the node that binds x has ID 5.
  let ((expr, val), i) = create $ (,) <$> var "x" <*> con "C"
      locals = Map.singleton "x" (5, Left val)
      result = runTryReduce mempty locals (expr, i)
  case result of
    Right (expr', LocalVarInline detail) -> do
      expr' ~= val

      localVarInlineLetID detail @?= 5
      localVarInlineVarID detail @?= 0
      localVarInlineBindingName detail @?= "x"
      localVarInlineValueID detail @?= 1
      localVarInlineReplacementID detail @?= 2
      localVarInlineIsTypeVar detail @?= False
    _ -> assertFailure $ show result

unit_tryReduce_local_type_var :: Assertion
unit_tryReduce_local_type_var = do
  -- We assume we're inside a larger expression (e.g. a let type) where the node that binds x has ID 5.
  let ((tyvar, val), i) = create $ (,) <$> tvar "x" <*> tcon "C"
      locals = Map.singleton "x" (5, Right val)
      result = runTryReduceType mempty locals (tyvar, i)
  case result of
    Right (ty, LocalTypeVarInline detail) -> do
      ty ~~= val

      localVarInlineLetID detail @?= 5
      localVarInlineVarID detail @?= 0
      localVarInlineBindingName detail @?= "x"
      localVarInlineValueID detail @?= 1
      localVarInlineReplacementID detail @?= 2
      localVarInlineIsTypeVar detail @?= True
    _ -> assertFailure $ show result

unit_tryReduce_global_var :: Assertion
unit_tryReduce_global_var = do
  let ((expr, def), i) = create $ do
        g <- global 10
        e <- lam "x" (var "x")
        t <- tfun (tcon "A") (tcon "B")
        pure (g, ASTDef{astDefID = 10, astDefName = "f", astDefExpr = e, astDefType = t})
      globals = Map.singleton 10 (DefAST def)
      result = runTryReduce globals mempty (expr, i)
      expectedResult = fst $ create $ ann (lam "x" (var "x")) (tfun (tcon "A") (tcon "B"))
  case result of
    Right (expr', GlobalVarInline detail) -> do
      expr' ~= expectedResult

      globalVarInlineDef detail @?= def
      globalVarInlineVar detail @?= expr
      globalVarInlineAfter detail @?= expr'
    _ -> assertFailure $ show result

unit_tryReduce_let :: Assertion
unit_tryReduce_let = do
  let (expr, i) = create $ let_ "x" (con "C") (con "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ con "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      letRemovalBefore detail @?= expr
      letRemovalAfter detail ~= expectedResult
      letRemovalBindingName detail @?= "x"
      letRemovalLetID detail @?= 0
      letRemovalBodyID detail @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_lettype :: Assertion
unit_tryReduce_lettype = do
  let (expr, i) = create $ letType "x" (tcon "C") (con "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ con "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      letRemovalBefore detail @?= expr
      letRemovalAfter detail ~= expectedResult
      letRemovalBindingName detail @?= "x"
      letRemovalLetID detail @?= 0
      letRemovalBodyID detail @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_letrec :: Assertion
unit_tryReduce_letrec = do
  let (expr, i) = create $ letrec "x" (con "C") (tcon "T") (con "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ con "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      letRemovalBefore detail @?= expr
      letRemovalAfter detail ~= expectedResult
      letRemovalBindingName detail @?= "x"
      letRemovalLetID detail @?= 0
      letRemovalBodyID detail @?= 3
    _ -> assertFailure $ show result

--     (letrec f = λx. x : T in λx. f x) D
-- ==> letrec f = λx. x : T in (λx. f x) D
unit_tryReduce_letrec_app :: Assertion
unit_tryReduce_letrec_app = do
  let ((arg, lambda, letrec_, expr), i) = create $ do
        arg_ <- con "D"
        lam_ <- lam "x" $ app (var "f") (var "x")
        lr <- letrec "f" (lam "x" (var "x")) (tcon "T") (pure lam_)
        expr_ <- app (pure lr) (pure arg_)
        pure (arg_, lam_, lr, expr_)
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ letrec "f" (lam "x" (var "x")) (tcon "T") (app (lam "x" (app (var "f") (var "x"))) (con "D"))
  case result of
    Right (expr', PushAppIntoLetrec detail) -> do
      expr' ~= expectedResult

      pushAppIntoLetrecBefore detail ~= expr
      pushAppIntoLetrecAfter detail ~= expectedResult
      pushAppIntoLetrecArgID detail @?= arg ^. _id
      pushAppIntoLetrecLamID detail @?= lambda ^. _id
      pushAppIntoLetrecLetrecID detail @?= letrec_ ^. _id
      pushAppIntoLetrecLetBindingName detail @?= "f"
      pushAppIntoLetrecIsTypeApplication detail @?= False
    _ -> assertFailure $ show result

--     (letrec f = Λx. A : T in Λx. f x) B
-- ==> letrec f = Λx. A : T in (Λx. f x) B
unit_tryReduce_letrec_APP :: Assertion
unit_tryReduce_letrec_APP = do
  let ((arg, lambda, letrec_, expr), i) = create $ do
        arg_ <- tcon "B"
        lam_ <- lAM "x" $ aPP (var "f") (tvar "x")
        lr <- letrec "f" (lAM "x" (con "A")) (tcon "T") (pure lam_)
        expr_ <- aPP (pure lr) (pure arg_)
        pure (arg_, lam_, lr, expr_)
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ letrec "f" (lAM "x" (con "A")) (tcon "T") (aPP (lAM "x" (aPP (var "f") (tvar "x"))) (tcon "B"))
  case result of
    Right (expr', PushAppIntoLetrec detail) -> do
      expr' ~= expectedResult

      pushAppIntoLetrecBefore detail ~= expr
      pushAppIntoLetrecAfter detail ~= expectedResult
      pushAppIntoLetrecArgID detail @?= arg ^. _id
      pushAppIntoLetrecLamID detail @?= lambda ^. _id
      pushAppIntoLetrecLetrecID detail @?= letrec_ ^. _id
      pushAppIntoLetrecLetBindingName detail @?= "f"
      pushAppIntoLetrecIsTypeApplication detail @?= True
    _ -> assertFailure $ show result

-- let f = D in (letrec f = λx. x : T in λx. f x) f
--                                                ^
-- doesn't reduce until this f is inlined, because it would be captured by the letrec.
unit_tryReduce_letrec_name_clash :: Assertion
unit_tryReduce_letrec_name_clash = do
  -- We construct the letrec expression, and a fake "let" expression whose ID we can insert into the
  -- locals map. This simulates focusing on the letrec inside the let expression.
  let ((expr, d, letd), i) = create $ do
        -- the value bound by the outer let
        d_ <- con "D"
        -- the application
        e <- app (letrec "f" (lam "x" (var "x")) (tcon "T") (lam "x" (app (var "f") (var "x")))) (var "f")
        -- the outer let
        letd_ <- let_ "f" (pure d_) (pure e)
        pure (e, d_, letd_)
      result = runTryReduce mempty (Map.fromList [("f", (letd ^. _id, Left d))]) (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_1 :: Assertion
unit_tryReduce_case_1 = do
  let (expr, i) = create $ case_ (con "C") [branch "B" [("b", Nothing)] (con "D"), branch "C" [] (con "E")]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ con "E"
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 1
      caseCtorName detail @?= "C"
      caseTargetArgIDs detail @?= []
      caseBranchBindingIDs detail @?= []
      caseBranchRhsID detail @?= 4
      caseLetIDs detail @?= []
    _ -> assertFailure $ show result

unit_tryReduce_case_2 :: Assertion
unit_tryReduce_case_2 = do
  let (expr, i) =
        create $
          case_
            (app (app (app (con "C") (lam "x" (var "x"))) (var "y")) (var "z"))
            [ branch "B" [("b", Nothing)] (con "D")
            , branch "C" [("c", Nothing), ("d", Nothing), ("e", Nothing)] (con "E")
            ]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ let_ "c" (lam "x" (var "x")) (let_ "d" (var "y") (let_ "e" (var "z") (con "E")))
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 4
      caseCtorName detail @?= "C"
      caseTargetArgIDs detail @?= [5, 7, 8]
      caseBranchBindingIDs detail @?= [11, 12, 13]
      caseBranchRhsID detail @?= 14
      caseLetIDs detail @?= [17, 16, 15]
    _ -> assertFailure $ show result

unit_tryReduce_case_3 :: Assertion
unit_tryReduce_case_3 = do
  let (expr, i) =
        create $
          case_
            (app (aPP (con "C") (tcon "D")) (con "E"))
            [ branch "B" [("b", Nothing)] (con "D")
            , branch "C" [("c", Nothing)] (con "F")
            ]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = fst $ create $ let_ "c" (con "E") (con "F")
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 3
      caseCtorName detail @?= "C"
      caseTargetArgIDs detail @?= [5]
      caseBranchBindingIDs detail @?= [8]
      caseBranchRhsID detail @?= 9
      caseLetIDs detail @?= [10]
    _ -> assertFailure $ show result

unit_tryReduce_case_too_many_bindings :: Assertion
unit_tryReduce_case_too_many_bindings = do
  let (expr, i) = create $ case_ (con "C") [branch "C" [("b", Nothing)] (con "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch

unit_tryReduce_case_too_few_bindings :: Assertion
unit_tryReduce_case_too_few_bindings = do
  let (expr, i) = create $ case_ (app (con "B") (var "y")) [branch "B" [] (con "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch

unit_tryReduce_case_scrutinee_not_redex :: Assertion
unit_tryReduce_case_scrutinee_not_redex = do
  let (expr, i) = create $ case_ (var "x") [branch "B" [] (con "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_no_matching_branch :: Assertion
unit_tryReduce_case_no_matching_branch = do
  let (expr, i) = create $ case_ (con "C") [branch "B" [] (con "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left NoMatchingCaseBranch

unit_tryReduce_prim :: Assertion
unit_tryReduce_prim = do
  let ((expr, expectedResult, globals), i) =
        create . withPrimDefs $ \defs m ->
          (,,)
            <$> global (defs ! "eqChar") `app` char 'a' `app` char 'a'
            <*> con "True"
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  case result of
    Right (expr', ApplyPrimFun detail) -> do
      expr' ~= expectedResult

      applyPrimFunBefore detail ~= expr
      applyPrimFunAfter detail ~= expr'
      applyPrimFunName detail @?= "eqChar"
      applyPrimFunArgIDs detail @?= [121, 122]
    _ -> assertFailure $ show result

unit_tryReduce_prim_fail_unsaturated :: Assertion
unit_tryReduce_prim_fail_unsaturated = do
  let ((expr, globals), i) =
        create . withPrimDefs $ \defs m ->
          (,)
            <$> global (defs ! "eqChar") `app` char 'a'
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_prim_fail_unreduced_args :: Assertion
unit_tryReduce_prim_fail_unreduced_args = do
  let ((expr, globals), i) =
        create . withPrimDefs $ \defs m ->
          (,)
            <$> global (defs ! "eqChar") `app` char 'a' `app` (global (defs ! "toUpper") `app` char 'a')
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  result @?= Left NotRedex

-- * 'findNodeByID' tests

unit_findNodeByID_letrec :: Assertion
unit_findNodeByID_letrec = do
  let expr = fst $ create $ letrec "x" (var "x") (tcon "T") (var "x")
      x = fst $ create $ var "x"
      t = fst $ create $ tcon "T"
  case findNodeByID 0 expr of
    Just (locals, Left z) -> do
      assertBool "no locals in scope at node 0" $ Map.null locals
      target z ~= expr
    _ -> assertFailure "node 0 not found"
  case findNodeByID 1 expr of
    Just (locals, Left z) -> do
      target z ~= x
      case Map.lookup "x" locals of
        Just (0, Left e) -> e ~= x
        _ -> assertFailure $ show locals
    _ -> assertFailure "node 1 not found"
  case findNodeByID 2 expr of
    Just (locals, Right z) -> do
      target z ~~= t
      assertBool "no locals in scope at node 2" $ Map.null locals
    _ -> assertFailure "node 2 not found"
  case findNodeByID 3 expr of
    Just (locals, Left z) -> do
      target z ~= x
      case Map.lookup "x" locals of
        Just (0, Left e) -> e ~= x
        _ -> assertFailure $ show locals
    _ -> assertFailure "node 3 not found"

unit_findNodeByID_1 :: Assertion
unit_findNodeByID_1 = do
  let (x, c, expr) = fst $
        create $ do
          -- id 0
          x_ <- var "x"
          -- id 1
          c_ <- con "C"
          -- id 2
          e <- let_ "x" (pure c_) (pure x_)
          pure (x_, c_, e)
  case findNodeByID 0 expr of
    Just (locals, Left z) -> do
      case Map.lookup "x" locals of
        Just (i, Left e) -> do
          i @?= 2
          e ~= c
        _ -> assertFailure $ show locals
      target z ~= x
    _ -> assertFailure "node 0 not found"

  case findNodeByID 1 expr of
    Just (locals, Left z) -> do
      locals @?= mempty
      target z ~= c
    _ -> assertFailure "node 1 not found"

  case findNodeByID 2 expr of
    Just (locals, Left z) -> do
      locals @?= mempty
      target z ~= expr
    _ -> assertFailure "node 2 not found"

unit_findNodeByID_2 :: Assertion
unit_findNodeByID_2 = do
  let (x, t, expr) = fst $
        create $ do
          -- id 0
          x_ <- tvar "x"
          -- id 1
          t_ <- tcon "T"
          -- id 2
          e <- letType "x" (pure t_) (ann (var "y") (pure x_))
          pure (x_, t_, e)
  case findNodeByID 0 expr of
    Just (locals, Right z) -> do
      case Map.lookup "x" locals of
        Just (i, Right e) -> do
          i @?= 2
          e ~~= t
        _ -> assertFailure $ show locals
      target z ~~= x
    _ -> assertFailure "node 0 not found"

-- * 'redexes' tests

-- In these tests we refer to node IDs in the expressions, because it's quite concise.
-- The downside is it's less obvious what each ID represents.
-- But if you read the DSL from left to right, you can work it out.
-- IDs start at 0.
-- e.g.
--
--  0        1    2        3          4    5        6         7
-- lam "y" (app (lam "x" (var "x")) (app (lam "z" (var "z")) (con "C")))

-- | A helper for these tests
redexesOf :: S Expr -> Set ID
redexesOf = redexes mempty . fst . create

-- | A variation of 'redexesOf' for when the expression tested requires primitives to be in scope.
-- Also provides a Map for looking up primitives' ids by name.
redexesOfWithPrims :: (Map Name ID -> S Expr) -> Set ID
redexesOfWithPrims x = uncurry redexes $ fst $ create $ withPrimDefs $ \defs globals -> (globals,) <$> x defs

unit_redexes_con :: Assertion
unit_redexes_con = redexesOf (con "C") @?= mempty

unit_redexes_lam_1 :: Assertion
unit_redexes_lam_1 =
  redexesOf (app (lam "x" (var "x")) (con "C")) @?= Set.singleton 0

unit_redexes_lam_2 :: Assertion
unit_redexes_lam_2 =
  redexesOf (lam "y" (app (lam "x" (var "x")) (con "C"))) @?= Set.singleton 1

unit_redexes_lam_3 :: Assertion
unit_redexes_lam_3 =
  redexesOf (lam "y" (app (lam "x" (var "x")) (app (lam "z" (var "z")) (con "C"))))
    @?= Set.fromList [1, 4]

unit_redexes_lam_4 :: Assertion
unit_redexes_lam_4 =
  redexesOf (lam "y" (app (lam "x" (var "x")) (app (lam "z" (var "z")) (con "C"))))
    @?= Set.fromList [1, 4]

unit_redexes_LAM_1 :: Assertion
unit_redexes_LAM_1 =
  redexesOf (lAM "a" (con "C")) @?= mempty

unit_redexes_LAM_2 :: Assertion
unit_redexes_LAM_2 =
  redexesOf (aPP (lAM "a" (con "C")) (tcon "A")) @?= Set.fromList [0]

unit_redexes_LAM_3 :: Assertion
unit_redexes_LAM_3 =
  redexesOf (lAM "a" (aPP (lAM "b" (con "X")) (tcon "T"))) @?= Set.fromList [1]

unit_redexes_LAM_4 :: Assertion
unit_redexes_LAM_4 =
  redexesOf (let_ "x" (con "C") (lAM "a" (aPP (lAM "b" (var "x")) (tcon "T"))))
    @?= Set.fromList [3, 5]

unit_redexes_let_1 :: Assertion
unit_redexes_let_1 =
  redexesOf (let_ "x" (con "C") (app (var "x") (var "y")))
    @?= Set.singleton 3

unit_redexes_let_2 :: Assertion
unit_redexes_let_2 =
  redexesOf (let_ "x" (con "C") (lam "x" (app (var "x") (var "y"))))
    @?= Set.singleton 0

unit_redexes_letrec_1 :: Assertion
unit_redexes_letrec_1 =
  redexesOf (letrec "x" (app (con "C") (var "x")) (tcon "T") (app (var "x") (var "y")))
    @?= Set.fromList [3, 6]

unit_redexes_letrec_2 :: Assertion
unit_redexes_letrec_2 =
  redexesOf (letrec "x" (app (con "C") (var "x")) (tcon "T") (var "y"))
    @?= Set.fromList [0, 3]

-- The application can be reduced by pushing the argument inside the letrec
unit_redexes_letrec_app_1 :: Assertion
unit_redexes_letrec_app_1 =
  redexesOf (app (letrec "e" (con "C") (tcon "T") (lam "x" (var "e"))) (con "D"))
    @?= Set.fromList [0, 5]

-- The application can't be reduced because variables in the argument clash with the letrec
unit_redexes_letrec_app_2 :: Assertion
unit_redexes_letrec_app_2 =
  redexesOf (let_ "e" (con "D") (app (letrec "e" (con "C") (tcon "T") (lam "x" (var "e"))) (var "e")))
    @?= Set.fromList [7, 8]

unit_redexes_letrec_APP_1 :: Assertion
unit_redexes_letrec_APP_1 =
  redexesOf (aPP (letrec "e" (con "C") (tcon "T") (lAM "x" (var "e"))) (tcon "D"))
    @?= Set.fromList [0, 5]

unit_redexes_letrec_APP_2 :: Assertion
unit_redexes_letrec_APP_2 =
  redexesOf (letType "e" (tcon "D") (aPP (letrec "e" (con "C") (tcon "T") (lAM "x" (var "e"))) (tvar "e")))
    @?= Set.fromList [7, 8]

unit_redexes_lettype_1 :: Assertion
unit_redexes_lettype_1 =
  redexesOf (letType "x" (tcon "T") (con "C")) @?= Set.fromList [0]

unit_redexes_lettype_2 :: Assertion
unit_redexes_lettype_2 =
  redexesOf (letType "x" (tcon "T") (aPP (con "C") (tvar "x"))) @?= Set.fromList [4]

unit_redexes_lettype_3 :: Assertion
unit_redexes_lettype_3 =
  redexesOf (letType "x" (tcon "T") (letrec "y" (con "C") (tvar "x") (var "y"))) @?= Set.fromList [4, 5]

unit_redexes_case_1 :: Assertion
unit_redexes_case_1 =
  redexesOf (case_ (con "C") [branch "C" [] (con "D")])
    @?= Set.singleton 0

-- Same as above, but the scrutinee has an annotation
unit_redexes_case_1_annotated :: Assertion
unit_redexes_case_1_annotated =
  redexesOf (case_ (ann (con "C") (tcon "C")) [branch "C" [] (con "D")])
    @?= Set.singleton 0

unit_redexes_case_2 :: Assertion
unit_redexes_case_2 =
  redexesOf (case_ (lam "x" (var "x")) [branch "C" [] (con "D")])
    @?= mempty

-- The case expression can be reduced, as can the variable x in the branch rhs.
unit_redexes_case_3 :: Assertion
unit_redexes_case_3 =
  redexesOf (let_ "x" (con "C") (case_ (con "C") [branch "C" [] (var "x")]))
    @?= Set.fromList [2, 4]

-- The variable x in the rhs is bound to the branch pattern, so is no longer reducible.
-- However this means the let is redundant, and can be reduced.
unit_redexes_case_4 :: Assertion
unit_redexes_case_4 =
  redexesOf (let_ "x" (con "C") (case_ (con "C") [branch "C" [("x", Nothing)] (var "x")]))
    @?= Set.fromList [0, 2]

-- If scrutinee of a case is a redex itself, we recognise that
unit_redexes_case_5 :: Assertion
unit_redexes_case_5 =
  redexesOf (let_ "x" (con "C") (case_ (var "x") [])) @?= Set.fromList [3]

unit_redexes_prim_1 :: Assertion
unit_redexes_prim_1 =
  redexesOfWithPrims (\defs -> global (defs ! "eqChar") `app` char 'a' `app` char 'b') @?= Set.fromList [118]

unit_redexes_prim_2 :: Assertion
unit_redexes_prim_2 =
  redexesOfWithPrims (\defs -> global (defs ! "eqChar") `app` var "a" `app` char 'b') @?= Set.empty

unit_redexes_prim_3 :: Assertion
unit_redexes_prim_3 =
  redexesOfWithPrims (\defs -> global (defs ! "eqChar") `app` char 'a') @?= Set.empty

unit_redexes_prim_ann :: Assertion
unit_redexes_prim_ann =
  redexesOfWithPrims expr @?= Set.singleton 118
  where
    expr defs =
      global (defs ! "toUpper")
        `ann` (tcon "Char" `tfun` tcon "Char")
          `app` (char 'a' `ann` tcon "Char")

-- Test that handleEvalRequest will reduce imported terms
unit_eval_modules :: Assertion
unit_eval_modules =
  let test = do
        p <- defaultPrimsProg
        importModules [progModule p]
        prog <- gets appProg
        let toUpperId = defID $ fromJust $ findGlobalByName prog "toUpper"
        foo <- global toUpperId `app` char 'a'
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- char 'A'
        pure $ e ~= expect
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Test that handleEvalRequest will reduce case analysis on imported types
unit_eval_modules_scrutinize_imported_type :: Assertion
unit_eval_modules_scrutinize_imported_type =
  let test = do
        importModules [m]
        foo <- case_ (con "True") [branch "True" [] $ con "False", branch "False" [] $ con "True"]
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- con "False"
        pure $ e ~= expect
      a = newEmptyApp
   in case fst $ runAppTestM (ID $ appIdCounter a) a test of
        Left err -> assertFailure $ show err
        Right assertion -> assertion
  where
    m =
      Module
        { moduleTypes = [TypeDefAST boolDef]
        , moduleDefs = mempty
        }

-- * Misc helpers

-- | Like '@?=' but specifically for expressions.
-- Ignores IDs and metadata.
(~=) :: Expr -> Expr -> Assertion
x ~= y = clearMeta x @?= clearMeta y
  where
    -- Clear all metadata in the given expression
    clearMeta :: Expr -> Expr' () ()
    clearMeta = over _exprMeta (const ()) . over _exprTypeMeta (const ())

-- | Like '~=' but for types.
(~~=) :: Type -> Type -> Assertion
x ~~= y = clearMeta x @?= clearMeta y
  where
    -- Clear all metadata in the given type
    clearMeta :: Type -> Type' ()
    clearMeta = over _typeMeta (const ())
