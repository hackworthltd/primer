module Tests.Eval where

import Foreword

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics ((^.))
import Primer.App (
  EvalReq (EvalReq, evalReqExpr, evalReqRedex),
  EvalResp (EvalResp, evalRespExpr),
  appIdCounter,
  handleEvalRequest,
  importModules,
  newEmptyApp,
 )
import Primer.Builtins (
  boolDef,
  builtinModule,
  cFalse,
  cNil,
  cTrue,
  cZero,
  tBool,
 )
import Primer.Core (
  ASTDef (..),
  Def (..),
  DefMap,
  Expr,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind (KType),
  Type,
  TypeDef (TypeDefAST),
  getID,
  _id,
 )
import Primer.Core.DSL
import Primer.Core.Utils (forgetIDs, forgetTypeIDs)
import Primer.Eval (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  CaseReductionDetail (..),
  EvalDetail (..),
  EvalError (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LetRenameDetail (..),
  LocalLet (LLet, LLetRec, LLetType),
  LocalVarInlineDetail (..),
  Locals,
  PushAppIntoLetrecDetail (..),
  RHSCaptured (Capture, NoCapture),
  findNodeByID,
  redexes,
  singletonLocal,
  step,
  tryReduceExpr,
  tryReduceType,
 )
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Primitives (primitiveGVar, primitiveModule, tChar)
import Primer.Zipper (target)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import TestM (evalTestM)
import TestUtils (gvn, vcn, withPrimDefs)
import Tests.Action.Prog (runAppTestM)

-- * 'tryReduce' tests

-- | A helper for these tests
runTryReduce :: DefMap -> Locals -> (Expr, ID) -> Either EvalError (Expr, EvalDetail)
runTryReduce globals locals (expr, i) = evalTestM i $ runExceptT $ tryReduceExpr globals locals expr

runTryReduceType :: DefMap -> Locals -> (Type, ID) -> Either EvalError (Type, EvalDetail)
runTryReduceType globals locals (ty, i) = evalTestM i $ runExceptT $ tryReduceType globals locals ty

unit_tryReduce_no_redex :: Assertion
unit_tryReduce_no_redex = do
  runTryReduce mempty mempty (create (con cZero)) @?= Left NotRedex

unit_tryReduce_beta :: Assertion
unit_tryReduce_beta = do
  let ((lambda, body, arg, input, expectedResult), maxid) =
        create $ do
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con cZero
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
          t1 <- tcon' ["M"] "A"
          t2 <- tcon' ["M"] "B"
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con' ["M"] "C"
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
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con' ["M"] "C"
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
          e <- lam "y" (lvar "x")
          l <- lam "x" (pure e)
          a <- con' ["M"] "C"
          i <- app (app (pure l) (pure a)) (con' ["M"] "D")
          r <- app (let_ "x" (pure a) (pure e)) (con' ["M"] "D")
          pure (l, e, a, i, r)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail) -> do
      expr ~= expectedResult
      betaBefore detail ~= create' (app (lam "x" (lam "y" (lvar "x"))) (con' ["M"] "C"))
      betaAfter detail ~= create' (let_ "x" (con' ["M"] "C") (lam "y" (lvar "x")))
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
          t1 <- tcon' ["M"] "A"
          t2 <- tcon' ["M"] "B"
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con' ["M"] "C"
          i <- app (app (ann (pure l) (tfun (pure t1) (pure t2))) (pure a)) (con' ["M"] "D")
          r <- app (ann (let_ "x" (ann (pure a) (pure t1)) (pure x)) (pure t2)) (con' ["M"] "D")
          pure (l, x, a, i, r, t1, t2)
      result = runTryReduce mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{betaTypes = Just (l, r)}) -> do
      expr ~= expectedResult
      betaBefore detail ~= create' (app (ann (lam "x" (lvar "x")) (tfun (tcon' ["M"] "A") (tcon' ["M"] "B"))) (con' ["M"] "C"))
      betaAfter detail ~= create' (ann (let_ "x" (ann (con' ["M"] "C") (tcon' ["M"] "A")) (lvar "x")) (tcon' ["M"] "B"))
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
          c_ <- con' ["M"] "C"
          e <- lam "x0" (lvar "x")
          l <- lam "x" (pure e)
          a <- lvar "x"
          i <- app (pure l) (pure a)
          r <- let_ "x1" (pure a) (lam "x0" (lvar "x1"))
          pure (c_, l, e, a, i, r)
      result = runTryReduce mempty (singletonLocal "x" (0, LLet c)) (input, maxid)
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
          b <- aPP (con cNil) (tvar "x")
          l <- lAM "x" (pure b)
          a <- tcon tBool
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

unit_tryReduce_BETA_name_clash :: Assertion
unit_tryReduce_BETA_name_clash = do
  let ((c, lambda, body, arg, input, expectedResult), maxid) =
        create $ do
          c_ <- tcon' (pure "M") "T"
          e <- lam "x0" (lvar "x0" `ann` tvar "x")
          l <- lAM "x" (pure e)
          a <- tvar "x"
          i <- aPP (pure l) (pure a)
          r <- letType "x1" (pure a) (lam "x0" (lvar "x0" `ann` tvar "x1"))
          pure (c_, l, e, a, i, r)
      result = runTryReduce mempty (singletonLocal "x" (0, LLetType c)) (input, maxid)
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
  let ((expr, val), i) = create $ (,) <$> lvar "x" <*> con' ["M"] "C"
      locals = singletonLocal "x" (5, LLet val)
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
  let ((tyvar, val), i) = create $ (,) <$> tvar "x" <*> tcon' ["M"] "C"
      locals = singletonLocal "x" (5, LLetType val)
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
  let f = gvn ["M"] "f"
      ((expr, def), i) = create $ do
        g <- gvar f
        e <- lam "x" (lvar "x")
        t <- tfun (tcon' ["M"] "A") (tcon' ["M"] "B")
        pure (g, ASTDef{astDefExpr = e, astDefType = t})
      globals = Map.singleton f (DefAST def)
      result = runTryReduce globals mempty (expr, i)
      expectedResult = create' $ ann (lam "x" (lvar "x")) (tfun (tcon' ["M"] "A") (tcon' ["M"] "B"))
  case result of
    Right (expr', GlobalVarInline detail) -> do
      expr' ~= expectedResult

      globalVarInlineDef detail @?= def
      globalVarInlineVar detail @?= expr
      globalVarInlineAfter detail @?= expr'
    _ -> assertFailure $ show result

unit_tryReduce_let :: Assertion
unit_tryReduce_let = do
  let (expr, i) = create $ let_ "x" (con' ["M"] "C") (con' ["M"] "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      letRemovalBefore detail @?= expr
      letRemovalAfter detail ~= expectedResult
      letRemovalBindingName detail @?= "x"
      letRemovalLetID detail @?= 0
      letRemovalBodyID detail @?= 2
    _ -> assertFailure $ show result

-- let x = x in x ==> let y = x in y
unit_tryReduce_let_self_capture :: Assertion
unit_tryReduce_let_self_capture = do
  let (expr, i) = create $ let_ "x" (lvar "x") (lvar "x")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ let_ "x0" (lvar "x") (lvar "x0")
  case result of
    Right (expr', LetRename detail) -> do
      expr' ~= expectedResult

      letRenameBefore detail @?= expr
      letRenameAfter detail ~= expectedResult
      letRenameBindingNameOld detail @?= "x"
      letRenameBindingNameNew detail @?= "x0"
      letRenameLetID detail @?= 0
      letRenameBindingOccurrences detail @?= [1]
      letRenameBodyID detail @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_lettype :: Assertion
unit_tryReduce_lettype = do
  let (expr, i) = create $ letType "x" (tcon' ["M"] "C") (con' ["M"] "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      letRemovalBefore detail @?= expr
      letRemovalAfter detail ~= expectedResult
      letRemovalBindingName detail @?= "x"
      letRemovalLetID detail @?= 0
      letRemovalBodyID detail @?= 2
    _ -> assertFailure $ show result

-- let type x = x in _ :: x ==> let y = x in _ :: y
unit_tryReduce_lettype_self_capture :: Assertion
unit_tryReduce_lettype_self_capture = do
  let (expr, i) = create $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ letType "x0" (tvar "x") (emptyHole `ann` tvar "x0")
  case result of
    Right (expr', LetRename detail) -> do
      expr' ~= expectedResult

      letRenameBefore detail @?= expr
      letRenameAfter detail ~= expectedResult
      letRenameBindingNameOld detail @?= "x"
      letRenameBindingNameNew detail @?= "x0"
      letRenameLetID detail @?= 0
      letRenameBindingOccurrences detail @?= [1]
      letRenameBodyID detail @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_letrec :: Assertion
unit_tryReduce_letrec = do
  let (expr, i) = create $ letrec "x" (con' ["M"] "C") (tcon' ["M"] "T") (con' ["M"] "D")
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "D"
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
        arg_ <- con' ["M"] "D"
        lam_ <- lam "x" $ app (lvar "f") (lvar "x")
        lr <- letrec "f" (lam "x" (lvar "x")) (tcon' ["M"] "T") (pure lam_)
        expr_ <- app (pure lr) (pure arg_)
        pure (arg_, lam_, lr, expr_)
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ letrec "f" (lam "x" (lvar "x")) (tcon' ["M"] "T") (app (lam "x" (app (lvar "f") (lvar "x"))) (con' ["M"] "D"))
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
        arg_ <- tcon' ["M"] "B"
        lam_ <- lAM "x" $ aPP (lvar "f") (tvar "x")
        lr <- letrec "f" (lAM "x" (con' ["M"] "A")) (tcon' ["M"] "T") (pure lam_)
        expr_ <- aPP (pure lr) (pure arg_)
        pure (arg_, lam_, lr, expr_)
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ letrec "f" (lAM "x" (con' ["M"] "A")) (tcon' ["M"] "T") (aPP (lAM "x" (aPP (lvar "f") (tvar "x"))) (tcon' ["M"] "B"))
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
        d_ <- con' ["M"] "D"
        -- the application
        e <- app (letrec "f" (lam "x" (lvar "x")) (tcon' ["M"] "T") (lam "x" (app (lvar "f") (lvar "x")))) (lvar "f")
        -- the outer let
        letd_ <- let_ "f" (pure d_) (pure e)
        pure (e, d_, letd_)
      result = runTryReduce mempty (singletonLocal "f" (letd ^. _id, LLetRec d)) (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_1 :: Assertion
unit_tryReduce_case_1 = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D"), branch' (["M"], "C") [] (con' ["M"] "E")]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ con' ["M"] "E"
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 1
      caseCtorName detail @?= vcn ["M"] "C"
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
            (app (app (app (con' ["M"] "C") (lam "x" (lvar "x"))) (lvar "y")) (lvar "z"))
            [ branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing), ("d", Nothing), ("e", Nothing)] (con' ["M"] "E")
            ]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ let_ "c" (lam "x" (lvar "x")) (let_ "d" (lvar "y") (let_ "e" (lvar "z") (con' ["M"] "E")))
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 4
      caseCtorName detail @?= vcn ["M"] "C"
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
            (app (aPP (con' ["M"] "C") (tcon' ["M"] "D")) (con' ["M"] "E"))
            [ branch' (["M"], "B") [("b", Nothing)] (con' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing)] (con' ["M"] "F")
            ]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult = create' $ let_ "c" (con' ["M"] "E") (con' ["M"] "F")
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 3
      caseCtorName detail @?= vcn ["M"] "C"
      caseTargetArgIDs detail @?= [5]
      caseBranchBindingIDs detail @?= [8]
      caseBranchRhsID detail @?= 9
      caseLetIDs detail @?= [10]
    _ -> assertFailure $ show result

unit_tryReduce_case_name_clash :: Assertion
unit_tryReduce_case_name_clash = do
  let (expr, i) =
        create $
          case_
            (con' ["M"] "C" `app` emptyHole `app` lvar "x")
            [branch' (["M"], "C") [("x", Nothing), ("y", Nothing)] emptyHole]
      result = runTryReduce mempty mempty (expr, i)
      expectedResult =
        create' $
          let_ "x0" emptyHole $ let_ "y" (lvar "x") emptyHole
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      caseBefore detail ~= expr
      caseAfter detail ~= expectedResult
      caseTargetID detail @?= 1
      caseTargetCtorID detail @?= 3
      caseCtorName detail @?= vcn ["M"] "C"
      caseTargetArgIDs detail @?= [4, 5]
      caseBranchBindingIDs detail @?= [6, 7]
      caseBranchRhsID detail @?= 8
      caseLetIDs detail @?= [10, 9]
    _ -> assertFailure $ show result

unit_tryReduce_case_too_many_bindings :: Assertion
unit_tryReduce_case_too_many_bindings = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "C") [("b", Nothing)] (con' ["M"] "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch

unit_tryReduce_case_too_few_bindings :: Assertion
unit_tryReduce_case_too_few_bindings = do
  let (expr, i) = create $ case_ (app (con' ["M"] "B") (lvar "y")) [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left CaseBranchBindingLengthMismatch

unit_tryReduce_case_scrutinee_not_redex :: Assertion
unit_tryReduce_case_scrutinee_not_redex = do
  let (expr, i) = create $ case_ (lvar "x") [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_no_matching_branch :: Assertion
unit_tryReduce_case_no_matching_branch = do
  let (expr, i) = create $ case_ (con' ["M"] "C") [branch' (["M"], "B") [] (con' ["M"] "D")]
      result = runTryReduce mempty mempty (expr, i)
  result @?= Left NoMatchingCaseBranch

unit_tryReduce_prim :: Assertion
unit_tryReduce_prim = do
  let ((expr, expectedResult, globals), i) =
        create . withPrimDefs $ \m ->
          (,,)
            <$> gvar (primitiveGVar "eqChar") `app` char 'a' `app` char 'a'
            <*> con cTrue
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  case result of
    Right (expr', ApplyPrimFun detail) -> do
      expr' ~= expectedResult

      applyPrimFunBefore detail ~= expr
      applyPrimFunAfter detail ~= expr'
      applyPrimFunName detail @?= primitiveGVar "eqChar"
      applyPrimFunArgIDs detail @?= [101, 102]
    _ -> assertFailure $ show result

unit_tryReduce_prim_fail_unsaturated :: Assertion
unit_tryReduce_prim_fail_unsaturated = do
  let ((expr, globals), i) =
        create . withPrimDefs $ \m ->
          (,)
            <$> gvar (primitiveGVar "eqChar") `app` char 'a'
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_prim_fail_unreduced_args :: Assertion
unit_tryReduce_prim_fail_unreduced_args = do
  let ((expr, globals), i) =
        create . withPrimDefs $ \m ->
          (,)
            <$> gvar (primitiveGVar "eqChar") `app` char 'a' `app` (gvar (primitiveGVar "toUpper") `app` char 'a')
            <*> pure m
      result = runTryReduce (DefPrim <$> globals) mempty (expr, i)
  result @?= Left NotRedex

-- One can call the eval-step api endpoint with an expression and ID
-- of one of its nodes where that node is not a redex. This will call
-- step with such data. We should not assume that all eval api calls
-- will actually be redexes.
-- In particular here we test variable occurrences which cannot be
-- reduced by inlining a let.
unit_step_non_redex :: Assertion
unit_step_non_redex =
  let ((r1, s1), (r2, s2)) = evalTestM 0 $ do
        e1 <- let_ "x" (con' ["M"] "C") $ lam "x" $ lvar "x"
        e2 <- let_ "x" (con' ["M"] "C" `app` lvar "x") $ lvar "x"
        let i1 = 3
        let i2 = 8 -- NB: e1 has nodes 0,1,2,3; e2 has 4,5,6,7,8
        s1' <- step mempty e1 i1
        s2' <- step mempty e2 i2
        pure ((Set.member i1 $ redexes mempty e1, s1'), (Set.member i2 $ redexes mempty e2, s2'))
   in do
        assertBool "Should not be in 'redexes', as shadowed by a lambda" $ not r1
        assertBool "Should not be in 'redexes', as would self-capture" $ not r2
        case s1 of
          Left NotRedex -> pure ()
          s1' -> assertFailure $ show s1'
        case s2 of
          Left NotRedex -> pure ()
          s2' -> assertFailure $ show s2'

-- * 'findNodeByID' tests

unit_findNodeByID_letrec :: Assertion
unit_findNodeByID_letrec = do
  let expr = create' $ letrec "x" (lvar "x") (tcon' ["M"] "T") (lvar "x")
      x = create' $ lvar "x"
      t = create' $ tcon' ["M"] "T"
  case findNodeByID 0 expr of
    Just (locals, Left z) -> do
      assertBool "no locals in scope at node 0" $ Map.null locals
      target z ~= expr
    _ -> assertFailure "node 0 not found"
  case findNodeByID 1 expr of
    Just (locals, Left z) -> do
      target z ~= x
      case Map.lookup "x" locals of
        Just (0, LLetRec e, NoCapture) -> e ~= x
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
        Just (0, LLetRec e, NoCapture) -> e ~= x
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
  case findNodeByID 0 expr of
    Just (locals, Left z) -> do
      case Map.lookup "x" locals of
        Just (i, LLet e, NoCapture) -> do
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
  let (x, t, expr) = create' $ do
        -- id 0
        x_ <- tvar "x"
        -- id 1
        t_ <- tcon' ["M"] "T"
        -- id 2
        e <- letType "x" (pure t_) (ann (lvar "y") (pure x_))
        pure (x_, t_, e)
  case findNodeByID 0 expr of
    Just (locals, Right z) -> do
      case Map.lookup "x" locals of
        Just (i, LLetType e, NoCapture) -> do
          i @?= 2
          e ~~= t
        _ -> assertFailure $ show locals
      target z ~~= x
    _ -> assertFailure "node 0 not found"

unit_findNodeByID_scoping_1 :: Assertion
unit_findNodeByID_scoping_1 = do
  let expr = create' $ let_ "x" (con' ["M"] "C") $ lam "x" $ lvar "x"
  case findNodeByID 3 expr of
    Just (locals, Left _) -> assertBool "Expected 'x' not to be in scope" (Map.null locals)
    _ -> assertFailure "Expected to find the lvar 'x'"

unit_findNodeByID_scoping_2 :: Assertion
unit_findNodeByID_scoping_2 = do
  let (bind, expr) = create' $ do
        b <- con' ["M"] "D"
        e <- let_ "x" (con' ["M"] "C") $ let_ "x" (pure b) $ lvar "x"
        pure (b, e)
  case findNodeByID 4 expr of
    Just (locals, Left _)
      | Map.size locals == 1
      , Map.lookup "x" locals == Just (3, LLet bind, NoCapture) ->
          pure ()
    Just (_, Left _) -> assertFailure "Expected to have inner let binding of 'x' reported"
    _ -> assertFailure "Expected to find the lvar 'x'"

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- some intervening binder. This tests findNodeByID (and step, and thus
-- tryReduce), see unit_redexes_let_capture for a test of redexes
unit_findNodeByID_capture :: Assertion
unit_findNodeByID_capture =
  let (expr, varOcc, reduct) = create' $ do
        v <- lvar "x"
        e <- letrec "x" (lvar "y") (tcon tBool) $ lam "y" $ pure v
        let r = getID v
        s <- step mempty expr r
        pure (e, r, s)
   in do
        case findNodeByID varOcc expr of
          Just (locals, Left _)
            | Map.size locals == 1
            , Just (1, LLetRec _, Capture) <- Map.lookup "x" locals ->
                pure ()
          Just (_, Left _) -> assertFailure "Expected let binding of 'x' to be reported as captured-if-inlined"
          _ -> assertFailure "Expected to find the lvar 'x'"
        case reduct of
          Left NotRedex -> pure ()
          e -> assertFailure $ show e

unit_findNodeByID_capture_type :: Assertion
unit_findNodeByID_capture_type = do
  let (expr, varOcc, reduct) = create' $ do
        v <- tvar "x"
        e <- letType "x" (tvar "y") (emptyHole `ann` tforall "y" KType (pure v))
        let r = getID v
        s <- step mempty expr r
        pure (e, r, s)
   in do
        case findNodeByID varOcc expr of
          Just (locals, Right _)
            | Map.size locals == 1
            , Just (1, LLetType _, Capture) <- Map.lookup "x" locals ->
                pure ()
          Just (_, Right _) -> assertFailure "Expected lettype binding of 'x' to be reported as captured-if-inlined"
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
redexesOf = redexes mempty . create'

-- | A variation of 'redexesOf' for when the expression tested requires primitives to be in scope.
redexesOfWithPrims :: S Expr -> Set ID
redexesOfWithPrims x = uncurry redexes $ create' $ withPrimDefs $ \globals -> (globals,) <$> x

unit_redexes_con :: Assertion
unit_redexes_con = redexesOf (con' ["M"] "C") @?= mempty

unit_redexes_lam_1 :: Assertion
unit_redexes_lam_1 =
  redexesOf (app (lam "x" (lvar "x")) (con' ["M"] "C")) @?= Set.singleton 0

unit_redexes_lam_2 :: Assertion
unit_redexes_lam_2 =
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (con' ["M"] "C"))) @?= Set.singleton 1

unit_redexes_lam_3 :: Assertion
unit_redexes_lam_3 =
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (app (lam "z" (lvar "z")) (con' ["M"] "C"))))
    @?= Set.fromList [1, 4]

unit_redexes_lam_4 :: Assertion
unit_redexes_lam_4 =
  redexesOf (lam "y" (app (lam "x" (lvar "x")) (app (lam "z" (lvar "z")) (con' ["M"] "C"))))
    @?= Set.fromList [1, 4]

unit_redexes_LAM_1 :: Assertion
unit_redexes_LAM_1 =
  redexesOf (lAM "a" (con' ["M"] "C")) @?= mempty

unit_redexes_LAM_2 :: Assertion
unit_redexes_LAM_2 =
  redexesOf (aPP (lAM "a" (con' ["M"] "C")) (tcon' ["M"] "A")) @?= Set.fromList [0]

unit_redexes_LAM_3 :: Assertion
unit_redexes_LAM_3 =
  redexesOf (lAM "a" (aPP (lAM "b" (con' ["M"] "X")) (tcon' ["M"] "T"))) @?= Set.fromList [1]

unit_redexes_LAM_4 :: Assertion
unit_redexes_LAM_4 =
  redexesOf (let_ "x" (con' ["M"] "C") (lAM "a" (aPP (lAM "b" (lvar "x")) (tcon' ["M"] "T"))))
    @?= Set.fromList [3, 5]

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
  -- We should maybe rename the lambda, see https://github.com/hackworthltd/primer/issues/509
  assertBool "Cannot inline the variable, as would cause capture" $
    Set.null $ redexesOf (let_ "x" (lvar "y") $ lam "y" $ lvar "x")

unit_redexes_lettype_capture :: Assertion
unit_redexes_lettype_capture =
  -- We should maybe rename the forall, see https://github.com/hackworthltd/primer/issues/509
  assertBool "Cannot inline the variable, as would cause capture" $
    Set.null $ redexesOf (letType "x" (tvar "y") (emptyHole `ann` tforall "y" KType (tvar "x")))

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

-- The application can be reduced by pushing the argument inside the letrec
unit_redexes_letrec_app_1 :: Assertion
unit_redexes_letrec_app_1 =
  redexesOf (app (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lam "x" (lvar "e"))) (con' ["M"] "D"))
    @?= Set.fromList [0, 5]

-- The application can't be reduced because variables in the argument clash with the letrec
unit_redexes_letrec_app_2 :: Assertion
unit_redexes_letrec_app_2 =
  redexesOf (let_ "e" (con' ["M"] "D") (app (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lam "x" (lvar "e"))) (lvar "e")))
    @?= Set.fromList [7, 8]

unit_redexes_letrec_APP_1 :: Assertion
unit_redexes_letrec_APP_1 =
  redexesOf (aPP (letrec "e" (con' ["M"] "C") (tcon' ["M"] "T") (lAM "x" (lvar "e"))) (tcon' ["M"] "D"))
    @?= Set.fromList [0, 5]

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

unit_redexes_prim_1 :: Assertion
unit_redexes_prim_1 =
  redexesOfWithPrims (gvar (primitiveGVar "eqChar") `app` char 'a' `app` char 'b') @?= Set.fromList [98]

unit_redexes_prim_2 :: Assertion
unit_redexes_prim_2 =
  redexesOfWithPrims (gvar (primitiveGVar "eqChar") `app` lvar "a" `app` char 'b') @?= Set.empty

unit_redexes_prim_3 :: Assertion
unit_redexes_prim_3 =
  redexesOfWithPrims (gvar (primitiveGVar "eqChar") `app` char 'a') @?= Set.empty

unit_redexes_prim_ann :: Assertion
unit_redexes_prim_ann =
  redexesOfWithPrims expr @?= Set.singleton 98
  where
    expr =
      gvar (primitiveGVar "toUpper")
        `ann` (tcon tChar `tfun` tcon tChar)
          `app` (char 'a' `ann` tcon tChar)

-- Test that handleEvalRequest will reduce imported terms
unit_eval_modules :: Assertion
unit_eval_modules =
  let test = do
        importModules [primitiveModule, builtinModule]
        foo <- gvar (primitiveGVar "toUpper") `app` char 'a'
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- char 'A'
        pure $ e ~= expect
      a = newEmptyApp
   in case fst $ runAppTestM (appIdCounter a) a test of
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
   in case fst $ runAppTestM (appIdCounter a) a test of
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
x ~= y = forgetIDs x @?= forgetIDs y

-- | Like '~=' but for types.
(~~=) :: HasCallStack => Type -> Type -> Assertion
x ~~= y = forgetTypeIDs x @?= forgetTypeIDs y
