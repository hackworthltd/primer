{-# LANGUAGE OverloadedRecordDot #-}

module Tests.Eval where

import Foreword

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.List (delete)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog (annotateShow, assert, discard, failure, label, success, (===))
import Hedgehog.Gen qualified as Gen
import Optics (elemOf, (^.))
import Primer.App (
  EvalReq (EvalReq, evalReqExpr, evalReqRedex),
  EvalResp (EvalResp, evalRespExpr),
  handleEvalRequest,
  importModules,
  newEmptyApp,
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
  Kind (KFun, KType),
  LocalName,
  Pattern (PatCon, PatPrim),
  PrimCon (PrimChar),
  Type,
  Type' (TCon, TEmptyHole, TFun, TVar),
  getID,
  unLocalName,
  unsafeMkGlobalName,
  _id,
 )
import Primer.Core.DSL
import Primer.Core.Utils (exprIDs, forgetMetadata)
import Primer.Def (ASTDef (..), Def (..), DefMap)
import Primer.Eval (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  CaseReductionDetail (..),
  CaseReductionTrivialDetail (..),
  Cxt (Cxt),
  Dir (Syn),
  EvalDetail (..),
  EvalError (..),
  EvalLog,
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LocalVarInlineDetail (..),
  findNodeByID,
  getNonCapturedLocal,
  redexes,
  singletonCxt,
  step,
  tryReduceExpr,
  tryReduceType,
 )
import Primer.Gen.Core.Typed (forAllT, propertyWT)
import Primer.Log (runPureLog, runPureLogT)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes), builtinModule, moduleDefsQualified, primitiveModule)
import Primer.Primitives (PrimDef (EqChar, ToUpper), primitiveGVar, tChar)
import Primer.Primitives.DSL (pfun)
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (assertNoSevereLogs, failWhenSevereLogs, gvn, primDefs, vcn)
import Primer.TypeDef (
  ASTTypeDef (
    ASTTypeDef,
    astTypeDefConstructors,
    astTypeDefNameHints,
    astTypeDefParameters
  ),
  TypeDef (..),
  TypeDefMap,
  ValCon (ValCon),
  generateTypeDefIDs,
 )
import Primer.Typecheck (typeDefs)
import Primer.Zipper (
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  LetTypeBinding' (LetTypeBind),
  target,
 )
import Tasty (Property, withDiscards, withTests)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import Tests.Action.Prog (runAppTestM)
import Tests.Eval.Utils (genDirTm, hasTypeLets, (~=), (~~=))
import Tests.Gen.Core.Typed (checkTest)

-- * 'tryReduce' tests

-- | A helper for these tests
runTryReduce :: HasCallStack => TypeDefMap -> DefMap -> Cxt -> (Expr, ID) -> IO (Either EvalError (Expr, EvalDetail))
runTryReduce tys globals locals (expr, i) = do
  let (r, logs) = evalTestM i $ runPureLogT $ runExceptT $ tryReduceExpr @EvalLog tys globals locals Syn expr
  assertNoSevereLogs logs
  pure r

-- For use in assertions
runTryReduceType :: HasCallStack => DefMap -> Cxt -> (Type, ID) -> IO (Either EvalError (Type, EvalDetail))
runTryReduceType globals locals (ty, i) = do
  let (r, logs) = evalTestM i $ runPureLogT $ runExceptT $ tryReduceType @EvalLog globals locals ty
  assertNoSevereLogs logs
  pure r

unit_tryReduce_no_redex :: Assertion
unit_tryReduce_no_redex = do
  r <- runTryReduce mempty mempty mempty (create (con0 cZero))
  r @?= Left NotRedex

unit_tryReduce_beta :: Assertion
unit_tryReduce_beta = do
  let (input, maxid) =
        create $ do
          x <- lvar "x"
          l <- lam "x" (pure x)
          a <- con0 cZero
          app (pure l) (pure a)
  result <- runTryReduce mempty mempty mempty (input, maxid)
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
          a <- con0' ["M"] "C"
          i <- app (ann (pure l) (tfun (pure t1) (pure t2))) (pure a)
          r <- ann (let_ "x" (ann (pure a) (pure t1)) (pure x)) (pure t2)
          pure (l, x, a, i, r, t1, t2)
  result <- runTryReduce tydefs mempty mempty (input, maxid)
  case result of
    Right (expr, BetaReduction detail@BetaReductionDetail{types = (l, r)}) -> do
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
          a <- con0' ["M"] "C"
          app (ann (pure l) tEmptyHole) (pure a)
  result <- runTryReduce tydefs mempty mempty (input, maxid)
  case result of
    Left NotRedex -> pure ()
    _ -> assertFailure $ show result

unit_tryReduce_BETA :: Assertion
unit_tryReduce_BETA = do
  let ((body, lambda, arg, input, expectedResult, k, ty), maxid) =
        create $ do
          b <- con cNil []
          l <- lAM "x" (pure b)
          a <- tcon tBool
          let k_ = KFun KType KType
          ty_ <- tEmptyHole
          i <- aPP (pure l `ann` tforall "a" k_ (pure ty_)) (pure a)
          r <- letType "x" (pure a) (pure b) `ann` tlet "a" (pure a) (pure ty_)
          pure (b, l, a, i, r, k_, ty_)
  result <- runTryReduce tydefs mempty mempty (input, maxid)
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
      detail.types @?= (k, ty)
    _ -> assertFailure $ show result

unit_tryReduce_local_term_var :: Assertion
unit_tryReduce_local_term_var = do
  -- We assume we're inside a larger expression (e.g. a let) where the node that binds x has ID 5.
  let ((expr, val), i) = create $ (,) <$> lvar "x" <*> con0' ["M"] "C"
      locals = singletonCxt @ID 5 $ LetBind "x" val
  result <- runTryReduce tydefs mempty locals (expr, i)
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
      locals = singletonCxt @ID 5 $ LetTyBind $ LetTypeBind "x" val
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
      expectedResult = create' $ ann (lam "x" (lvar "x")) (tfun (tcon' ["M"] "A") (tcon' ["M"] "B"))
  result <- runTryReduce tydefs globals mempty (expr, i)
  case result of
    Right (expr', GlobalVarInline detail) -> do
      expr' ~= expectedResult

      detail.def @?= def
      detail.var @?= expr
      detail.after @?= expr'
    _ -> assertFailure $ show result

unit_tryReduce_let :: Assertion
unit_tryReduce_let = do
  let (expr, i) = create $ let_ "x" (con0' ["M"] "C") (con0' ["M"] "D")
      expectedResult = create' $ con0' ["M"] "D"
  result <- runTryReduce tydefs mempty mempty (expr, i)
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
      expectedResult = create' $ let_ "a3" (lvar "x") $ let_ "x" (lvar "a3") (lvar "x")
  result <- runTryReduce tydefs mempty mempty (expr, i)
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingNamesOld @?= ["x"]
      detail.bindingNamesNew @?= ["a3"]
      detail.bindersOld @?= [0]
      detail.bindersNew @?= [6]
      detail.bindingOccurrences @?= [1]
      detail.renamingLets @?= [4]
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_lettype :: Assertion
unit_tryReduce_lettype = do
  let (expr, i) = create $ letType "x" (tcon' ["M"] "C") (con0' ["M"] "D")
      expectedResult = create' $ con0' ["M"] "D"
  result <- runTryReduce tydefs mempty mempty (expr, i)
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
      expectedResult = create' $ letType "a5" (tvar "x") $ letType "x" (tvar "a5") (emptyHole `ann` tvar "x")
  result <- runTryReduce tydefs mempty mempty (expr, i)
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingNamesOld @?= ["x"]
      detail.bindingNamesNew @?= ["a5"]
      detail.bindersOld @?= [0]
      detail.bindersNew @?= [8]
      detail.bindingOccurrences @?= [1]
      detail.renamingLets @?= [6]
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
    Right (ty', TBindRename detail) -> do
      ty' ~~= expectedResult

      detail.before @?= ty
      detail.after ~~= expectedResult
      detail.bindingNamesOld @?= ["x"]
      detail.bindingNamesNew @?= [unLocalName n]
      detail.bindersOld @?= [0]
      detail.bindersNew @?= [6]
      detail.bindingOccurrences @?= [1]
      detail.renamingLets @?= [4]
      detail.bodyID @?= 2
    _ -> assertFailure $ show result

unit_tryReduce_letrec :: Assertion
unit_tryReduce_letrec = do
  let (expr, i) = create $ letrec "x" (con0' ["M"] "C") (tcon' ["M"] "T") (con0' ["M"] "D")
      expectedResult = create' $ con0' ["M"] "D"
  result <- runTryReduce tydefs mempty mempty (expr, i)
  case result of
    Right (expr', LetRemoval detail) -> do
      expr' ~= expectedResult

      detail.before @?= expr
      detail.after ~= expectedResult
      detail.bindingName @?= "x"
      detail.letID @?= 0
      detail.bodyID @?= 3
    _ -> assertFailure $ show result

-- case-of-constructor does not reduce if the constructor is not annotated
-- (indeed, it is not even well-typed)
unit_tryReduce_case_1 :: Assertion
unit_tryReduce_case_1 = do
  let (expr, i) = create $ case_ (con0' ["M"] "C") [branch' (["M"], "B") [("b", Nothing)] (con0' ["M"] "D"), branch' (["M"], "C") [] (con0' ["M"] "E")]
  result <- runTryReduce tydefs mempty mempty (expr, i)
  case result of
    Left NotRedex -> pure ()
    _ -> assertFailure $ show result

unit_tryReduce_case_2 :: Assertion
unit_tryReduce_case_2 = do
  let (expr, i) =
        create $
          case_
            (con' ["M"] "C" [lam "x" (lvar "x"), lvar "y", lvar "z"] `ann` tcon' ["M"] "T")
            [ branch' (["M"], "B") [("b", Nothing)] (con0' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing), ("d", Nothing), ("e", Nothing)] (con0' ["M"] "E")
            ]
      x = unsafeMkGlobalName (["M"], "X")
      y = unsafeMkGlobalName (["M"], "Y")
      z = unsafeMkGlobalName (["M"], "Z")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = []
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TCon () x, TCon () y, TCon () z]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult =
        create' $
          let_
            "c"
            (lam "x" (lvar "x") `ann` tcon x)
            ( let_
                "d"
                (lvar "y" `ann` tcon y)
                ( let_
                    "e"
                    (lvar "z" `ann` tcon z)
                    (con0' ["M"] "E")
                )
            )
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 2
      detail.ctorName @?= PatCon (vcn ["M"] "C")
      detail.targetArgIDs @?= [3, 5, 6]
      detail.branchBindingIDs @?= [10, 11, 12]
      detail.branchRhsID @?= 13
      detail.letIDs @?= [21, 18, 15]
    _ -> assertFailure $ show result

unit_tryReduce_case_3 :: Assertion
unit_tryReduce_case_3 = do
  let (expr, i) =
        create $
          case_
            ( con' ["M"] "C" [con0' ["M"] "E"]
                `ann` (tcon' ["M"] "T" `tapp` tcon' ["M"] "D")
            )
            [ branch' (["M"], "B") [("b", Nothing)] (con0' ["M"] "D")
            , branch' (["M"], "C") [("c", Nothing)] (con0' ["M"] "F")
            ]
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' $ let_ "c" (con0' ["M"] "E" `ann` tlet "a" (tcon' ["M"] "D") (tvar "a" `tfun` tvar "a")) (con0' ["M"] "F")
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 2
      detail.ctorName @?= PatCon (vcn ["M"] "C")
      detail.targetArgIDs @?= [3]
      detail.branchBindingIDs @?= [9]
      detail.branchRhsID @?= 10
      detail.letIDs @?= [16]
    _ -> assertFailure $ show result

unit_tryReduce_case_fallback_1 :: Assertion
unit_tryReduce_case_fallback_1 = do
  let (expr, i) =
        create $
          caseFB_
            ( con' ["M"] "C" [con0' ["M"] "E"]
                `ann` (tcon' ["M"] "T" `tapp` tcon' ["M"] "D")
            )
            [branch' (["M"], "B") [("b", Nothing)] (con0' ["M"] "D")]
            (con0' ["M"] "F")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' (con0' ["M"] "F")
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 2
      detail.ctorName @?= PatCon (vcn ["M"] "C")
      detail.targetArgIDs @?= [3]
      detail.branchBindingIDs @?= []
      detail.branchRhsID @?= 9
      detail.letIDs @?= []
    _ -> assertFailure $ show result

unit_tryReduce_case_fallback_2 :: Assertion
unit_tryReduce_case_fallback_2 = do
  let (expr, i) =
        create $
          caseFB_
            ( con' ["M"] "C" [con0' ["M"] "E"]
                `ann` (tcon' ["M"] "T" `tapp` tcon' ["M"] "D")
            )
            [branch' (["M"], "C") [("c", Nothing)] (con0' ["M"] "F")]
            (con0' ["M"] "D")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' $ let_ "c" (con0' ["M"] "E" `ann` tlet "a" (tcon' ["M"] "D") (tvar "a" `tfun` tvar "a")) (con0' ["M"] "F")
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 2
      detail.ctorName @?= PatCon (vcn ["M"] "C")
      detail.targetArgIDs @?= [3]
      detail.branchBindingIDs @?= [7]
      detail.branchRhsID @?= 8
      detail.letIDs @?= [15]
    _ -> assertFailure $ show result

unit_tryReduce_case_fallback_3 :: Assertion
unit_tryReduce_case_fallback_3 = do
  let (expr, i) =
        create $
          caseFB_
            ( con' ["M"] "C" [con0' ["M"] "E"]
                `ann` (tcon' ["M"] "T" `tapp` tcon' ["M"] "D")
            )
            []
            (con0' ["M"] "D")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' $ con0' ["M"] "D"
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReductionTrivial detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.branchRhsID @?= 7
    _ -> assertFailure $ show result

unit_tryReduce_case_name_clash :: Assertion
unit_tryReduce_case_name_clash = do
  let (expr, i) =
        create $
          case_
            (con' ["M"] "C" [emptyHole, lvar "x"] `ann` tcon' ["M"] "T")
            [branch' (["M"], "C") [("x", Nothing), ("y", Nothing)] emptyHole]
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = []
              , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "C")) [TEmptyHole (), TEmptyHole ()]]
              , astTypeDefNameHints = []
              }
      expectedResult =
        create' $
          case_
            (con' ["M"] "C" [emptyHole, lvar "x"] `ann` tcon' ["M"] "T")
            [branch' (["M"], "C") [("a9", Nothing), ("y", Nothing)] $ let_ "x" (lvar "a9") emptyHole]
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', BindRename detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.bindingNamesOld @?= ["x", "y"]
      detail.bindingNamesNew @?= ["a9", "y"]
      detail.bindersOld @?= [6, 7]
      detail.bindersNew @?= [6, 7]
      detail.bindingOccurrences @?= []
      detail.renamingLets @?= [10]
      detail.bodyID @?= 8
    _ -> assertFailure $ show result

unit_tryReduce_case_scrutinee_not_redex :: Assertion
unit_tryReduce_case_scrutinee_not_redex = do
  let (expr, i) = create $ case_ (lvar "x") [branch' (["M"], "B") [] (con0' ["M"] "D")]
  result <- runTryReduce tydefs mempty mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_case_prim_1 :: Assertion
unit_tryReduce_case_prim_1 = do
  let (expr, i) =
        create $
          caseFB_
            (char 'b')
            [branchPrim (PrimChar 'b') (con0' ["M"] "F")]
            (con0' ["M"] "D")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' $ con0' ["M"] "F"
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 1
      detail.ctorName @?= PatPrim (PrimChar 'b')
      detail.targetArgIDs @?= []
      detail.branchBindingIDs @?= []
      detail.branchRhsID @?= 2
      detail.letIDs @?= []
    _ -> assertFailure $ show result

unit_tryReduce_case_prim_2 :: Assertion
unit_tryReduce_case_prim_2 = do
  let (expr, i) =
        create $
          caseFB_
            (char 'b')
            [branchPrim (PrimChar 'c') (con0' ["M"] "F")] -- not b
            (con0' ["M"] "D")
      tydef =
        Map.singleton (unsafeMkGlobalName (["M"], "T")) $
          TypeDefAST $
            ASTTypeDef
              { astTypeDefParameters = [("a", KType)]
              , astTypeDefConstructors =
                  [ ValCon (unsafeMkGlobalName (["M"], "B")) [TEmptyHole ()]
                  , ValCon (unsafeMkGlobalName (["M"], "C")) [TFun () (TVar () "a") (TVar () "a")]
                  ]
              , astTypeDefNameHints = []
              }
      expectedResult = create' $ con0' ["M"] "D"
  result <- runTryReduce tydef mempty mempty (expr, i)
  case result of
    Right (expr', CaseReduction detail) -> do
      expr' ~= expectedResult

      detail.before ~= expr
      detail.after ~= expectedResult
      detail.targetID @?= 1
      detail.targetCtorID @?= 1
      detail.ctorName @?= PatPrim (PrimChar 'b')
      detail.targetArgIDs @?= []
      detail.branchBindingIDs @?= []
      detail.branchRhsID @?= 3
      detail.letIDs @?= []
    _ -> assertFailure $ show result

unit_tryReduce_prim :: Assertion
unit_tryReduce_prim = do
  let ((expr, expectedResult), i) =
        create $
          (,)
            <$> pfun EqChar
            `app` char 'a'
            `app` char 'a'
            <*> con0 cTrue
            `ann` tcon tBool
  result <- runTryReduce tydefs primDefs mempty (expr, i)
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
  result <- runTryReduce tydefs primDefs mempty (expr, i)
  result @?= Left NotRedex

unit_tryReduce_prim_fail_unreduced_args :: Assertion
unit_tryReduce_prim_fail_unreduced_args = do
  let (expr, i) =
        create $
          pfun EqChar
            `app` char 'a'
            `app` (pfun ToUpper `app` char 'a')
  result <- runTryReduce tydefs primDefs mempty (expr, i)
  result @?= Left NotRedex

runStep :: ID -> TypeDefMap -> DefMap -> (Expr, ID) -> IO (Either EvalError (Expr, EvalDetail))
runStep i' tys globals (e, i) = do
  let (r, logs) = evalTestM i' $ runPureLogT $ step @EvalLog tys globals e Syn i
  assertNoSevereLogs logs
  pure r

-- One can call the eval-step api endpoint with an expression and ID
-- of one of its nodes where that node is not a redex. This will call
-- step with such data. We should not assume that all eval api calls
-- will actually be redexes.
-- In particular here we test variable occurrences which cannot be
-- reduced by inlining a let.
unit_step_non_redex :: Assertion
unit_step_non_redex =
  let ((idX, e1, e2), maxID) = create $ do
        x <- lvar "x"
        e1' <- let_ "x" (lam "eta" $ con1' ["M"] "C" $ lvar "eta") $ lam "x" $ pure x
        e2' <- let_ "x" (con1' ["M"] "C" $ lvar "x") $ pure x
        pure (getID x, e1', e2')
   in do
        assertBool "Should not be in 'redexes', as shadowed by a lambda"
          . notElem idX
          =<< redexes' mempty mempty Syn e1
        assertBool "Should not be in 'redexes', as would self-capture"
          . notElem idX
          =<< redexes' mempty mempty Syn e2
        s1 <- runStep maxID tydefs mempty (e1, idX)
        s2 <- runStep maxID tydefs mempty (e2, idX)
        case s1 of
          Left NotRedex -> pure ()
          s1' -> assertFailure $ show s1'
        case s2 of
          Left NotRedex -> pure ()
          s2' -> assertFailure $ show s2'

-- * 'findNodeByID' tests

lookupNonCaptured :: LocalName k -> Cxt -> Maybe (ID, LetBinding)
lookupNonCaptured = runReader . runMaybeT . getNonCapturedLocal

lookupCaptured :: LocalName k -> Cxt -> Maybe (ID, LetBinding)
lookupCaptured n c@(Cxt c')
  | Nothing <- lookupNonCaptured n c
  , Just (Just r, i, _) <- Map.lookup (unLocalName n) c' =
      pure (i, r)
  | otherwise = Nothing

unit_findNodeByID_letrec :: Assertion
unit_findNodeByID_letrec = do
  let expr = create' $ letrec "x" (lvar "x") (tcon' ["M"] "T") (lvar "x")
      x = create' $ lvar "x"
      t = create' $ tcon' ["M"] "T"
  case findNodeByID 0 Syn expr of
    Just (Cxt locals, Left (_, z)) -> do
      assertBool "no locals in scope at node 0" $ Map.null locals
      target z ~= expr
    _ -> assertFailure "node 0 not found"
  case findNodeByID 1 Syn expr of
    Just (locals, Left (_, z)) -> do
      target z ~= x
      case lookupNonCaptured "x" locals of
        Just (0, LetrecBind _ e _) -> e ~= x
        r -> assertFailure $ "expected to find 'x' bound at id 0, with rhs = 'x', but found " <> show r
    _ -> assertFailure "node 1 not found"
  case findNodeByID 2 Syn expr of
    Just (Cxt locals, Right z) -> do
      target z ~~= t
      assertBool "no locals in scope at node 2" $ Map.null locals
    _ -> assertFailure "node 2 not found"
  case findNodeByID 3 Syn expr of
    Just (locals, Left (_, z)) -> do
      target z ~= x
      case lookupNonCaptured "x" locals of
        Just (0, LetrecBind _ e _) -> e ~= x
        r -> assertFailure $ "expected to find 'x' bound at id 0, with rhs = 'x', but found " <> show r
    _ -> assertFailure "node 3 not found"

unit_findNodeByID_1 :: Assertion
unit_findNodeByID_1 = do
  let (x, c, expr) = create' $ do
        -- id 0
        x_ <- lvar "x"
        -- id 1
        c_ <- con0' ["M"] "C"
        -- id 2
        e <- let_ "x" (pure c_) (pure x_)
        pure (x_, c_, e)
  case findNodeByID 0 Syn expr of
    Just (locals, Left (_, z)) -> do
      case lookupNonCaptured "x" locals of
        Just (i, LetBind _ e) -> do
          i @?= 2
          e ~= c
        Just _ -> assertFailure "expected to find 'x' let-bound, but found some other flavor of let"
        Nothing -> assertFailure "expected to find 'x' bound, but did not"
      target z ~= x
    _ -> assertFailure "node 0 not found"

  case findNodeByID 1 Syn expr of
    Just (Cxt locals, Left (_, z)) -> do
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
      case lookupNonCaptured "x" locals of
        Just (i, LetTyBind (LetTypeBind _ e)) -> do
          i @?= 2
          e ~~= t
        Just _ -> assertFailure "expected to find a type 'x' bound, but found a term"
        Nothing -> assertFailure "expected to find 'x' bound, but did not"
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
  case findNodeByID 0 Syn expr of
    Just (locals, Right z) -> do
      case lookupNonCaptured "x" locals of
        Just (i, LetTyBind (LetTypeBind _ e)) -> do
          i @?= 4
          e ~~= t
        Just _ -> assertFailure "expected to find a type 'x' bound, but found a term"
        Nothing -> assertFailure "expected to find 'x' bound, but did not"
      target z ~~= x
    _ -> assertFailure "node 0 not found"

unit_findNodeByID_scoping_1 :: Assertion
unit_findNodeByID_scoping_1 = do
  let expr = create' $ let_ "x" (con0' ["M"] "C") $ lam "x" $ lvar "x"
  case findNodeByID 3 Syn expr of
    Just (Cxt locals, Left _)
      | Just (Nothing, _, _) <- Map.lookup "x" locals ->
          pure ()
      | otherwise -> assertFailure "Expected 'x' to be in scope but not have a substitution"
    _ -> assertFailure "Expected to find the lvar 'x'"

unit_findNodeByID_scoping_2 :: Assertion
unit_findNodeByID_scoping_2 = do
  let (bind, expr) = create' $ do
        b <- con0' ["M"] "D"
        e <- let_ "x" (con0' ["M"] "C") $ let_ "x" (pure b) $ lvar "x"
        pure (b, e)
  case findNodeByID 4 Syn expr of
    Just (locals@(Cxt locals'), Left _)
      | Map.size locals' == 1
      , lookupNonCaptured "x" locals == Just (3, LetBind "x" bind) ->
          pure ()
    Just (_, Left _) -> assertFailure "Expected to have inner let binding of 'x' reported"
    _ -> assertFailure "Expected to find the lvar 'x'"

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- some intervening binder. This tests findNodeByID (and step, and thus
-- tryReduce), see unit_redexes_let_capture for a test of redexes
unit_findNodeByID_capture :: Assertion
unit_findNodeByID_capture =
  let ((expr, varOcc), maxID) = create $ do
        v <- lvar "x"
        e <- letrec "x" (lvar "y") (tcon tBool) $ lam "y" $ pure v
        pure (e, getID v)
   in do
        case findNodeByID varOcc Syn expr of
          Just (locals@(Cxt locals'), Left _)
            | Map.size locals' == 2
            , Just (1, LetrecBind{}) <- lookupCaptured "x" locals ->
                pure ()
          Just (_, Left _) -> assertFailure "Expected let binding of 'x' to be reported as captured-if-inlined"
          _ -> assertFailure "Expected to find the lvar 'x'"
        reduct <- runStep maxID mempty mempty (expr, varOcc)
        case reduct of
          Left NotRedex -> pure ()
          e -> assertFailure $ show e

unit_findNodeByID_capture_type :: Assertion
unit_findNodeByID_capture_type =
  let ((expr, varOcc), maxID) = create $ do
        v <- tvar "x"
        e <- letType "x" (tvar "y") (emptyHole `ann` tlet "z" (tvar "y") (tforall "y" KType (pure v)))
        pure (e, getID v)
   in do
        case findNodeByID varOcc Syn expr of
          Just (locals@(Cxt locals'), Right _)
            | Map.size locals' == 3
            , Just (1, LetTyBind _) <- lookupCaptured "x" locals
            , Just (5, LetTyBind _) <- lookupCaptured "z" locals ->
                pure ()
          Just (_, Right _) -> assertFailure "Expected lettype binding of 'x' and the tlet binding of 'z' to be reported as captured-if-inlined"
          _ -> assertFailure "Expected to find the lvar 'x'"
        reduct <- runStep maxID mempty mempty (expr, varOcc)
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
redexes' :: TypeDefMap -> DefMap -> Dir -> Expr -> IO (Set ID)
redexes' types prims d e = do
  let (rs, logs) = runPureLog $ redexes types prims d e
  assertNoSevereLogs @EvalLog logs
  pure $ Set.fromList rs

redexesOf :: S Expr -> IO (Set ID)
redexesOf = redexes' tydefs mempty Syn . create'

-- | A variation of 'redexesOf' for when the expression tested requires primitives to be in scope.
redexesOfWithPrims :: S Expr -> IO (Set ID)
redexesOfWithPrims = redexes' tydefs primDefs Syn . create'

tydefs :: TypeDefMap
tydefs = c <> d
  where
    c =
      Map.singleton (unsafeMkGlobalName (["M"], "C")) $
        TypeDefAST $
          ASTTypeDef
            { astTypeDefParameters = []
            , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "C")) []]
            , astTypeDefNameHints = []
            }
    d =
      Map.singleton (unsafeMkGlobalName (["M"], "D")) $
        TypeDefAST $
          ASTTypeDef
            { astTypeDefParameters = []
            , astTypeDefConstructors = [ValCon (unsafeMkGlobalName (["M"], "D")) []]
            , astTypeDefNameHints = []
            }

unit_redexes_con :: Assertion
unit_redexes_con = redexesOf (con0' ["M"] "C") <@?=> mempty

(<@?=>) :: (HasCallStack, Eq a, Show a) => IO a -> a -> Assertion
m <@?=> x = m >>= (@?= x)

-- | Helper for some tests, see 'withAnn'
noAnn :: S Expr -> S Type -> S Expr
noAnn e _ = e

-- | Helper for some tests, c.f. 'noAnn'
--
-- These are intended to be used to define some term which is parameterised
-- by "does it have annotations": @let e mkAnn = ... `mkAnn` ...@.
-- This term can then be tested with @e withAnn@ and without @e noAnn@ said annotations.
-- The aim is to make the relationship between these two tests clearer than writing out
-- the same expression twice, differing in whether annotations are included.
-- (This is mostly a concern for larger terms).
withAnn :: S Expr -> S Type -> S Expr
withAnn = ann

unit_redexes_lam_1 :: Assertion
unit_redexes_lam_1 =
  let e mkAnn =
        app
          ( lam "x" (lvar "x")
              `mkAnn` (tvar "a" `tfun` tvar "a")
          )
          (con0' ["M"] "C")
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.singleton 0

unit_redexes_lam_2 :: Assertion
unit_redexes_lam_2 =
  let e mkAnn =
        lam
          "y"
          ( app
              ( lam "x" (lvar "x")
                  `mkAnn` (tvar "a" `tfun` tvar "a")
              )
              (con0' ["M"] "C")
          )
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.singleton 1

unit_redexes_lam_3 :: Assertion
unit_redexes_lam_3 =
  let e mkAnn =
        lam
          "y"
          ( app
              (lam "x" (lvar "x") `mkAnn` (tvar "a" `tfun` tvar "a"))
              (app (lam "z" (lvar "z") `mkAnn` (tvar "a" `tfun` tvar "a")) (con0' ["M"] "C"))
          )
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.fromList [1, 8]

unit_redexes_lam_4 :: Assertion
unit_redexes_lam_4 =
  let e mkAnn =
        lam
          "y"
          ( app
              (lam "x" (lvar "x") `mkAnn` (tvar "a" `tfun` tvar "a"))
              (app (lam "z" (lvar "z") `mkAnn` (tvar "a" `tfun` tvar "a")) (con0' ["M"] "C"))
          )
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.fromList [1, 8]

unit_redexes_LAM_1 :: Assertion
unit_redexes_LAM_1 =
  redexesOf (lAM "a" (con0' ["M"] "C")) <@?=> mempty

unit_redexes_LAM_2 :: Assertion
unit_redexes_LAM_2 =
  let e mkAnn =
        aPP
          (lAM "a" (con0' ["M"] "C") `mkAnn` tforall "a" KType (tcon' ["M"] "C"))
          (tcon' ["M"] "A")
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.fromList [0]

unit_redexes_LAM_3 :: Assertion
unit_redexes_LAM_3 =
  let e mkAnn =
        lAM
          "a"
          ( aPP
              (lAM "b" (con0' ["M"] "X") `mkAnn` tforall "a" KType (tcon' ["M"] "C"))
              (tcon' ["M"] "T")
          )
   in do
        redexesOf (e noAnn) <@?=> mempty
        redexesOf (e withAnn) <@?=> Set.fromList [1]

unit_redexes_LAM_4 :: Assertion
unit_redexes_LAM_4 =
  let e mkAnn =
        let_
          "x"
          (con0' ["M"] "C")
          ( lAM
              "a"
              ( aPP
                  ( lAM "b" (lvar "x")
                      `mkAnn` tforall "a" KType (tcon' ["M"] "C")
                  )
                  (tcon' ["M"] "T")
              )
          )
   in do
        redexesOf (e noAnn) <@?=> Set.singleton 5
        redexesOf (e withAnn) <@?=> Set.fromList [3, 6]

unit_redexes_let_1 :: Assertion
unit_redexes_let_1 =
  redexesOf (let_ "x" (con0' ["M"] "C") (app (lvar "x") (lvar "y")))
    <@?=> Set.singleton 3

unit_redexes_let_2 :: Assertion
unit_redexes_let_2 =
  redexesOf (let_ "x" (con0' ["M"] "C") (lam "x" (app (lvar "x") (lvar "y"))))
    <@?=> Set.singleton 0

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_let_3 :: Assertion
unit_redexes_let_3 = do
  -- NB we must not say node 3 (the occurrence of the variable) is a redex
  redexesOf (lam "x" $ let_ "x" (lvar "x") (lvar "x")) <@?=> Set.fromList [1]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- some intervening binder.
unit_redexes_let_capture :: Assertion
unit_redexes_let_capture =
  -- We should rename the lambda, and not inline the variable
  redexesOf (let_ "x" (lvar "y") $ lam "y" $ lvar "x") <@?=> Set.singleton 2

unit_redexes_lettype_capture :: Assertion
unit_redexes_lettype_capture =
  -- We should rename the forall and not inline the variable
  redexesOf (letType "x" (tvar "y") (emptyHole `ann` tforall "y" KType (tvar "x"))) <@?=> Set.singleton 4

unit_redexes_letrec_1 :: Assertion
unit_redexes_letrec_1 =
  redexesOf (letrec "x" (con1' ["M"] "C" $ lvar "x") (tcon' ["M"] "T") (app (lvar "x") (lvar "y")))
    <@?=> Set.fromList [2, 5]

unit_redexes_letrec_2 :: Assertion
unit_redexes_letrec_2 =
  redexesOf (letrec "x" (con1' ["M"] "C" $ lvar "x") (tcon' ["M"] "T") (lvar "y"))
    <@?=> Set.fromList [0, 2]

-- Test that our self-capture logic does not apply to letrec.
unit_redexes_letrec_3 :: Assertion
unit_redexes_letrec_3 =
  -- If this were a let, we would not be able to substitute, but it is possible for letrec
  redexesOf (lAM "a" $ lam "x" $ letrec "x" (lvar "x") (tvar "a") (lvar "x")) <@?=> Set.fromList [3, 5]

-- The application could potentially be reduced by pushing the
-- argument inside the letrec, but that is not a reduction rule. Once
-- we inline the letrec enough we would be able to see the beta.
unit_redexes_letrec_app_1 :: Assertion
unit_redexes_letrec_app_1 =
  let e mkAnn =
        app
          ( letrec
              "e"
              (con0' ["M"] "C")
              (tcon' ["M"] "T")
              (mkAnn (lam "x" (lvar "e")) (tcon' ["M"] "D" `tfun` tcon' ["M"] "T"))
          )
          (con0' ["M"] "D")
   in do
        redexesOf (e noAnn) <@?=> Set.fromList [5]
        redexesOf (e withAnn) <@?=> Set.fromList [6]

-- The application could potentially be reduced by pushing the
-- argument inside the letrec, but that is not a reduction rule. Once
-- we inline the letrec enough we would be able to see the beta.
unit_redexes_letrec_APP_1 :: Assertion
unit_redexes_letrec_APP_1 =
  let e mkAnn =
        aPP
          ( letrec
              "e"
              (con0' ["M"] "C")
              (tcon' ["M"] "T")
              (lAM "x" (lvar "e") `mkAnn` tforall "a" KType (tcon' ["M"] "T"))
          )
          (tcon' ["M"] "D")
   in do
        redexesOf (e noAnn) <@?=> Set.fromList [5]
        redexesOf (e withAnn) <@?=> Set.fromList [6]

unit_redexes_lettype_1 :: Assertion
unit_redexes_lettype_1 =
  redexesOf (letType "x" (tcon' ["M"] "T") (con0' ["M"] "C")) <@?=> Set.fromList [0]

unit_redexes_lettype_2 :: Assertion
unit_redexes_lettype_2 =
  redexesOf (letType "x" (tcon' ["M"] "T") (con0' ["M"] "C" `ann` tvar "x")) <@?=> Set.fromList [4]

unit_redexes_lettype_3 :: Assertion
unit_redexes_lettype_3 =
  redexesOf (letType "x" (tcon' ["M"] "T") (letrec "y" (con0' ["M"] "C") (tvar "x") (lvar "y"))) <@?=> Set.fromList [4, 5]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_lettype_4 :: Assertion
unit_redexes_lettype_4 = do
  -- NB we must not say node 5 (the occurrence of the variable) is a redex
  redexesOf (lAM "x" $ letType "x" (tvar "x") (emptyHole `ann` tvar "x")) <@?=> Set.fromList [1]

unit_redexes_tlet_1 :: Assertion
unit_redexes_tlet_1 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tcon' ["M"] "S")) <@?=> Set.fromList [2]

unit_redexes_tlet_2 :: Assertion
unit_redexes_tlet_2 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tapp (tcon' ["M"] "S") (tvar "x"))) <@?=> Set.fromList [6]

unit_redexes_tlet_3 :: Assertion
unit_redexes_tlet_3 =
  redexesOf (emptyHole `ann` tlet "x" (tcon' ["M"] "T") (tlet "y" (tcon' ["M"] "S") (tapp (tvar "x") (tvar "y")))) <@?=> Set.fromList [7, 8]

-- We cannot substitute one occurrence of a let-bound variable if it
-- would result in capture of a free variable in the bound term by the
-- let binder itself.
unit_redexes_tlet_4 :: Assertion
unit_redexes_tlet_4 = do
  -- NB we must not say node 5 (the occurrence of the variable) is a redex
  redexesOf (lAM "x" $ emptyHole `ann` tlet "x" (tvar "x") (tvar "x")) <@?=> Set.fromList [3]

-- case-of-constructor does not reduce if the constructor is not annotated
-- (indeed, it is not even well-typed)
unit_redexes_case_1 :: Assertion
unit_redexes_case_1 =
  redexesOf (case_ (con0' ["M"] "C") [branch' (["M"], "C") [] (con0' ["M"] "D")])
    <@?=> mempty

-- Same as above, but the scrutinee has an annotation
unit_redexes_case_1_annotated :: Assertion
unit_redexes_case_1_annotated =
  redexesOf (case_ (ann (con0' ["M"] "C") (tcon' ["M"] "C")) [branch' (["M"], "C") [] (con0' ["M"] "D")])
    <@?=> Set.singleton 0

unit_redexes_case_2 :: Assertion
unit_redexes_case_2 =
  redexesOf
    ( case_
        (lam "x" (lvar "x") `ann` (tEmptyHole `tfun` tEmptyHole))
        [branch' (["M"], "C") [] (con0' ["M"] "D")]
    )
    <@?=> mempty

-- The case expression can be reduced, as can the variable x in the branch rhs.
unit_redexes_case_3 :: Assertion
unit_redexes_case_3 =
  redexesOf
    ( let_
        "x"
        (con0' ["M"] "C")
        ( case_
            (con0' ["M"] "C" `ann` tcon' ["M"] "C")
            [branch' (["M"], "C") [] (lvar "x")]
        )
    )
    <@?=> Set.fromList [2, 6]

-- The variable x in the rhs is bound to the branch pattern, so is no longer reducible.
-- However this means the let is redundant, and can be reduced.
unit_redexes_case_4 :: Assertion
unit_redexes_case_4 =
  redexesOf
    ( let_
        "x"
        (con0' ["M"] "C")
        ( case_
            (con0' ["M"] "C" `ann` tcon' ["M"] "C")
            [branch' (["M"], "C") [("x", Nothing)] (lvar "x")]
        )
    )
    <@?=> Set.fromList [0, 2]

-- If scrutinee of a case is a redex itself, we recognise that
unit_redexes_case_5 :: Assertion
unit_redexes_case_5 =
  redexesOf (let_ "x" (con0' ["M"] "C") (case_ (lvar "x") [])) <@?=> Set.fromList [3]

unit_redexes_case_fallback_1 :: Assertion
unit_redexes_case_fallback_1 =
  redexesOf (caseFB_ (ann (con0' ["M"] "C") (tcon' ["M"] "C")) [branch' (["M"], "C") [] (con0' ["M"] "D")] emptyHole)
    <@?=> Set.singleton 0

unit_redexes_case_fallback_2 :: Assertion
unit_redexes_case_fallback_2 =
  redexesOf (caseFB_ (ann (con0' ["M"] "C") (tcon' ["M"] "C")) [] emptyHole)
    <@?=> Set.singleton 0

unit_redexes_case_fallback_3 :: Assertion
unit_redexes_case_fallback_3 =
  redexesOf (caseFB_ (ann (con0' ["M"] "C") (tcon' ["M"] "C")) [] (let_ "x" emptyHole emptyHole))
    <@?=> Set.fromList [0, 4]

unit_redexes_case_prim :: Assertion
unit_redexes_case_prim = do
  redexesOf (caseFB_ (char 'b') [branchPrim (PrimChar 'b') emptyHole] (let_ "x" emptyHole emptyHole))
    <@?=> Set.fromList [0, 3]
  redexesOf (caseFB_ (char 'b') [] (let_ "x" emptyHole emptyHole))
    <@?=> Set.fromList [0, 2]

-- The body of a let has the same directionality as the let itself
unit_redexes_let_upsilon :: Assertion
unit_redexes_let_upsilon = do
  let t = tforall "a" KType (tvar "a")
  redexesOf (let_ "x" (lam "x" emptyHole `ann` t) $ lam "x" emptyHole `ann` t) <@?=> Set.fromList [0]
  redexesOf (lam "x" $ let_ "x" (lam "x" emptyHole `ann` t) $ emptyHole `ann` t) <@?=> Set.fromList [1, 7]
  redexesOf (letType "x" t $ lam "x" emptyHole `ann` t) <@?=> Set.fromList [0]
  redexesOf (lam "x" $ letType "x" t $ emptyHole `ann` t) <@?=> Set.fromList [1, 4]
  redexesOf (letrec "x" (lam "x" emptyHole `ann` t) t $ lam "x" emptyHole `ann` t) <@?=> Set.fromList [0, 1]
  redexesOf (lam "x" $ letrec "x" (lam "x" emptyHole `ann` t) t $ emptyHole `ann` t) <@?=> Set.fromList [1, 2, 9]

unit_redexes_prim_1 :: Assertion
unit_redexes_prim_1 =
  redexesOfWithPrims (pfun EqChar `app` char 'a' `app` char 'b') <@?=> Set.fromList [0]

unit_redexes_prim_2 :: Assertion
unit_redexes_prim_2 =
  redexesOfWithPrims (pfun EqChar `app` lvar "a" `app` char 'b') <@?=> Set.empty

unit_redexes_prim_3 :: Assertion
unit_redexes_prim_3 =
  redexesOfWithPrims (pfun EqChar `app` char 'a') <@?=> Set.empty

unit_redexes_prim_ann :: Assertion
unit_redexes_prim_ann =
  redexesOfWithPrims expr <@?=> Set.fromList [0, 6]
  where
    expr =
      pfun ToUpper
        `ann` (tcon tChar `tfun` tcon tChar)
        `app` (char 'a' `ann` tcon tChar)

-- Test that handleEvalRequest will reduce imported terms
unit_eval_modules :: Assertion
unit_eval_modules =
  let test = do
        builtinModule' <- builtinModule
        importModules [primitiveModule, builtinModule']
        foo <- pfun ToUpper `app` char 'a'
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- char 'A'
        pure $ e ~= expect
      a = newEmptyApp
   in runAppTestM a test <&> fst >>= \case
        Left err -> assertFailure $ show err
        Right assertion -> assertion

-- Test that handleEvalRequest will reduce case analysis on imported types
unit_eval_modules_scrutinize_imported_type :: Assertion
unit_eval_modules_scrutinize_imported_type =
  let test = do
        m' <- m
        importModules [m']
        foo <-
          case_
            (con0 cTrue `ann` tcon tBool)
            [branch cTrue [] $ con0 cFalse, branch cFalse [] $ con0 cTrue]
        EvalResp{evalRespExpr = e} <-
          handleEvalRequest
            EvalReq{evalReqExpr = foo, evalReqRedex = getID foo}
        expect <- con0 cFalse
        pure $ e ~= expect
      a = newEmptyApp
   in runAppTestM a test <&> fst >>= \case
        Left err -> assertFailure $ show err
        Right assertion -> assertion
  where
    m = do
      boolDef' <- generateTypeDefIDs $ TypeDefAST boolDef
      pure $
        Module
          { moduleName = qualifiedModule tBool
          , moduleTypes = Map.singleton (baseName tBool) boolDef'
          , moduleDefs = mempty
          }

-- | Evaluation preserves types
-- (assuming we don't end with a 'LetType' in the term, as the typechecker
-- cannot currently deal with those)
tasty_type_preservation :: Property
tasty_type_preservation =
  let testModules = [builtinModule, pure primitiveModule]
   in withTests 200 $
        withDiscards 2000 $
          propertyWT testModules $ do
            let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
            tds <- asks typeDefs
            (dir, t, ty) <- genDirTm
            rs <- failWhenSevereLogs $ redexes @EvalLog tds globs dir t
            when (null rs) discard
            r <- forAllT $ Gen.element rs
            s <- failWhenSevereLogs $ step @EvalLog tds globs t dir r
            case s of
              Left err -> annotateShow err >> failure
              Right (s', _) ->
                if hasTypeLets s'
                  then label "skipped due to LetType" >> success
                  else do
                    s'' <- checkTest ty s'
                    forgetMetadata s' === forgetMetadata s'' -- check no smart holes happened

-- | Reductions do not interfere with each other
-- if @i,j ∈ redexes e@  (and @i /= j@), and @e@ reduces to @e'@ via redex @i@
-- then @j ∈ redexes e'@,
-- unless @j@ no longer exists in @e'@ or @j@ was a rename-binding which is no longer required
tasty_redex_independent :: Property
tasty_redex_independent =
  let testModules = [builtinModule, pure primitiveModule]
   in withTests 200 $
        withDiscards 2000 $
          propertyWT testModules $ do
            let globs = foldMap' moduleDefsQualified $ create' $ sequence testModules
            tds <- asks typeDefs
            (dir, t, _) <- genDirTm
            annotateShow dir
            annotateShow t
            rs <- failWhenSevereLogs $ redexes @EvalLog tds globs dir t
            when (length rs <= 1) discard
            i <- forAllT $ Gen.element rs
            j <- forAllT $ Gen.element $ delete i rs
            s <- failWhenSevereLogs $ step @EvalLog tds globs t dir i
            case s of
              Left err -> annotateShow err >> failure
              Right (s', _) -> do
                annotateShow s'
                if elemOf exprIDs j s'
                  then do
                    sj <- failWhenSevereLogs $ step @EvalLog tds globs t dir j
                    case sj of
                      Right (_, BindRename{}) -> success
                      _ -> assert . elem j =<< failWhenSevereLogs (redexes @EvalLog tds globs dir s')
                  else success
