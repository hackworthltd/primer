-- | Tests for the typechecker
module Tests.Typecheck where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Hedgehog hiding (Property, Var, check, property, withDiscards, withTests)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics (over, set, (%), (%~))
import Primer.App (
  Prog (Prog, progImports, progLog, progSelection, progSmartHoles),
  appIdCounter,
  appInit,
  appNameCounter,
  appProg,
  defaultLog,
  newEmptyProg',
  newProg',
  progModules,
  tcWholeProg,
  tcWholeProgWithImports,
 )
import Primer.App qualified as App
import Primer.Builtins (
  boolDef,
  cCons,
  cFalse,
  cNil,
  cSucc,
  cTrue,
  cZero,
  eitherDef,
  listDef,
  natDef,
  tBool,
  tEither,
  tList,
  tMaybe,
  tNat,
 )
import Primer.Builtins.DSL (
  listOf,
 )
import Primer.Core (
  Expr,
  Expr' (..),
  ExprMeta,
  GlobalName (baseName),
  ID,
  Kind (KFun, KHole, KType),
  Meta (..),
  ModuleName (ModuleName),
  TmVarRef (LocalVarRef),
  TyConName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  _exprMeta,
  _exprTypeMeta,
  _type,
  _typeMeta,
 )
import Primer.Core.DSL
import Primer.Core.Utils (alphaEqTy, forgetMetadata, forgetTypeMetadata, generateIDs, generateTypeIDs)
import Primer.Def (
  ASTDef (ASTDef, astDefExpr),
  Def (..),
  defAST,
  defType,
 )
import Primer.Gen.App (genProg)
import Primer.Gen.Core.Raw (
  evalExprGen,
  genTyConName,
  genType,
 )
import Primer.Gen.Core.Typed (
  forAllT,
  genChk,
  genSyn,
  genWTType,
  propertyWT,
 )
import Primer.Module (Module (..), builtinModule, primitiveModule)
import Primer.Name (Name, NameCounter)
import Primer.Primitives (PrimDef (HexToNat), tChar)
import Primer.Primitives.DSL (pfun)
import Primer.Test.TestM (TestM, evalTestM)
import Primer.Test.Util (
  tcn,
  vcn,
  zeroIDs,
  zeroTypeIDs,
 )
import Primer.TypeDef (
  ASTTypeDef (..),
  TypeDef (..),
  ValCon (..),
  astTypeDefConstructors,
  typeDefKind,
  valConType,
 )
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt (smartHoles),
  ExprT,
  KindError (..),
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError (..),
  buildTypingContextFromModules,
  check,
  checkEverything,
  checkKind,
  decomposeTAppCon,
  exprTtoExpr,
  mkTAppCon,
  synth,
  synthKind,
  typeTtoType,
 )
import Tasty (Property, property, withDiscards, withTests)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, (@?=))
import Tests.Gen.Core.Typed

unit_identity :: Assertion
unit_identity =
  expectTyped $ ann (lam "x" (lvar "x")) (tfun (tcon tBool) (tcon tBool))

unit_undefined_variable :: Assertion
unit_undefined_variable =
  ann (lam "x" (lvar "y")) tEmptyHole `expectFailsWith` const (UnknownVariable $ LocalVarRef "y")

unit_const :: Assertion
unit_const =
  expectTyped $
    ann
      (lam "x" (lam "y" (lvar "x")))
      (tfun (tcon tBool) (tfun (tcon tBool) (tcon tBool)))

unit_true :: Assertion
unit_true = expectTyped $ con cTrue

unit_constructor_doesn't_exist :: Assertion
unit_constructor_doesn't_exist =
  con nope `expectFailsWith` const (UnknownConstructor nope)
  where
    nope = vcn ["M"] "Nope"

unit_inc :: Assertion
unit_inc =
  expectTyped $
    ann
      (lam "n" (app (con cSucc) (lvar "n")))
      (tfun (tcon tNat) (tcon tNat))

unit_compose_nat :: Assertion
unit_compose_nat =
  expectTyped $
    ann
      (lam "f" (lam "g" (app (lvar "f") (hole (lvar "g")))))
      ( tfun
          (tfun (tcon tNat) (tcon tNat))
          ( tfun
              (tfun (tcon tNat) (tcon tNat))
              (tcon tNat)
          )
      )

-- let x = True in x
unit_let :: Assertion
unit_let =
  expectTyped $ let_ "x" (con cTrue) (lvar "x")

-- Normal lets do not permit recursion
unit_recursive_let :: Assertion
unit_recursive_let =
  let_ "x" (lvar "x") (lvar "x")
    `expectFailsWith` const (UnknownVariable $ LocalVarRef "x")

-- letrec x : Bool = x in x
unit_letrec_1 :: Assertion
unit_letrec_1 =
  expectTyped $
    letrec "x" (lvar "x") (tcon tBool) (lvar "x")

-- let double : Nat -> Nat
--     double = \x -> case x of
--                 Zero -> Zero
--                 Succ n -> Succ (Succ (double n))
--  in double (Succ Zero)
unit_letrec_2 :: Assertion
unit_letrec_2 =
  expectTyped $
    letrec
      "double"
      ( lam
          "x"
          ( case_
              (lvar "x")
              [ branch cZero [] (con cZero)
              , branch
                  cSucc
                  [("n", Nothing)]
                  ( app
                      (con cSucc)
                      (app (con cSucc) (app (lvar "double") (lvar "n")))
                  )
              ]
          )
      )
      (tfun (tcon tNat) (tcon tNat))
      (app (lvar "double") (app (con cSucc) (con cZero)))

-- let x = True
--  in let y = False
--      in x
unit_nested_let :: Assertion
unit_nested_let =
  expectTyped $ let_ "x" (con cTrue) (let_ "y" (con cFalse) (lvar "x"))

-- let yes = \x -> True : Bool -> Bool
--  in let y = False
--      in yes y
unit_let_function :: Assertion
unit_let_function =
  expectTyped $
    let_
      "yes"
      (ann (lam "x" (con cTrue)) (tfun (tcon tBool) (tcon tBool)))
      (let_ "y" (con cFalse) (app (lvar "yes") (lvar "y")))

-- (\f -> f : (Bool -> Bool) -> (Bool -> Bool)) (let y = True in \x -> y)
unit_let_in_arg :: Assertion
unit_let_in_arg =
  expectTyped $
    app
      ( ann
          (lam "f" (lvar "f"))
          (tfun (tfun (tcon tBool) (tcon tBool)) (tfun (tcon tBool) (tcon tBool)))
      )
      (let_ "y" (con cTrue) (lam "x" (lvar "y")))

unit_mkTAppCon :: Assertion
unit_mkTAppCon = do
  mkTAppCon c [] @?= TCon () c
  mkTAppCon c [TCon () x] @?= TApp () (TCon () c) (TCon () x)
  mkTAppCon c [TCon () x, TCon () y] @?= TApp () (TApp () (TCon () c) (TCon () x)) (TCon () y)
  where
    c = tcn ["M1"] "C"
    x = tcn ["M2"] "X"
    y = tcn ["M2"] "Y"

-- Note [cover]
-- We disable coverage checking as it causes spurious hydra failures which are
-- annoying to track down. These are much more common than one may think, as CI
-- runs tests on a bunch of different platforms/build configurations, which
-- need to all pass. Having spurious failures runs the risk that we get used to
-- ignoring CI failures, and miss something more important later.
--
-- We should keep in mind that we should check whether these tests still have
-- decent coverage if we change the generators!
tasty_decomposeTAppCon :: Property
tasty_decomposeTAppCon = property $ do
  -- We correctly decompose "good" values
  let genArgs = Gen.list (Range.linear 0 5) $ forgetTypeMetadata <$> genType
  nargs <- forAll $ evalExprGen 0 $ liftA2 (,) genTyConName genArgs
  tripping nargs (uncurry mkTAppCon) decomposeTAppCon
  -- Also test that if we decompose, then it was "good"
  ty <- forAll $ evalExprGen 0 $ forgetTypeMetadata <$> genType
  let dec = decomposeTAppCon ty
  -- See Note [cover]
  -- cover 30 "decomposable" $ isJust dec
  -- cover 30 "non-decomposable" $ isNothing dec
  case dec of
    Nothing -> success
    Just (n, args) -> ty === mkTAppCon n args

unit_typeDefKind :: Assertion
unit_typeDefKind = do
  typeDefKind (TypeDefAST boolDef) @?= KType
  typeDefKind (TypeDefAST natDef) @?= KType
  typeDefKind (TypeDefAST listDef) @?= KFun KType KType
  typeDefKind (TypeDefAST eitherDef) @?= KFun KType (KFun KType KType)

unit_valConType :: Assertion
unit_valConType = do
  f tBool boolDef @?= [TCon () tBool, TCon () tBool]
  f tNat natDef @?= [TCon () tNat, TFun () (TCon () tNat) (TCon () tNat)]
  f tList listDef
    @?= [ TForall () "a" KType (TApp () (TCon () tList) (TVar () "a"))
        , TForall () "a" KType $ TFun () (TVar () "a") $ TFun () (TApp () (TCon () tList) (TVar () "a")) $ TApp () (TCon () tList) (TVar () "a")
        ]
  f tEither eitherDef
    @?= [ TForall () "a" KType $
            TForall () "b" KType $
              TFun () (TVar () "a") $
                mkTAppCon tEither [TVar () "a", TVar () "b"]
        , TForall () "a" KType $
            TForall () "b" KType $
              TFun () (TVar () "b") $
                mkTAppCon tEither [TVar () "a", TVar () "b"]
        ]
  where
    f tc td = map (valConType tc td) (astTypeDefConstructors td)

-- Nat -> Bool accepts \x . case x of Z -> True ; S _ -> False
unit_case_isZero :: Assertion
unit_case_isZero =
  expectTyped $
    ann (lam "x" $ case_ (lvar "x") [branch cZero [] (con cTrue), branch cSucc [("n", Nothing)] (con cFalse)]) (tfun (tcon tNat) (tcon tBool))

-- Nat -> Bool rejects \x . case x of {}
unit_case_badEmpty :: Assertion
unit_case_badEmpty =
  ann (lam "x" $ case_ (lvar "x") []) (tfun (tcon tNat) (tcon tBool))
    `expectFailsWith` const (WrongCaseBranches tNat [])

-- Cannot case on a Nat -> Nat
unit_case_badType :: Assertion
unit_case_badType =
  ann (lam "x" $ case_ (lvar "x") []) (tfun (tfun (tcon tNat) (tcon tNat)) (tcon tBool))
    `expectFailsWith` const (CannotCaseNonADT $ TFun () (TCon () tNat) (TCon () tNat))

-- Cannot annotate something with a non-existent type constructor
unit_ann_bad :: Assertion
unit_ann_bad =
  ann emptyHole (tcon nonexistant) `expectFailsWith` const (KindError $ UnknownTypeConstructor nonexistant)
  where
    nonexistant = tcn ["M"] "IDoNotExist"

unit_ann_insert :: Assertion
unit_ann_insert =
  app (lam "x" $ lvar "x") (con cZero)
    `smartSynthGives` app (ann (lam "x" $ lvar "x") tEmptyHole) (con cZero)

unit_app_not_arrow :: Assertion
unit_app_not_arrow =
  app (con cZero) (con cZero)
    `smartSynthGives` app (hole (con cZero)) (con cZero)

-- Note: there is something odd with this test, related to
-- annotations-changing-types/chk-annotations I think the correct thing to give
-- is Succ {? \x.x : ? ?}, but the hole is actually removable:
-- Succ (\x.x : ?) is fine in our system (but argueably this is a bug).
-- The smartTC currently gives an annotation inside a hole.
unit_chk_lam_not_arrow :: Assertion
unit_chk_lam_not_arrow =
  app (con cSucc) (lam "x" $ lvar "x")
    `smartSynthGives` app (con cSucc) (hole $ ann (lam "x" $ lvar "x") tEmptyHole)

unit_check_emb :: Assertion
unit_check_emb =
  app (con cSucc) (con cTrue)
    `smartSynthGives` app (con cSucc) (hole $ con cTrue)

unit_case_scrutinee :: Assertion
unit_case_scrutinee =
  ann (case_ (con cSucc) [branch' (["M"], "C") [] $ lvar "x"]) (tcon tBool)
    `smartSynthGives` ann (case_ (hole $ con cSucc) []) (tcon tBool)

unit_case_branches :: Assertion
unit_case_branches =
  ann (case_ (con cZero) [branch' (["M"], "C") [] $ lvar "x"]) (tcon tBool)
    `smartSynthGives` ann (case_ (con cZero) [branch cZero [] emptyHole, branch cSucc [("a7", Nothing)] emptyHole]) (tcon tBool) -- Fragile name here "a7"

unit_remove_hole :: Assertion
unit_remove_hole =
  ann (lam "x" $ hole (lvar "x")) (tfun (tcon tNat) (tcon tNat))
    `smartSynthGives` ann (lam "x" $ lvar "x") (tfun (tcon tNat) (tcon tNat))

-- It is not clear how to (efficiently) remove the hole in
-- {? Succ ?} Zero
-- We don't have enough information to see that it is redundant...
-- One thing we could try is: remove the hole and recheck from scratch
-- but that seems inefficient!
-- This is tracked as https://github.com/hackworthltd/primer/issues/7
unit_remove_hole_not_perfect :: Assertion
unit_remove_hole_not_perfect =
  app (hole (con cSucc)) (con cZero)
    `smartSynthGives` app (hole (con cSucc)) (con cZero) -- We currently give this as output
    -- app (con cSucc) (con cZero) -- We would prefer to see the hole removed

-- When not using "smart" TC which automatically inserts holes etc,
-- one would have to do a bit of dance to build a case expression, and
-- have lots of holes and annotations to clean up at the end.
-- Check that they are now automatically if they occur
unit_smart_remove_clean_case :: Assertion
unit_smart_remove_clean_case =
  ann
    ( lam "x" $
        hole $
          ann
            ( case_
                (lvar "x")
                [branch cTrue [] (con cZero), branch cFalse [] emptyHole]
            )
            tEmptyHole
    )
    (tfun (tcon tBool) (tcon tNat))
    `smartSynthGives` ann
      ( lam "x" $
          case_
            (lvar "x")
            [branch cTrue [] (con cZero), branch cFalse [] emptyHole]
      )
      (tfun (tcon tBool) (tcon tNat))

unit_poly :: Assertion
unit_poly =
  expectTyped $
    ann
      (lam "id" $ lAM "a" $ aPP (lvar "id") (tvar "a"))
      (tforall "c" KType (tvar "c" `tfun` tvar "c") `tfun` tforall "b" KType (tvar "b" `tfun` tvar "b"))

unit_poly_head_Nat :: Assertion
unit_poly_head_Nat =
  expectTyped $
    ann
      ( lam "x" $
          case_
            (lvar "x")
            [ branch cNil [] (con cZero)
            , branch cCons [("y", Nothing), ("ys", Nothing)] $ con cSucc `app` lvar "y"
            ]
      )
      ((tcon tList `tapp` tcon tNat) `tfun` tcon tNat)

-- ? ∋ Λa . (? : (a ?))
-- note that this requires 'a' to be higher-kinded, a : Type -> Type.
-- and thus requires that a type hole can act as a higher-kinded forall.
unit_higher_kinded_match_forall :: Assertion
unit_higher_kinded_match_forall =
  expectTyped $ lAM "a" (emptyHole `ann` (tvar "a" `tapp` tEmptyHole)) `ann` tEmptyHole

unit_type_hole_1 :: Assertion
unit_type_hole_1 = tEmptyHole `expectKinded` KHole

unit_type_hole_2 :: Assertion
unit_type_hole_2 = tapp tEmptyHole (tcon tBool) `expectKinded` KHole

unit_type_hole_3 :: Assertion
unit_type_hole_3 = tapp tEmptyHole (tcon tList) `expectKinded` KHole

unit_type_hole_4 :: Assertion
unit_type_hole_4 = tapp (tcon tMaybeT) tEmptyHole `expectKinded` KFun KType KType

unit_type_hole_5 :: Assertion
unit_type_hole_5 = tforall "a" KType tEmptyHole `expectKinded` KType

unit_type_hole_6 :: Assertion
unit_type_hole_6 = thole (tcon tBool) `expectKinded` KHole

unit_smart_type_not_arrow :: Assertion
unit_smart_type_not_arrow =
  tapp (tcon tBool) (tcon tBool)
    `smartSynthKindGives` tapp (thole $ tcon tBool) (tcon tBool)

unit_smart_type_forall :: Assertion
unit_smart_type_forall =
  tforall "a" KType (tcon tList)
    `smartSynthKindGives` tforall "a" KType (thole $ tcon tList)

unit_smart_type_not_type :: Assertion
unit_smart_type_not_type =
  listOf (tcon tList)
    `smartSynthKindGives` listOf (thole $ tcon tList)

unit_smart_type_fun :: Assertion
unit_smart_type_fun =
  tfun (tcon tList) (tcon tMaybeT)
    `smartSynthKindGives` tfun (thole $ tcon tList) (thole $ tcon tMaybeT)

unit_smart_type_inside_hole_1 :: Assertion
unit_smart_type_inside_hole_1 =
  thole (tcon tBool `tapp` tcon tMaybeT)
    `smartSynthKindGives` (thole (tcon tBool) `tapp` tcon tMaybeT)

unit_smart_type_inside_hole_2 :: Assertion
unit_smart_type_inside_hole_2 =
  thole (tcon tList `tapp` tcon tMaybeT)
    `smartSynthKindGives` (tcon tList `tapp` thole (tcon tMaybeT))

unit_smart_type_inside_hole_3 :: Assertion
unit_smart_type_inside_hole_3 =
  (tcon tList `tapp` thole (tcon tMaybeT `tapp` tcon tBool))
    `smartSynthKindGives` (tcon tList `tapp` thole (tcon tMaybeT `tapp` thole (tcon tBool)))

unit_smart_type_remove_1 :: Assertion
unit_smart_type_remove_1 =
  tapp (thole $ tcon tList) (tcon tBool)
    `smartSynthKindGives` listOf (tcon tBool)

unit_smart_type_remove_2 :: Assertion
unit_smart_type_remove_2 =
  tforall "a" KType (thole $ tcon tBool)
    `smartSynthKindGives` tforall "a" KType (tcon tBool)

unit_smart_type_remove_3 :: Assertion
unit_smart_type_remove_3 =
  listOf (thole $ tcon tBool)
    `smartSynthKindGives` listOf (tcon tBool)

unit_smart_type_remove_4 :: Assertion
unit_smart_type_remove_4 =
  tfun (thole $ tcon tBool) (thole $ tcon tNat)
    `smartSynthKindGives` tfun (tcon tBool) (tcon tNat)

unit_smart_type_remove_5 :: Assertion
unit_smart_type_remove_5 =
  thole (listOf tEmptyHole)
    `smartSynthKindGives` listOf tEmptyHole

unit_prim_char :: Assertion
unit_prim_char =
  expectTypedWithPrims $ ann (char 'a') (tcon tChar)

unit_prim_fun :: Assertion
unit_prim_fun =
  expectTypedWithPrims $ ann (pfun HexToNat) (tfun (tcon tChar) (tapp (tcon tMaybe) (tcon tNat)))

unit_prim_fun_applied :: Assertion
unit_prim_fun_applied =
  expectTypedWithPrims $ ann (app (pfun HexToNat) (char 'a')) (tapp (tcon tMaybe) (tcon tNat))

-- Whenever we synthesise a type, then it kind-checks against KType
tasty_synth_well_typed_extcxt :: Property
tasty_synth_well_typed_extcxt = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt [builtinModule, primitiveModule] $ do
      (e, _ty) <- forAllT genSyn
      ty' <- generateTypeIDs . fst =<< synthTest =<< generateIDs e
      void $ checkKindTest KType ty'

-- As tasty_synth_well_typed_extcxt, but in the empty context
-- this is in case there are problems with primitive constructors
-- (which cannot be used unless their corresponding type is in scope)
tasty_synth_well_typed_defcxt :: Property
tasty_synth_well_typed_defcxt = withTests 1000 $
  withDiscards 2000 $
    propertyWT [] $ do
      (e, _ty) <- forAllT genSyn
      ty' <- generateTypeIDs . fst =<< synthTest =<< generateIDs e
      void $ checkKindTest KType ty'

-- Regression test: when we created holes at change-of-direction when checking,
-- (i.e. when we were checking T ∋ e for some synthesisable e ∈ S with S /= T)
-- we previously only ascribed a TCSynthed rather than the TCBoth that it would
-- get next time where we check the new hole T ∋ {? e ?}
--
-- NB: typechecking a definition proceeds by TC the type giving a new
-- type via smartholes, and then TC the term against the new type. We
-- do the same here, even though it obviously won't modify the type in
-- this case.
unit_smartholes_idempotent_created_hole_typecache :: Assertion
unit_smartholes_idempotent_created_hole_typecache =
  let x = runTypecheckTestM SmartHoles $ do
        ty <- tfun (tEmptyHole `tfun` tEmptyHole) (tEmptyHole `tapp` tEmptyHole)
        e <- lam "x" $ lvar "x"
        ty' <- checkKind KType ty
        e' <- check (forgetTypeMetadata ty') e
        ty'' <- checkKind KType ty'
        e'' <- check (forgetTypeMetadata ty'') $ exprTtoExpr e'
        pure (ty, ty', ty'', e, e', e'')
   in case x of
        Left err -> assertFailure $ show err
        Right (ty, ty', ty'', _e, e', e'') -> do
          forgetKindCache ty' @?= ty
          forgetMetadata e' @?= forgetMetadata (create' $ lam "x" $ hole $ lvar "x")
          ty'' @?= ty'
          e'' @?= e'

forgetKindCache :: Type' (Meta b) -> Type
forgetKindCache = set (_typeMeta % _type) Nothing

-- Also clears the kind cache in any embedded types
forgetTypeCache :: Expr' (Meta a) (Meta b) -> Expr
forgetTypeCache = set (_exprMeta % _type) Nothing . set (_exprTypeMeta % _type) Nothing

-- Regression test: for constructions which do not fit the required type,
-- we wrap in a hole and annotation of TEmptyHole (as needed to get
-- directions to work). However, we would previously then remove the
-- hole if it gets checked again.
-- (e.g. Bool ∋ λx.x  fails and gives Bool ∋ {? λx.x : ? ?},
--       and the next iteration would think this hole is redundant,
--       and would return Bool ∋ λx.x : ?)
-- This is because holey annotations act similar to non-empty holes
-- cf https://github.com/hackworthltd/primer/issues/85.
unit_smartholes_idempotent_holey_ann :: Assertion
unit_smartholes_idempotent_holey_ann =
  let x = runTypecheckTestM SmartHoles $ do
        ty <- tcon tBool
        e <- lam "x" $ lvar "x"
        ty' <- checkKind KType ty
        e' <- check (forgetTypeMetadata ty') e
        ty'' <- checkKind KType ty'
        e'' <- check (forgetTypeMetadata ty'') $ exprTtoExpr e'
        pure (ty, ty', ty'', e, e', e'')
   in case x of
        Left err -> assertFailure $ show err
        Right (ty, ty', ty'', _e, e', e'') -> do
          forgetKindCache ty' @?= ty
          forgetMetadata e' @?= forgetMetadata (create' $ hole $ lam "x" (lvar "x") `ann` tEmptyHole)
          ty'' @?= ty'
          e'' @?= e'

-- Demonstration that smartholes is only idempotent up to alpha equality in typecaches.
-- The problem is that we check ∀a.∀b. _ ∋ Λb._ twice, and each requires a substitution
-- (∀b._)[b/a], which needs an alpha-conversion.
-- Each of these bumps the name counter, yielding different metadata!
--
-- One may think that we could roll back the name counter changes done by TC,
-- but we generate names when making case branches, and these also use the counter,
-- so we cannot roll back entirely as then we may get clashes, and we cannot
-- roll back partially since we don't have that ability
--
-- Alternatively, if we moved away from generating fresh names by using the
-- name counter we could possibly make this idempotent on the nose.
unit_smartholes_idempotent_alpha_typecache :: Assertion
unit_smartholes_idempotent_alpha_typecache =
  let x = runTypecheckTestM SmartHoles $ do
        ty <- tforall "a" KType $ tforall "foo" KType $ tvar "a" `tfun` tvar "foo"
        e <- lAM "foo" emptyHole -- Important that this is the "inner" name: i.e. must be exactly "foo" given ty
        ty' <- checkKind KType ty
        e' <- check (forgetTypeMetadata ty') e
        ty'' <- checkKind KType ty'
        e'' <- check (forgetTypeMetadata ty'') $ exprTtoExpr e'
        pure (ty, ty', ty'', e, e', e'')
   in case x of
        Left err -> assertFailure $ show err
        Right (ty, ty', ty'', e, e', e'') -> do
          forgetKindCache ty' @?= ty
          forgetTypeCache e' @?= e
          ty'' @?= ty'
          assertBool "Typecache is only idempotent up to alpha" (e'' /= e')
          TypeCacheAlpha e'' @?= TypeCacheAlpha e'

-- A helper type for smartholes idempotent tests
-- Equality is as normal, except in the typecache, where it is up-to-alpha
newtype TypeCacheAlpha a = TypeCacheAlpha {unTypeCacheAlpha :: a}
  deriving stock (Show)
instance Eq (TypeCacheAlpha TypeCache) where
  TypeCacheAlpha (TCSynthed s) == TypeCacheAlpha (TCSynthed t) =
    s `alphaEqTy` t
  TypeCacheAlpha (TCChkedAt s) == TypeCacheAlpha (TCChkedAt t) =
    s `alphaEqTy` t
  TypeCacheAlpha (TCEmb (TCBoth s t)) == TypeCacheAlpha (TCEmb (TCBoth s' t')) =
    s `alphaEqTy` s' && t `alphaEqTy` t'
  _ == _ = False
tcaFunctorial :: (Functor f, Eq (f (TypeCacheAlpha a))) => TypeCacheAlpha (f a) -> TypeCacheAlpha (f a) -> Bool
tcaFunctorial = (==) `on` (fmap TypeCacheAlpha . unTypeCacheAlpha)
instance Eq (TypeCacheAlpha a) => Eq (TypeCacheAlpha (Maybe a)) where
  (==) = tcaFunctorial
instance (Eq (TypeCacheAlpha a), Eq b) => Eq (TypeCacheAlpha (Expr' (Meta a) b)) where
  (==) = (==) `on` (((_exprMeta % _type) %~ TypeCacheAlpha) . unTypeCacheAlpha)
instance Eq (TypeCacheAlpha Def) where
  TypeCacheAlpha (DefAST (ASTDef e1 t1)) == TypeCacheAlpha (DefAST (ASTDef e2 t2)) =
    TypeCacheAlpha e1 == TypeCacheAlpha e2 && t1 == t2
  TypeCacheAlpha (DefPrim p1) == TypeCacheAlpha (DefPrim p2) =
    p1 == p2
  _ == _ = False
instance Eq (TypeCacheAlpha (Map Name Def)) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha Module) where
  TypeCacheAlpha (Module n1 tds1 ds1) == TypeCacheAlpha (Module n2 tds2 ds2) =
    n1 == n2 && tds1 == tds2 && TypeCacheAlpha ds1 == TypeCacheAlpha ds2
instance Eq (TypeCacheAlpha [Module]) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha ExprMeta) where
  (==) = tcaFunctorial
instance Eq (TypeCacheAlpha App.NodeSelection) where
  TypeCacheAlpha (App.NodeSelection t1 m1) == TypeCacheAlpha (App.NodeSelection t2 m2) =
    t1 == t2 && ((==) `on` first TypeCacheAlpha) m1 m2
instance Eq (TypeCacheAlpha App.Selection) where
  TypeCacheAlpha (App.Selection d1 n1) == TypeCacheAlpha (App.Selection d2 n2) =
    d1 == d2 && TypeCacheAlpha n1 == TypeCacheAlpha n2
instance Eq (TypeCacheAlpha Prog) where
  TypeCacheAlpha (Prog i1 m1 s1 sh1 l1) == TypeCacheAlpha (Prog i2 m2 s2 sh2 l2) =
    TypeCacheAlpha i1 == TypeCacheAlpha i2
      && TypeCacheAlpha m1 == TypeCacheAlpha m2
      && TypeCacheAlpha s1 == TypeCacheAlpha s2
      && sh1 == sh2
      && l1 == l2
instance Eq (TypeCacheAlpha App.App) where
  TypeCacheAlpha a1 == TypeCacheAlpha a2 =
    appInit a1 == appInit a2
      && appIdCounter a1 == appIdCounter a2
      && appNameCounter a1 == appNameCounter a2
      && TypeCacheAlpha (appProg a1) == TypeCacheAlpha (appProg a2)

-- Test that smartholes is idempotent (for well-typed input)
tasty_smartholes_idempotent_syn :: Property
tasty_smartholes_idempotent_syn = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt [builtinModule, primitiveModule] $ do
      local (\c -> c{smartHoles = SmartHoles}) $ do
        (e, _ty) <- forAllT genSyn
        (ty', e') <- synthTest =<< generateIDs e
        (ty'', e'') <- synthTest $ exprTtoExpr e'
        ty' === ty''
        TypeCacheAlpha e' === TypeCacheAlpha e''

-- Test that smartholes is idempotent (for well-typed input)
-- This also shows that checkKind is idempotent-on-the-nose
tasty_smartholes_idempotent_chk :: Property
tasty_smartholes_idempotent_chk = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt [builtinModule, primitiveModule] $
      local (\c -> c{smartHoles = SmartHoles}) $ do
        ty <- forAllT $ genWTType KType
        e <- forAllT $ genChk ty
        tyI <- generateTypeIDs ty
        ty' <- checkKindTest KType tyI
        -- Note that ty /= ty' in general, as the generators can create a THole _ (TEmptyHole _)
        annotateShow ty'
        e' <- checkTest (forgetTypeMetadata ty') =<< generateIDs e
        annotateShow e'
        ty'' <- checkKindTest KType $ typeTtoType ty'
        annotateShow ty''
        e'' <- checkTest (forgetTypeMetadata ty'') $ exprTtoExpr e'
        annotateShow e''
        ty' === ty''
        TypeCacheAlpha e' === TypeCacheAlpha e''

-- We must ensure that when we check a program with smartholes that
-- any updates to any types are taken into account when checking any
-- terms that may depend on them (e.g. within a recursive group of
-- definitions)
-- Thus if we define
--   foo :: {? ∀a.a ?} ; foo = _
--   bar :: Bool ; bar = foo
-- then normalising should give
--   foo :: ∀a.a ; foo = _
--   bar :: Bool ; bar = {? foo ?}
unit_tcWholeProg_notice_type_updates :: Assertion
unit_tcWholeProg_notice_type_updates =
  let mkDefs e' t' =
        (\ef tf eb tb -> Map.fromList [("foo", DefAST $ ASTDef ef tf), ("bar", DefAST $ ASTDef eb tb)])
          <$> emptyHole
          <*> t'
          <*> e'
          <*> tcon tBool
      d0 = create' $ mkDefs (gvar' ["M"] "foo") (thole $ tforall "a" KType $ tvar "a")
      d1 = create' $ mkDefs (hole $ gvar' ["M"] "foo") (tforall "a" KType $ tvar "a")
      mkProg ds =
        Prog
          { progImports = [builtinModule]
          , progModules = [Module (ModuleName ["M"]) mempty ds]
          , progSmartHoles = SmartHoles
          , progSelection = Nothing
          , progLog = defaultLog
          }
      a0 = mkProg d0
      a1 = mkProg d1
      a1' = evalTestM 0 $ runExceptT @TypeError $ tcWholeProg a0
      defsNoIDs a = foldMap' (fmap (\d -> (forgetTypeMetadata $ defType d, forgetMetadata . astDefExpr <$> defAST d)) . Map.elems . moduleDefs) $ progModules a
   in do
        fmap defsNoIDs a1' @?= Right (defsNoIDs a1)

-- This is only up to alpha in the TypeCaches, for the same reasons as
-- unit_smartholes_idempotent_alpha_typecache
tasty_tcWholeProg_idempotent :: Property
tasty_tcWholeProg_idempotent = withTests 500 $
  withDiscards 2000 $
    propertyWT [] $ do
      base <- forAllT $ Gen.element [[], [builtinModule], [builtinModule, primitiveModule]]
      p <- forAllT $ genProg SmartHoles base
      case runTypecheckTestM SmartHoles $ do
        p' <- tcWholeProgWithImports p
        p'' <- tcWholeProgWithImports p'
        pure (p', p'') of
        Left err -> annotateShow err >> failure
        Right (p', p'') -> TypeCacheAlpha p' === TypeCacheAlpha p''

-- Check that all our builtins are well formed
-- (these are used to seed initial programs)
checkProgWellFormed :: HasCallStack => (forall m. MonadFresh ID m => m Prog) -> Assertion
checkProgWellFormed p' = case runTypecheckTestM NoSmartHoles $ do
  p <- p'
  App.checkProgWellFormed p of
  Left err -> assertFailure $ show err
  Right _ -> pure ()

unit_good_defaults :: Assertion
unit_good_defaults = do
  checkProgWellFormed $ pure newEmptyProg'
  checkProgWellFormed $ pure newProg'

-- Check that our higher-order test typedef is well formed
unit_good_maybeT :: Assertion
unit_good_maybeT = case runTypecheckTestM NoSmartHoles $
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = [builtinModule]
      , toCheck = [Module (ModuleName ["TestModule"]) (Map.singleton (baseName tMaybeT) (TypeDefAST maybeTDef)) mempty]
      } of
  Left err -> assertFailure $ show err
  Right _ -> pure ()

-- * Helpers
expectTyped :: HasCallStack => TypecheckTestM Expr -> Assertion
expectTyped m =
  case runTypecheckTestM NoSmartHoles (m >>= synth) of
    Left err -> assertFailure $ show err
    Right _ -> pure ()
expectTypedWithPrims :: HasCallStack => TypecheckTestM Expr -> Assertion
expectTypedWithPrims m =
  case runTypecheckTestMWithPrims NoSmartHoles (m >>= synth) of
    Left err -> assertFailure $ show err
    Right _ -> pure ()

expectKinded :: HasCallStack => TypecheckTestM Type -> Kind -> Assertion
expectKinded m k =
  case runTypecheckTestM NoSmartHoles (m >>= synthKind) of
    Left err -> assertFailure $ show err
    Right (k', _) -> k' @?= k

expectFailsWith :: HasCallStack => TypecheckTestM Expr -> (Expr -> TypeError) -> Assertion
expectFailsWith m err = do
  expr <- case runTypecheckTestM NoSmartHoles m of
    Left constructionErr -> assertFailure $ show constructionErr
    Right expr' -> pure expr'
  case runTypecheckTestM NoSmartHoles (m >>= synth) of
    Left e -> err expr @?= e
    Right _ -> assertFailure "Expected failure but succeeded"

smartSynthGives :: HasCallStack => TypecheckTestM Expr -> TypecheckTestM Expr -> Assertion
smartSynthGives eIn eExpect =
  case ( runTypecheckTestM SmartHoles (eIn >>= synth)
       , runTypecheckTestM NoSmartHoles (eExpect >>= synth)
       ) of
    (_, Left err) -> assertFailure $ "Error in expected: " <> show err
    (Left err, _) -> assertFailure $ "Error in input: " <> show err
    -- Compare result to input, ignoring any difference in IDs
    (Right (_, eGot), Right (_, eExpect')) -> on (@?=) (normaliseAnnotations . zeroIDs) eGot eExpect'
  where
    -- We want eGot and eExpect' to have the same type annotations, but they
    -- may differ on whether they were synthed or checked, and this is OK
    normaliseAnnotations :: ExprT -> Expr' (Meta (Type' ())) (Meta Kind)
    normaliseAnnotations = over (_exprMeta % _type) f
      where
        f :: TypeCache -> Type' ()
        f = \case
          TCSynthed t -> t
          TCChkedAt t -> t
          -- if there are both, we arbitrarily choose the synthed type
          TCEmb TCBoth{tcSynthed = t} -> t

smartSynthKindGives :: HasCallStack => TypecheckTestM Type -> TypecheckTestM Type -> Assertion
smartSynthKindGives tIn tExpect =
  case ( runTypecheckTestM SmartHoles (tIn >>= synthKind)
       , runTypecheckTestM NoSmartHoles (tExpect >>= synthKind)
       ) of
    (_, Left err) -> assertFailure $ "Error in expected: " <> show err
    (Left err, _) -> assertFailure $ "Error in input: " <> show err
    -- Compare result to input, ignoring any difference in IDs
    (Right (_, tGot), Right (_, tExpect')) -> on (@?=) zeroTypeIDs tGot tExpect'

newtype TypecheckTestM a = TypecheckTestM {unTypecheckTestM :: ExceptT TypeError (ReaderT Cxt TestM) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFresh ID
    , MonadFresh NameCounter
    , MonadReader Cxt
    , MonadError TypeError
    )

runTypecheckTestMIn :: Cxt -> TypecheckTestM a -> Either TypeError a
runTypecheckTestMIn cxt =
  evalTestM 0
    . flip runReaderT cxt
    . runExceptT
    . unTypecheckTestM
runTypecheckTestM :: SmartHoles -> TypecheckTestM a -> Either TypeError a
runTypecheckTestM sh = runTypecheckTestMIn (buildTypingContextFromModules [testModule, builtinModule] sh)
runTypecheckTestMWithPrims :: SmartHoles -> TypecheckTestM a -> Either TypeError a
runTypecheckTestMWithPrims sh =
  runTypecheckTestMIn (buildTypingContextFromModules [testModule, builtinModule, primitiveModule] sh)

testModule :: Module
testModule =
  Module
    { moduleName = ModuleName ["TestModule"]
    , moduleTypes = Map.singleton (baseName tMaybeT) (TypeDefAST maybeTDef)
    , moduleDefs = mempty
    }

tMaybeT :: TyConName
tMaybeT = tcn ["TestModule"] "MaybeT"

maybeTDef :: ASTTypeDef
maybeTDef =
  ASTTypeDef
    { astTypeDefParameters = [("m", KFun KType KType), ("a", KType)]
    , astTypeDefConstructors = [ValCon (vcn ["TestModule"] "MakeMaybeT") [TApp () (TVar () "m") (TApp () (TCon () tMaybe) (TVar () "a"))]]
    , astTypeDefNameHints = []
    }
