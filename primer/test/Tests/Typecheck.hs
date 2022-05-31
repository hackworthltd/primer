{-# LANGUAGE RankNTypes #-}

-- | Tests for the typechecker
module Tests.Typecheck where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as Map
import Gen.Core.Raw (
  evalExprGen,
  genTyConName,
  genType,
 )
import Gen.Core.Typed (
  forAllT,
  genSyn,
  propertyWT,
 )
import Hedgehog hiding (check)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Optics (over)
import Primer.App (
  Prog,
  newEmptyProg,
  newProg,
  progAllModules,
  progModules,
 )
import Primer.Builtins (
  boolDef,
  builtinModule,
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
import Primer.Core (
  ASTTypeDef (..),
  Def (..),
  Expr,
  Expr',
  ID,
  Kind (KFun, KHole, KType),
  Meta (..),
  ModuleName (ModuleName),
  PrimDef (PrimDef, primDefName, primDefType),
  TmVarRef (LocalVarRef),
  TyConName,
  Type,
  Type' (TApp, TCon, TForall, TFun, TVar),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeDef (..),
  ValCon (..),
  astTypeDefConstructors,
  typeDefKind,
  valConType,
  _exprMeta,
 )
import Primer.Core.DSL
import Primer.Core.Utils (forgetTypeIDs, generateIDs, generateTypeIDs)
import Primer.Module
import Primer.Name (NameCounter)
import Primer.Primitives (primitiveGVar, primitiveModule, tChar)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt,
  ExprT,
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError (..),
  buildTypingContextFromModules,
  checkEverything,
  decomposeTAppCon,
  mkTAppCon,
  synth,
  synthKind,
 )
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import TestM (TestM, evalTestM)
import TestUtils (gvn, tcn, vcn, zeroIDs, zeroTypeIDs)
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
hprop_decomposeTAppCon :: Property
hprop_decomposeTAppCon = property $ do
  -- We correctly decompose "good" values
  let genArgs = Gen.list (Range.linear 0 5) $ forgetTypeIDs <$> genType
  nargs <- forAll $ evalExprGen 0 $ liftA2 (,) genTyConName genArgs
  tripping nargs (uncurry mkTAppCon) decomposeTAppCon
  -- Also test that if we decompose, then it was "good"
  ty <- forAll $ evalExprGen 0 $ forgetTypeIDs <$> genType
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
  f boolDef @?= [TCon () tBool, TCon () tBool]
  f natDef @?= [TCon () tNat, TFun () (TCon () tNat) (TCon () tNat)]
  f listDef
    @?= [ TForall () "a" KType (TApp () (TCon () tList) (TVar () "a"))
        , TForall () "a" KType $ TFun () (TVar () "a") $ TFun () (TApp () (TCon () tList) (TVar () "a")) $ TApp () (TCon () tList) (TVar () "a")
        ]
  f eitherDef
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
    f t = map (valConType t) (astTypeDefConstructors t)

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
  ann emptyHole (tcon nonexistant) `expectFailsWith` const (UnknownTypeConstructor nonexistant)
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
          ann
            ( case_
                (lvar "x")
                [branch cTrue [] (con cZero), branch cFalse [] emptyHole]
            )
            tEmptyHole
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
  tapp (tcon tList) (tcon tList)
    `smartSynthKindGives` tapp (tcon tList) (thole $ tcon tList)

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
    `smartSynthKindGives` tapp (tcon tList) (tcon tBool)

unit_smart_type_remove_2 :: Assertion
unit_smart_type_remove_2 =
  tforall "a" KType (thole $ tcon tBool)
    `smartSynthKindGives` tforall "a" KType (tcon tBool)

unit_smart_type_remove_3 :: Assertion
unit_smart_type_remove_3 =
  tapp (tcon tList) (thole $ tcon tBool)
    `smartSynthKindGives` tapp (tcon tList) (tcon tBool)

unit_smart_type_remove_4 :: Assertion
unit_smart_type_remove_4 =
  tfun (thole $ tcon tBool) (thole $ tcon tNat)
    `smartSynthKindGives` tfun (tcon tBool) (tcon tNat)

unit_smart_type_remove_5 :: Assertion
unit_smart_type_remove_5 =
  thole (tapp (tcon tList) tEmptyHole)
    `smartSynthKindGives` tapp (tcon tList) tEmptyHole

unit_prim_char :: Assertion
unit_prim_char =
  expectTypedWithPrims $ ann (char 'a') (tcon tChar)

unit_prim_fun :: Assertion
unit_prim_fun =
  expectTypedWithPrims $ ann (gvar $ primitiveGVar "hexToNat") (tfun (tcon tChar) (tapp (tcon tMaybe) (tcon tNat)))

unit_prim_fun_applied :: Assertion
unit_prim_fun_applied =
  expectTypedWithPrims $ ann (app (gvar $ primitiveGVar "hexToNat") (char 'a')) (tapp (tcon tMaybe) (tcon tNat))

-- Whenever we synthesise a type, then it kind-checks against KType
hprop_synth_well_typed_extcxt :: Property
hprop_synth_well_typed_extcxt = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt [builtinModule, primitiveModule] $ do
      (e, _ty) <- forAllT genSyn
      ty' <- generateTypeIDs . fst =<< synthTest =<< generateIDs e
      void $ checkKindTest KType ty'

-- As hprop_synth_well_typed_extcxt, but in the empty context
-- this is in case there are problems with primitive constructors
-- (which cannot be used unless their corresponding type is in scope)
hprop_synth_well_typed_defcxt :: Property
hprop_synth_well_typed_defcxt = withTests 1000 $
  withDiscards 2000 $
    propertyWT [] $ do
      (e, _ty) <- forAllT genSyn
      ty' <- generateTypeIDs . fst =<< synthTest =<< generateIDs e
      void $ checkKindTest KType ty'

-- Check that all our builtins are well formed
-- (these are used to seed initial programs)
checkProgWellFormed :: HasCallStack => (forall m. MonadFresh ID m => m Prog) -> Assertion
checkProgWellFormed p' = case runTypecheckTestM NoSmartHoles $ do
  p <- p'
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = mempty
      , toCheck = progAllModules p
      } of
  Left err -> assertFailure $ show err
  Right _ -> pure ()

unit_good_defaults :: Assertion
unit_good_defaults = do
  checkProgWellFormed $ pure newEmptyProg
  checkProgWellFormed $ pure newProg

-- Check that our higher-order test typedef is well formed
unit_good_maybeT :: Assertion
unit_good_maybeT = case runTypecheckTestM NoSmartHoles $
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = [builtinModule]
      , toCheck = [Module (ModuleName ["TestModule"]) (mkTypeDefMap [TypeDefAST maybeTDef]) mempty]
      } of
  Left err -> assertFailure $ show err
  Right _ -> pure ()

unit_bad_prim_map_base :: Assertion
unit_bad_prim_map_base = case runTypecheckTestM NoSmartHoles $ do
  fooType <- tcon tNat
  let foo = PrimDef{primDefName = gvn ["M"] "bar", primDefType = fooType}
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = progModules newProg
      , toCheck = [Module (ModuleName ["M"]) mempty $ Map.singleton "foo" $ DefPrim foo]
      } of
  Left err -> err @?= InternalError "Inconsistent names in moduleDefs map for module M"
  Right _ -> assertFailure "Expected failure but succeeded"

unit_bad_prim_map_module :: Assertion
unit_bad_prim_map_module = case runTypecheckTestM NoSmartHoles $ do
  fooType <- tcon tNat
  let foo = PrimDef{primDefName = gvn ["OtherMod"] "foo", primDefType = fooType}
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = progModules newProg
      , toCheck = [Module (ModuleName ["M"]) mempty $ Map.singleton "foo" $ DefPrim foo]
      } of
  Left err -> err @?= InternalError "Inconsistent names in moduleDefs map for module M"
  Right _ -> assertFailure "Expected failure but succeeded"

unit_bad_prim_type :: Assertion
unit_bad_prim_type = case runTypecheckTestM NoSmartHoles $ do
  fooType <- tcon' ["M"] "NonExistant"
  let foo = PrimDef{primDefName = gvn ["M"] "foo", primDefType = fooType}
  checkEverything
    NoSmartHoles
    CheckEverything
      { trusted = progModules newProg
      , toCheck = [Module (ModuleName ["M"]) mempty $ Map.singleton "foo" $ DefPrim foo]
      } of
  Left err -> err @?= UnknownTypeConstructor (tcn ["M"] "NonExistant")
  Right _ -> assertFailure "Expected failure but succeeded"

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
    normaliseAnnotations = over _exprMeta f
      where
        f :: Meta TypeCache -> Meta (Type' ())
        f (Meta i c v) =
          let c' = case c of
                TCSynthed t -> t
                TCChkedAt t -> t
                -- if there are both, we arbitrarily choose the synthed type
                TCEmb TCBoth{tcSynthed = t} -> t
           in Meta i c' v

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
    , moduleTypes = mkTypeDefMap [TypeDefAST maybeTDef]
    , moduleDefs = mempty
    }

tMaybeT :: TyConName
tMaybeT = tcn ["TestModule"] "MaybeT"

maybeTDef :: ASTTypeDef
maybeTDef =
  ASTTypeDef
    { astTypeDefName = tMaybeT
    , astTypeDefParameters = [("m", KFun KType KType), ("a", KType)]
    , astTypeDefConstructors = [ValCon (vcn ["TestModule"] "MakeMaybeT") [TApp () (TVar () "m") (TApp () (TCon () tMaybe) (TVar () "a"))]]
    , astTypeDefNameHints = []
    }
