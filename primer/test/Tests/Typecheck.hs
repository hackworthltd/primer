-- | Tests for the typechecker
module Tests.Typecheck where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Gen.Core.Raw (
  evalExprGen,
  genName,
  genType,
 )
import Hedgehog hiding (check)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Optics (over, set)
import Primer.Core (
  Expr,
  Expr',
  ID,
  Kind (KFun, KHole, KType),
  Meta (..),
  Type,
  Type' (TApp, TCon, TForall, TFun, TVar),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeDef (..),
  ValCon (..),
  boolDef,
  defaultTypeDefs,
  eitherDef,
  listDef,
  natDef,
  setID,
  typeDefConstructors,
  typeDefKind,
  valConType,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL
import Primer.Name (NameCounter)
import Primer.Typecheck (
  Cxt,
  ExprT,
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError (..),
  buildTypingContext,
  decomposeTAppCon,
  mkTAppCon,
  synth,
  synthKind,
 )
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import TestM (TestM, evalTestM)

unit_identity :: Assertion
unit_identity =
  expectTyped $ ann (lam "x" (var "x")) (tfun (tcon "Bool") (tcon "Bool"))

unit_undefined_variable :: Assertion
unit_undefined_variable =
  ann (lam "x" (var "y")) tEmptyHole `expectFailsWith` const (UnknownVariable "y")

unit_const :: Assertion
unit_const =
  expectTyped $
    ann
      (lam "x" (lam "y" (var "x")))
      (tfun (tcon "Bool") (tfun (tcon "Bool") (tcon "Bool")))

unit_true :: Assertion
unit_true = expectTyped $ con "True"

unit_constructor_doesn't_exist :: Assertion
unit_constructor_doesn't_exist =
  con "Nope" `expectFailsWith` const (UnknownConstructor "Nope")

unit_inc :: Assertion
unit_inc =
  expectTyped $
    ann
      (lam "n" (app (con "Succ") (var "n")))
      (tfun (tcon "Nat") (tcon "Nat"))

unit_compose_nat :: Assertion
unit_compose_nat =
  expectTyped $
    ann
      (lam "f" (lam "g" (app (var "f") (hole (var "g")))))
      ( tfun
          (tfun (tcon "Nat") (tcon "Nat"))
          ( tfun
              (tfun (tcon "Nat") (tcon "Nat"))
              (tcon "Nat")
          )
      )

-- let x = True in x
unit_let :: Assertion
unit_let =
  expectTyped $ let_ "x" (con "True") (var "x")

-- Normal lets do not permit recursion
unit_recursive_let :: Assertion
unit_recursive_let =
  let_ "x" (var "x") (var "x")
    `expectFailsWith` const (UnknownVariable "x")

-- letrec x : Bool = x in x
unit_letrec_1 :: Assertion
unit_letrec_1 =
  expectTyped $
    letrec "x" (var "x") (tcon "Bool") (var "x")

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
              (var "x")
              [ branch "Zero" [] (con "Zero")
              , branch
                  "Succ"
                  [("n", Nothing)]
                  ( app
                      (con "Succ")
                      (app (con "Succ") (app (var "double") (var "n")))
                  )
              ]
          )
      )
      (tfun (tcon "Nat") (tcon "Nat"))
      (app (var "double") (app (con "Succ") (con "Zero")))

-- let x = True
--  in let y = False
--      in x
unit_nested_let :: Assertion
unit_nested_let =
  expectTyped $ let_ "x" (con "True") (let_ "y" (con "False") (var "x"))

-- let yes = \x -> True : Bool -> Bool
--  in let y = False
--      in yes y
unit_let_function :: Assertion
unit_let_function =
  expectTyped $
    let_
      "yes"
      (ann (lam "x" (con "True")) (tfun (tcon "Bool") (tcon "Bool")))
      (let_ "y" (con "False") (app (var "yes") (var "y")))

-- (\f -> f : (Bool -> Bool) -> (Bool -> Bool)) (let y = True in \x -> y)
unit_let_in_arg :: Assertion
unit_let_in_arg =
  expectTyped $
    app
      ( ann
          (lam "f" (var "f"))
          (tfun (tfun (tcon "Bool") (tcon "Bool")) (tfun (tcon "Bool") (tcon "Bool")))
      )
      (let_ "y" (con "True") (lam "x" (var "y")))

unit_mkTAppCon :: Assertion
unit_mkTAppCon = do
  mkTAppCon "C" [] @?= TCon () "C"
  mkTAppCon "C" [TCon () "X"] @?= TApp () (TCon () "C") (TCon () "X")
  mkTAppCon "C" [TCon () "X", TCon () "Y"] @?= TApp () (TApp () (TCon () "C") (TCon () "X")) (TCon () "Y")

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
  let genArgs = Gen.list (Range.linear 0 5) $ set _typeMeta () <$> genType
  nargs <- forAll $ evalExprGen 0 $ liftA2 (,) genName genArgs
  tripping nargs (uncurry mkTAppCon) decomposeTAppCon
  -- Also test that if we decompose, then it was "good"
  ty <- forAll $ evalExprGen 0 $ set _typeMeta () <$> genType
  let dec = decomposeTAppCon ty
  -- See Note [cover]
  --cover 30 "decomposable" $ isJust dec
  --cover 30 "non-decomposable" $ isNothing dec
  case dec of
    Nothing -> success
    Just (n, args) -> ty === mkTAppCon n args

unit_typeDefKind :: Assertion
unit_typeDefKind = do
  typeDefKind boolDef @?= KType
  typeDefKind natDef @?= KType
  typeDefKind listDef @?= KFun KType KType
  typeDefKind eitherDef @?= KFun KType (KFun KType KType)

unit_valConType :: Assertion
unit_valConType = do
  f boolDef @?= [TCon () "Bool", TCon () "Bool"]
  f natDef @?= [TCon () "Nat", TFun () (TCon () "Nat") (TCon () "Nat")]
  f listDef
    @?= [ TForall () "a" KType (TApp () (TCon () "List") (TVar () "a"))
        , TForall () "a" KType $ TFun () (TVar () "a") $ TFun () (TApp () (TCon () "List") (TVar () "a")) $ TApp () (TCon () "List") (TVar () "a")
        ]
  f eitherDef
    @?= [ TForall () "a" KType $
            TForall () "b" KType $
              TFun () (TVar () "a") $
                TApp () (TApp () (TCon () "Either") (TVar () "a")) (TVar () "b")
        , TForall () "a" KType $
            TForall () "b" KType $
              TFun () (TVar () "b") $
                TApp () (TApp () (TCon () "Either") (TVar () "a")) (TVar () "b")
        ]
  where
    f t = map (valConType t) (typeDefConstructors t)

-- Nat -> Bool accepts \x . case x of Z -> True ; S _ -> False
unit_case_isZero :: Assertion
unit_case_isZero =
  expectTyped $
    ann (lam "x" $ case_ (var "x") [branch "Zero" [] (con "True"), branch "Succ" [("n", Nothing)] (con "False")]) (tfun (tcon "Nat") (tcon "Bool"))

-- Nat -> Bool rejects \x . case x of {}
unit_case_badEmpty :: Assertion
unit_case_badEmpty =
  ann (lam "x" $ case_ (var "x") []) (tfun (tcon "Nat") (tcon "Bool"))
    `expectFailsWith` const (WrongCaseBranches "Nat" [])

-- Cannot case on a Nat -> Nat
unit_case_badType :: Assertion
unit_case_badType =
  ann (lam "x" $ case_ (var "x") []) (tfun (tfun (tcon "Nat") (tcon "Nat")) (tcon "Bool"))
    `expectFailsWith` const (CannotCaseNonADT $ TFun () (TCon () "Nat") (TCon () "Nat"))

-- Cannot annotate something with a non-existent type constructor
unit_ann_bad :: Assertion
unit_ann_bad =
  ann emptyHole (tcon "IDoNotExist") `expectFailsWith` const (UnknownTypeConstructor "IDoNotExist")

unit_ann_insert :: Assertion
unit_ann_insert =
  app (lam "x" $ var "x") (con "Zero")
    `smartSynthGives` app (ann (lam "x" $ var "x") tEmptyHole) (con "Zero")

unit_app_not_arrow :: Assertion
unit_app_not_arrow =
  app (con "Zero") (con "Zero")
    `smartSynthGives` app (hole (con "Zero")) (con "Zero")

-- Note: there is something odd with this test, related to
-- annotations-changing-types/chk-annotations I think the correct thing to give
-- is Succ {? \x.x : ? ?}, but the hole is actually removable:
-- Succ (\x.x : ?) is fine in our system (but argueably this is a bug).
-- The smartTC currently gives an annotation inside a hole.
unit_chk_lam_not_arrow :: Assertion
unit_chk_lam_not_arrow =
  app (con "Succ") (lam "x" $ var "x")
    `smartSynthGives` app (con "Succ") (hole $ ann (lam "x" $ var "x") tEmptyHole)

unit_check_emb :: Assertion
unit_check_emb =
  app (con "Succ") (con "True")
    `smartSynthGives` app (con "Succ") (hole $ con "True")

unit_case_scrutinee :: Assertion
unit_case_scrutinee =
  ann (case_ (con "Succ") [branch "C" [] $ var "x"]) (tcon "Bool")
    `smartSynthGives` ann (case_ (hole $ con "Succ") []) (tcon "Bool")

unit_case_branches :: Assertion
unit_case_branches =
  ann (case_ (con "Zero") [branch "C" [] $ var "x"]) (tcon "Bool")
    `smartSynthGives` ann (case_ (con "Zero") [branch "Zero" [] emptyHole, branch "Succ" [("a7", Nothing)] emptyHole]) (tcon "Bool") -- Fragile name here "a7"

unit_remove_hole :: Assertion
unit_remove_hole =
  ann (lam "x" $ hole (var "x")) (tfun (tcon "Nat") (tcon "Nat"))
    `smartSynthGives` ann (lam "x" $ var "x") (tfun (tcon "Nat") (tcon "Nat"))

-- It is not clear how to (efficiently) remove the hole in
-- {? Succ ?} Zero
-- We don't have enough information to see that it is redundant...
-- One thing we could try is: remove the hole and recheck from scratch
-- but that seems inefficient!
-- This is tracked as https://github.com/hackworthltd/primer/issues/7
unit_remove_hole_not_perfect :: Assertion
unit_remove_hole_not_perfect =
  app (hole (con "Succ")) (con "Zero")
    `smartSynthGives` app (hole (con "Succ")) (con "Zero") -- We currently give this as output
    --app (con "Succ") (con "Zero") -- We would prefer to see the hole removed

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
                (var "x")
                [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
            )
            tEmptyHole
    )
    (tfun (tcon "Bool") (tcon "Nat"))
    `smartSynthGives` ann
      ( lam "x" $
          ann
            ( case_
                (var "x")
                [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
            )
            tEmptyHole
      )
      (tfun (tcon "Bool") (tcon "Nat"))

unit_poly :: Assertion
unit_poly =
  expectTyped $
    ann
      (lam "id" $ lAM "a" $ aPP (var "id") (tvar "a"))
      (tforall "c" KType (tvar "c" `tfun` tvar "c") `tfun` tforall "b" KType (tvar "b" `tfun` tvar "b"))

unit_poly_head_Nat :: Assertion
unit_poly_head_Nat =
  expectTyped $
    ann
      ( lam "x" $
          case_
            (var "x")
            [ branch "Nil" [] (con "Zero")
            , branch "Cons" [("y", Nothing), ("ys", Nothing)] $ con "Succ" `app` var "y"
            ]
      )
      ((tcon "List" `tapp` tcon "Nat") `tfun` tcon "Nat")

unit_type_hole_1 :: Assertion
unit_type_hole_1 = tEmptyHole `expectKinded` KHole

unit_type_hole_2 :: Assertion
unit_type_hole_2 = tapp tEmptyHole (tcon "Bool") `expectKinded` KHole

unit_type_hole_3 :: Assertion
unit_type_hole_3 = tapp tEmptyHole (tcon "List") `expectKinded` KHole

unit_type_hole_4 :: Assertion
unit_type_hole_4 = tapp (tcon "MaybeT") tEmptyHole `expectKinded` KFun KType KType

unit_type_hole_5 :: Assertion
unit_type_hole_5 = tforall "a" KType tEmptyHole `expectKinded` KType

unit_type_hole_6 :: Assertion
unit_type_hole_6 = thole (tcon "Bool") `expectKinded` KHole

unit_smart_type_not_arrow :: Assertion
unit_smart_type_not_arrow =
  tapp (tcon "Bool") (tcon "Bool")
    `smartSynthKindGives` tapp (thole $ tcon "Bool") (tcon "Bool")

unit_smart_type_forall :: Assertion
unit_smart_type_forall =
  tforall "a" KType (tcon "List")
    `smartSynthKindGives` tforall "a" KType (thole $ tcon "List")

unit_smart_type_not_type :: Assertion
unit_smart_type_not_type =
  tapp (tcon "List") (tcon "List")
    `smartSynthKindGives` tapp (tcon "List") (thole $ tcon "List")

unit_smart_type_fun :: Assertion
unit_smart_type_fun =
  tfun (tcon "List") (tcon "MaybeT")
    `smartSynthKindGives` tfun (thole $ tcon "List") (thole $ tcon "MaybeT")

unit_smart_type_inside_hole_1 :: Assertion
unit_smart_type_inside_hole_1 =
  thole (tcon "Bool" `tapp` tcon "MaybeT")
    `smartSynthKindGives` (thole (tcon "Bool") `tapp` tcon "MaybeT")

unit_smart_type_inside_hole_2 :: Assertion
unit_smart_type_inside_hole_2 =
  thole (tcon "List" `tapp` tcon "MaybeT")
    `smartSynthKindGives` (tcon "List" `tapp` thole (tcon "MaybeT"))

unit_smart_type_inside_hole_3 :: Assertion
unit_smart_type_inside_hole_3 =
  (tcon "List" `tapp` thole (tcon "MaybeT" `tapp` tcon "Bool"))
    `smartSynthKindGives` (tcon "List" `tapp` thole (tcon "MaybeT" `tapp` thole (tcon "Bool")))

unit_smart_type_remove_1 :: Assertion
unit_smart_type_remove_1 =
  tapp (thole $ tcon "List") (tcon "Bool")
    `smartSynthKindGives` tapp (tcon "List") (tcon "Bool")

unit_smart_type_remove_2 :: Assertion
unit_smart_type_remove_2 =
  tforall "a" KType (thole $ tcon "Bool")
    `smartSynthKindGives` tforall "a" KType (tcon "Bool")

unit_smart_type_remove_3 :: Assertion
unit_smart_type_remove_3 =
  tapp (tcon "List") (thole $ tcon "Bool")
    `smartSynthKindGives` tapp (tcon "List") (tcon "Bool")

unit_smart_type_remove_4 :: Assertion
unit_smart_type_remove_4 =
  tfun (thole $ tcon "Bool") (thole $ tcon "Nat")
    `smartSynthKindGives` tfun (tcon "Bool") (tcon "Nat")

unit_smart_type_remove_5 :: Assertion
unit_smart_type_remove_5 =
  thole (tapp (tcon "List") tEmptyHole)
    `smartSynthKindGives` tapp (tcon "List") tEmptyHole

-- * Helpers

expectTyped :: TypecheckTestM Expr -> Assertion
expectTyped m =
  case runTypecheckTestM NoSmartHoles (m >>= synth) of
    Left err -> assertFailure $ show err
    Right _ -> pure ()

expectKinded :: TypecheckTestM Type -> Kind -> Assertion
expectKinded m k =
  case runTypecheckTestM NoSmartHoles (m >>= synthKind) of
    Left err -> assertFailure $ show err
    Right (k', _) -> k' @?= k

expectFailsWith :: TypecheckTestM Expr -> (Expr -> TypeError) -> Assertion
expectFailsWith m err = do
  expr <- case runTypecheckTestM NoSmartHoles m of
    Left constructionErr -> assertFailure $ show constructionErr
    Right expr' -> pure expr'
  case runTypecheckTestM NoSmartHoles (m >>= synth) of
    Left e -> err expr @?= e
    Right _ -> assertFailure "Expected failure but succeeded"

smartSynthGives :: TypecheckTestM Expr -> TypecheckTestM Expr -> Assertion
smartSynthGives eIn eExpect =
  case ( runTypecheckTestM SmartHoles (eIn >>= synth)
       , runTypecheckTestM NoSmartHoles (eExpect >>= synth)
       ) of
    (_, Left err) -> assertFailure $ "Error in expected: " <> show err
    (Left err, _) -> assertFailure $ "Error in input: " <> show err
    -- Compare result to input, ignoring any difference in IDs
    (Right (_, eGot), Right (_, eExpect')) -> on (@?=) (normaliseAnnotations . zeroIDs) eGot eExpect'
  where
    zeroIDs :: ExprT -> ExprT
    zeroIDs = over _exprMeta (setID 0) . over _exprTypeMeta (setID 0)
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

smartSynthKindGives :: TypecheckTestM Type -> TypecheckTestM Type -> Assertion
smartSynthKindGives tIn tExpect =
  case ( runTypecheckTestM SmartHoles (tIn >>= synthKind)
       , runTypecheckTestM NoSmartHoles (tExpect >>= synthKind)
       ) of
    (_, Left err) -> assertFailure $ "Error in expected: " <> show err
    (Left err, _) -> assertFailure $ "Error in input: " <> show err
    -- Compare result to input, ignoring any difference in IDs
    (Right (_, tGot), Right (_, tExpect')) -> on (@?=) zeroIDs tGot tExpect'
  where
    zeroIDs = over _typeMeta (setID 0)

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

runTypecheckTestM :: SmartHoles -> TypecheckTestM a -> Either TypeError a
runTypecheckTestM sh = evalTestM 0 . flip runReaderT (buildTypingContext testingTypeDefs mempty sh) . runExceptT . unTypecheckTestM

testingTypeDefs :: [TypeDef]
testingTypeDefs = maybeTDef : defaultTypeDefs

maybeTDef :: TypeDef
maybeTDef =
  TypeDef
    { typeDefName = "MaybeT"
    , typeDefParameters = [("m", KFun KType KType), ("a", KType)]
    , typeDefConstructors = [ValCon "MakeMaybeT" [TApp () (TVar () "m") (TApp () (TVar () "Maybe") (TVar () "a"))]]
    , typeDefNameHints = []
    }
