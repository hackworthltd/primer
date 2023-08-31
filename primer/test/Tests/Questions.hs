-- | Tests for Question logic
module Tests.Questions where

import Foreword hiding (diff)

import Data.List.Extra (nubOrdOn, nubSort)
import Hedgehog hiding (Property, check, property)
import Hedgehog.Classes
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Builtins
import Primer.Core (
  Expr,
  GVarName,
  GlobalName (baseName),
  Kind' (KFun, KType),
  LVarName,
  LocalName (LocalName, unLocalName),
  ModuleName (ModuleName),
  TyVarName,
  Type,
  Type' (TCon),
  qualifyName,
 )
import Primer.Core.DSL
import Primer.Core.Utils (forgetKindMetadata, forgetTypeMetadata)
import Primer.Gen.Core.Raw (evalExprGen, genKind, genName, genTyVarName, genType)
import Primer.Module (builtinModule)
import Primer.Name
import Primer.Questions (
  ShadowedVarsExpr (M),
  ShadowedVarsTy (N),
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.Typecheck (
  Cxt,
  SmartHoles (NoSmartHoles),
  buildTypingContextFromModules',
  exprTtoExpr,
  synth,
 )
import Primer.Zipper (ExprZ, Loc' (InExpr), TypeZip, down, focus, right)
import Tasty (Property, property)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import Test.Tasty.Hedgehog
import Tests.Typecheck (runTypecheckTestM)

test_laws :: TestTree
test_laws =
  testGroup
    "Laws"
    [ testGroup
        "STV"
        $ map
          lawsToTestTree
          [ semigroupLaws genSTV
          , monoidLaws genSTV
          ]
    , testGroup
        "STE"
        $ map
          lawsToTestTree
          [ semigroupLaws genSTE
          , monoidLaws genSTE
          ]
    ]
  where
    -- ideally there'd be a library for this - see https://github.com/hedgehogqa/haskell-hedgehog-classes/issues/13
    lawsToTestTree (Laws className props) =
      testGroup className $
        map (\(n, p) -> testPropertyNamed n "<internal-property>" p) props

-- * Properties of monoids handling shadowing

tasty_shadow_monoid_types :: Property
tasty_shadow_monoid_types = property $ do
  nks <- forAll genSTV'
  let N nonShadowed = foldMap' (\nk -> N [nk]) nks
  annotateShow nonShadowed
  label $ if length nonShadowed == length nks then "no shadowing" else "shadowing"
  -- We end up with fewer elements than we started with
  diff nks ((>=) `on` length) nonShadowed
  let nonShNames = map fst nonShadowed
  -- There are no duplicate names in the output
  assert $ ordNub nonShNames == nonShNames
  -- We keep exactly one of each input name,
  -- and the ordering is the same (as ordNub preserves order)
  assert $ nonShNames == ordNub (map fst nks)

-- Generates data that could be contained in a ShadowedVarsTy, except
-- it may have duplicated names
genSTV' :: Gen [(TyVarName, Kind' ())]
genSTV' = evalExprGen 0 $ Gen.list (Range.linear 0 20) $ (,) <$> genTyVarName <*> (forgetKindMetadata <$> genKind)

genSTV :: Gen ShadowedVarsTy
genSTV = N . nubOrdOn fst <$> genSTV'

tasty_shadow_monoid_expr :: Property
tasty_shadow_monoid_expr = property $ do
  ns <- forAll genSTE'
  let split = \case
        TyVar v -> M [v] [] []
        TmVar v -> M [] [v] []
        Global v -> M [] [] [v]
  let M tyV tmV glV = foldMap' split ns
  annotateShow tyV
  annotateShow tmV
  annotateShow glV
  let lenIn = length ns
  let lenOut = length tyV + length tmV + length glV
  label $ if lenIn == lenOut then "no shadowing" else "shadowing"
  -- We end up with fewer elements than we started with
  assert $ lenIn >= lenOut
  let nonShNames = map (unLocalName . fst) tyV ++ map (unLocalName . fst) tmV ++ map (baseName . fst) glV
  annotateShow nonShNames
  -- there are no duplicate names in the output
  assert $ ordNub nonShNames == nonShNames
  -- We keep exactly one of each input name.
  -- Contrary to tasty_shadow_monoid_types, we don't check the ordering
  -- is the same, but only because it is more awkward to test (need that the
  -- three lists tyV, tmV, glV can be interleaved and then "stretches into" ns)
  -- than the benefit would be worth
  assert $ sort nonShNames == nubSort (map nameSTE' ns)

data STE'
  = TyVar (TyVarName, Kind' ())
  | TmVar (LVarName, Type' () ())
  | Global (GVarName, Type' () ())
  deriving stock (Show)

nameSTE' :: STE' -> Name
nameSTE' = \case
  TyVar (n, _) -> unLocalName n
  TmVar (n, _) -> unLocalName n
  Global (n, _) -> baseName n

-- Generates data that could be contained in a ShadowedVarsExpr, except
-- it may have duplicated names, and is not split into three sections,
-- but jumbled together
genSTE' :: Gen [STE']
genSTE' =
  let g = Gen.either_ (forgetKindMetadata <$> genKind) $ (,) <$> fmap forgetTypeMetadata genType <*> Gen.bool
      toSTE' m n = \case
        Left k -> TyVar (LocalName n, k)
        Right (ty, False) -> TmVar (LocalName n, ty)
        Right (ty, True) -> Global (qualifyName m n, ty)
   in evalExprGen 0 $ Gen.list (Range.linear 0 20) $ toSTE' <$> genModuleName <*> genName <*> g
  where
    genModuleName = ModuleName <$> Gen.element [["M"], ["M1"]]

genSTE :: Gen ShadowedVarsExpr
genSTE = deal . nubOrdOn nameSTE' <$> genSTE'
  where
    deal = \case
      [] -> M [] [] []
      TyVar v : vs -> let M ty tm gl = deal vs in M (v : ty) tm gl
      TmVar v : vs -> let M ty tm gl = deal vs in M ty (v : tm) gl
      Global v : vs -> let M ty tm gl = deal vs in M ty tm (v : gl)

-- * 'variablesInScope'

-- Given an empty AST, we return an empty set of variables
unit_variablesInScope_empty :: Assertion
unit_variablesInScope_empty =
  hasVariables emptyHole pure mempty

-- Given a single lambda, we return just the variable it binds
unit_variablesInScope_lambda :: Assertion
unit_variablesInScope_lambda = do
  let expr = ann (lam "x" emptyHole) (tfun (tcon tBool) (tcon tBool))
  hasVariables expr pure []
  hasVariables expr down []
  hasVariables expr (down >=> down) [("x", TCon () tBool)]

-- Given a let, its bound variable is in scope in the body but not the bound expression
unit_variablesInScope_let :: Assertion
unit_variablesInScope_let = do
  let oneLet = let_ "x" (con0 cTrue `ann` tcon tBool) emptyHole
      twoLet = let_ "x" (con0 cTrue `ann` tcon tBool) (let_ "y" (con0 cZero `ann` tcon tNat) emptyHole)
  hasVariables oneLet pure mempty
  hasVariables oneLet down mempty
  hasVariables oneLet (down >=> right) [("x", TCon () tBool)]
  hasVariables
    twoLet
    (down >=> right >=> down)
    [("x", TCon () tBool)]
  hasVariables
    twoLet
    (down >=> right >=> down >=> right)
    [("x", TCon () tBool), ("y", TCon () tNat)]

-- Given a letrec, its bound variable is in scope in both the body and the bound expression
unit_variablesInScope_letrec :: Assertion
unit_variablesInScope_letrec = do
  let expr = letrec "x" (con0 cTrue) (tcon tBool) emptyHole
  hasVariables expr pure []
  hasVariables expr down [("x", TCon () tBool)]
  hasVariables expr (down >=> right) [("x", TCon () tBool)]

-- NB: there are no tests for LetType or TLet because they are unsupported by the typechecker
-- and should never appear in programs. Thus they shouldn't appear in anything of which we
-- ask the variablesInScope question.

-- Given a case expression, any variables bound by its branches are in scope in their corresponding
-- LHS.
unit_variablesInScope_case :: Assertion
unit_variablesInScope_case = do
  let expr = ann (case_ (con0 cZero `ann` tcon tNat) [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]) (tcon tNat)
  hasVariables expr pure []
  hasVariables expr down []
  hasVariables expr (down >=> down) []
  hasVariables expr (down >=> down >=> right) []
  hasVariables expr (down >=> down >=> right >=> right) [("n", TCon () tNat)]

unit_variablesInScope_type :: Assertion
unit_variablesInScope_type = do
  let ty = tforall "a" ktype $ tfun (tvar "a") (tapp tEmptyHole tEmptyHole)
  hasVariablesType ty pure []
  hasVariablesType ty down [("a", KType ())]
  hasVariablesType ty (down >=> down) [("a", KType ())]

-- * Tests that we do not report shadowed vars

unit_variablesInScope_shadowed :: Assertion
unit_variablesInScope_shadowed = do
  let ty = tforall "a" (kfun ktype ktype) $ tforall "b" ktype $ tcon tNat `tfun` tforall "a" ktype (tcon tBool `tfun` (tcon tList `tapp` tvar "b"))
      expr' = lAM "c" $ lAM "d" $ lam "c" $ lAM "c" $ lam "c" $ emptyHole `ann` (tcon tList `tapp` tvar "d")
      expr = ann expr' ty
  hasVariablesType ty pure []
  hasVariablesType ty down [("a", KFun () (KType ()) (KType ()))]
  hasVariablesType ty (down >=> down) [("a", KFun () (KType ()) (KType ())), ("b", KType ())]
  hasVariablesType ty (down >=> down >=> down >=> right >=> down) [("b", KType ()), ("a", KType ())]
  hasVariablesType ty (down >=> down >=> down >=> right >=> down >=> down >=> right >=> down >=> right) [("b", KType ()), ("a", KType ())]
  hasVariablesTyTm expr pure [] []
  hasVariablesTyTm expr (down >=> down) [("c", KFun () (KType ()) (KType ()))] []
  hasVariablesTyTm expr (down >=> down >=> down) [("c", KFun () (KType ()) (KType ())), ("d", KType ())] []
  hasVariablesTyTm expr (down >=> down >=> down >=> down) [("d", KType ())] [("c", TCon () tNat)]
  hasVariablesTyTm expr (down >=> down >=> down >=> down >=> down) [("d", KType ()), ("c", KType ())] []
  hasVariablesTyTm expr (down >=> down >=> down >=> down >=> down >=> down) [("d", KType ())] [("c", TCon () tBool)]

-- | Test that if we walk 'path' to some node in 'expr', that node will have
-- 'expected' in-scope variables.
-- We start by typechecking the expression, so it is annotated with types.
hasVariables :: S Expr -> (ExprZ -> Maybe ExprZ) -> [(LVarName, Type' () ())] -> Assertion
hasVariables expr path expected = do
  let e = create' expr
  case runTypecheckTestM NoSmartHoles (synth e) of
    Left err -> assertFailure $ show err
    Right (_, exprT) -> case path $ focus $ exprTtoExpr exprT of
      Just z' -> let (_, locals, _) = variablesInScopeExpr mempty (InExpr z') in locals @?= expected
      Nothing -> assertFailure ""

-- | Like 'hasVariables' but for type variables inside terms also
hasVariablesTyTm :: S Expr -> (ExprZ -> Maybe ExprZ) -> [(TyVarName, Kind' ())] -> [(LVarName, Type' () ())] -> Assertion
hasVariablesTyTm expr path expectedTy expectedTm = do
  let e = create' expr
  case runTypecheckTestM NoSmartHoles (synth e) of
    Left err -> assertFailure $ show err
    Right (_, exprT) -> case path $ focus $ exprTtoExpr exprT of
      Just z' -> do
        let (tyvars, tmvars, _) = variablesInScopeExpr mempty (InExpr z')
        tyvars @?= expectedTy
        tmvars @?= expectedTm
      Nothing -> assertFailure ""

-- | Like 'hasVariables' but for types
hasVariablesType :: S Type -> (TypeZip -> Maybe TypeZip) -> [(TyVarName, Kind' ())] -> Assertion
hasVariablesType ty path expected = do
  let t = create' ty
  case path $ focus t of
    Just z -> variablesInScopeTy (Left z) @?= expected
    Nothing -> assertFailure ""

-- Test type-directed names
unit_hasGeneratedNames_1 :: Assertion
unit_hasGeneratedNames_1 = do
  hasGeneratedNamesExpr emptyHole Nothing pure ["x", "y", "z"]
  hasGeneratedNamesExpr emptyHole (Just $ tfun tEmptyHole tEmptyHole) pure ["f", "g", "h"]
  hasGeneratedNamesExpr emptyHole (Just $ tcon tNat) pure ["i", "j", "m", "n"]
  hasGeneratedNamesExpr emptyHole (Just $ tcon tList `tapp` tcon tNat) pure ["xs", "ys", "zs"]
  let expr = lam "x" $ lam "i" emptyHole
  hasGeneratedNamesExpr expr Nothing pure ["y", "z", "x1"]
  hasGeneratedNamesExpr expr (Just $ tcon tNat) pure ["j", "m", "n", "i1"]
  hasGeneratedNamesExpr expr (Just $ tcon tList `tapp` tcon tNat) pure ["xs", "ys", "zs"]

-- test type-level names
unit_hasGeneratedNames_2 :: Assertion
unit_hasGeneratedNames_2 = do
  hasGeneratedNamesTy tEmptyHole Nothing pure ["α", "β", "γ"]
  hasGeneratedNamesTy tEmptyHole (Just (KType ())) pure ["α", "β", "γ"]
  hasGeneratedNamesTy tEmptyHole (Just $ KFun () (KType ()) (KType ())) pure ["f", "m", "t"]
  let ty = tforall "α" ktype tEmptyHole
  hasGeneratedNamesTy ty Nothing pure ["β", "γ", "α1"]
  hasGeneratedNamesTy ty (Just (KType ())) pure ["β", "γ", "α1"]
  hasGeneratedNamesTy ty (Just $ KFun () (KType ()) (KType ())) pure ["f", "m", "t"]

-- test uniqueness works correctly wrt branching
unit_hasGeneratedNames_3 :: Assertion
unit_hasGeneratedNames_3 = do
  let expr = lam "x" $ app emptyHole $ lam "y" emptyHole
  hasGeneratedNamesExpr expr Nothing pure ["z", "x1", "y1"]
  hasGeneratedNamesExpr expr Nothing down ["z", "x1", "y1"]
  hasGeneratedNamesExpr expr Nothing (down >=> down) ["y", "z", "x1"]
  hasGeneratedNamesExpr expr Nothing (down >=> down >=> right) ["z", "x1", "y1"]

defCxt :: Cxt
defCxt = buildTypingContextFromModules' [builtinModule] NoSmartHoles

hasGeneratedNamesExpr :: S Expr -> Maybe (S Type) -> (ExprZ -> Maybe ExprZ) -> [Name] -> Assertion
hasGeneratedNamesExpr expr ty path expected = do
  let (e, t) = create' $ (,) <$> expr <*> sequence ty
  case path $ focus e of
    Just z -> runReader (generateNameExpr (Left $ fmap forgetTypeMetadata t) (InExpr z)) defCxt @?= expected
    Nothing -> assertFailure ""

hasGeneratedNamesTy :: S Type -> Maybe (Kind' ()) -> (TypeZip -> Maybe TypeZip) -> [Name] -> Assertion
hasGeneratedNamesTy ty k path expected = do
  let t = create' ty
  case path $ focus t of
    Just z -> runReader (generateNameTy (Right k) (Left z)) defCxt @?= expected
    Nothing -> assertFailure ""
