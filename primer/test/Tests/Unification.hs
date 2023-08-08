module Tests.Unification where

import Foreword hiding (diff)

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Data.Set qualified as S
import Hedgehog (
  GenT,
  PropertyT,
  annotateShow,
  assert,
  diff,
  discard,
  failure,
  (===),
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Builtins (tList, tNat)
import Primer.Core (
  ID,
  Kind' (KFun, KHole, KType),
  TyVarName,
  Type' (TApp, TCon, TEmptyHole, TForall, TFun, THole, TVar),
 )
import Primer.Core.DSL (S)
import Primer.Core.Utils (forgetTypeMetadata, freeVarsTy, generateTypeIDs)
import Primer.Gen.Core.Typed (
  WT,
  forAllT,
  freshLVarNameForCxt,
  freshTyVarNameForCxt,
  genCxtExtendingGlobal,
  genWTKind,
  genWTType,
  propertyWT,
 )
import Primer.Module (Module, builtinModule, primitiveModule)
import Primer.Name (NameCounter)
import Primer.Primitives (tInt)
import Primer.Subst (substTySimul)
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (tcn)
import Primer.TypeDef (ASTTypeDef (ASTTypeDef, astTypeDefConstructors, astTypeDefNameHints, astTypeDefParameters), TypeDef (TypeDefAST))
import Primer.Typecheck (
  Cxt,
  Kind,
  SmartHoles (NoSmartHoles),
  Type,
  buildTypingContextFromModules',
  consistentTypes,
  extendLocalCxt,
  extendLocalCxtTy,
  extendTypeDefCxt,
 )
import Primer.Unification (unify)
import Tasty (Property, withDiscards)
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Tests.Gen.Core.Typed (
  checkKindTest,
  checkValidContextTest,
  propertyWTInExtendedLocalGlobalCxt,
  synthKindTest,
 )

defaultCxt :: Cxt
defaultCxt = buildTypingContextFromModules' [builtinModule, primitiveModule] NoSmartHoles

unify' ::
  (MonadFresh NameCounter m, MonadFresh ID m) =>
  Cxt ->
  S.Set TyVarName ->
  Type ->
  Type ->
  m (Maybe (M.Map TyVarName Type))
unify' cxt uvs s t = fmap (either crash identity) $ runExceptT $ unify cxt uvs s t
  where
    -- If we run across a bug whilst testing, crash loudly
    crash = panic . ("InternalUnifyError: " <>) . show

-- unify [] [] Int Int = Just []
unit_Int_refl :: Assertion
unit_Int_refl =
  evalTestM
    0
    ( unify'
        defaultCxt
        mempty
        (TCon () tInt)
        (TCon () tInt)
    )
    @?= Just mempty

unit_diff_module_not_refl :: Assertion
unit_diff_module_not_refl =
  evalTestM
    0
    ( unify'
        (extendTypeDefCxt (M.singleton (tcn ["M"] "Int") mint) defaultCxt)
        mempty
        (TCon () tInt)
        (TCon () $ tcn ["M"] "Int")
    )
    @?= Nothing
  where
    mint =
      TypeDefAST $
        ASTTypeDef
          { astTypeDefParameters = mempty
          , astTypeDefConstructors = mempty
          , astTypeDefNameHints = mempty
          }

-- unify [...,a:*] [] a a = Just []
unit_a_refl :: Assertion
unit_a_refl =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        mempty
        (TVar () "a")
        (TVar () "a")
    )
    @?= Just mempty

-- unify [...,a:*] [] a Nat = Nothing
unit_var_con :: Assertion
unit_var_con =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        mempty
        (TVar () "a")
        (TCon () tNat)
    )
    @?= Nothing

-- unify [...,a:*] [a] a Nat = Just [Nat/a]
unit_unif_var_con :: Assertion
unit_unif_var_con =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TCon () tNat)
    )
    @?= Just (M.singleton "a" $ TCon () tNat)

-- unify [...,a:*] [a] a a = Just []
unit_unif_var_refl :: Assertion
unit_unif_var_refl =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TVar () "a")
    )
    @?= Just mempty

-- unify [...,a:*->*] [a] a Nat = Nothing (but note stupid input: should have k∋S and k∋T for same k)
unit_ill_kinded :: Assertion
unit_ill_kinded =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KFun () (KType ()) (KType ())) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TCon () tNat)
    )
    @?= Nothing

-- unify [...,a:*] [a] a (List Nat) = Just [List Nat/a]
unit_List_Nat :: Assertion
unit_List_Nat =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TApp () (TCon () tList) (TCon () tNat))
    )
    @?= Just (M.singleton "a" $ TApp () (TCon () tList) (TCon () tNat))

-- unify [...,a:*] [a] (List a) (List Nat) = Just [Nat/a]
unit_List :: Assertion
unit_List =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TApp () (TCon () tList) (TVar () "a"))
        (TApp () (TCon () tList) (TCon () tNat))
    )
    @?= Just (M.singleton "a" $ TCon () tNat)

-- unify [...,a:*] [a] (List Nat) (List Nat) = Just []
unit_List_Nat_refl :: Assertion
unit_List_Nat_refl =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TApp () (TCon () tList) (TCon () tNat))
        (TApp () (TCon () tList) (TCon () tNat))
    )
    @?= Just mempty

-- unify [...,a:*->*] [a] (a Nat) (List Nat) = Just [List/a]
unit_higher_kinded :: Assertion
unit_higher_kinded =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KFun () (KType ()) (KType ())) defaultCxt)
        (S.singleton "a")
        (TApp () (TVar () "a") (TCon () tNat))
        (TApp () (TCon () tList) (TCon () tNat))
    )
    @?= Just (M.singleton "a" $ TCon () tList)

-- unify [...<NO 'a' HERE>] [a] (? List) (List a) fails, as 'a' is not in the context
-- In particular, it does not succeed with the nonsense [List/a], as we throw
-- an error due to not knowing how to kind check the solution for 'a'
unit_ill_kinded_0 :: Assertion
unit_ill_kinded_0 =
  let res =
        evalTestM
          0
          ( runExceptT $
              unify
                defaultCxt
                (S.singleton "a")
                (TApp () (TEmptyHole ()) (TCon () tList))
                (TApp () (TCon () tList) (TVar () "a"))
          )
   in assertBool "Should have detected a unification variable was not in the context" $ isLeft res

-- unify [...,b:*,<NO 'a' HERE>] [a,b] Nat b succeeds, even though 'a' is not
-- in the context, because it is unconstrained (c.f. unit_ill_kinded_0).
unit_uv_not_in_context :: Assertion
unit_uv_not_in_context =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("b", KType ()) defaultCxt)
        (S.fromList ["a", "b"])
        (TCon () tNat)
        (TVar () "b")
    )
    @?= Just (M.singleton "b" $ TCon () tNat)

-- unify [...,a:*] [a] (? List) (List a) = Nothing
unit_ill_kinded_1 :: Assertion
unit_ill_kinded_1 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TApp () (TEmptyHole ()) (TCon () tList))
        (TApp () (TCon () tList) (TVar () "a"))
    )
    @?= Nothing

-- unify [...,a:*->*] [a] (? List) (List a) = Just [List/a]
-- This shows that we don't keep track of the kinds of holes: the
-- ill-kindedness here is not noticed!
unit_ill_kinded_2 :: Assertion
unit_ill_kinded_2 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KFun () (KType ()) (KType ())) defaultCxt)
        (S.singleton "a")
        (TApp () (TEmptyHole ()) (TCon () tList))
        (TApp () (TCon () tList) (TVar () "a"))
    )
    @?= Just (M.singleton "a" $ TCon () tList)

-- unify [...,a:*,b:*->*] [a] a b = Nothing
unit_ill_kinded_3 :: Assertion
unit_ill_kinded_3 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("b", KFun () (KType ()) (KType ())) $ extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TVar () "b")
    )
    @?= Nothing

-- unify [...,a:*,b:*->*] [a,b] a b = Nothing
unit_ill_kinded_4 :: Assertion
unit_ill_kinded_4 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("b", KFun () (KType ()) (KType ())) $ extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.fromList ["a", "b"])
        (TVar () "a")
        (TVar () "b")
    )
    @?= Nothing

-- unify [...,a:*,b:*] [a] a b = Just [b/a]
unit_unify_uv_uv_1 :: Assertion
unit_unify_uv_uv_1 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("b", KType ()) $ extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TVar () "b")
    )
    @?= Just (M.singleton "a" $ TVar () "b")

-- unify [...,a:*,b:*] [a,b] a b = Just [b/a]
unit_unify_uv_uv_2 :: Assertion
unit_unify_uv_uv_2 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("b", KType ()) $ extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.fromList ["a", "b"])
        (TVar () "a")
        (TVar () "b")
    )
    @?= Just (M.singleton "a" $ TVar () "b")

-- unify [...,a:*] [a] a (a -> ?) = Nothing
unit_unify_occurs :: Assertion
unit_unify_occurs =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TVar () "a")
        (TFun () (TVar () "a") (TEmptyHole ()))
    )
    @?= Nothing

-- unify [...,a:*] [a] (∀a.a) (∀b.a) = Nothing
unit_unify_forall :: Assertion
unit_unify_forall =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TForall () "a" (KType ()) $ TVar () "a")
        (TForall () "b" (KType ()) $ TVar () "a")
    )
    @?= Nothing

-- Holes trivially unify: unify [...] [] ? {? Nat ?} == Just []
unit_unify_hole_trivial_1 :: Assertion
unit_unify_hole_trivial_1 =
  evalTestM
    0
    ( unify'
        defaultCxt
        mempty
        (TEmptyHole ())
        (THole () $ TCon () tNat)
    )
    @?= Just mempty

-- Holes trivially unify: unify [...,a:*] [] ? a == Just []
unit_unify_hole_trivial_2 :: Assertion
unit_unify_hole_trivial_2 =
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TEmptyHole ())
        (TVar () "a")
    )
    @?= Just mempty

-- Forall-bound vars shadow unification variables
--   unify [a:*] [a] (∀b.Nat -> Nat) (∀a.a -> Nat) == Nothing
-- in the RHS, the uv is shadowed. We must not report a solution of a := Nat !
unit_unify_shadow :: Assertion
unit_unify_shadow = do
  evalTestM
    0
    ( unify'
        (extendLocalCxtTy ("a", KType ()) defaultCxt)
        (S.singleton "a")
        (TForall () "b" (KType ()) $ TFun () (TCon () tNat) (TCon () tNat))
        (TForall () "a" (KType ()) $ TFun () (TVar () "a") (TCon () tNat))
    )
    @?= Nothing

-- Generate an extension of the base context (from the reader monad) with more
-- local term and type vars, some of which are unif vars.
genCxtExtendingLocalUVs :: GenT WT (Cxt, M.Map TyVarName Kind)
genCxtExtendingLocalUVs = do
  n <- Gen.int $ Range.linear 0 20
  go n mempty
  where
    go 0 uvs = asks (,uvs)
    go i uvs = do
      (uvsE, cxtE) <-
        Gen.choice
          [ (\n k -> (identity, extendLocalCxtTy (n, k))) <$> freshTyVarNameForCxt <*> genWTKind
          , (\n k -> ((M.singleton n k <>), extendLocalCxtTy (n, k))) <$> freshTyVarNameForCxt <*> genWTKind
          , (\n t -> (identity, extendLocalCxt (n, t))) <$> freshLVarNameForCxt <*> genWTType (KType ())
          ]
      local cxtE $ go (i - 1) $ uvsE uvs

-- Run a property in a context extended with typedefs, globals and locals. Some
-- of the locals (mentioned in the Set) are considered unification variables.
propertyWTInExtendedUVCxt' :: [S Module] -> (M.Map TyVarName Kind -> PropertyT WT ()) -> Property
propertyWTInExtendedUVCxt' mods p = propertyWT mods $ do
  cxtG <- forAllT genCxtExtendingGlobal
  local (const cxtG) $ do
    (cxtL, uvs) <- forAllT genCxtExtendingLocalUVs
    annotateShow uvs
    local (const cxtL) $ p uvs

propertyWTInExtendedUVCxt :: [S Module] -> (S.Set TyVarName -> PropertyT WT ()) -> Property
propertyWTInExtendedUVCxt mods p = propertyWTInExtendedUVCxt' mods $ p . M.keysSet

tasty_extendedUVCxt_typechecks :: Property
tasty_extendedUVCxt_typechecks = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \_ ->
  checkValidContextTest =<< ask

-- unify _ _ T T  is Just []
tasty_refl :: Property
tasty_refl = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  t <- forAllT $ genWTType k
  u <- unify' cxt uvs t t
  u === Just mempty

-- unify _ [] S T  is Nothing or Just [], exactly when S = T up to holes
tasty_eq :: Property
tasty_eq = propertyWTInExtendedLocalGlobalCxt [builtinModule, primitiveModule] $ do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt mempty s t
  let con = consistentTypes s t
  case u of
    Nothing -> assert $ not con
    Just m | M.null m -> assert con
    Just _ -> failure
  u === if con then Just mempty else Nothing

-- unify ga uvs S T = Maybe sub => sub <= uvs
tasty_only_sub_uvs :: Property
tasty_only_sub_uvs = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt uvs s t
  case u of
    Nothing -> discard
    Just sub -> assert $ M.keysSet sub `S.isSubsetOf` uvs

-- unify ga uvs S T = Maybe sub => S[sub] = T[sub]
tasty_sub_unifies :: Property
tasty_sub_unifies = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt uvs s t
  case u of
    Nothing -> discard
    Just sub -> do
      s' <- substTySimul sub s
      t' <- substTySimul sub t
      diff s' consistentTypes t'

-- unify ga uvs S T = Maybe sub => for t/a in sub, have checkKind uvs(a) t
tasty_sub_checks :: Property
tasty_sub_checks = propertyWTInExtendedUVCxt' [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt (M.keysSet uvs) s t
  case u of
    Nothing -> discard
    Just sub -> do
      forM_ (M.toList sub) $ \(n, sb) -> do
        sb' <- checkKindTest (uvs M.! n) =<< generateTypeIDs sb
        sb === forgetTypeMetadata sb' -- check no smartholes happened

-- (S,T kind check and) unify ga uvs S T = Maybe sub => S[sub] , T[sub] kind check
tasty_unified_checks :: Property
tasty_unified_checks = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt uvs s t
  case u of
    Nothing -> discard
    Just sub -> do
      s' <- substTySimul sub s
      t' <- substTySimul sub t
      s'' <- checkKindTest k =<< generateTypeIDs s'
      s' === forgetTypeMetadata s'' -- check no smartholes happened
      t'' <- checkKindTest k =<< generateTypeIDs t'
      t' === forgetTypeMetadata t'' -- check no smartholes happened

-- S,T diff kinds => unify ga uvs S T fails
-- This requires each to not be holey - i.e. don't synthesise KHole
tasty_diff_kinds_never_unify :: Property
tasty_diff_kinds_never_unify = withDiscards 5000 $
  propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
    cxt <- ask
    k1 <- forAllT genWTKind
    k2 <- forAllT genWTKind
    when (k1 == k2) discard
    s <- forAllT $ genWTType k1
    t <- forAllT $ genWTType k2
    (sk, _) <- synthKindTest =<< generateTypeIDs s
    when (sk == KHole ()) discard
    (tk, _) <- synthKindTest =<< generateTypeIDs t
    when (tk == KHole ()) discard
    u <- unify' cxt uvs s t
    u === Nothing

-- unification is symmetric
tasty_sym :: Property
tasty_sym = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u1 <- unify' cxt uvs s t
  u2 <- unify' cxt uvs t s
  u1 === u2

-- the sub should be "grounded", "idempotent", or "non-cyclic", i.e. any sub should
-- need to be applied only once. I.e. there are no solved unification
-- variables in the solution of any u.v.
tasty_non_cyclic :: Property
tasty_non_cyclic = propertyWTInExtendedUVCxt [builtinModule, primitiveModule] $ \uvs -> do
  cxt <- ask
  k <- forAllT genWTKind
  s <- forAllT $ genWTType k
  t <- forAllT $ genWTType k
  u <- unify' cxt uvs s t
  case u of
    Nothing -> discard
    Just u' ->
      let solved = M.keysSet u'
          inRHS = foldMap' freeVarsTy u'
       in assert $ S.disjoint solved inRHS

-- unifying a unif var gives simple success
tasty_uv_succeeds :: Property
tasty_uv_succeeds = propertyWT [builtinModule, primitiveModule] $ do
  k <- forAllT genWTKind
  t <- forAllT $ genWTType k
  uv <- forAllT freshTyVarNameForCxt
  local (extendLocalCxtTy (uv, k)) $ do
    cxt <- ask
    u <- unify' cxt (S.singleton uv) (TVar () uv) t
    case t of
      -- Holes trivially match with anything, so the uv is unconstrained
      TEmptyHole{} -> u === Just mempty
      THole{} -> u === Just mempty
      _ -> u === Just (M.singleton uv t)
