{-# LANGUAGE TupleSections #-}

module Tests.Refine where

import Foreword hiding (diff)

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as M
import qualified Data.Set as S
import Gen.Core.Typed (
  forAllT,
  freshTyVarNameForCxt,
  genInstApp,
  genWTKind,
  genWTType,
 )
import Hedgehog (
  Property,
  annotateShow,
  diff,
  discard,
  failure,
  success,
  withDiscards,
  (===),
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Primer.App (defaultTypeDefs)
import Primer.Core (
  Expr' (APP, Ann, App, EmptyHole),
  ID,
  Kind (KFun, KType),
  Type' (TApp, TCon, TEmptyHole, TForall, TFun, THole, TVar),
  astTypeDefConstructors,
  astTypeDefName,
  astTypeDefParameters,
  typeDefAST,
  valConType,
 )
import Primer.Core.Utils (forgetIDs, freeVarsTy, generateIDs, noHoles)
import Primer.Name (NameCounter)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.Subst (substTy, substTys)
import Primer.Typecheck (
  Cxt,
  SmartHoles (NoSmartHoles),
  Type,
  buildTypingContext,
  consistentTypes,
  extendLocalCxtTy,
  typeDefs,
 )
import Test.Tasty.HUnit (Assertion, (@?=))
import TestM (evalTestM)
import Tests.Gen.Core.Typed (propertyWTInExtendedLocalGlobalCxt, synthTest)

defaultCxt :: Cxt
defaultCxt = buildTypingContext defaultTypeDefs mempty NoSmartHoles

refine' :: (MonadFresh NameCounter m, MonadFresh ID m) => Cxt -> Type -> Type -> m (Maybe ([Inst], Type))
refine' cxt s t = fmap (either crash identity) $ runExceptT $ refine cxt s t
  where
    -- If we run across a bug whilst testing, crash loudly
    crash = panic . ("InternalUnifyError: " <>) . show

-- refine [...,a:*] a a = Just ([],a)
unit_var_refl :: Assertion
unit_var_refl =
  evalTestM
    0
    ( refine'
        (extendLocalCxtTy ("a", KType) defaultCxt)
        (TVar () "a")
        (TVar () "a")
    )
    @?= Just ([], TVar () "a")

-- refine [...] Nat Nat succeeds
unit_con_refl :: Assertion
unit_con_refl =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TCon () "Nat")
    )
    @?= Just ([], TCon () "Nat")

-- refine [...] Nat Bool  fails
unit_distinct_con :: Assertion
unit_distinct_con =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TCon () "Bool")
    )
    @?= Nothing

-- refine [...] Nat (Bool -> Nat) succeeds: need to instantiate with a Bool
unit_instApp :: Assertion
unit_instApp =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TFun () (TCon () "Bool") (TCon () "Nat"))
    )
    @?= Just ([InstApp $ TCon () "Bool"], TCon () "Nat")

-- refine [...] Nat (∀a.Nat) succeeds: have an unconstraind APP to do
unit_instUnconstrainedAPP :: Assertion
-- NB: fragile name "a1" here
unit_instUnconstrainedAPP =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TForall () "a" KType (TCon () "Nat"))
    )
    @?= Just ([InstUnconstrainedAPP "a1" KType], TCon () "Nat")

-- refine [...] Nat (∀a.a) succeeds: have an APP Nat to do
unit_instAPP :: Assertion
unit_instAPP =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TForall () "a" KType (TVar () "a"))
    )
    @?= Just ([InstAPP $ TCon () "Nat"], TCon () "Nat")

-- refine [...] Nat (∀a.List a) fails
unit_forall_fail :: Assertion
unit_forall_fail =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TForall () "a" KType $ TApp () (TCon () "List") (TVar () "a"))
    )
    @?= Nothing

-- refine [...] Nat (∀a:*->*. a) fails (note that the input is ill-kinded)
unit_ill_kinded_fail :: Assertion
unit_ill_kinded_fail =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TCon () "Nat")
        (TForall () "a" (KFun KType KType) $ TVar () "a")
    )
    @?= Nothing

-- refine [...] (? List) (∀a:*. List a) fails as the only "solution" is a=List, which is ill-kinded
unit_ill_kinded_fail_2 :: Assertion
unit_ill_kinded_fail_2 =
  evalTestM
    0
    ( refine'
        defaultCxt
        (TApp () (TEmptyHole ()) (TCon () "List"))
        (TForall () "a" KType $ TApp () (TCon () "List") (TVar () "a"))
    )
    @?= Nothing

-- refine [...] (∀a. List a) (∀b. List b) succeeds, trivially
unit_alpha :: Assertion
unit_alpha =
  let t n = (TForall () n KType $ TApp () (TCon () "List") (TVar () n))
   in evalTestM 0 (refine' defaultCxt (t "a") (t "b")) @?= Just ([], t "b")

-- refine cxt T T succeeds
hprop_refl :: Property
hprop_refl = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  k <- forAllT genWTKind
  ty <- forAllT $ genWTType k
  cxt <- ask
  r <- refine' cxt ty ty
  r === Just ([], ty)

-- refine _ ? S succeeds
hprop_tgt_hole :: Property
hprop_tgt_hole = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  k <- forAllT genWTKind
  tgt <- forAllT $ Gen.choice [pure $ TEmptyHole (), THole () <$> genWTType k]
  src <- forAllT $ genWTType k
  cxt <- ask
  r <- refine' cxt tgt src
  r === Just ([], src)

-- refine _ T ? succeeds
hprop_src_hole :: Property
hprop_src_hole = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  k <- forAllT genWTKind
  tgt <- forAllT $ genWTType k
  src <- forAllT $ Gen.choice [pure $ TEmptyHole (), THole () <$> genWTType k]
  cxt <- ask
  r <- refine' cxt tgt src
  r === Just ([], src)

-- constructor types refine to their fully-applied typedef
hprop_con :: Property
hprop_con = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  tcs <- asks $ mapMaybe typeDefAST . M.elems . typeDefs
  -- NB: this only works because defaultCxt has at least one tydef with a constructor
  td <- forAllT $ Gen.element tcs
  let cons = astTypeDefConstructors td
  when (null cons) discard
  vc <- forAllT $ Gen.element cons
  let src = valConType td vc
  annotateShow src
  tgt' <- forAllT $ traverse (genWTType . snd) $ astTypeDefParameters td
  let tgt = foldl (TApp ()) (TCon () $ astTypeDefName td) tgt'
  annotateShow tgt
  cxt <- ask
  r <- refine' cxt tgt src
  case r of
    Nothing -> failure
    Just _ -> success

-- refine cxt T S succeds when S is built from T, S1 -> _, ∀a._
-- The success may not instantiate as much as one would expect, if T has holes in
hprop_arr_app :: Property
hprop_arr_app = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  tgt <- forAllT $ genWTType KType
  when (isHole tgt) discard
  src' <- forAllT $ Gen.list (Range.linear 0 10) $ Gen.choice [Left <$> genWTType KType, curry Right <$> freshTyVarNameForCxt <*> genWTKind]
  let src = foldr (\case Left t -> TFun () t; Right (n, k) -> TForall () n k) tgt src'
  annotateShow src
  let inst = map (\case Left t -> InstApp t; Right (n, k) -> InstUnconstrainedAPP n k) src'
  cxt <- ask
  r <- refine' cxt tgt src
  annotateShow r
  case r of
    Nothing -> failure
    Just (inst', ty) -> do
      diff inst' instSub inst
      when (noHoles tgt) $ length inst' === length inst >> ty === tgt
  where
    instSub is is' = length is <= length is' && and (zipWith instEq' is is')
    instEq' i j = case (i, j) of
      (InstApp t, InstApp s) -> t == s
      (InstUnconstrainedAPP _ k1, InstUnconstrainedAPP _ k2) -> k1 == k2
      _ -> False

-- if refine _ T S = Just (I:IS,_) , then refine _ T (S $ I) = Just (IS,_); here "S $ I" means "inspect S, I assert they match and strip off a layer"
hprop_matches :: Property
hprop_matches = withDiscards 2000 $
  propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
    tgt <- forAllT $ genWTType KType
    src <- forAllT $ genWTType KType
    cxt <- ask
    r <- refine' cxt tgt src
    annotateShow r
    case r of
      Just (i : is, _) -> do
        (cxtExt, s') <- inst1 src i
        annotateShow s'
        r' <- refine' (cxtExt cxt) tgt s'
        annotateShow r'
        case r' of
          Just (is', _) -> length is === length is' -- Not being precise here, because namings differ between is and is'
          _ -> failure
      _ -> discard
  where
    inst1 (TFun _ s t) (InstApp s') | s == s' = pure (identity, t)
    inst1 (TForall _ a k t) (InstUnconstrainedAPP b k') | k == k' = (extendLocalCxtTy (b, k),) <$> substTy a (TVar () b) t
    inst1 (TForall _ a _ t) (InstAPP s) = (identity,) <$> substTy a s t
    inst1 _ _ = failure

-- if refine cxt tgt s = Just (is,ty)   =>  (? : s) $ <stuff checking against is>  ∈ ty[instantiation vars substituted appropriately] ~ tgt
hprop_refinement_synths :: Property
hprop_refinement_synths = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  cxt <- ask
  r <- refine' cxt tgt src
  annotateShow r
  case r of
    Just (is, instTy) -> do
      (_, apps) <- forAllT $ genInstApp is
      let f x = \case Right tm -> App () x tm; Left ty' -> APP () x ty'
          e = foldl f (Ann () (EmptyHole ()) src) apps
      annotateShow e
      (ty, e') <- synthTest =<< generateIDs e
      e === forgetIDs e' -- check no smart holes stuff happened
      let g i a = case (i, a) of (InstUnconstrainedAPP n _, Left t) -> Just (n, t); _ -> Nothing
          sb = catMaybes $ zipWith g is apps
      instTy' <- substTys sb instTy
      ty === instTy'
      diff ty consistentTypes tgt
    _ -> discard

-- | (Because unif vars are only in one side) the names from
-- 'InstUnconstrainedAPP' do not appear in 'InstAPP's (but can in 'InstApp's)
hprop_scoping :: Property
hprop_scoping = propertyWTInExtendedLocalGlobalCxt defaultCxt $ do
  tgt <- forAllT $ genWTType KType
  src <- forAllT $ genWTType KType
  cxt <- ask
  r <- refine' cxt tgt src
  annotateShow r
  case r of
    Just (is, _) -> do
      let ns = S.fromList $ mapMaybe unconstrName is
          ts = mapMaybe aPPTy is
          fvs = mconcat $ map freeVarsTy ts
      ns `S.intersection` fvs === mempty
    _ -> discard
  where
    unconstrName = \case
      InstUnconstrainedAPP n _ -> Just n
      _ -> Nothing
    aPPTy = \case
      InstAPP t -> Just t
      _ -> Nothing

isHole :: Type' a -> Bool
isHole (TEmptyHole _) = True
isHole (THole _ _) = True
isHole _ = False
