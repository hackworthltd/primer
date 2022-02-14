-- ApplicativeDo: generators shrink much better if applicative (though much of
-- this module is inherently monadic)
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module generates well-typed terms and types.
-- It is however, slow and the distribution is not very even.
--
-- For quickly generating non-well-typed-or-scoped terms, see "Gen.Core.Raw".
module Gen.Core.Typed (
  WT,
  isolateWT,
  genWTType,
  genWTKind,
  genSyns,
  genSyn,
  genChk,
  genInstApp,
  genCxtExtendingGlobal,
  genCxtExtendingLocal,
  genPrimCon,
  forAllT,
  propertyWT,
  freshNameForCxt,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (mapReaderT)
import qualified Data.Map as M
import Hedgehog (
  GenT,
  MonadGen,
  Property,
  PropertyT,
  property,
 )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range as Range
import Primer.Core (
  ASTTypeDef (..),
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  Expr' (..),
  ID,
  Kind (..),
  PrimCon (..),
  Type' (..),
  TypeDef (..),
  ValCon (..),
  primConName,
  typeDefKind,
  typeDefName,
  typeDefParameters,
  valConName,
  valConType,
 )
import Primer.Name (Name, NameCounter, freshName)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.Subst (substTy, substTys)
import Primer.Typecheck (
  Cxt (),
  KindOrType (K, T),
  TypeDefError (TDIHoleType),
  consistentKinds,
  extendLocalCxt,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxts,
  getGlobalNames,
  globalCxt,
  instantiateValCons,
  localCxt,
  localTmVars,
  matchArrowType,
  matchForallType,
  mkTypeDefMap,
  typeDefs,
 )
import TestM (TestM, evalTestM, isolateTestM)

{-
Generate well scoped and typed expressions.
We run in a GenT WT monad, so we have a Reader Cxt and a TestM in our monad
stack when running generators. We are using the TC's Cxt to keep track of what
is in scope, but ignore the smartholes-ness. The TestM satisfies MonadFresh
constraints, which are only used for generating fresh names. Since it is too
awkward to generate correct TypeCache information, and IDs are not needed
(other than global variables) except for communication with the frontend, we
generate un-adorned types and expressions. Unfortunately, some bits of the
backend (especially the typechecker) work in terms of annotated
types/expressions, but it is easy to have a post-processing step of adding IDs
and empty TypeCaches to everything.
-}

type TypeG = Type' ()

type ExprG = Expr' () ()

newtype WT a = WT {unWT :: ReaderT Cxt TestM a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Cxt
    , MonadFresh NameCounter
    , MonadFresh ID
    )

-- | Run an action and ignore any effect on the fresh name/id state
isolateWT :: WT a -> WT a
isolateWT x = WT $ mapReaderT isolateTestM $ unWT x

instance MonadFresh NameCounter (GenT WT) where
  fresh = lift fresh

instance MonadFresh ID (GenT WT) where
  fresh = lift fresh

instance MonadFresh NameCounter (PropertyT WT) where
  fresh = lift fresh

instance MonadFresh ID (PropertyT WT) where
  fresh = lift fresh

freshNameForCxt :: GenT WT Name
freshNameForCxt = do
  globs <- getGlobalNames
  locals <- asks $ M.keysSet . localCxt
  freshName $ globs <> locals

-- genSyns T with cxt Γ should generate (e,S) st Γ |- e ∈ S and S ~ T (i.e. same up to holes and alpha)
genSyns :: TypeG -> GenT WT (ExprG, TypeG)
genSyns ty = do
  genSpine' <- lift genSpine
  Gen.recursive Gen.choice [genEmptyHole, genAnn] [genHole, genApp, genAPP, genLet, genSpine']
  where
    genEmptyHole = pure (EmptyHole (), TEmptyHole ())
    genAnn = do
      t <- genChk ty
      pure (Ann () t ty, ty)
    genHole = do
      (e, _) <- genSyn
      pure (Hole () e, TEmptyHole ())
    genSpine :: WT (GenT WT (ExprG, TypeG))
    genSpine = fmap Gen.justT genSpineHeadFirst
    genSpineHeadFirst :: WT (GenT WT (Maybe (ExprG, TypeG)))
    -- todo: maybe add some lets in as post-processing? I could even add them to the locals for generation in the head
    genSpineHeadFirst = do
      localTms <- asks localTmVars
      let locals' = map (first (Var ())) $ M.toList localTms
      globals <- asks $ M.map snd . globalCxt
      let globals' = map (first (GlobalVar ())) $ M.toList globals
      cons <- asks allCons
      let cons' = map (first (Con ())) $ M.toList cons
      let hs = locals' ++ globals' ++ cons'
      pure $ do
        primCons <- (\con -> (PrimCon () con, TCon () $ primConName con)) <<$>> genPrimCon
        (he, hT) <- Gen.element $ hs ++ primCons
        cxt <- ask
        runExceptT (refine cxt ty hT) >>= \case
          -- This error case indicates a bug. Crash and fail loudly!
          Left err -> panic $ "Internal refine/unify error: " <> show err
          Right Nothing -> pure Nothing
          Right (Just (inst, instTy)) -> do
            (sb, is) <- genInstApp inst
            let f e = \case Right tm -> App () e tm; Left ty' -> APP () e ty'
            Just . (foldl f he is,) <$> substTys sb instTy
    genApp = do
      s <- genWTType KType
      (f, fTy) <- genSyns (TFun () s ty)
      (s', t') <- maybe Gen.discard pure $ matchArrowType fTy -- discard should never happen: fTy should be consistent with (TFun _ s ty)
      a <- App () f <$> genChk s'
      pure (a, t')
    -- APPs are difficult. We take the approach of throwing stuff at the wall and seeing what sticks...
    genAPP = justT $ do
      k <- genWTKind
      n <- freshNameForCxt
      (s, sTy) <- genSyns $ TForall () n k $ TEmptyHole ()
      cxt <- ask
      runExceptT (refine cxt ty sTy) >>= \case
        Right (Just ([InstAPP aTy], instTy)) -> pure $ Just (APP () s aTy, instTy)
        Right (Just ([InstUnconstrainedAPP a ak], instTy)) -> do
          aTy <- genWTType ak
          Just . (APP () s aTy,) <$> substTy a aTy instTy
        _ -> pure Nothing
    genLet =
      Gen.choice
        [ -- let
          do
            x <- freshNameForCxt
            (e, eTy) <- genSyn
            (f, fTy) <- local (extendLocalCxt (x, eTy)) $ genSyns ty
            pure (Let () x e f, fTy)
        , -- letrec
          do
            x <- freshNameForCxt
            eTy <- genWTType KType
            (e, (f, fTy)) <- local (extendLocalCxt (x, eTy)) $ (,) <$> genChk eTy <*> genSyns ty
            pure (Letrec () x e eTy f, fTy)
            -- lettype
            {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               (lettype a = Nat -> Nat in λx.x : a), for instance
               See https://github.com/hackworthltd/primer/issues/5
            do
              x <- freshNameForCxt
              k <- genWTKind
              t <- genWTType k
              (e, eTy) <- local (extendLocalCxtTy (x, k)) $ genSyns ty
              pure (LetType () x t e, eTy)
            -}
        ]

-- | Similar to 'Gen.justT', but doesn't resize the generator.
-- (But thus won't make progress if the reason for failure is due to the size being too small.)
-- There is a problem using recursive generators with 'just', e.g. the rather odd
--
-- > gen = Gen.recursive Gen.choice [Gen.int $ Range.linear 0 10] [Gen.just $ gen *> pure Nothing]
--
-- (This basic pattern happens with the @genAPP@ case of 'genSyns'.)
-- One would expect this to behave as
--
-- > Gen.frequency [(p,Gen.int $ Range.linear 0 10),(q,Gen.discard)]
--
-- for some @p@, @q@ but much less efficient.
-- In particular, one would expect the size should decrease on each recursion,
-- limiting the depth of the search tree, and then the recursive cases should
-- eventually all give up. However, 'Gen.just' also scales the size up (by more
-- than recursive scales it down), and this can lead to non-termination, and
-- out-of-memory issues.
justT :: MonadGen m => m (Maybe a) -> m a
justT g = Gen.sized $ \s -> Gen.justT $ Gen.resize s g

genInstApp :: [Inst] -> GenT WT ([(Name, Type' ())], [Either TypeG ExprG])
genInstApp = reify []
  where
    reify sb = \case
      [] -> pure (sb, [])
      InstApp t : is -> (\a -> second (Right a :)) <$> (substTys sb t >>= genChk) <*> reify sb is
      InstAPP t : is -> (\t' -> second (Left t' :)) <$> substTys sb t <*> reify sb is
      InstUnconstrainedAPP v k : is -> genWTType k >>= \t' -> second (Left t' :) <$> reify ((v, t') : sb) is

genSyn :: GenT WT (ExprG, TypeG)
-- Note that genSyns will generate things consistent with the given type, i.e.
-- of any type
genSyn = genSyns (TEmptyHole ())

allCons :: Cxt -> M.Map Name (Type' ())
allCons cxt = M.fromList $ concatMap consForTyDef $ M.elems $ typeDefs cxt
  where
    consForTyDef = \case
      TypeDefAST td -> map (\vc -> (valConName vc, valConType td vc)) (astTypeDefConstructors td)
      TypeDefPrim _ -> []

genChk :: TypeG -> GenT WT ExprG
genChk ty = do
  cse <- lift case_
  abst' <- lift abst
  let rec = genLet : catMaybes [lambda, abst', cse]
  Gen.recursive Gen.choice [emb] rec
  where
    emb = fst <$> genSyns ty
    lambda =
      matchArrowType ty <&> \(sTy, tTy) -> do
        n <- freshNameForCxt
        Lam () n <$> local (extendLocalCxt (n, sTy)) (genChk tTy)
    abst = do
      mfa <- matchForallType ty
      pure $
        mfa <&> \(n, k, t) -> do
          m <- freshNameForCxt
          ty' <- substTy n (TVar () m) t
          LAM () m <$> local (extendLocalCxtTy (m, k)) (genChk ty')
    genLet =
      Gen.choice
        [ -- let
          do
            x <- freshNameForCxt
            (e, eTy) <- genSyn
            Let () x e <$> local (extendLocalCxt (x, eTy)) (genChk ty)
        , -- letrec
          do
            x <- freshNameForCxt
            eTy <- genWTType KType
            Letrec () x <$> genChk eTy <*> pure eTy <*> genChk ty
            -- lettype
            {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               (lettype a = Nat -> Nat in λx.x : a), for instance
               See https://github.com/hackworthltd/primer/issues/5
            do
              x <- freshNameForCxt
              k <- genWTKind
              LetType () x <$> genWTType k <*> local (extendLocalCxtTy (x, k)) (genChk ty)
            -}
        ]
    case_ :: WT (Maybe (GenT WT ExprG))
    case_ =
      asks (M.elems . typeDefs) <&> \adts ->
        if null adts
          then Nothing
          else Just $ do
            td <- Gen.element adts
            let t = foldr (\_ t' -> TApp () t' (TEmptyHole ())) (TCon () $ typeDefName td) (typeDefParameters td)
            (e, brs) <- Gen.justT $ do
              (e, eTy) <- genSyns t -- NB: this could return something only consistent with t, e.g. if t=List ?, could get eT=? Nat
              vcs' <- instantiateValCons eTy
              fmap (e,) <$> case vcs' of
                Left TDIHoleType -> pure $ Just []
                Left _err -> pure Nothing -- if we didn't get an instance of t, try again; TODO: this is rather inefficient, and discards a lot...
                Right (_, vcs) -> fmap Just . for vcs $ \(c, params) -> do
                  ns <- replicateM (length params) freshNameForCxt
                  let binds = map (Bind ()) ns
                  CaseBranch c binds <$> local (extendLocalCxts $ zip ns params) (genChk ty)
            pure $ Case () e brs

-- | Generates types which infer kinds consistent with the argument
-- I.e. @genWTType k@ will generate types @ty@ such that @synthKind ty = k'@
-- with @consistentKinds k k'@. See 'Tests.Gen.Core.Typed.hprop_genTy'
genWTType :: Kind -> GenT WT TypeG
genWTType k = do
  vars <- lift vari
  cons <- lift constr
  let nonrec = ehole : catMaybes [vars, cons]
  let rec = hole : app : catMaybes [arrow, poly]
  Gen.recursive Gen.choice nonrec rec
  where
    ehole :: GenT WT TypeG
    ehole = pure $ TEmptyHole ()
    hole :: GenT WT TypeG
    hole = THole () <$> genWTType KHole
    app = do k' <- genWTKind; TApp () <$> genWTType (KFun k' k) <*> genWTType k'
    vari :: WT (Maybe (GenT WT TypeG))
    vari = do
      let f = \case K v -> Just v; T _ -> Nothing
      localTyVars <- asks $ M.mapMaybe f . localCxt
      let goodVars = filter (consistentKinds k . snd) $ M.toList localTyVars
      if null goodVars
        then pure Nothing
        else pure $ Just $ Gen.element $ map (TVar () . fst) goodVars
    constr :: WT (Maybe (GenT WT TypeG))
    constr = do
      tds <- asks $ M.elems . typeDefs
      let goodTCons = filter (consistentKinds k . typeDefKind) tds
      if null goodTCons
        then pure Nothing
        else pure $ Just $ Gen.element $ map (TCon () . typeDefName) goodTCons
    arrow :: Maybe (GenT WT TypeG)
    arrow =
      if k == KHole || k == KType
        then Just $ TFun () <$> genWTType KType <*> genWTType KType
        else Nothing
    poly :: Maybe (GenT WT TypeG)
    poly =
      if k == KHole || k == KType
        then Just $ do
          k' <- genWTKind
          n <- freshNameForCxt
          TForall () n k' <$> local (extendLocalCxtTy (n, k')) (genWTType KType)
        else Nothing

-- | Generates an arbitary kind. Note that all kinds are well-formed.
genWTKind :: GenT WT Kind
genWTKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genWTKind <*> genWTKind]

-- NB: we are only generating the context entries, and so don't
-- need definitions for the symbols!
genGlobalCxtExtension :: GenT WT [(ID, (Name, TypeG))]
genGlobalCxtExtension =
  local forgetLocals $
    Gen.list (Range.linear 1 5) $
      (\i n t -> (i, (n, t))) <$> fresh <*> freshNameForCxt <*> genWTType KType
  where
    -- we are careful to not let the globals depend on whatever locals may be in
    -- the cxt
    forgetLocals cxt = cxt{localCxt = mempty}

-- Generates a group of potentially-mutually-recursive typedefs
genTypeDefGroup :: GenT WT [TypeDef]
genTypeDefGroup = do
  let genParams = Gen.list (Range.linear 0 5) $ (,) <$> freshNameForCxt <*> genWTKind
  nps <- Gen.list (Range.linear 1 5) $ (,) <$> freshNameForCxt <*> genParams
  -- create empty typedefs to temporarilly extend the context, so can do recursive types
  let types =
        map
          ( \(n, ps) ->
              TypeDefAST
                ASTTypeDef
                  { astTypeDefName = n
                  , astTypeDefParameters = ps
                  , astTypeDefConstructors = []
                  , astTypeDefNameHints = []
                  }
          )
          nps
  let genConArgs params = Gen.list (Range.linear 0 5) $ local (extendLocalCxtTys params . addTypeDefs types) $ genWTType KType -- params+types scope...
  let genCons params = Gen.list (Range.linear 0 5) $ ValCon <$> freshNameForCxt <*> genConArgs params
  let genTD (n, ps) =
        ( \cons ->
            TypeDefAST
              ASTTypeDef
                { astTypeDefName = n
                , astTypeDefParameters = ps
                , astTypeDefConstructors = cons
                , astTypeDefNameHints = []
                }
        )
          <$> genCons ps
  mapM genTD nps

addTypeDefs :: [TypeDef] -> Cxt -> Cxt
addTypeDefs tds cxt = cxt{typeDefs = typeDefs cxt <> mkTypeDefMap tds}

extendGlobals :: [(ID, (Name, TypeG))] -> Cxt -> Cxt
extendGlobals nts cxt = cxt{globalCxt = globalCxt cxt <> M.fromList nts}

-- Generate an extension of the base context (from the reader monad) with more
-- typedefs and globals.
-- (It is probably worth seeding with some interesting types, to ensure decent
-- coverage)
genCxtExtendingGlobal :: GenT WT Cxt
genCxtExtendingGlobal = do
  tds <- genTypeDefGroup
  globals <- local (addTypeDefs tds) genGlobalCxtExtension
  asks $ extendGlobals globals . addTypeDefs tds

-- Generate an extension of the base context (from the reader monad) with more
-- local term and type vars.
genCxtExtendingLocal :: GenT WT Cxt
genCxtExtendingLocal = do
  n <- Gen.int $ Range.linear 1 10
  go n
  where
    go 0 = ask
    go n = do
      cxtE <-
        Gen.choice
          [ curry extendLocalCxtTy <$> freshNameForCxt <*> genWTKind
          , curry extendLocalCxt <$> freshNameForCxt <*> genWTType KType
          ]
      local cxtE $ go (n - 1)

genPrimCon :: MonadGen m => m [PrimCon]
genPrimCon = do
  char <- Gen.unicode
  pure
    [ PrimChar char
    ]

hoist' :: Applicative f => Cxt -> WT a -> f a
hoist' cxt = pure . evalTestM 0 . flip runReaderT cxt . unWT

-- | Convert a @PropertyT WT ()@ into a @Property@, which Hedgehog can test.
-- It is recommended to do more than default number of tests when using this module.
-- That is to say, generating well-typed syntax is hard, and you probably want
-- to increase the number of tests run to get decent coverage.
propertyWT :: Cxt -> PropertyT WT () -> Property
propertyWT cxt = property . hoist (hoist' cxt)
