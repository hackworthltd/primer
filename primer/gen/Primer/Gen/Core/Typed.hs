-- ApplicativeDo: generators shrink much better if applicative (though much of
-- this module is inherently monadic)
{-# LANGUAGE ApplicativeDo #-}

-- |
-- This module generates well-typed terms and types.
-- It is however, slow and the distribution is not very even.
--
-- For quickly generating non-well-typed-or-scoped terms, see "Primer.Gen.Core.Raw".
module Primer.Gen.Core.Typed (
  WT,
  isolateWT,
  genList,
  genWTType,
  genWTKind,
  genSyns,
  genSyn,
  genChk,
  genInstApp,
  genCxtExtendingGlobal,
  genCxtExtendingLocal,
  genPrimCon,
  genTypeDefGroup,
  forAllT,
  propertyWT,
  freshNameForCxt,
  freshLVarNameForCxt,
  freshTyVarNameForCxt,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh, fresh)
import Control.Monad.Morph (hoist)
import Control.Monad.Reader (mapReaderT)
import Data.Map qualified as M
import Hedgehog (
  GenT,
  MonadGen,
  PropertyT,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  Expr' (..),
  GVarName,
  GlobalName (qualifiedModule),
  ID (),
  Kind (..),
  LVarName,
  LocalName (LocalName, unLocalName),
  ModuleName (),
  PrimCon (..),
  TmVarRef (..),
  TyConName,
  TyVarName,
  Type' (..),
  ValConName,
  qualifyName,
 )
import Primer.Core.DSL (S)
import Primer.Core.Utils (freeVarsTy)
import Primer.Gen.Core.Raw (genLVarName, genModuleName, genName, genTyVarName)
import Primer.Module (Module (..))
import Primer.Name (Name, NameCounter, freshName, unName, unsafeMkName)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.Subst (substTy, substTySimul)
import Primer.Test.TestM (
  TestM,
  evalTestM,
  isolateTestM,
 )
import Primer.TypeDef (
  ASTTypeDef (..),
  TypeDef (..),
  ValCon (..),
  typeDefKind,
  typeDefParameters,
 )
import Primer.Typecheck (
  Cxt (),
  SmartHoles (NoSmartHoles),
  TypeDefError (TDIHoleType),
  buildTypingContextFromModules',
  consistentKinds,
  consistentTypes,
  extendLocalCxt,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxts,
  extendTypeDefCxt,
  getGlobalBaseNames,
  globalCxt,
  instantiateValCons,
  localCxt,
  localTmVars,
  localTyVars,
  matchArrowType,
  matchForallType,
  mkTAppCon,
  primConInScope,
  typeDefs,
 )
import Tasty (Property, property)

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
  deriving newtype
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
  globs <- getGlobalBaseNames
  locals <- asks $ M.keysSet . localCxt
  freshName $ globs <> locals

freshLVarNameForCxt :: GenT WT LVarName
freshLVarNameForCxt = LocalName <$> freshNameForCxt

freshTyVarNameForCxt :: GenT WT TyVarName
freshTyVarNameForCxt = LocalName <$> freshNameForCxt

freshTyConNameForCxt :: GenT WT TyConName
freshTyConNameForCxt = qualifyName <$> genModuleName <*> freshNameForCxt

-- We try to have a decent distribution of names, where there is a
-- significant chance that the same name is reused (both in disjoint
-- contexts, and with shadowing). However, we need to ensure that our
-- generated terms are well scoped and well typed.  This is mostly
-- handled by carrying around a context and only generating variables
-- from that context. However, there is one place we need to be
-- careful: we may put the aimed-for type verbatim into an
-- annotation. We must ensure that binders do not capture type
-- variable references that are introduced in that way.
-- For this we use 'genLVarNameAvoiding' and 'genTyVarNameAvoiding'.
-- Note that the aimed-for type may change during generation; in
-- particular, it can change to be the type of a non-shadowed term
-- variable (or a subexpression of said type), for example in genApp.
-- Thus we must avoid free variables occuring in the types assigned
-- by the context to term variables as well as those in the aimed-for
-- type.
--
-- NB: the returned name may itself count as one of these term
-- variables in the context; i.e. if this name will be used to make a
-- binding whose scope we enter, we need to avoid names appearing free
-- in the type assigned to the returned name itself. This avoids
-- situations where we have original context "foo : K KType" and then
-- bind a term variable "foo : T foo": if we changed the aimed-for
-- type to the type of the term variable "foo", we would have captured
-- the original type variable "foo" by our new term variable "foo".
genLVarNameAvoiding :: [TypeG] -> GenT WT LVarName
genLVarNameAvoiding ty =
  (\vs -> freshen (foldMap' freeVarsTy ty <> foldMap' freeVarsTy vs) 0)
    <$> asks localTmVars
    <*> genLVarName

genTyVarNameAvoiding :: TypeG -> GenT WT TyVarName
genTyVarNameAvoiding ty =
  (\vs -> freshen (freeVarsTy ty <> foldMap' freeVarsTy vs) 0)
    <$> asks localTmVars
    <*> genTyVarName

freshen :: Set (LocalName k') -> Int -> LocalName k -> LocalName k
freshen fvs i n =
  let suffix = if i > 0 then "_" <> show i else ""
      m = LocalName $ unsafeMkName $ unName (unLocalName n) <> suffix
   in if m `elem` fvs
        then freshen fvs (i + 1) n
        else m

-- genSyns T with cxt Γ should generate (e,S) st Γ |- e ∈ S and S ~ T (i.e. same up to holes and alpha)
genSyns :: HasCallStack => TypeG -> GenT WT (ExprG, TypeG)
genSyns ty = do
  genSpine' <- lift genSpine
  genPrimCon'' <- lift genPrimCon'
  Gen.recursive Gen.choice [genEmptyHole, genAnn] $ [genHole, genApp, genAPP, genLet] ++ catMaybes [genPrimCon'', genSpine']
  where
    genEmptyHole = pure (EmptyHole (), TEmptyHole ())
    genAnn = do
      t <- genChk ty
      pure (Ann () t ty, ty)
    genHole = do
      t <- genChk $ TEmptyHole ()
      pure (Hole () t, TEmptyHole ())
    genSpine :: WT (Maybe (GenT WT (ExprG, TypeG)))
    genSpine = fmap (fmap Gen.justT) genSpineHeadFirst
    genSpineHeadFirst :: WT (Maybe (GenT WT (Maybe (ExprG, TypeG))))
    -- todo: maybe add some lets in as post-processing? I could even add them to the locals for generation in the head
    genSpineHeadFirst = do
      localTms <- asks localTmVars
      let locals' = map (first (Var () . LocalVarRef)) $ M.toList localTms
      globals <- asks globalCxt
      let globals' = map (first (Var () . GlobalVarRef)) $ M.toList globals
      let hs = locals' ++ globals'
      pure $
        if null hs
          then Nothing
          else Just $ do
            (he, hT) <- Gen.element hs
            cxt <- ask
            runExceptT (refine cxt ty hT) >>= \case
              -- This error case indicates a bug. Crash and fail loudly!
              Left err -> panic $ "Internal refine/unify error: " <> show err
              Right Nothing -> pure Nothing
              Right (Just (inst, instTy)) -> do
                (sb, is) <- genInstApp inst
                let f e = \case Right tm -> App () e tm; Left ty' -> APP () e ty'
                Just . (foldl' f he is,) <$> substTySimul sb instTy
    genApp = do
      s <- genWTType KType
      (f, fTy) <- genSyns (TFun () s ty)
      (s', t') <- maybe Gen.discard pure $ matchArrowType fTy -- discard should never happen: fTy should be consistent with (TFun _ s ty)
      a <- App () f <$> genChk s'
      pure (a, t')
    -- APPs are difficult. We take the approach of throwing stuff at the wall and seeing what sticks...
    genAPP = justT $ do
      k <- genWTKind
      n <- genTyVarName
      (s, sTy) <- genSyns $ TForall () n k $ TEmptyHole ()
      cxt <- ask
      runExceptT (refine cxt ty sTy) >>= \case
        Right (Just ([InstAPP aTy], instTy)) -> pure $ Just (APP () s aTy, instTy)
        Right (Just ([InstUnconstrainedAPP a ak], instTy)) -> do
          aTy <- genWTType ak
          Just . (APP () s aTy,) <$> substTy a aTy instTy
        _ -> pure Nothing
    genPrimCon' = do
      genPrimCon <&> map (bimap (fmap $ PrimCon ()) (TCon ())) <&> filter (consistentTypes ty . snd) <&> \case
        [] -> Nothing
        gens -> Just $ Gen.choice $ (\(g, t) -> (,t) <$> g) <$> gens
    genLet =
      Gen.choice
        [ -- let
          do
            (e, eTy) <- genSyn
            x <- genLVarNameAvoiding [ty, eTy]
            (f, fTy) <- local (extendLocalCxt (x, eTy)) $ genSyns ty
            pure (Let () x e f, fTy)
        , -- letrec
          do
            eTy <- genWTType KType
            x <- genLVarNameAvoiding [ty, eTy]
            (e, (f, fTy)) <- local (extendLocalCxt (x, eTy)) $ do
              (,) <$> genChk eTy <*> genSyns ty
            pure (Letrec () x e eTy f, fTy)
            -- lettype
            {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               (lettype a = Nat -> Nat in λx.x : a), for instance
               See https://github.com/hackworthltd/primer/issues/5
            do
              x <- genTyVarNameAvoiding ty
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

-- | Given an output of 'refine', e.g. @refine cxt tgtTy srcTy = Just (insts, resTy)@,
-- generate some concrete types and terms corresponding to the instantiation.
-- If @genInstApp insts = (sub, apps)@, then:
-- - @apps@ is the same length as @insts@, and the entries correspond in the way
--   documented by 'refine'.
-- - the size of @sub@ is the number of 'InstUnconstrainedApp' in @inst@, and
--   these entries correspond (by name).
-- - thus if @insts !! n = InstUnconstrainedAPP a k@, then (for some type @t@ of kind @k@)
--   @sub ! a = t@ and @apps !! n = Left t@.
-- - @sub@ is idempotent, and @apps@ do not refer to these names. I.e. the names
--   in @InstUnconstrainedAPP@ do not appear free in @apps@ or the rhs of @sub@.
genInstApp :: [Inst] -> GenT WT (Map TyVarName (Type' ()), [Either TypeG ExprG])
genInstApp = reify mempty
  where
    reify sb = \case
      [] -> pure (sb, [])
      InstApp t : is -> (\a -> second (Right a :)) <$> (substTySimul sb t >>= genChk) <*> reify sb is
      InstAPP t : is -> (\t' -> second (Left t' :)) <$> substTySimul sb t <*> reify sb is
      InstUnconstrainedAPP v k : is -> genWTType k >>= \t' -> second (Left t' :) <$> reify (M.insert v t' sb) is

genSyn :: GenT WT (ExprG, TypeG)
-- Note that genSyns will generate things consistent with the given type, i.e.
-- of any type
genSyn = genSyns (TEmptyHole ())

-- | Returns each ADT constructor's name along with its "telescope" of arguments:
--  - the parameters of the datatype (which are type arguments to the constructor)
--  - the types of its fields (and the names of the parameters scope over this)
--  - the ADT it belongs to (if @c@ maps to @([(p1,k1),(p2,k2)],_,T)@ in the
--    returned map, then @c [A,B] _ ∈ T A B@ for any @A@ of kind @k1@ and @B@
--    of kind @k2@)
allCons :: Cxt -> M.Map ValConName ([(TyVarName, Kind)], [Type' ()])
allCons cxt = M.fromList $ concatMap consForTyDef $ typeDefs cxt
  where
    consForTyDef = \case
      TypeDefAST td ->
        map
          ( \vc ->
              ( valConName vc
              ,
                ( astTypeDefParameters td
                , valConArgs vc
                )
              )
          )
          (astTypeDefConstructors td)
      TypeDefPrim _ -> []

genChk :: TypeG -> GenT WT ExprG
genChk ty = do
  cse <- lift case_
  abst' <- lift abst
  genCon' <- lift genCon
  let rec = genLet : catMaybes [genCon', lambda, abst', cse]
  Gen.recursive Gen.choice [emb] rec
  where
    emb = fst <$> genSyns ty
    genCon =
      instantiateValCons ty >>= \case
        Left TDIHoleType ->
          asks allCons <&> \case
            -- We have no constraints, generate any ctor
            m | null m -> Nothing
            cons -> Just $ do
              let cons' =
                    M.toList cons <&> \(c, (params, fldsTys0)) -> do
                      indicesMap <- for params $ \(p, k) -> (p,) <$> genWTType k
                      -- NB: it is vital to use simultaneous substitution here.
                      -- Consider the case where we have a local type variable @a@
                      -- in scope, say because we have already generated a
                      -- @Λa. ...@, and we are considering the case of the @MkPair@
                      -- constructor for the type @data Pair a b = MkPair a b@.
                      -- The two "a"s (locally Λ-bound and from the typedef) refer
                      -- to completely different things. We may well generate the
                      -- substitution [a :-> Bool, b :-> a]. We must then say that
                      -- the fields of the @MkPair@ constructor are @Bool@ and (the
                      -- locally-bound) @a@. We must do a simultaneous substitution
                      -- to avoid substituting @b@ into @a@ and then further into
                      -- @Bool@.
                      fldsTys <- traverse (substTySimul $ M.fromList indicesMap) fldsTys0
                      flds <- traverse (Gen.small . genChk) fldsTys
                      pure $ Con () c flds
              Gen.choice cons'
        Left _ -> pure Nothing -- not an ADT
        Right (_, _, []) -> pure Nothing -- is an empty ADT
        Right (_, _, vcs) ->
          pure $
            Just $
              Gen.choice $
                vcs <&> \(vc, tmArgTypes) ->
                  Con () vc <$> traverse genChk tmArgTypes
    lambda =
      matchArrowType ty <&> \(sTy, tTy) -> do
        n <- genLVarNameAvoiding [tTy, sTy]
        Lam () n <$> local (extendLocalCxt (n, sTy)) (genChk tTy)
    abst = do
      mfa <- matchForallType ty
      pure $
        mfa <&> \(n, k, t) -> do
          m <- genTyVarNameAvoiding ty
          ty' <- substTy n (TVar () m) t
          LAM () m <$> local (extendLocalCxtTy (m, k)) (genChk ty')
    genLet =
      Gen.choice
        [ -- let
          do
            (e, eTy) <- genSyn
            x <- genLVarNameAvoiding [ty, eTy]
            Let () x e <$> local (extendLocalCxt (x, eTy)) (genChk ty)
        , -- letrec
          do
            eTy <- genWTType KType
            x <- genLVarNameAvoiding [ty, eTy]
            (e, f) <- local (extendLocalCxt (x, eTy)) $ (,) <$> genChk eTy <*> genChk ty
            pure $ Letrec () x e eTy f
            -- lettype
            {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               (lettype a = Nat -> Nat in λx.x : a), for instance
               See https://github.com/hackworthltd/primer/issues/5
            do
              x <- genTyVarNameAvoiding ty
              k <- genWTKind
              LetType () x <$> genWTType k <*> local (extendLocalCxtTy (x, k)) (genChk ty)
            -}
        ]
    case_ :: WT (Maybe (GenT WT ExprG))
    case_ =
      asks (M.assocs . typeDefs) <&> \adts ->
        if null adts
          then Nothing
          else Just $ do
            (tc, td) <- Gen.element adts
            let t = mkTAppCon tc (TEmptyHole () <$ typeDefParameters td)
            (e, brs) <- Gen.justT $ do
              (e, eTy) <- genSyns t -- NB: this could return something only consistent with t, e.g. if t=List ?, could get eT=? Nat
              vcs' <- instantiateValCons eTy
              fmap (e,) <$> case vcs' of
                Left TDIHoleType -> pure $ Just []
                Left _err -> pure Nothing -- if we didn't get an instance of t, try again; TODO: this is rather inefficient, and discards a lot...
                Right (_, _, vcs) -> fmap Just . for vcs $ \(c, params) -> do
                  ns <- for params $ \nt -> (,nt) <$> genLVarNameAvoiding [ty, nt]
                  let binds = map (Bind () . fst) ns
                  CaseBranch c binds <$> local (extendLocalCxts ns) (genChk ty)
            pure $ Case () e brs

-- | Generates types which infer kinds consistent with the argument
-- I.e. @genWTType k@ will generate types @ty@ such that @synthKind ty = k'@
-- with @consistentKinds k k'@. See 'Tests.Gen.Core.Typed.tasty_genTy'
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
      goodVars <- filter (consistentKinds k . snd) . M.toList <$> asks localTyVars
      pure $
        if null goodVars
          then Nothing
          else Just $ Gen.element $ map (TVar () . fst) goodVars
    constr :: WT (Maybe (GenT WT TypeG))
    constr = do
      tds <- asks $ M.assocs . typeDefs
      let goodTCons = filter (consistentKinds k . typeDefKind . snd) tds
      pure $
        if null goodTCons
          then Nothing
          else Just $ Gen.element $ map (TCon () . fst) goodTCons
    arrow :: Maybe (GenT WT TypeG)
    arrow =
      if k == KHole || k == KType
        then Just $ TFun () <$> genWTType KType <*> genWTType KType
        else Nothing
    {- TODO: reinstate once the TC handles them! and then be careful to do
               interesting things where we need to expand the synonym
               See https://github.com/hackworthltd/primer/issues/5
    tlet :: GenT WT TypeG
    tlet = do
      k' <- genWTKind
      n <- genTyVarName
      TLet () n <$> genWTType k' <*> local (extendLocalCxtTy (n,k')) (genWTType k)
    -}
    poly :: Maybe (GenT WT TypeG)
    poly =
      if k == KHole || k == KType
        then Just $ do
          k' <- genWTKind
          n <- genTyVarName
          TForall () n k' <$> local (extendLocalCxtTy (n, k')) (genWTType KType)
        else Nothing

-- | Generates an arbitary kind. Note that all kinds are well-formed.
genWTKind :: GenT WT Kind
genWTKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genWTKind <*> genWTKind]

-- NB: we are only generating the context entries, and so don't
-- need definitions for the symbols!
genGlobalCxtExtension :: GenT WT [(GVarName, TypeG)]
genGlobalCxtExtension =
  local forgetLocals $
    Gen.list (Range.linear 1 5) $
      (,) <$> (qualifyName <$> genModuleName <*> genName) <*> genWTType KType

-- We are careful to not let generated globals depend on whatever
-- locals may be in the cxt
forgetLocals :: Cxt -> Cxt
forgetLocals cxt = cxt{localCxt = mempty}

-- | Like 'Gen.list', but weighted to produce empty list 10% of time,
-- and length between 1 and argument the rest of the time (linearly
-- scaled with size)
genList :: MonadGen g => Int -> g a -> g [a]
genList n g = Gen.frequency [(1, pure []), (9, Gen.list (Range.linear 1 n) g)]

-- Generates a group of potentially-mutually-recursive typedefs
-- If given a module name, they will all live in that module,
-- otherwise they may live in disparate modules
genTypeDefGroup :: Maybe ModuleName -> GenT WT [(TyConName, TypeDef ())]
genTypeDefGroup mod = local forgetLocals $ do
  let genParams = Gen.list (Range.linear 0 5) $ (,) <$> freshTyVarNameForCxt <*> genWTKind
  let tyconName = case mod of
        Nothing -> freshTyConNameForCxt
        Just m -> qualifyName m <$> freshNameForCxt
  nps <- genList 5 $ (,) <$> tyconName <*> genParams
  -- create empty typedefs to temporarilly extend the context, so can do recursive types
  let types =
        map
          ( \(n, ps) ->
              ( n
              , TypeDefAST
                  ASTTypeDef
                    { astTypeDefParameters = ps
                    , astTypeDefConstructors = []
                    , astTypeDefNameHints = []
                    }
              )
          )
          nps
  let genConArgs params = Gen.list (Range.linear 0 5) $ local (extendLocalCxtTys params . addTypeDefs types) $ genWTType KType -- params+types scope...
  let freshValConNameForCxt tyConName = qualifyName (qualifiedModule tyConName) <$> freshNameForCxt
  let genCons ty params = Gen.list (Range.linear 0 5) $ ValCon <$> freshValConNameForCxt ty <*> genConArgs params
  let genTD (n, ps) =
        ( \cons ->
            ( n
            , TypeDefAST
                ASTTypeDef
                  { astTypeDefParameters = ps
                  , astTypeDefConstructors = cons
                  , astTypeDefNameHints = []
                  }
            )
        )
          <$> genCons n ps
  mapM genTD nps

addTypeDefs :: [(TyConName, TypeDef ())] -> Cxt -> Cxt
addTypeDefs = extendTypeDefCxt . M.fromList

extendGlobals :: [(GVarName, TypeG)] -> Cxt -> Cxt
extendGlobals nts cxt = cxt{globalCxt = globalCxt cxt <> M.fromList nts}

-- Generate an extension of the base context (from the reader monad) with more
-- typedefs and globals.
-- (It is probably worth seeding with some interesting types, to ensure decent
-- coverage)
genCxtExtendingGlobal :: GenT WT Cxt
genCxtExtendingGlobal = do
  tds <- genTypeDefGroup Nothing
  globals <- local (addTypeDefs tds) genGlobalCxtExtension
  asks $ extendGlobals globals . addTypeDefs tds

-- Generate an extension of the base context (from the reader monad) with more
-- local term and type vars.
-- Note that here we need to generate fresh names, as we test that the
-- whole context typechecks. Since we represent contexts as a 'Map'
-- for efficiency, we do not keep track of scoping, and need to not
-- overwrite previous elements.  For instance, we cannot faithfully
-- represent the context @x : TYPE, y : x, x : TYPE -> TYPE@: we would
-- forget the first @x@, and thus it would appear that @y@ is
-- ill-typed (a term variable must have a type of kind TYPE).
genCxtExtendingLocal :: GenT WT Cxt
genCxtExtendingLocal = do
  n <- Gen.int $ Range.linear 1 10
  go n
  where
    go 0 = ask
    go n = do
      cxtE <-
        Gen.choice
          [ curry extendLocalCxtTy <$> freshTyVarNameForCxt <*> genWTKind
          , curry extendLocalCxt <$> freshLVarNameForCxt <*> genWTType KType
          ]
      local cxtE $ go (n - 1)

-- We have to be careful to only generate primitive constructors which are
-- in scope (i.e. their type is in scope)
genPrimCon :: forall mc mg. (MonadReader Cxt mc, MonadGen mg) => mc [(mg PrimCon, TyConName)]
genPrimCon = catMaybes <$> sequence [genChar, genInt]
  where
    genChar = whenInScope PrimChar 'a' Gen.unicode
    intBound = fromIntegral (maxBound :: Word64) -- arbitrary
    genInt = whenInScope PrimInt 0 $ Gen.integral $ Range.linear (-intBound) intBound
    -- The 'tst' is arbitrary, only used for checking if the primcon is in scope
    -- and does not affect the generator.
    whenInScope :: (a -> PrimCon) -> a -> mg a -> mc (Maybe (mg PrimCon, TyConName))
    whenInScope f tst g = do
      s <- asks $ primConInScope (f tst)
      pure $ case s of
        (False, _) -> Nothing
        (True, tc) -> Just $ (,tc) $ f <$> g
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this generator.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()

hoist' :: Applicative f => Cxt -> WT a -> f a
hoist' cxt = pure . evalTestM 0 . flip runReaderT cxt . unWT

-- | Convert a @PropertyT WT ()@ into a @Property@, which Hedgehog can test.
-- It is recommended to do more than default number of tests when using this module.
-- That is to say, generating well-typed syntax is hard, and you probably want
-- to increase the number of tests run to get decent coverage.
-- The modules form the 'Cxt' in the environment of the 'WT' monad
-- (thus the definitions of terms is ignored)
propertyWT :: [S Module] -> PropertyT WT () -> Property
propertyWT mods = property . hoist (hoist' $ buildTypingContextFromModules' mods NoSmartHoles)
