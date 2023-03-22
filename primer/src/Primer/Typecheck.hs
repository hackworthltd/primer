{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Typechecking for Core expressions.
-- This closely follows the type system of Hazelnut, but supports arbitrary
-- types rather than just numbers.
-- In future we will want to extend it to support more features such as
-- polymorphism.
module Primer.Typecheck (
  Type,
  Expr,
  ExprT,
  SmartHoles (..),
  synth,
  check,
  synthKind,
  checkKind,
  checkTypeDefs,
  checkValidContext,
  CheckEverythingRequest (..),
  checkEverything,
  Cxt (..),
  KindOrType (..),
  initialCxt,
  buildTypingContext,
  buildTypingContextFromModules,
  buildTypingContextFromModules',
  TypeError (..),
  KindError (..),
  typeOf,
  maybeTypeOf,
  matchArrowType,
  matchForallType,
  decomposeTAppCon,
  mkTAppCon,
  TypeDefInfo (..),
  TypeDefError (..),
  getTypeDefInfo,
  getTypeDefInfo',
  lookupConstructor,
  instantiateValCons,
  instantiateValCons',
  exprTtoExpr,
  typeTtoType,
  getGlobalNames,
  getGlobalBaseNames,
  lookupGlobal,
  lookupVar,
  primConInScope,
  consistentKinds,
  consistentTypes,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxt,
  extendLocalCxts,
  extendGlobalCxt,
  extendTypeDefCxt,
  localTmVars,
  localTyVars,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.NestedError (MonadNestedError (..), modifyError')
import Data.List (lookup)
import Data.Map qualified as M
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Optics (
  A_Traversal,
  AppendIndices,
  IxFold,
  IxTraversal',
  JoinKinds,
  Optic',
  WithIx,
  equality,
  icompose,
  itoListOf,
  itraversed,
  over,
  reindexed,
  selfIndex,
  set,
  to,
  traverseOf,
  (%),
 )
import Optics.Traversal (traversed)
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind (..),
  LVarName,
  LocalName (LocalName),
  Meta (..),
  PrimCon,
  TmVarRef (..),
  TyConName,
  TyVarName,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeMeta,
  ValConName,
  bindName,
  qualifyName,
  unLocalName,
  _bindMeta,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL (S, branch, create', emptyHole, meta, meta')
import Primer.Core.Transform (decomposeTAppCon, mkTAppCon, unfoldTApp)
import Primer.Core.Utils (
  alphaEqTy,
  forgetTypeMetadata,
  freshLocalName,
  freshLocalName',
  generateTypeIDs,
  noHoles,
 )
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
  defType,
 )
import Primer.Module (
  Module (
    moduleDefs,
    moduleName
  ),
  moduleDefsQualified,
  moduleTypesQualified,
 )
import Primer.Name (Name, NameCounter)
import Primer.Primitives (primConName)
import Primer.Subst (substTy)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefConstructors, astTypeDefParameters),
  TypeDef (..),
  TypeDefMap,
  ValCon (valConArgs, valConName),
  typeDefAST,
 )
import Primer.Typecheck.Cxt (Cxt (Cxt, globalCxt, localCxt, smartHoles, typeDefs))
import Primer.Typecheck.Kindcheck (
  KindError (..),
  KindOrType (K, T),
  Type,
  TypeT,
  annotate,
  checkKind,
  consistentKinds,
  extendLocalCxtTy,
  extendLocalCxtTys,
  localTyVars,
  synthKind,
 )
import Primer.Typecheck.SmartHoles (SmartHoles (..))
import Primer.Typecheck.TypeError (TypeError (..))
import Primer.Typecheck.Utils (
  TypeDefError (TDIHoleType, TDINotADT, TDINotSaturated, TDIUnknown),
  TypeDefInfo (TypeDefInfo),
  getGlobalBaseNames,
  getGlobalNames,
  getTypeDefInfo,
  getTypeDefInfo',
  instantiateValCons,
  instantiateValCons',
  lookupConstructor,
  maybeTypeOf,
  typeOf,
  _typecache,
 )

-- | Typechecking takes as input an Expr with 'Maybe Type' annotations and
-- produces an Expr with 'Type' annotations - i.e. every node in the output is
-- given a type. The type annotation isn't itself part of the editable program
-- so it has no metadata - hence the '()' argument inside 'TypeCache'.
--
-- The 'Type' annotations cache the type which a term synthesised/was checked
-- at. For "embeddings" where typechecking defers to synthesis, we record the
-- synthesised type, not the checked one. For example, when checking that
-- @Int -> ?@ accepts @\x . x@, we record that the variable node has type
-- @Int@, rather than @?@.
type ExprT = Expr' (Meta TypeCache) (Meta Kind)

assert :: MonadNestedError TypeError e m => Bool -> Text -> m ()
assert b s = unless b $ throwError' (InternalError s)

lookupLocal :: LVarName -> Cxt -> Either TypeError Type
lookupLocal v cxt = case M.lookup (unLocalName v) $ localCxt cxt of
  Just (T t) -> Right t
  Just (K _) -> Left $ TmVarWrongSort (unLocalName v)
  Nothing -> Left $ UnknownVariable $ LocalVarRef v

lookupGlobal :: GVarName -> Cxt -> Maybe Type
lookupGlobal v cxt = M.lookup v $ globalCxt cxt

lookupVar :: TmVarRef -> Cxt -> Either TypeError Type
lookupVar v cxt = case v of
  LocalVarRef name -> lookupLocal name cxt
  GlobalVarRef name ->
    ( \case
        Just t -> Right t
        Nothing -> Left $ UnknownVariable v
    )
      (lookupGlobal name cxt)

extendLocalCxt :: (LVarName, Type) -> Cxt -> Cxt
extendLocalCxt (name, ty) cxt = cxt{localCxt = Map.insert (unLocalName name) (T ty) (localCxt cxt)}

extendLocalCxts :: [(LVarName, Type)] -> Cxt -> Cxt
extendLocalCxts x cxt = cxt{localCxt = Map.fromList (bimap unLocalName T <$> x) <> localCxt cxt}

extendGlobalCxt :: [(GVarName, Type)] -> Cxt -> Cxt
extendGlobalCxt globals cxt = cxt{globalCxt = Map.fromList globals <> globalCxt cxt}

extendTypeDefCxt :: TypeDefMap -> Cxt -> Cxt
extendTypeDefCxt typedefs cxt = cxt{typeDefs = typedefs <> typeDefs cxt}

localTmVars :: Cxt -> Map LVarName Type
localTmVars = M.mapKeys LocalName . M.mapMaybe (\case T t -> Just t; K _ -> Nothing) . localCxt

noSmartHoles :: Cxt -> Cxt
noSmartHoles cxt = cxt{smartHoles = NoSmartHoles}

-- An empty typing context
initialCxt :: SmartHoles -> Cxt
initialCxt sh =
  Cxt
    { smartHoles = sh
    , typeDefs = mempty
    , localCxt = mempty
    , globalCxt = mempty
    }

-- | Construct an initial typing context, with all given definitions in scope as global variables.
buildTypingContext :: TypeDefMap -> DefMap -> SmartHoles -> Cxt
buildTypingContext tydefs defs sh =
  let globals = Map.assocs $ fmap defType defs
   in extendTypeDefCxt tydefs $ extendGlobalCxt globals $ initialCxt sh

buildTypingContextFromModules :: [Module] -> SmartHoles -> Cxt
buildTypingContextFromModules modules =
  buildTypingContext
    (foldMap' moduleTypesQualified modules)
    (foldMap' moduleDefsQualified modules)

buildTypingContextFromModules' :: [S Module] -> SmartHoles -> Cxt
-- NB: we don't care about IDs/TypeMeta here, since we remove them in
-- buildTypingContextFromModules, thus @create'@ is ok.
buildTypingContextFromModules' = buildTypingContextFromModules . create' . sequence

-- | A shorthand for the constraints needed when kindchecking
type TypeM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e m -- can throw type errors
  )

-- | Check a context is valid
checkValidContext ::
  (MonadFresh ID m, MonadFresh NameCounter m, MonadNestedError TypeError e (ReaderT Cxt m)) =>
  Cxt ->
  m ()
checkValidContext cxt = do
  let tds = typeDefs cxt
  runReaderT (checkTypeDefs tds) $ initialCxt NoSmartHoles
  runReaderT (checkGlobalCxt $ globalCxt cxt) $ (initialCxt NoSmartHoles){typeDefs = tds}
  checkLocalCxtTys $ localTyVars cxt
  runReaderT (checkLocalCxtTms $ localTmVars cxt) $ extendLocalCxtTys (M.toList $ localTyVars cxt) (initialCxt NoSmartHoles){typeDefs = tds}
  where
    checkGlobalCxt = mapM_ (checkKind' KType <=< fakeMeta)
    -- a tyvar just declares its kind. There are no possible errors in kinds.
    checkLocalCxtTys _tyvars = pure ()
    checkLocalCxtTms = mapM_ (checkKind' KType <=< fakeMeta)
    -- We need metadata to use checkKind, but we don't care about the output,
    -- just a yes/no answer. In this case it is fine to put nonsense in the
    -- metadata as it won't be inspected.
    fakeMeta = generateTypeIDs

-- | Check all type definitions, as one recursive group, in some monadic environment
checkTypeDefs ::
  TypeM e m =>
  TypeDefMap ->
  m ()
checkTypeDefs tds = do
  existingTypes <- asks typeDefs
  -- NB: we expect the frontend to only submit acceptable typedefs, so all
  -- errors here are "internal errors" and should never be seen.
  -- (This is not quite true, see
  -- https://github.com/hackworthltd/primer/issues/3)
  assert (Map.disjoint existingTypes tds) "Duplicate-ly-named TypeDefs"
  -- Note that constructors names must be globally unique. This is
  -- required when checking @? ∋ Con ...@, as we need to be able to
  -- work out what typedef the constructor belongs to without any
  -- extra information.
  let atds = Map.mapMaybe typeDefAST tds
  let allAtds = Map.mapMaybe typeDefAST existingTypes <> atds
  assert
    (distinct $ concatMap (map valConName . astTypeDefConstructors) allAtds)
    "Duplicate-ly-named constructor (perhaps in different typedefs)"
  -- Note that these checks only apply to non-primitives:
  -- duplicate type names are checked elsewhere, kinds are correct by construction, and there are no constructors.
  local (extendTypeDefCxt tds) $ traverseWithKey_ checkTypeDef atds
  where
    traverseWithKey_ :: Applicative f => (k -> v -> f ()) -> Map k v -> f ()
    traverseWithKey_ f = void . Map.traverseWithKey f
    -- In the core, we have many different namespaces, so the only name-clash
    -- checking we must do is
    -- - between two constructors (possibly of different types)
    -- - between two type names
    -- However, we forbid much more than this for UI and pedagogy purposes. We
    -- actually check
    -- - In one typedef
    --   - parameters are distinct
    --   - type name is not shadowed by a parameter
    --   - parameters and constructors do not clash
    -- - Globally
    --   - all types have distinct names
    --   - all constructors have distinct names
    -- But note that we allow
    -- - type names clashing with constructor names (possibly in different
    --   types)

    checkTypeDef tc td = do
      let params = astTypeDefParameters td
      let cons = astTypeDefConstructors td
      assert
        ( (1 ==) . S.size $
            S.fromList $
              qualifiedModule tc : fmap (qualifiedModule . valConName) cons
        )
        "Module name of type and all constructors must be the same"
      assert
        (distinct $ map (unLocalName . fst) params <> map (baseName . valConName) cons)
        "Duplicate names in one tydef: between parameter-names and constructor-names"
      assert
        (notElem (baseName tc) $ map (unLocalName . fst) params)
        "Duplicate names in one tydef: between type-def-name and parameter-names"
      local (noSmartHoles . extendLocalCxtTys params) $
        mapM_ (checkKind' KType <=< fakeMeta) $
          concatMap valConArgs cons
    -- We need metadata to use checkKind, but we don't care about the output,
    -- just a yes/no answer. In this case it is fine to put nonsense in the
    -- metadata as it won't be inspected.
    fakeMeta = generateTypeIDs

distinct :: Ord a => [a] -> Bool
distinct = go mempty
  where
    go _ [] = True
    go seen (x : xs)
      | x `S.member` seen = False
      | otherwise = go (S.insert x seen) xs

data CheckEverythingRequest = CheckEverything
  { trusted :: [Module]
  , toCheck :: [Module]
  }

-- | Check a (mutually-recursive set of) module(s), in a given trusted
-- environment of modules.
-- Returns just the modules that were requested 'toCheck', with updated cached
-- type information etc.
--
-- This checks every type definition and every global term definition.
--
-- In particular, this typechecks all the definitions, in one recursive group.
-- This checks that the type signature is well-formed, then checks the body
-- (for an ASTDef) against the signature.
-- (If SmartHoles edits a type, the body of every function is checked in the
-- environment with the updated type)
checkEverything ::
  forall e m.
  (MonadFresh ID m, MonadFresh NameCounter m, MonadNestedError TypeError e (ReaderT Cxt m)) =>
  SmartHoles ->
  CheckEverythingRequest ->
  m [Module]
checkEverything sh CheckEverything{trusted, toCheck} =
  let cxt = buildTypingContextFromModules trusted sh
   in flip runReaderT cxt $ do
        let newTypes = foldMap' moduleTypesQualified toCheck
        checkTypeDefs newTypes
        local (extendTypeDefCxt newTypes) $ do
          -- Kind check and update (for smartholes) all the types.
          -- Note that this may give ill-typed definitions if the type changes
          -- since we have not checked the expressions against the new types.
          updatedTypes <- traverseOf (traverseDefs % #_DefAST % #astDefType) (fmap typeTtoType . checkKind' KType) toCheck
          -- Now extend the context with the new types
          let defsUpdatedTypes = itoListOf foldDefTypesWithName updatedTypes
          local (extendGlobalCxt defsUpdatedTypes) $
            -- Check the body (of AST definitions) against the new type
            traverseOf
              (traverseDefs % #_DefAST)
              ( \def -> do
                  e <- check (forgetTypeMetadata $ astDefType def) (astDefExpr def)
                  pure $ def{astDefExpr = exprTtoExpr e}
              )
              updatedTypes
  where
    -- The first argument of traverseDefs' is intended to either
    -- - be equality, giving a traveral
    -- - specify an index (using selfIndex and reindexed), giving a fold
    traverseDefs' ::
      ( JoinKinds A_Traversal k l
      , JoinKinds l A_Traversal l
      , AppendIndices is (WithIx Name) js
      ) =>
      Optic' k is Module Module ->
      Optic' l js [Module] Def
    traverseDefs' o = traversed % o % (#moduleDefs % itraversed)
    traverseDefs :: IxTraversal' Name [Module] Def
    traverseDefs = traverseDefs' equality
    foldDefTypesWithName :: IxFold GVarName [Module] Type
    foldDefTypesWithName =
      icompose qualifyName $
        traverseDefs' (reindexed moduleName selfIndex)
          % to defType
          % to forgetTypeMetadata

{- HLINT ignore synth "Avoid lambda using `infix`" -}
-- Note [Let expressions]
-- Let expressions are typechecked flexibly in order to minimise the instances
-- where an annotation must be added. Hence we can both synthesise and check
-- let expressions.
--
-- We can currently use lets to mimic top-level definitions, but when top-level
-- definitions become a first-class concept will we want to enforce that they
-- have an explicit type declaration.

-- | Synthesise a type for an expression.
-- We optionally insert/remove holes and insert annotations where
-- needed/possible, based on the SmartHoles option in the TypeM reader monad.
-- When we 'NoSmartHoles', the output will be the same as the input, modulo
-- caching type information in the metadata.
-- When we insert and remove holes and annotations, the AST may change.
-- The only changes we make to the AST are
-- - wrapping/unwrapping holes and annotations to make the types line up.
-- - recreating case branches if necessary (deleting their RHSs)
-- We return the synthesised type so one does not need to rely on
-- the cached type in the output being TCSynthed.
-- INVARIANT: if @synth e@ gives @(T,e')@, then @e@ and @e'@ agree up to their
-- cached types, and @TCSynthed T == typeOf e'@
synth :: TypeM e m => Expr -> m (Type, ExprT)
synth = \case
  Var i x -> do
    t <- either throwError' pure . lookupVar x =<< ask
    pure $ annSynth1 t i Var x
  App i e1 e2 -> do
    -- Synthesise e1
    (t1, e1') <- synth e1
    -- Check that e1 has an arrow type
    case matchArrowType t1 of
      Just (t2, t) -> do
        -- Check e2 against the domain type of e1
        e2' <- check t2 e2
        pure $ annSynth2 t i App e1' e2'
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchArrow t1
          SmartHoles -> do
            e1Wrap <- Hole <$> meta <*> pure e1
            synth $ App i e1Wrap e2
  APP i e t -> do
    (et, e') <- synth e
    matchForallType et >>= \case
      Just (v, vk, b) -> do
        t' <- checkKind' vk t
        bSub <- substTy v (forgetTypeMetadata t') b
        pure (bSub, APP (annotate (TCSynthed bSub) i) e' t')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchForall et
          SmartHoles -> do
            eWrap <- Hole <$> meta <*> pure e
            synth $ APP i eWrap t
  Ann i e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind' KType t
    let t'' = forgetTypeMetadata t'
    -- Check e against the annotation
    e' <- check t'' e
    -- Annotate the Ann with the same type as e
    pure $ annSynth2 t'' i Ann e' t'
  EmptyHole i -> pure $ annSynth0 (TEmptyHole ()) i EmptyHole
  -- When synthesising a hole, we first check that the expression inside it
  -- synthesises a type successfully (see Note [Holes and bidirectionality]).
  -- TODO: we would like to remove this hole (leaving e) if possible, but I
  -- don't see how to do this nicely as we don't know what constraints the
  -- synthesised type needs. Consider {? 1 ?} True: we can't remove the hole,
  -- but we don't know that when we come to synthesise its type. Potentially we
  -- could remove it here and let the App rule re-add it if necessary, but then
  -- consider {? ? : Nat -> Nat ?} True: then we could remove the hole, and App
  -- would see the function has an arrow type and check Nat ∋ True which fails,
  -- leaving (? : Nat -> Nat) {? True ?}. This causes holes to jump around
  -- which is bad UX.
  -- See https://github.com/hackworthltd/primer/issues/7
  Hole i e -> do
    (_, e') <- synth e
    pure $ annSynth1 (TEmptyHole ()) i Hole e'
  Let i x a b -> do
    -- Synthesise a type for the bound expression
    (aT, a') <- synth a
    -- Extend the context with the binding, and synthesise the body
    (bT, b') <- local (extendLocalCxt (x, aT)) $ synth b
    pure $ annSynth3 bT i Let x a' b'
  Letrec i x a tA b -> do
    -- Check that tA is well-formed
    tA' <- checkKind' KType tA
    let t = forgetTypeMetadata tA'
        ctx' = extendLocalCxt (x, t)
    -- Check the bound expression against its annotation
    a' <- local ctx' $ check t a
    -- Extend the context with the binding, and synthesise the body
    (bT, b') <- local ctx' $ synth b
    pure $ annSynth4 bT i Letrec x a' tA' b'
  PrimCon i pc -> do
    (inScope, tyCon) <- asks (primConInScope pc)
    -- We expect any frontend to avoid this situation, and thus we do not
    -- try to recover with SmartHoles
    unless inScope $ throwError' $ PrimitiveTypeNotInScope tyCon
    pure $ annSynth0 (TCon () tyCon) i (\m -> PrimCon m pc)
  e ->
    asks smartHoles >>= \case
      NoSmartHoles -> throwError' $ CannotSynthesiseType e
      SmartHoles -> do
        ann <- TEmptyHole <$> meta' (Just KType)
        eMeta <- meta
        synth $ Ann eMeta e ann
  where
    -- We could combine these with some type class shenanigans, but it doesn't
    -- seem worth it. The general scheme is
    -- annSynthN t i c x1 ... xn = (t,c (annotate (TCSynthed t) i) x1 ... xn)
    annSynth0 t i x = (t, x $ annotate (TCSynthed t) i)
    annSynth1 t i c = annSynth0 t i . flip c
    annSynth2 t i c = annSynth1 t i . flip c
    annSynth3 t i c = annSynth2 t i . flip c
    annSynth4 t i c = annSynth3 t i . flip c

zipWithExactM :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> Maybe (f [c])
zipWithExactM _ [] [] = Just $ pure []
zipWithExactM _ [] _ = Nothing
zipWithExactM _ _ [] = Nothing
zipWithExactM f (x : xs) (y : ys) = ((:) <$> f x y <*>) <$> zipWithExactM f xs ys

ensureJust :: MonadNestedError e e' m => e -> Maybe (m a) -> m a
ensureJust e Nothing = throwError' e
ensureJust _ (Just x) = x

-- There is a hard-wired map 'primConName' which associates each PrimCon to
-- its PrimTypeDef (by name -- PrimTypeDefs have hardwired names).
-- However, these PrimTypeDefs may or may not be in the Cxt.
-- If they are not (and in that case, also if a user has defined some
-- other type with the same name), we should reject the use of the
-- primitive constructor.
-- Essentially, PrimCons are always-in-scope terms whose type is one of
-- the primitive types. Normally we ensure that the types of all global
-- definitions are well-kinded (in particular, only refer to types that
-- are in scope). This is just the analogue that check, but we have to
-- do it lazily (i.e. on use) for primitive constructors.
--
-- returns: whether it is in scope or not, and also the type of which it
-- (should) construct a value
primConInScope :: PrimCon -> Cxt -> (Bool, TyConName)
primConInScope pc cxt =
  let tyCon = primConName pc
      typeDef = M.lookup tyCon $ typeDefs cxt
   in case typeDef of
        Nothing -> (False, tyCon)
        Just (TypeDefAST _) -> (False, tyCon)
        Just (TypeDefPrim _) -> (True, tyCon)

-- | Similar to synth, but for checking rather than synthesis.
check :: TypeM e m => Type -> Expr -> m ExprT
check t = \case
  -- We assume that constructor names are unique
  -- See Note [Checkable constructors] in Core.hs
  con@(Con i c tys tms) -> do
    -- If the input type @t@ is a hole-applied-to-some-arguments,
    -- then refine it to the parent type of @c@ applied to some holes plus those original arguments
    (cParent, parentParams) <-
      asks (flip lookupConstructor c . typeDefs) >>= \case
        Just (_, tn, td) -> pure (tn, length $ astTypeDefParameters td)
        Nothing -> throwError' $ UnknownConstructor c -- unrecoverable error, smartholes can do nothing here
    let t' = case unfoldTApp t of
          (TEmptyHole{}, args)
            | missing <- parentParams - length args
            , missing >= 0 ->
                mkTAppCon cParent $ replicate missing (TEmptyHole ()) <> args
          (THole{}, args)
            | missing <- parentParams - length args
            , missing >= 0 ->
                mkTAppCon cParent $ replicate missing (TEmptyHole ()) <> args
          _ -> t
    -- If typechecking fails because the type @t'@ is not an ADT with a
    -- constructor @c@, and smartholes is on, we attempt to change the term to
    -- '{? c : ? ?}' to recover.
    let recoverSH err =
          asks smartHoles >>= \case
            NoSmartHoles -> throwError' err
            SmartHoles -> do
              -- 'synth' will take care of adding an annotation - no need to do it
              -- explicitly here
              (_, con') <- synth con
              Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t', tcSynthed = TEmptyHole ()}) <*> pure con'
    instantiateValCons t' >>= \case
      Left TDIHoleType -> throwError' $ InternalError "t' is not a hole, as we refined to parent type of c"
      Left TDIUnknown{} -> throwError' $ InternalError "input type to check is not in scope"
      Left TDINotADT -> recoverSH $ ConstructorNotFullAppADT t' c
      Left TDINotSaturated -> recoverSH $ ConstructorNotFullAppADT t' c
      -- If the input type @t@ is a fully-applied ADT constructor 'T As'
      -- And 'C' is a constructor of 'T' (writing 'T's parameters as 'ps' with kinds 'ks')
      -- with arguments 'Rs[ps]',
      -- then this particular instantiation should have arguments 'Rs[As]'
      Right (tc, td, instVCs) -> case lookup c instVCs of
        Nothing -> recoverSH $ ConstructorWrongADT tc c
        Just _argTys -> do
          -- TODO (saturated constructors) unfortunately, due to constructors
          -- currently having type arguments, this is not quite true. We instead
          -- - check the kinding of @tys@ to ensure the third point is sane
          -- - check consistency of @tys@ and 'As' (we do this before the next
          --   point as SmartHoles may kick in in *both* the previous point and
          --   this point, for example eliding a hole above and then re-inserting
          --   it here. We need to check the arguments at the consistent type!)
          -- - instantiate 'T' at @tys@ to find the required types of the term arguments
          tys' <- ensureJust ConstructorTypeArgsKinding $ zipWithExactM checkKind' (snd <$> astTypeDefParameters td) tys
          tAs <-
            ensureJust (InternalError "instantiateValCons succeeded, but decomposeTAppCon did not") $
              pure . snd <$> decomposeTAppCon t'
          -- See comments on UnsaturatedConstructor error below about the fatal 'ensureJust' error
          --
          -- If a type argument is inconsistent between the type and the
          -- constructor, then wrap that of the constructor in a hole.
          -- Arguably we should be finer-grained here, say changing
          -- @Maybe (List Bool) ∋ Just (List Int) t@ into @Just (List {?
          -- Int ?}) t@, but we do not have the infrastructure to do that,
          -- and it is only temporary that constructors have type
          -- arguments, so it does not seem worthwhile. (Note that
          -- normally when we do a consistency check, neither side is
          -- verbatim from the AST, and thus it normally does not make
          -- sense to do smartholes on that problem.)
          tys'' <-
            ensureJust ConstructorTypeArgsInconsistentNumber $
              zipWithExactM
                ( \(tFromConOrig, tFromCon) tFromType ->
                    if consistentTypes (forgetTypeMetadata tFromCon) tFromType
                      then pure tFromCon
                      else
                        asks smartHoles >>= \case
                          NoSmartHoles -> throwError' ConstructorTypeArgsInconsistentTypes
                          -- We are careful to not remove an outer hole when kind checking, only
                          -- to re-add it here with a different ID.
                          SmartHoles -> case tFromConOrig of
                            THole (Meta id _ m) _ -> pure $ THole (Meta id KHole m) tFromCon
                            _ -> THole <$> meta' KHole <*> pure tFromCon
                )
                (zip tys tys')
                tAs
          let tys''NoMeta = forgetTypeMetadata <$> tys''
          instantiateValCons (foldl' (TApp ()) (TCon () tc) tys''NoMeta) >>= \case
            Left _ -> throwError' $ InternalError "instantiateValCons succeeded, but changing type args to others of same kind made it fail"
            Right (_, _, instVCs') -> case lookup c instVCs' of
              Nothing -> throwError' $ InternalError "same ADT now does not contain the constructor"
              Just argTys -> do
                -- Check that the arguments have the correct type
                -- Note that being unsaturated is a fatal error and SmartHoles will not try to recover
                -- (this is a design decision -- we put the burden onto code that builds ASTs,
                -- e.g. the action code is responsible for only creating saturated constructors)
                tms' <- ensureJust (UnsaturatedConstructor c) $ zipWithExactM check argTys tms
                pure $ Con (annotate (TCChkedAt t') i) c tys'' tms'
  lam@(Lam i x e) -> do
    case matchArrowType t of
      Just (t1, t2) -> do
        e' <- local (extendLocalCxt (x, t1)) $ check t2 e
        pure (Lam (annotate (TCChkedAt t) i) x e')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchArrow t
          SmartHoles -> do
            -- 'synth' will take care of adding an annotation - no need to do it
            -- explicitly here
            (_, lam') <- synth lam
            Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t, tcSynthed = TEmptyHole ()}) <*> pure lam'
  lAM@(LAM i n e) -> do
    matchForallType t >>= \case
      Just (m, k, b) -> do
        b' <- substTy m (TVar () n) b
        e' <- local (extendLocalCxtTy (n, k)) $ check b' e
        pure $ LAM (annotate (TCChkedAt t) i) n e'
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchForall t
          SmartHoles -> do
            -- 'synth' will take care of adding an annotation - no need to do it
            -- explicitly here
            (_, lAM') <- synth lAM
            Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t, tcSynthed = TEmptyHole ()}) <*> pure lAM'
  Let i x a b -> do
    -- Synthesise a type for the bound expression
    (aT, a') <- synth a
    -- Extend the context with the binding, and check the body against the type
    b' <- local (extendLocalCxt (x, aT)) $ check t b
    -- NB here: if b were synthesisable, we bubble that information up to the
    -- let, saying @typeOf b'@ rather than @TCChkedAt t@
    -- TODO: why do we do this?
    pure $ Let (annotate (typeOf b') i) x a' b'
  Letrec i x a tA b -> do
    -- Check that tA is well-formed
    tA' <- checkKind' KType tA
    let ctx' = extendLocalCxt (x, forgetTypeMetadata tA')
    -- Check the bound expression against its annotation
    a' <- local ctx' $ check (forgetTypeMetadata tA') a
    -- Extend the context with the binding, and synthesise the body
    b' <- local ctx' $ check t b
    pure $ Letrec (annotate (TCChkedAt t) i) x a' tA' b'
  Case i e brs -> do
    (eT, e') <- synth e
    let caseMeta = annotate (TCChkedAt t) i
    instantiateValCons eT >>= \case
      -- we allow 'case' on a thing of type TEmptyHole iff we have zero branches
      Left TDIHoleType ->
        if null brs
          then pure $ Case caseMeta e' []
          else
            asks smartHoles >>= \case
              NoSmartHoles -> throwError' CaseOfHoleNeedsEmptyBranches
              SmartHoles -> pure $ Case caseMeta e' []
      Left TDINotADT ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonADT eT
          SmartHoles -> do
            -- NB: we wrap the scrutinee in a hole and DELETE the branches
            scrutWrap <- Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
            pure $ Case caseMeta scrutWrap []
      Left (TDIUnknown ty) -> throwError' $ InternalError $ "We somehow synthesised the unknown type " <> show ty <> " for the scrutinee of a case"
      Left TDINotSaturated ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonSaturatedADT eT
          SmartHoles -> do
            -- NB: we wrap the scrutinee in a hole and DELETE the branches
            scrutWrap <- Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
            pure $ Case caseMeta scrutWrap []
      Right (tc, _, expected) -> do
        let branchNames = map (\(CaseBranch n _ _) -> n) brs
        let conNames = map fst expected
        sh <- asks smartHoles
        brs' <- case (branchNames == conNames, sh) of
          (False, NoSmartHoles) -> throwError' $ WrongCaseBranches tc branchNames
          -- create branches with the correct name but wrong parameters,
          -- they will be fixed up in checkBranch later
          (False, SmartHoles) -> traverse (\c -> branch c [] emptyHole) conNames
          (True, _) -> pure brs
        brs'' <- zipWithM (checkBranch t) expected brs'
        pure $ Case caseMeta e' brs''
  e -> do
    sh <- asks smartHoles
    let default_ = do
          (t', e') <- synth e
          if consistentTypes t t'
            then pure (set _typecache (TCEmb TCBoth{tcChkedAt = t, tcSynthed = t'}) e')
            else case sh of
              NoSmartHoles -> throwError' (InconsistentTypes t t')
              SmartHoles -> Hole <$> meta' (TCEmb TCBoth{tcChkedAt = t, tcSynthed = TEmptyHole ()}) <*> pure e'
    case (e, sh) of
      -- If the hole can be dropped leaving a type-correct term, do so
      -- We don't want the recursive call to create a fresh hole though -
      -- this can lead to the output being the same as the input, but with
      -- ID of the top hole changed, leading to losing cursor positions etc.
      -- But we do want to remove nested holes.
      (Hole _ e'@Hole{}, SmartHoles) ->
        check t e' -- we strip off one layer, and hit this case again.
      (Hole _ (Ann _ e' TEmptyHole{}), SmartHoles) ->
        -- We do want to remove (e.g.) {? λx.x : ? ?} to get λx.x,
        -- if that typechecks. (But only a simple hole annotation, as we do
        -- not wish to delete any interesting annotations.)
        flip catchError (const default_) $
          check t e' >>= \case
            Hole{} -> default_ -- Don't let the recursive call mint a hole.
            e'' -> pure e''
      (Hole _ (Ann _ _ ty), SmartHoles)
        | not (noHoles ty) ->
            -- Don't want to, e.g., remove {? λx.x : ? ?} to get λx.x : ?
            -- Since holey annotations behave like non-empty holes, we will
            -- not elide non-empty holes if they have a holey annotation.
            -- (This is needed for idempotency, since we return non-empty
            -- holes with holey-annotated contents in the case a construction
            -- cannot typecheck, e.g. Bool ∋ λx.t returns {? λx.t : ? ?}
            default_
      (Hole _ e', SmartHoles) ->
        flip catchError (const default_) $
          check t e' >>= \case
            Hole{} -> default_ -- Don't let the recursive call mint a hole.
            e'' -> pure e''
      _ -> default_

-- | Similar to check, but for the RHS of case branches
-- We assume that the branch is for this constructor
-- The passed in 'ValCon' is assumed to be a constructor of the passed in 'TypeDef'
checkBranch ::
  forall e m.
  TypeM e m =>
  Type ->
  (ValConName, [Type' ()]) -> -- The constructor and its instantiated parameter types
  CaseBranch' ExprMeta TypeMeta ->
  m (CaseBranch' (Meta TypeCache) (Meta Kind))
checkBranch t (vc, args) (CaseBranch nb patterns rhs) =
  do
    -- We check an invariant due to paranoia
    assertCorrectCon
    sh <- asks smartHoles
    (fixedPats, fixedRHS) <- case (length args == length patterns, sh) of
      (False, NoSmartHoles) -> throwError' CaseBranchWrongNumberPatterns
      -- if the branch is nonsense, replace it with a sensible pattern and an empty hole
      (False, SmartHoles) -> do
        -- Avoid automatically generated names shadowing anything
        globals <- getGlobalBaseNames
        locals <- asks $ M.keysSet . localCxt
        liftA2 (,) (mapM (createBinding (locals <> globals)) args) emptyHole
      -- otherwise, convert all @Maybe TypeCache@ metadata to @TypeCache@
      -- otherwise, annotate each binding with its type
      (True, _) ->
        let args' = zipWith (\ty bind -> (over _bindMeta (annotate (TCChkedAt ty)) bind, ty)) args patterns
         in pure (args', rhs)
    rhs' <- local (extendLocalCxts (map (first bindName) fixedPats)) $ check t fixedRHS
    pure $ CaseBranch nb (map fst fixedPats) rhs'
  where
    createBinding :: S.Set Name -> Type' () -> m (Bind' (Meta TypeCache), Type' ())
    createBinding namesInScope ty = do
      -- Avoid automatically generated names shadowing anything
      name <- freshLocalName' namesInScope
      bind <- Bind <$> meta' (TCChkedAt ty) <*> pure name
      pure (bind, ty)
    assertCorrectCon =
      assert (vc == nb) $
        "checkBranch: expected a branch on "
          <> show vc
          <> " but found branch on "
          <> show nb

-- | Checks if a type can be unified with a function (arrow) type. Returns the
-- arrowised version - i.e. if it's a hole then it returns an arrow type with
-- holes on both sides.
matchArrowType :: Type -> Maybe (Type, Type)
matchArrowType (TEmptyHole _) = pure (TEmptyHole (), TEmptyHole ())
matchArrowType (THole _ _) = pure (TEmptyHole (), TEmptyHole ())
matchArrowType (TFun _ a b) = pure (a, b)
matchArrowType _ = Nothing

-- | Checks if a type can be hole-refined to a forall, and if so returns the
-- forall'd version.
matchForallType :: MonadFresh NameCounter m => Type -> m (Maybe (TyVarName, Kind, Type))
-- These names will never enter the program, so we don't need to avoid shadowing
matchForallType (TEmptyHole _) = (\n -> Just (n, KHole, TEmptyHole ())) <$> freshLocalName mempty
matchForallType (THole _ _) = (\n -> Just (n, KHole, TEmptyHole ())) <$> freshLocalName mempty
matchForallType (TForall _ a k t) = pure $ Just (a, k, t)
matchForallType _ = pure Nothing

-- | Two types are consistent if they are equal (up to IDs and alpha) when we
-- also count holes as being equal to anything.
consistentTypes :: Type -> Type -> Bool
consistentTypes x y = uncurry eqType $ holepunch x y
  where
    -- We punch holes in each type so they "match" in the sense that
    -- they have holes in the same places. (At least, until we find
    -- obviously different constructors.)
    holepunch (TEmptyHole _) _ = (TEmptyHole (), TEmptyHole ())
    holepunch _ (TEmptyHole _) = (TEmptyHole (), TEmptyHole ())
    holepunch (THole _ _) _ = (TEmptyHole (), TEmptyHole ())
    holepunch _ (THole _ _) = (TEmptyHole (), TEmptyHole ())
    holepunch (TFun _ s t) (TFun _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TFun () hs ht, TFun () hs' ht')
    holepunch (TApp _ s t) (TApp _ s' t') =
      let (hs, hs') = holepunch s s'
          (ht, ht') = holepunch t t'
       in (TApp () hs ht, TApp () hs' ht')
    holepunch (TForall _ n k s) (TForall _ m l t) =
      let (hs, ht) = holepunch s t
       in -- Perhaps we need to compare the kinds up to holes also?
          (TForall () n k hs, TForall () m l ht)
    holepunch s t = (s, t)

-- | Compare two types for alpha equality, ignoring their IDs
eqType :: Type' a -> Type' b -> Bool
eqType t1 t2 = forgetTypeMetadata t1 `alphaEqTy` forgetTypeMetadata t2

-- | Convert @Expr (Meta Type) (Meta Kind)@ to @Expr (Meta (Maybe Type)) (Meta (Maybe Kind))@
exprTtoExpr :: ExprT -> Expr
exprTtoExpr = over _exprTypeMeta (fmap Just) . over _exprMeta (fmap Just)

-- | Convert @Type (Meta Kind)@ to @Type (Meta (Maybe Kind))@
typeTtoType :: TypeT -> Type' TypeMeta
typeTtoType = over _typeMeta (fmap Just)

checkKind' :: TypeM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind' k t = modifyError' KindError (checkKind k t)
