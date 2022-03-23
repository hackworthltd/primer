{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

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
  TypeError (..),
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
  checkDef,
  substituteTypeVars,
  getGlobalNames,
  lookupGlobal,
  lookupLocal,
  lookupVar,
  primConInScope,
  mkTypeDefMap,
  consistentKinds,
  consistentTypes,
  extendLocalCxtTy,
  extendLocalCxtTys,
  extendLocalCxt,
  extendLocalCxts,
  extendGlobalCxt,
  localTmVars,
  localTyVars,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.NestedError (MonadNestedError (..))
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Generics.Product (HasType, position, typed)
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Data.String (String)
import Optics (Lens', over, set, traverseOf, view, (%))
import Optics.Traversal (traversed)
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Bind' (..),
  CaseBranch' (..),
  Def (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName (unGVarName),
  ID,
  Kind (..),
  Meta (..),
  PrimCon,
  PrimDef (primDefType),
  TyConName (unTyConName),
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeDef (..),
  TypeMeta,
  ValCon (valConArgs, valConName),
  ValConName (unValConName),
  VarRef (..),
  bindName,
  defName,
  defType,
  primConName,
  typeDefAST,
  typeDefKind,
  typeDefName,
  typeDefParameters,
  valConType,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL (branch, emptyHole, meta, meta')
import Primer.Core.Utils (alphaEqTy, forgetTypeIDs, generateTypeIDs)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, ToJSON, VJSON)
import Primer.Module (Module (moduleDefs, moduleTypes))
import Primer.Name (Name, NameCounter, freshName)
import Primer.Subst (substTy)

-- | Typechecking takes as input an Expr with 'Maybe Type' annotations and
-- produces an Expr with 'Type' annotations - i.e. every node in the output is
-- given a type. The type annotation isn't itself part of the editable program
-- so it has no metadata - hence the '()' argument.
--
-- The 'Type' annotations cache the type which a term synthesised/was checked
-- at. For "embeddings" where typechecking defers to synthesis, we record the
-- synthesised type, not the checked one. For example, when checking that
-- @Int -> ?@ accepts @\x . x@, we record that the variable node has type
-- @Int@, rather than @?@.
type Type = Type' ()

type ExprT = Expr' (Meta TypeCache) (Meta Kind)

type TypeT = Type' (Meta Kind)

-- We should replace this use of `String`. See:
-- https://github.com/hackworthltd/primer/issues/149
data TypeError
  = InternalError String
  | UnknownVariable VarRef
  | UnknownTypeVariable Name
  | WrongSortVariable Name -- type var instead of term var or vice versa
  | UnknownConstructor ValConName
  | UnknownTypeConstructor TyConName
  | -- | Cannot use a PrimCon when either no type of the appropriate name is
    -- in scope, or it is a user-defined type
    PrimitiveTypeNotInScope TyConName
  | CannotSynthesiseType Expr
  | InconsistentTypes Type Type
  | TypeDoesNotMatchArrow Type
  | TypeDoesNotMatchForall Type
  | CaseOfHoleNeedsEmptyBranches
  | CannotCaseNonADT Type
  | CannotCaseNonSaturatedADT Type
  | -- | Either wrong number, wrong constructors or wrong order. The fields are @name of the ADT@, @branches given@
    WrongCaseBranches TyConName [ValConName]
  | CaseBranchWrongNumberPatterns
  | InconsistentKinds Kind Kind
  | KindDoesNotMatchArrow Kind
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON TypeError

assert :: MonadNestedError TypeError e m => Bool -> String -> m ()
assert b s = unless b $ throwError' (InternalError s)

data SmartHoles = SmartHoles | NoSmartHoles
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON SmartHoles

data KindOrType = K Kind | T Type
  deriving (Show, Eq)

data Cxt = Cxt
  { smartHoles :: SmartHoles
  , typeDefs :: M.Map TyConName TypeDef
  -- ^ invariant: the key matches the 'typeDefName' inside the 'TypeDef'
  , localCxt :: Map Name KindOrType
  -- ^ local variables
  , globalCxt :: Map GVarName Type
  -- ^ global variables (i.e. IDs of top-level definitions)
  }
  deriving (Show)

lookupLocal :: Name -> Cxt -> Maybe KindOrType
lookupLocal v cxt = M.lookup v $ localCxt cxt

lookupGlobal :: GVarName -> Cxt -> Maybe Type
lookupGlobal v cxt = M.lookup v $ globalCxt cxt

lookupVar :: VarRef -> Cxt -> Either TypeError Type
lookupVar v cxt = case v of
  LocalVarRef name ->
    pure (lookupLocal name cxt) >>= \case
      Just (T t) -> Right t
      Just (K _) -> Left $ WrongSortVariable name
      Nothing -> Left $ UnknownVariable v
  GlobalVarRef name ->
    pure (lookupGlobal name cxt) >>= \case
      Just t -> Right t
      Nothing -> Left $ UnknownVariable v

extendLocalCxt :: (Name, Type) -> Cxt -> Cxt
extendLocalCxt (name, ty) cxt = cxt{localCxt = Map.insert name (T ty) (localCxt cxt)}

extendLocalCxtTy :: (Name, Kind) -> Cxt -> Cxt
extendLocalCxtTy (name, k) cxt = cxt{localCxt = Map.insert name (K k) (localCxt cxt)}

extendLocalCxts :: [(Name, Type)] -> Cxt -> Cxt
extendLocalCxts x cxt = cxt{localCxt = Map.fromList (map (second T) x) <> localCxt cxt}

extendLocalCxtTys :: [(Name, Kind)] -> Cxt -> Cxt
extendLocalCxtTys x cxt = cxt{localCxt = Map.fromList (map (second K) x) <> localCxt cxt}

extendGlobalCxt :: [(GVarName, Type)] -> Cxt -> Cxt
extendGlobalCxt globals cxt = cxt{globalCxt = Map.fromList globals <> globalCxt cxt}

extendTypeDefCxt :: [TypeDef] -> Cxt -> Cxt
extendTypeDefCxt typedefs cxt = cxt{typeDefs = mkTypeDefMap typedefs <> typeDefs cxt}

localTmVars :: Cxt -> Map Name Type
localTmVars = M.mapMaybe (\case T t -> Just t; K _ -> Nothing) . localCxt

localTyVars :: Cxt -> Map Name Kind
localTyVars = M.mapMaybe (\case K k -> Just k; T _ -> Nothing) . localCxt

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
buildTypingContext :: [TypeDef] -> Map GVarName Def -> SmartHoles -> Cxt
buildTypingContext tydefs defs sh =
  let globals = Map.elems $ fmap (\def -> (defName def, forgetTypeIDs (defType def))) defs
   in extendTypeDefCxt tydefs $ extendGlobalCxt globals $ initialCxt sh

-- | Create a mapping of name to typedef for fast lookup.
-- Ensures that @typeDefName (mkTypeDefMap ! n) == n@
mkTypeDefMap :: [TypeDef] -> Map TyConName TypeDef
mkTypeDefMap defs = M.fromList $ map (\d -> (typeDefName d, d)) defs

-- | A shorthand for the constraints needed when typechecking
type TypeM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e m -- can throw type errors
  )

-- | A lens for the type annotation of an Expr
_typecache :: HasType TypeCache a => Lens' (Expr' a b) TypeCache
_typecache = position @1 % typed @TypeCache

-- | A lens for the (potentially absent) type annotation of an Expr
_maybeTypecache :: HasType (Maybe TypeCache) a => Lens' (Expr' a b) (Maybe TypeCache)
_maybeTypecache = position @1 % typed @(Maybe TypeCache)

-- | Get the (potentially absent) type of an Expr
maybeTypeOf :: HasType (Maybe TypeCache) a => Expr' a b -> Maybe TypeCache
maybeTypeOf = view _maybeTypecache

-- | Get the type of an Expr
typeOf :: HasType TypeCache a => Expr' a b -> TypeCache
typeOf = view _typecache

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v

-- | Check a context is valid
checkValidContext ::
  (MonadFresh ID m, MonadFresh NameCounter m, MonadNestedError TypeError e (ReaderT Cxt m)) =>
  Cxt ->
  m ()
checkValidContext cxt = do
  let tds = typeDefs cxt
  runReaderT (checkTypeDefsMap tds) $ initialCxt NoSmartHoles
  runReaderT (checkGlobalCxt $ globalCxt cxt) $ (initialCxt NoSmartHoles){typeDefs = tds}
  checkLocalCxtTys $ localTyVars cxt
  runReaderT (checkLocalCxtTms $ localTmVars cxt) $ extendLocalCxtTys (M.toList $ localTyVars cxt) (initialCxt NoSmartHoles){typeDefs = tds}
  where
    checkGlobalCxt = mapM_ (checkKind KType <=< fakeMeta)
    -- a tyvar just declares its kind. There are no possible errors in kinds.
    checkLocalCxtTys _tyvars = pure ()
    checkLocalCxtTms = mapM_ (checkKind KType <=< fakeMeta)
    -- We need metadata to use checkKind, but we don't care about the output,
    -- just a yes/no answer. In this case it is fine to put nonsense in the
    -- metadata as it won't be inspected.
    fakeMeta = generateTypeIDs

-- | Check all type definitions, as one recursive group
-- This is the same as 'checkTypeDefs', except it also checks the keys of the
-- map are consistent with the names in the 'TypeDef's
checkTypeDefsMap ::
  TypeM e m =>
  Map TyConName TypeDef ->
  m ()
checkTypeDefsMap tds =
  if and $ M.mapWithKey (\n td -> n == typeDefName td) tds
    then checkTypeDefs $ M.elems tds
    else throwError' $ InternalError "Inconsistent names in a Map TyConName TypeDef"

-- | Check all type definitions, as one recursive group, in some monadic environment
checkTypeDefs ::
  TypeM e m =>
  [TypeDef] ->
  m ()
checkTypeDefs tds = do
  existingTypes <- asks $ Map.elems . typeDefs
  -- NB: we expect the frontend to only submit acceptable typedefs, so all
  -- errors here are "internal errors" and should never be seen.
  -- (This is not quite true, see
  -- https://github.com/hackworthltd/primer/issues/3)
  assert (distinct $ map typeDefName $ existingTypes <> tds) "Duplicate-ly-named TypeDefs"
  -- Note that constructors are synthesisable, so their names must be globally
  -- unique. We need to be able to work out the type of @TCon "C"@ without any
  -- extra information.
  let atds = mapMaybe typeDefAST tds
  let allAtds = mapMaybe typeDefAST existingTypes <> atds
  assert
    (distinct $ concatMap (map valConName . astTypeDefConstructors) allAtds)
    "Duplicate-ly-named constructor (perhaps in different typedefs)"
  -- Note that these checks only apply to non-primitives:
  -- duplicate type names are checked elsewhere, kinds are correct by construction, and there are no constructors.
  local (extendTypeDefCxt tds) $ mapM_ checkTypeDef atds
  where
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

    checkTypeDef td = do
      let params = astTypeDefParameters td
      let cons = astTypeDefConstructors td
      assert
        (distinct $ map fst params <> map (unValConName . valConName) cons)
        "Duplicate names in one tydef: between parameter-names and constructor-names"
      assert
        (notElem (unTyConName $ astTypeDefName td) $ map fst params)
        "Duplicate names in one tydef: between type-def-name and parameter-names"
      local (noSmartHoles . extendLocalCxtTys params) $
        mapM_ (checkKind KType <=< fakeMeta) $ concatMap valConArgs cons
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
checkEverything ::
  forall e m.
  (MonadFresh ID m, MonadFresh NameCounter m, MonadNestedError TypeError e (ReaderT Cxt m)) =>
  SmartHoles ->
  CheckEverythingRequest ->
  m [Module]
checkEverything sh CheckEverything{trusted, toCheck} =
  let cxt =
        buildTypingContext
          (concatMap moduleTypes trusted)
          (foldMap moduleDefs trusted)
          sh
   in flip runReaderT cxt $ do
        -- Check that the definition map has the right keys
        for_ toCheck $ \m -> flip Map.traverseWithKey (moduleDefs m) $ \n d ->
          unless (n == defName d) $ throwError' $ InternalError "Inconsistant names in moduleDefs map"
        checkTypeDefs $ concatMap moduleTypes toCheck
        let newTypes = foldMap moduleTypes toCheck
            newDefs =
              foldMap (\d -> [(defName d, forgetTypeIDs $ defType d)]) $
                foldMap moduleDefs toCheck
        local (extendGlobalCxt newDefs . extendTypeDefCxt newTypes) $
          traverseOf (traversed % #moduleDefs % traversed) checkDef toCheck

-- | Typecheck a definition.
-- This checks that the type signature is well-formed, then checks the body
-- (for an ASTDef) against the signature.
checkDef :: TypeM e m => Def -> m Def
checkDef def = do
  t <- checkKind KType (defType def)
  case def of
    DefAST def' -> do
      e <- check (forgetTypeIDs t) (astDefExpr def')
      pure $ DefAST $ def'{astDefType = typeTtoType t, astDefExpr = exprTtoExpr e}
    DefPrim def' -> do
      pure $ DefPrim $ def'{primDefType = typeTtoType t}

-- We assume that constructor names are unique, returning the first one we find
lookupConstructor :: M.Map TyConName TypeDef -> ValConName -> Maybe (ValCon, ASTTypeDef)
lookupConstructor tyDefs c =
  let allCons = do
        TypeDefAST td <- M.elems tyDefs
        vc <- astTypeDefConstructors td
        pure (vc, td)
   in find ((== c) . valConName . fst) allCons

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
        t' <- checkKind vk t
        bSub <- substTy v (forgetTypeIDs t') b
        pure (bSub, APP (annotate (TCSynthed bSub) i) e' t')
      Nothing ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ TypeDoesNotMatchForall et
          SmartHoles -> do
            eWrap <- Hole <$> meta <*> pure e
            synth $ APP i eWrap t
  Ann i e t -> do
    -- Check that the type is well-formed by synthesising its kind
    t' <- checkKind KType t
    let t'' = forgetTypeIDs t'
    -- Check e against the annotation
    e' <- check t'' e
    -- Annotate the Ann with the same type as e
    pure $ annSynth2 t'' i Ann e' t'
  EmptyHole i -> pure $ annSynth0 (TEmptyHole ()) i EmptyHole
  -- We assume that constructor names are unique
  -- See Note [Synthesisable constructors] in Core.hs
  Con i c -> do
    asks (flip lookupConstructor c . typeDefs) >>= \case
      Just (vc, td) -> let t = valConType td vc in pure $ annSynth1 t i Con c
      Nothing -> throwError' $ UnknownConstructor c
  -- When synthesising a hole, we first check that the expression inside it
  -- synthesises a type successfully.
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
    tA' <- checkKind KType tA
    let t = forgetTypeIDs tA'
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
            Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure lam'
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
            Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure lAM'
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
    tA' <- checkKind KType tA
    let ctx' = extendLocalCxt (x, forgetTypeIDs tA')
    -- Check the bound expression against its annotation
    a' <- local ctx' $ check (forgetTypeIDs tA') a
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
      Left (TDIUnknownADT ty) -> throwError' $ InternalError $ "We somehow synthesised the unknown type " <> show ty <> " for the scrutinee of a case"
      Left (TDIMalformed wanted found) -> throwError' $ InternalError $ "invariant failed: Looked up def for " <> show wanted <> ", but found a def for " <> show found
      Left TDINotSaturated ->
        asks smartHoles >>= \case
          NoSmartHoles -> throwError' $ CannotCaseNonSaturatedADT eT
          SmartHoles -> do
            -- NB: we wrap the scrutinee in a hole and DELETE the branches
            scrutWrap <- Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
            pure $ Case caseMeta scrutWrap []
      Right (defT, expected) -> do
        let branchNames = map (\(CaseBranch n _ _) -> n) brs
        let conNames = map fst expected
        sh <- asks smartHoles
        brs' <- case (branchNames == conNames, sh) of
          (False, NoSmartHoles) -> throwError' $ WrongCaseBranches (astTypeDefName defT) branchNames
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
              SmartHoles -> Hole <$> meta' (TCSynthed (TEmptyHole ())) <*> pure e'
    case (e, sh) of
      -- If the hole can be dropped leaving a type-correct term, do so
      -- We don't want the recursive call to create a fresh hole though -
      -- this can lead to the output being the same as the input, but with
      -- ID of the top hole changed, leading to losing cursor positions etc.
      -- But we do want to remove nested holes.
      (Hole _ e'@Hole{}, SmartHoles) ->
        check t e' -- we strip off one layer, and hit this case again.
      (Hole _ e', SmartHoles) ->
        flip catchError (const default_) $
          check t e' >>= \case
            Hole{} -> default_ -- Don't let the recursive call mint a hole.
            e'' -> pure e''
      _ -> default_

-- Synthesise a kind for the given type
-- TypeHoles are always considered to have kind KHole - a kind hole.
-- When SmartHoles is on, we essentially remove all holes, and re-insert where
-- necessary.
-- However, we take care not to remove a non-empty hole only to immediately
-- re-insert it, since this would needlessly change its ID, resulting in
-- problems if an action left the cursor on such a hole: "lost ID after
-- typechecking". For example, consider (numbers are denoting IDs inside the
-- metadata)
--   synthKind $ TApp 0 (THole 1 (TCon 2 Bool)) t
-- If we removed the hole, we would then note that Bool does not have an arrow
-- kind, and so wrap it in a hole again, returning something like
--   TApp 0 (THole 3 (TCon 2 Bool)) t
-- A similar thing would happen with
--   synthKind $ TApp 0 (TCon 1 List) (THole 2 (TCon 3 List))
-- because we do not have checkKind KType List
synthKind :: TypeM e m => Type' (Meta a) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KHole, TEmptyHole (annotate KHole m))
  THole m t -> do
    sh <- asks smartHoles
    (k, t') <- synthKind t
    case sh of
      NoSmartHoles -> pure (KHole, THole (annotate KHole m) t')
      SmartHoles -> pure (k, t')
  TCon m c -> do
    typeDef <- asks (M.lookup c . typeDefs)
    case typeDef of
      Nothing -> throwError' $ UnknownTypeConstructor c
      Just def -> let k = typeDefKind def in pure (k, TCon (annotate k m) c)
  TFun m a b -> do
    a' <- checkKind KType a
    b' <- checkKind KType b
    pure (KType, TFun (annotate KType m) a' b')
  TVar m v -> do
    asks (lookupLocal v) >>= \case
      Just (K k) -> pure (k, TVar (annotate k m) v)
      Just (T _) -> throwError' $ WrongSortVariable v
      Nothing -> throwError' (UnknownTypeVariable v)
  TApp ma (THole mh s) t -> do
    -- If we didn't have this special case, we might remove this hole (in a
    -- recursive call), only to reintroduce it again with a different ID
    -- TODO: ugly and duplicated...
    sh <- asks smartHoles
    (k, s') <- synthKind s
    case (matchArrowKind k, sh) of
      (_, NoSmartHoles) -> checkKind KHole t >>= \t' -> pure (KHole, TApp (annotate KHole ma) (THole (annotate KHole mh) s') t')
      (Nothing, SmartHoles) -> checkKind KHole t >>= \t' -> pure (KHole, TApp (annotate KHole ma) (THole (annotate KHole mh) s') t')
      (Just (k1, k2), SmartHoles) -> checkKind k1 t >>= \t' -> pure (k2, TApp (annotate k2 ma) s' t')
  TApp m s t -> do
    sh <- asks smartHoles
    (k, s') <- synthKind s
    case (matchArrowKind k, sh) of
      (Nothing, NoSmartHoles) -> throwError' $ KindDoesNotMatchArrow k
      (Nothing, SmartHoles) -> do
        sWrap <- THole <$> meta' KHole <*> pure s'
        t' <- checkKind KHole t
        pure (KHole, TApp (annotate KHole m) sWrap t')
      (Just (k1, k2), _) -> checkKind k1 t >>= \t' -> pure (k2, TApp (annotate k2 m) s' t')
  TForall m n k t -> do
    t' <- local (extendLocalCxtTy (n, k)) $ checkKind KType t
    pure (KType, TForall (annotate KType m) n k t')

checkKind :: TypeM e m => Kind -> Type' (Meta a) -> m TypeT
checkKind k (THole m t) = do
  -- If we didn't have this special case, we might remove this hole (in a
  -- recursive call), only to reintroduce it again with a different ID
  -- TODO: ugly and duplicated...
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (_, NoSmartHoles) -> pure $ THole (annotate KHole m) t'
    (True, SmartHoles) -> pure t'
    (False, SmartHoles) -> pure $ THole (annotate KHole m) t'
checkKind k t = do
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (True, _) -> pure t'
    (False, NoSmartHoles) -> throwError' $ InconsistentKinds k k'
    (False, SmartHoles) -> THole <$> meta' KHole <*> pure t'

data TypeDefError
  = TDIHoleType -- a type hole
  | TDINotADT -- e.g. a function type etc
  | TDIUnknownADT TyConName -- not in scope
  | TDIMalformed TyConName TyConName -- TDIMalformed T S: looking up @T@ gives a 'TypeDef' for with name @S@ different to @T@
  | TDINotSaturated -- e.g. @List@ or @List a b@ rather than @List a@

data TypeDefInfo a = TypeDefInfo [Type' a] TypeDef -- instantiated parameters, and the typedef, i.e. [Int] are the parameters for @List Int@

getTypeDefInfo :: MonadReader Cxt m => Type' a -> m (Either TypeDefError (TypeDefInfo a))
getTypeDefInfo t = reader $ flip getTypeDefInfo' t . typeDefs

getTypeDefInfo' :: Map TyConName TypeDef -> Type' a -> Either TypeDefError (TypeDefInfo a)
getTypeDefInfo' _ (TEmptyHole _) = Left TDIHoleType
getTypeDefInfo' _ (THole _ _) = Left TDIHoleType
getTypeDefInfo' tydefs ty =
  case decomposeTAppCon ty of
    Nothing -> Left TDINotADT
    Just (tycon, params) -> do
      case M.lookup tycon tydefs of
        Nothing -> Left $ TDIUnknownADT tycon
        Just tydef
          -- a check out of paranoia
          | typeDefName tydef /= tycon -> Left $ TDIMalformed tycon (typeDefName tydef)
          -- this check would be redundant if we were sure that the input type
          -- were of kind KType, alternatively we should do kind checking here
          | length (typeDefParameters tydef) /= length params -> Left TDINotSaturated
          | otherwise -> Right $ TypeDefInfo params tydef

-- | Takes a particular instance of a parameterised type (e.g. @List Nat@), and
-- extracts both both the raw typedef (e.g. @List a = Nil | Cons a (List a)@)
-- and the constructors with instantiated argument types
-- (e.g. @Nil : List Nat ; Cons : Nat -> List Nat -> List Nat@)
instantiateValCons :: (MonadFresh NameCounter m, MonadReader Cxt m) => Type' () -> m (Either TypeDefError (ASTTypeDef, [(ValConName, [Type' ()])]))
instantiateValCons t = do
  tds <- asks typeDefs
  let instCons = instantiateValCons' tds t
      sequence4 =
        fmap (getCompose . getCompose . getCompose . getCompose)
          . sequence
          . Compose
          . Compose
          . Compose
          . Compose
  sequence4 instCons

-- | As 'instantiateValCons', but pulls out the relevant bits of the monadic
-- context into an argument
instantiateValCons' :: MonadFresh NameCounter m => Map TyConName TypeDef -> Type' () -> Either TypeDefError (ASTTypeDef, [(ValConName, [m (Type' ())])])
instantiateValCons' tyDefs t = do
  TypeDefInfo params def <- getTypeDefInfo' tyDefs t
  case def of
    TypeDefPrim _ -> Left TDINotADT
    TypeDefAST tda -> do
      let defparams = map fst $ astTypeDefParameters tda
          f c = (valConName c, map (substituteTypeVars $ zip defparams params) $ valConArgs c)
      pure (tda, map f $ astTypeDefConstructors tda)

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
        globals <- getGlobalNames
        locals <- asks $ M.keysSet . localCxt
        liftA2 (,) (mapM (createBinding (locals <> globals)) args) emptyHole
      -- otherwise, convert all @Maybe TypeCache@ metadata to @TypeCache@
      -- otherwise, annotate each binding with its type
      (True, _) ->
        let args' = zipWith (\ty bind -> (over (position @1) (annotate (TCChkedAt ty)) bind, ty)) args patterns
         in pure (args', rhs)
    rhs' <- local (extendLocalCxts (map (first bindName) fixedPats)) $ check t fixedRHS
    pure $ CaseBranch nb (map fst fixedPats) rhs'
  where
    createBinding :: S.Set Name -> Type' () -> m (Bind' (Meta TypeCache), Type' ())
    createBinding namesInScope ty = do
      -- Avoid automatically generated names shadowing anything
      name <- freshName namesInScope
      bind <- Bind <$> meta' (TCChkedAt ty) <*> pure name
      pure (bind, ty)
    assertCorrectCon =
      assert (vc == nb) $
        "checkBranch: expected a branch on "
          <> show vc
          <> " but found branch on "
          <> show nb

substituteTypeVars :: MonadFresh NameCounter m => [(Name, Type' ())] -> Type' () -> m (Type' ())
substituteTypeVars = flip $ foldrM (uncurry substTy)

-- | Decompose @C X Y Z@ to @(C,[X,Y,Z])@
decomposeTAppCon :: Type' a -> Maybe (TyConName, [Type' a])
decomposeTAppCon ty = do
  (con, args) <- go ty
  pure (con, reverse args)
  where
    go (TCon _ con) = Just (con, [])
    go (TApp _ t s) = do
      (con, args) <- go t
      pure (con, s : args)
    go _ = Nothing

-- | @mkTAppCon C [X,Y,Z] = C X Y Z@
mkTAppCon :: TyConName -> [Type' ()] -> Type' ()
mkTAppCon c = foldl' (TApp ()) (TCon () c)

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
-- NB: holes can behave as ∀(a:KType). ..., but not of any higher kind
-- (We may revisit this later)
matchForallType :: MonadFresh NameCounter m => Type -> m (Maybe (Name, Kind, Type))
-- These names will never enter the program, so we don't need to avoid shadowing
matchForallType (TEmptyHole _) = (\n -> Just (n, KType, TEmptyHole ())) <$> freshName mempty
matchForallType (THole _ _) = (\n -> Just (n, KType, TEmptyHole ())) <$> freshName mempty
matchForallType (TForall _ a k t) = pure $ Just (a, k, t)
matchForallType _ = pure Nothing

matchArrowKind :: Kind -> Maybe (Kind, Kind)
matchArrowKind KHole = pure (KHole, KHole)
matchArrowKind KType = Nothing
matchArrowKind (KFun k1 k2) = pure (k1, k2)

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

consistentKinds :: Kind -> Kind -> Bool
consistentKinds KHole _ = True
consistentKinds _ KHole = True
consistentKinds KType KType = True
consistentKinds (KFun k1 k2) (KFun k1' k2') = consistentKinds k1 k1' && consistentKinds k2 k2'
consistentKinds _ _ = False

-- | Compare two types for alpha equality, ignoring their IDs
eqType :: Type' a -> Type' b -> Bool
eqType t1 t2 = forgetTypeIDs t1 `alphaEqTy` forgetTypeIDs t2

-- | Convert @Expr (Meta Type) (Meta Kind)@ to @Expr (Meta (Maybe Type)) (Meta (Maybe Kind))@
exprTtoExpr :: ExprT -> Expr
exprTtoExpr = over _exprTypeMeta (fmap Just) . over _exprMeta (fmap Just)

-- | Convert @Type (Meta Kind)@ to @Type (Meta (Maybe Kind))@
typeTtoType :: TypeT -> Type' TypeMeta
typeTtoType = over _typeMeta (fmap Just)

-- Helper to create fresh names
getGlobalNames :: MonadReader Cxt m => m (S.Set Name)
getGlobalNames = do
  tyDefs <- asks typeDefs
  topLevel <- asks $ S.fromList . map unGVarName . M.keys . globalCxt
  let ctors =
        Map.foldMapWithKey
          ( \t def ->
              S.fromList $
                (unTyConName t :) $
                  map (unValConName . valConName) $ maybe [] astTypeDefConstructors $ typeDefAST def
          )
          tyDefs
  pure $ S.union topLevel ctors
