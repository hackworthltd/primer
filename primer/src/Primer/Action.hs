{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Action (
  Action (..),
  ActionError (..),
  Movement (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  applyActionsToExpr,
  moveExpr,
  uniquifyDefName,
  toProgActionInput,
  toProgActionNoInput,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (Value)
import Data.Generics.Product (typed)
import Data.Bifunctor.Swap qualified as Swap
import Data.List (findIndex)
import Data.List.NonEmpty qualified as NE
import Data.List.Extra ((!?))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics (set, (%), (?~), (^.), (^?), _Just)
import Primer.Action.Actions (Action (..), Movement (..), QualifiedText)
import Primer.Action.Available qualified as Available
import Primer.Action.Errors (ActionError (..))
import Primer.Action.ProgAction (ProgAction (..))
import Primer.App.Base (NodeType (..))
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  HasID,
  HasMetadata (_metadata),
  ID,
  LVarName,
  LocalName (LocalName, unLocalName),
  PrimCon (PrimChar, PrimInt),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  ValConName,
  baseName,
  bindName,
  getID,
  qualifiedModule,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _chkedAt,
  _exprMetaLens,
  _type,
 )
import Primer.Core qualified as C
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  apps',
  branch,
  case_,
  con0,
  emptyHole,
  hole,
  lAM,
  lam,
  let_,
  letrec,
  prim,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
  var, conSat, apps,
 )
import Primer.Core.Transform (renameLocalVar, renameTyVar, renameTyVarExpr, unfoldFun)
import Primer.Core.Utils (forgetTypeMetadata, generateTypeIDs)
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
 )
import Primer.Module (Module, insertDef)
import Primer.Name (Name, NameCounter, unName, unsafeMkName)
import Primer.Name.Fresh (
  isFresh,
  isFreshTy,
  mkFreshName,
  mkFreshNameTy,
 )
import Primer.Questions (uniquify)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.TypeDef (ASTTypeDef (..), TypeDef (..), ValCon (..), valConType)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles,
  TypeError,
  buildTypingContextFromModules,
  check,
  checkEverything,
  exprTtoExpr,
  getTypeDefInfo,
  lookupConstructor,
  lookupVar,
  maybeTypeOf,
  synth,
 )
import Primer.Typecheck qualified as TC
import Primer.Zipper (
  BindLoc' (..),
  CaseBindZ,
  ExprZ,
  IsZipper,
  Loc,
  Loc' (..),
  SomeNode (..),
  TypeZ,
  down,
  findNodeWithParent,
  findType,
  focus,
  focusLoc,
  focusOn,
  focusType,
  locToEither,
  replace,
  right,
  target,
  top,
  unfocus,
  unfocusExpr,
  unfocusLoc,
  unfocusType,
  up,
  updateCaseBind,
  _target, focusConTypes,
 )
import Primer.ZipperCxt (localVariablesInScopeExpr)

-- | Given a definition name and a program, return a unique variant of
-- that name (within the specified module). Note that if no definition
-- of the given name already exists in the program, this function will
-- return the same name it's been given.
uniquifyDefName :: C.ModuleName -> Text -> DefMap -> Text
uniquifyDefName m name' defs = unName $ uniquify avoid $ unsafeMkName name'
  where
    f qn
      | qualifiedModule qn == m = Set.singleton $ baseName qn
      | otherwise = mempty
    avoid :: Set Name
    avoid = foldMap' f $ Map.keys defs

-- | A shorthand for the constraints needed when applying actions
type ActionM m =
  ( Monad m
  , MonadFresh ID m -- can generate fresh IDs
  , MonadFresh NameCounter m -- can generate fresh names
  , MonadError ActionError m -- can raise errors
  , MonadReader TC.Cxt m -- has access to a typing context
  )

-- | Apply a sequence of actions to the type signature of a definition
-- We apply the actions to the type, then typecheck the body of the definition against the new type.
-- We must then typecheck the whole program to check any uses of the definition.
-- Note that this may introduce new holes when using SmartHoles, and thus we
-- return a whole set of modules as well as the one definition we wanted to
-- change.
applyActionsToTypeSig ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [Module] ->
  -- | The @Module@ we are focused on, and all the other editable modules
  (Module, [Module]) ->
  -- | This must be one of the definitions in the @Module@, with its correct name
  (Name, ASTDef) ->
  [Action] ->
  m (Either ActionError ([Module], TypeZ))
applyActionsToTypeSig smartHoles imports (mod, mods) (defName, def) actions =
  runReaderT
    go
    (buildTypingContextFromModules (mod : mods <> imports) smartHoles)
    & runExceptT
  where
    go :: ActionM m => m ([Module], TypeZ)
    go = do
      zt <- withWrappedType (astDefType def) (\zt -> foldlM (flip applyActionAndSynth) (InType zt) actions)
      let t = target (top zt)
      e <- check (forgetTypeMetadata t) (astDefExpr def)
      let def' = def{astDefExpr = exprTtoExpr e, astDefType = t}
          mod' = insertDef mod defName (DefAST def')
      -- The actions were applied to the type successfully, and the definition body has been
      -- typechecked against the new type.
      -- Now we need to typecheck the whole program again, to check any uses of the definition
      -- We make sure that the updated type is present in the global context.
      -- Here we just check the whole of the mutable prog, excluding imports.
      -- (for efficiency, we need not check the type definitions, but we do not implement this optimisation)
      checkEverything smartHoles (CheckEverything{trusted = imports, toCheck = mod' : mods})
        >>= \checkedMods -> pure (checkedMods, zt)
    -- Actions expect that all ASTs have a top-level expression of some sort.
    -- Signatures don't have this: they're just a type.
    -- We fake it by wrapping the type in a top-level annotation node, then unwrapping afterwards.
    withWrappedType :: ActionM m => Type -> (TypeZ -> m Loc) -> m TypeZ
    withWrappedType ty f = do
      wrappedType <- ann emptyHole (pure ty)
      let unwrapError = throwError $ InternalFailure "applyActionsToTypeSig: failed to unwrap type"
          wrapError = throwError $ InternalFailure "applyActionsToTypeSig: failed to wrap type"
          focusedType = focusType $ focus wrappedType
      case focusedType of
        -- This should be impossible
        Nothing -> wrapError
        Just wrappedTy ->
          f wrappedTy >>= \case
            InType zt -> pure zt
            -- This probably shouldn't happen, but it may be the case that an action accidentally
            -- exits the type and ends up in the outer expression that we have created as a wrapper.
            -- In this case we just refocus on the top of the type.
            z -> maybe unwrapError pure (focusType (unfocusLoc z))

data Refocus = Refocus
  { pre :: Loc
  , post :: Expr
  }

-- If smartholes is on, we may refocus on the interior of an elided hole,
-- or the expression under an elided annotation
refocus :: MonadReader TC.Cxt m => Refocus -> m (Maybe Loc)
refocus Refocus{pre, post} = do
  sh <- asks TC.smartHoles
  let candidateIDs = case sh of
        TC.NoSmartHoles -> [getID pre]
        TC.SmartHoles -> case pre of
          InExpr e -> candidateIDsExpr $ target e
          InType t -> candidateIDsType $ target t
          InBind (BindCase ze) -> [getID ze]
  pure . getFirst . mconcat $ fmap (\i -> First $ focusOn i post) candidateIDs
  where
    candidateIDsExpr e =
      getID e : case e of
        Hole _ e' -> candidateIDsExpr e'
        Ann _ e' _ -> candidateIDsExpr e'
        _ -> []
    candidateIDsType t =
      getID t : case t of
        THole _ t' -> candidateIDsType t'
        _ -> []

-- | Apply a sequence of actions to the body of a definition, producing a new Expr or an error if
-- any of the actions failed to apply.
-- After applying the actions, we check the new Expr against the type sig of the definition.
applyActionsToBody ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [Module] ->
  ASTDef ->
  [Action] ->
  m (Either ActionError (ASTDef, Loc))
applyActionsToBody sh modules def actions =
  go
    & flip runReaderT (buildTypingContextFromModules modules sh)
    & runExceptT
  where
    go :: ActionM m => m (ASTDef, Loc)
    go = do
      ze <- foldlM (flip (applyActionAndCheck (astDefType def))) (focusLoc (astDefExpr def)) actions
      e' <- exprTtoExpr <$> check (forgetTypeMetadata (astDefType def)) (unfocus ze)
      let def' = def{astDefExpr = e'}
      refocus Refocus{pre = ze, post = e'} >>= \case
        Nothing -> throwError $ InternalFailure "lost ID after typechecking"
        Just z -> pure (def', z)

applyActionAndCheck :: ActionM m => Type -> Action -> Loc -> m Loc
applyActionAndCheck ty action z = do
  z' <- applyAction' action z
  typedAST <- check (forgetTypeMetadata ty) $ unfocus z'
  -- Refocus on where we were previously
  refocus Refocus{pre = z', post = exprTtoExpr typedAST} >>= \case
    Just z'' -> pure z''
    Nothing -> throwError $ CustomFailure action "internal error: lost ID after typechecking"

-- This is currently only used for tests.
-- We may need it in the future for a REPL, where we want to build standalone expressions.
-- We take a list of the modules that should be in scope for the test.
applyActionsToExpr :: (MonadFresh ID m, MonadFresh NameCounter m) => SmartHoles -> [Module] -> Expr -> [Action] -> m (Either ActionError (Either ExprZ TypeZ))
applyActionsToExpr sh modules expr actions =
  foldlM (flip applyActionAndSynth) (focusLoc expr) actions -- apply all actions
    <&> locToEither
    & flip runReaderT (buildTypingContextFromModules modules sh)
    & runExceptT -- catch any errors

applyActionAndSynth :: ActionM m => Action -> Loc -> m Loc
applyActionAndSynth action z = do
  z' <- applyAction' action z
  synthZ z' >>= \case
    Just z'' -> pure z''
    Nothing -> throwError $ CustomFailure action "internal error: lost ID after typechecking"

-- There's some fiddly manipulations here because the output of the typechecker
-- is @Expr' (Meta Type) (Meta Kind)@ but we need
-- @Expr' (Meta (Maybe Type)) (Meta (Maybe Kind))@, i.e. we need to wrap each
-- type and kind annotation in @Just@.
--
-- 'Nothing' means the current focussed ID disappeared after typechecking
synthZ :: ActionM m => Loc -> m (Maybe Loc)
synthZ z = do
  (_, typedAST) <- synth $ unfocus z
  -- Refocus on where we were previously
  refocus Refocus{pre = z, post = exprTtoExpr typedAST}

applyAction' :: ActionM m => Action -> Loc -> m Loc
applyAction' a = case a of
  NoOp -> pure
  SetCursor i -> setCursor i . unfocusLoc
  Move m -> \case
    InExpr z -> InExpr <$> moveExpr m z
    InType z -> InType <$> moveType m z
    z@(InBind _) -> case m of
      -- If we're moving up from a binding, then shift focus to the nearest parent expression.
      -- This is exactly what 'unfocusLoc' does if the 'Loc' is a binding.
      Parent -> pure . InExpr $ unfocusLoc z
      _ -> throwError $ CustomFailure (Move m) "Can only move up from a binding"
  Delete -> \case
    InExpr ze -> InExpr . flip replace ze <$> emptyHole
    InType zt -> InType . flip replace zt <$> tEmptyHole
    InBind _ -> throwError $ CustomFailure Delete "Cannot delete a binding"
  SetMetadata d -> \case
    InExpr ze -> pure $ InExpr $ setMetadata d ze
    InType zt -> pure $ InType $ setMetadata d zt
    InBind (BindCase zb) -> pure $ InBind $ BindCase $ setMetadata d zb
  EnterHole -> termAction enterHole "non-empty type holes not supported"
  FinishHole -> termAction finishHole "there are no non-empty holes in types"
  ConstructVar x -> termAction (constructVar x) "cannot construct var in type"
  InsertSaturatedVar x -> termAction (insertSatVar x) "cannot insert var in type"
  InsertRefinedVar x -> termAction (insertRefinedVar x) "cannot insert var in type"
  ConstructApp -> termAction constructApp "cannot construct app in type"
  ConstructAPP -> termAction constructAPP "cannot construct term-to-type app in type"
  ConstructAnn -> termAction constructAnn "cannot construct annotation in type"
  RemoveAnn -> termAction removeAnn "there are no annotations in types"
  ConstructLam x -> termAction (constructLam x) "cannot construct function in type"
  ConstructLAM x -> termAction (constructLAM x) "cannot construct function in type"
  ConstructPrim p -> termAction (constructPrim p) "cannot construct primitive literal in type"
  ConstructSaturatedCon c -> termAction (constructSatCon c) "cannot construct con in type"
  ConstructRefinedCon c -> termAction (constructRefinedCon c) "cannot construct con in type"
  ConstructLet x -> termAction (constructLet x) "cannot construct let in type"
  ConstructLetrec x -> termAction (constructLetrec x) "cannot construct letrec in type"
  ConvertLetToLetrec -> termAction convertLetToLetrec "cannot convert type to letrec"
  ConstructCase -> termAction constructCase "cannot construct case in type"
  RenameLam x -> termAction (renameLam x) "cannot rename lam in type"
  RenameLAM x -> termAction (renameLAM x) "cannot rename LAM in type"
  RenameLet x -> termAction (renameLet x) "cannot rename let in type"
  EnterType -> \case
    InExpr ze -> InType <$> enterType ze
    _ -> throwError $ CustomFailure EnterType "cannot enter type - already in type"
  ExitType -> \case
    InType zt -> pure $ InExpr $ unfocusType zt
    _ -> throwError $ CustomFailure ExitType "cannot exit type - not in type"
  EnterConTypeArgument n -> \case
    InExpr ze ->  case (!? n) <$> focusConTypes ze of
      Nothing -> throwError $ CustomFailure (EnterConTypeArgument n) "Move-to-constructor-argument failed: this is not a constructor"
      Just Nothing -> throwError $ CustomFailure (EnterConTypeArgument n) "Move-to-constructor-argument failed: no such argument"
      Just (Just z') -> pure $ InType z'
    _ -> throwError $ CustomFailure (EnterConTypeArgument n) "cannot enter value constructors argument - not in expr"
  ConstructArrowL -> typeAction constructArrowL "cannot construct arrow - not in type"
  ConstructArrowR -> typeAction constructArrowR "cannot construct arrow - not in type"
  ConstructTCon c -> typeAction (constructTCon c) "cannot construct tcon in expr"
  ConstructTVar v -> typeAction (constructTVar v) "cannot construct tvar in expr"
  ConstructTForall n -> typeAction (constructTForall n) "cannot construct forall in expr"
  ConstructTApp -> typeAction constructTApp "cannot construct type-level application in expr"
  RenameForall x -> typeAction (renameForall x) "cannot rename forall in expr"
  RenameCaseBinding x -> \case
    InBind (BindCase z) -> InBind . BindCase <$> renameCaseBinding x z
    _ -> throwError $ CustomFailure a "cannot rename this node - not a case binding"
  where
    termAction f s = \case
      InExpr ze -> InExpr <$> f ze
      _ -> throwError $ CustomFailure a s
    typeAction f s = \case
      InType zt -> InType <$> f zt
      _ -> throwError $ CustomFailure a s

setCursor :: ActionM m => ID -> ExprZ -> m Loc
setCursor i e = case focusOn i (unfocusExpr e) of
  Just e' -> pure e'
  Nothing -> throwError $ IDNotFound i

-- | Apply a movement to a zipper
moveExpr :: ActionM m => Movement -> ExprZ -> m ExprZ
moveExpr m@(Branch c) z | Case _ _ brs <- target z =
  case findIndex (\(C.CaseBranch n _ _) -> c == n) brs of
    Nothing -> throwError $ CustomFailure (Move m) "Move-to-branch failed: no such branch"
    -- 'down' moves into the scrutinee, 'right' then steps through branch
    -- rhss
    Just i -> case foldr (\_ z' -> right =<< z') (down z) [0 .. i] of
      Just z' -> pure z'
      Nothing -> throwError $ CustomFailure (Move m) "internal error: movement failed, even though branch exists"
moveExpr m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch failed: this is not a case expression"
moveExpr m@(ConChild n) z | Con{} <- target z =
    -- 'down' moves into the first argument, 'right' steps through the various arguments
  case foldr (\_ z' -> right =<< z') (down z) [1..n] of
      Just z' -> pure z'
      Nothing -> throwError $ CustomFailure (Move m) "Move-to-constructor-argument failed: no such argument"
moveExpr m@(ConChild _) _ = throwError $ CustomFailure (Move m) "Move-to-constructor-argument failed: this is not a constructor"
moveExpr Child2 z
  | Case{} <- target z =
      throwError $ CustomFailure (Move Child2) "cannot move to 'Child2' of a case: use Branch instead"
moveExpr m z = move m z

-- | Apply a movement to a zipper
moveType :: ActionM m => Movement -> TypeZ -> m TypeZ
moveType m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch unsupported in types (there are no cases in types!)"
moveType m@(ConChild _) _ = throwError $ CustomFailure (Move m) "Move-to-constructor-argument unsupported in types (type constructors do not directly store their arguments)"
moveType m z = move m z

-- | Apply a movement to a generic zipper - does not support movement to a case
-- branch, or into an argument of a constructor
move :: forall m za a. (ActionM m, IsZipper za a, HasID za) => Movement -> za -> m za
move m z = do
  mz' <- move' m z
  case mz' of
    Just z' -> pure z'
    Nothing -> throwError $ MovementFailed (getID z, m)
  where
    move' :: Movement -> za -> m (Maybe za)
    move' Parent = pure . up
    move' Child1 = pure . down
    move' Child2 = pure . (down >=> right)
    move' (Branch _) = const $ throwError $ InternalFailure "move does not support Branch moves"
    move' (ConChild _) = const $ throwError $ InternalFailure "move does not support Constructor argument moves"

setMetadata :: (IsZipper za a, HasMetadata a) => Value -> za -> za
setMetadata d z = z & _target % _metadata ?~ d

enterHole :: ActionM m => ExprZ -> m ExprZ
enterHole ze = case target ze of
  EmptyHole{} -> do
    result <- flip replace ze <$> hole emptyHole
    move Child1 result
  e -> throwError $ NeedEmptyHole EnterHole e

finishHole :: ActionM m => ExprZ -> m ExprZ
finishHole ze = case target ze of
  Hole _ e -> pure $ replace e ze
  e -> throwError $ NeedNonEmptyHole FinishHole e

constructVar :: ActionM m => TmVarRef -> ExprZ -> m ExprZ
constructVar x ast = case target ast of
  EmptyHole{} -> flip replace ast <$> var x
  e -> throwError $ NeedEmptyHole (ConstructVar x) e

insertSatVar :: ActionM m => TmVarRef -> ExprZ -> m ExprZ
insertSatVar x ast = case target ast of
  -- TODO: for now, rely on SmartHoles to fix up the type. I think we may want
  -- to do this manually to handle running with NoSmartHoles. There is no concern
  -- over "jumpy holes", as we will never emit something synthesing an arrow/forall
  -- type which would force an argument into a hole.
  --
  -- Revisit this once we decide on a smart hole toggle strategy. See:
  -- https://github.com/hackworthltd/primer/issues/9
  EmptyHole{} -> flip replace ast <$> saturatedApplication ast x
  e -> throwError $ NeedEmptyHole (InsertSaturatedVar x) e

-- TODO: Add some tests for this. See:
-- https://github.com/hackworthltd/primer/issues/10
saturatedApplication :: ActionM m => ExprZ -> TmVarRef -> m Expr
saturatedApplication ast v = do
  (appHead, vTy) <-
    getVarType ast v >>= \case
      Left err -> throwError $ SaturatedApplicationError $ Right err
      Right t -> pure (var v, t)
  mkSaturatedApplication appHead vTy

getVarType ::
  MonadReader TC.Cxt m =>
  ExprZ ->
  TmVarRef ->
  m (Either TypeError TC.Type)
getVarType ast x =
  local extendCxt $ lookupVar x <$> ask
  where
    -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
    -- See https://github.com/hackworthltd/primer/issues/11
    extendCxt =
      let (tycxt, tmcxt) = localVariablesInScopeExpr $ Left ast
       in \cxt ->
            cxt
              { TC.localCxt =
                  Map.fromList $
                    map (bimap unLocalName TC.T) tmcxt
                      <> map (bimap unLocalName TC.K) tycxt
              }

mkSaturatedApplicationArgs :: MonadFresh ID m => TC.Type -> [Either (m Expr) (m Type)]
mkSaturatedApplicationArgs = \case
  TFun _ _ t -> Left emptyHole : mkSaturatedApplicationArgs t
  -- possibly we should substitute a type hole for the newly free var in t, but it doesn't matter for this algorithm
  TForall _ _ _ t -> Right tEmptyHole : mkSaturatedApplicationArgs t
  _ -> []

mkSaturatedApplication :: MonadFresh ID m => m Expr -> TC.Type -> m Expr
mkSaturatedApplication e = apps' e . mkSaturatedApplicationArgs

insertRefinedVar :: ActionM m => TmVarRef -> ExprZ -> m ExprZ
insertRefinedVar x ast = do
  (v, vTy) <-
    getVarType ast x >>= \case
      Left err -> throwError $ RefineError $ Right err
      Right t -> pure (var x, t)
  -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
  -- See https://github.com/hackworthltd/primer/issues/11
  -- We only care about the type context for refine
  let (tycxt, _) = localVariablesInScopeExpr (Left ast)
  cxt <- asks $ TC.extendLocalCxtTys tycxt
  tgtTy <- getTypeCache $ target ast
  case target ast of
    EmptyHole{} -> getRefinedApplications cxt vTy tgtTy >>= \case
      -- See Note [No valid refinement]
      Nothing -> flip replace ast <$> hole v
      Just as -> flip replace ast <$> apps' v (Swap.swap . bimap pure pure <$> as)
      --flip replace ast <$> mkRefinedApplication cxt v vTy tgtTyCache
    e -> throwError $ NeedEmptyHole (InsertRefinedVar x) e

{-
Note [No valid refinement]
Consider having @foo : List Nat -> Bool@ in scope, and being asked to "refine"
@foo@ to fit in a hole of type @Maybe Unit@ (i.e. find some type/term
applications @is@ such that @Maybe Unit ∋ foo is@). This is obviously
impossible. In keeping with our strategy of being permissive and avoiding
showing error messages to students, we will instead return @{? foo ?}@, i.e.
put the requested function inside a hole when we cannot find a suitable
application spine.
-}

getRefinedApplications :: (MonadError ActionError m,
                           MonadFresh NameCounter m, MonadFresh ID m) => TC.Cxt -> TC.Type -> TypeCache -> m (Maybe [Either Type Expr])
getRefinedApplications cxt eTy tgtTy' = do
  tgtTy <- case tgtTy' of
    TCChkedAt t -> pure t
    TCEmb b -> pure $ tcChkedAt b
    TCSynthed{} -> throwError $ RefineError $ Left "Don't have a type we were checked at"
  mInst <-
    runExceptT (refine cxt tgtTy eTy) >>= \case
      -- Errors are only internal failures. Refinement failing is signaled with
      -- a Maybe
      Left err -> throwError $ InternalFailure $ show err
      Right x -> pure $ fst <$> x
  traverse (mapM f) mInst
  where
    -- Note that whilst the names in 'InstUnconstrainedAPP' scope over the
    -- following 'Insts', and should be substituted with whatever the
    -- 'InstUnconstrainedAPP' is instatiated to (here, 'tEmptyHole'), they
    -- actually only ever appear in 'InstApp', rather than 'InstAPP' (see
    -- 'Tests.Refine.tasty_scoping'). Since we ignore the type of an 'InstApp'
    -- and unconditionally put a hole, we do not have to worry about doing this
    -- substitution.
    f = \case
      InstApp _ -> Right <$> emptyHole
      InstAPP a -> Left <$> generateTypeIDs a
      InstUnconstrainedAPP _ _ -> Left <$> tEmptyHole

{-  
mkRefinedApplication :: ActionM m => TC.Cxt -> m Expr -> TC.Type -> Maybe TypeCache -> m Expr
mkRefinedApplication cxt e eTy tgtTy' = do
  tgtTy <- case tgtTy' of
    Just (TCChkedAt t) -> pure t
    Just (TCEmb b) -> pure $ tcChkedAt b
    _ -> throwError $ RefineError $ Left "Don't have a type we were checked at"
  mInst <-
    runExceptT (refine cxt tgtTy eTy) >>= \case
      -- Errors are only internal failures. Refinement failing is signaled with
      -- a Maybe
      Left err -> throwError $ InternalFailure $ show err
      Right x -> pure $ fst <$> x
  case mInst of
    -- See Note [No valid refinement]
    Nothing -> hole e
    Just inst -> foldl' f e inst
  where
    -- Note that whilst the names in 'InstUnconstrainedAPP' scope over the
    -- following 'Insts', and should be substituted with whatever the
    -- 'InstUnconstrainedAPP' is instatiated to (here, 'tEmptyHole'), they
    -- actually only ever appear in 'InstApp', rather than 'InstAPP' (see
    -- 'Tests.Refine.tasty_scoping'). Since we ignore the type of an 'InstApp'
    -- and unconditionally put a hole, we do not have to worry about doing this
    -- substitution.
    f x = \case
      InstApp _ -> x `app` emptyHole
      InstAPP a -> x `aPP` generateTypeIDs a
      InstUnconstrainedAPP _ _ -> x `aPP` tEmptyHole
-}

constructApp :: ActionM m => ExprZ -> m ExprZ
constructApp ze = flip replace ze <$> app (pure (target ze)) emptyHole

constructAPP :: ActionM m => ExprZ -> m ExprZ
constructAPP ze = flip replace ze <$> aPP (pure (target ze)) tEmptyHole

constructAnn :: ActionM m => ExprZ -> m ExprZ
constructAnn ze = flip replace ze <$> ann (pure (target ze)) tEmptyHole

removeAnn :: ActionM m => ExprZ -> m ExprZ
removeAnn ze = case target ze of
  Ann _ e _ -> pure $ replace e ze
  e -> throwError $ NeedAnn RemoveAnn e

-- | Construct a lambda around an expression
-- We leave the cursor on the original expression, which is now the body of the lambda.
constructLam :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLam mx ze = do
  -- If a name is provided, use that. Otherwise, generate a fresh one.
  x <- case mx of
    Nothing -> mkFreshName ze
    Just x -> pure (unsafeMkLocalName x)
  unless (isFresh x (target ze)) $ throwError NameCapture
  result <- flip replace ze <$> lam x (pure (target ze))
  moveExpr Child1 result

-- | Similar comments apply here as to 'constructLam'
constructLAM :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLAM mx ze = do
  -- If a name is provided, use that. Otherwise, generate a fresh one.
  x <- case mx of
    Nothing -> mkFreshName ze
    Just x -> pure (unsafeMkLocalName x)
  unless (isFresh x (target ze)) $ throwError NameCapture
  result <- flip replace ze <$> lAM x (pure (target ze))
  moveExpr Child1 result

constructPrim :: ActionM m => PrimCon -> ExprZ -> m ExprZ
constructPrim p ze = case target ze of
  EmptyHole{} -> flip replace ze <$> prim p
  e -> throwError $ NeedEmptyHole (ConstructPrim p) e

constructSatCon :: ActionM m => QualifiedText -> ExprZ -> m ExprZ
constructSatCon c ze = case target ze of
  -- Similar comments re smartholes apply as to insertSatVar
  EmptyHole{} -> do
    (nTyArgs, nTmArgs) <-
      conInfo n >>= \case
        Left err -> throwError $ SaturatedApplicationError $ Left err
        Right t -> pure t
    flip replace ze <$> conSat n (replicate nTyArgs tEmptyHole) (replicate nTmArgs emptyHole)
  e -> throwError $ NeedEmptyHole (ConstructSaturatedCon c) e
  where
    n = unsafeMkGlobalName c

-- TODO: rename, dry (cf allCons'). Probably want to return more info so can use for refine also
conInfo ::
  MonadReader TC.Cxt m =>
  ValConName ->
  m (Either Text (Int,Int))
conInfo c =
  asks (flip lookupConstructor c . TC.typeDefs) <&> \case
    Just (vc, tc, td) -> Right $ (length $ td.astTypeDefParameters, length $ vc.valConArgs)
    Nothing -> Left $ "Could not find constructor " <> show c


getConstructorType ::
  MonadReader TC.Cxt m =>
  ValConName ->
  m (Either Text TC.Type)
getConstructorType c =
  asks (flip lookupConstructor c . TC.typeDefs) <&> \case
    Just (vc, tc, td) -> Right $ valConType tc td vc
    Nothing -> Left $ "Could not find constructor " <> show c

constructRefinedCon :: ActionM m => QualifiedText -> ExprZ -> m ExprZ
constructRefinedCon c ze = do
  let n = unsafeMkGlobalName c
  cTy <-
    getConstructorType n >>= \case
      Left err -> throwError $ RefineError $ Left err
      Right t -> pure t
  -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
  -- See https://github.com/hackworthltd/primer/issues/11
  -- We only care about the type context for refine
  let (tycxt, _) = localVariablesInScopeExpr (Left ze)
  cxt <- asks $ TC.extendLocalCxtTys tycxt
  tgtTy <- getTypeCache $ target ze
  case target ze of
    -- TODO (saturated constructors) this use of application nodes will be rejected once full-saturation is enforced
    EmptyHole{} -> breakLR <<$>> getRefinedApplications cxt cTy tgtTy >>= \case
      -- See Note [No valid refinement]
      Nothing -> flip replace ze <$> hole (conSat n [] [])
      -- TODO: in enforced-saturation-world, neither the above nor the Just Just case are valid:
      -- the above obviously may not be saturated and the inside of the hole is not synthesisable (but maybe we don't care, and rely on smartholes to fix it?), and the below may not be if the target type is not an applied-ADT
      -- (todo: add reference to innards-of-hole-must-be-syn note from todo list "Note [Holes and bidirectionality]")
      Just Nothing -> throwError $ InternalFailure "Types of constructors always have type abstractions before term abstractions"
      Just (Just (tys,tms)) -> flip replace ze <$> conSat n (pure <$> tys) (pure <$> tms)
    e -> throwError $ NeedEmptyHole (ConstructRefinedCon c) e

getTypeCache :: MonadError ActionError m => Expr -> m TypeCache
getTypeCache = maybeTypeOf <&> \case
  Nothing -> throwError $ RefineError $ Left "Don't have a cached type"
  Just ty -> pure ty

-- | A view for lists where all 'Left' come before all 'Right'
breakLR :: [Either a b] -> Maybe ([a],[b])
breakLR = spanMaybe leftToMaybe <&> \case
  (ls,rest) -> (ls,) <$> traverse rightToMaybe rest

-- TODO/REVIEW: DRY this with primer-service/exe-replay/Main.hs
-- | Similar to 'Data.List.span', but takes a 'Maybe' predicate.
spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
  where
    go [] = ([], [])
    go xxs@(x : xs) = case f x of
      Just b -> first (b :) $ go xs
      Nothing -> ([], xxs)

  
constructLet :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLet mx ze = case target ze of
  EmptyHole{} -> do
    -- If a name is provided, use that. Otherwise, generate a fresh one.
    x <- case mx of
      Nothing -> mkFreshName ze
      Just x -> pure (unsafeMkLocalName x)
    flip replace ze <$> let_ x emptyHole emptyHole
  e -> throwError $ NeedEmptyHole (ConstructLet mx) e

constructLetrec :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLetrec mx ze = case target ze of
  EmptyHole{} -> do
    -- If a name is provided, use that. Otherwise, generate a fresh one.
    x <- case mx of
      Nothing -> mkFreshName ze
      Just x -> pure (unsafeMkLocalName x)
    flip replace ze <$> letrec x emptyHole tEmptyHole emptyHole
  e -> throwError $ NeedEmptyHole (ConstructLetrec mx) e

convertLetToLetrec :: ActionM m => ExprZ -> m ExprZ
convertLetToLetrec ze = case target ze of
  Let m x e1 e2 -> do
    t1 <- tEmptyHole
    unless (isFresh x e1) $ throwError NameCapture
    pure $ replace (Letrec m x e1 t1 e2) ze
  _ -> throwError $ CustomFailure ConvertLetToLetrec "can only convert let to letrec"

constructCase :: ActionM m => ExprZ -> m ExprZ
constructCase ze = do
  ty' <- case maybeTypeOf $ target ze of
    Just t -> pure t
    -- If there is no cached type, we would like to synthesise one
    -- but unfortunately it is awkward to do the context-wrangling
    -- needed via the zipper, so we simply call 'synthZ', which
    -- unfocuses to top-level before synthesising, and then searches
    -- for the correct focus afterwards.
    Nothing ->
      let handler _ = throwError $ CustomFailure ConstructCase "failed to synthesise the type of the scrutinee"
       in synthZ (InExpr ze) `catchError` handler >>= \case
            Nothing -> throwError $ CustomFailure ConstructCase "internal error when synthesising the type of the scruntinee: focused expression went missing after typechecking"
            Just (InType _) -> throwError $ CustomFailure ConstructCase "internal error when synthesising the type of the scruntinee: focused expression changed into a type after typechecking"
            Just (InBind _) -> throwError $ CustomFailure ConstructCase "internal error: scrutinee became a binding after synthesis"
            Just (InExpr ze') -> case maybeTypeOf $ target ze' of
              Nothing -> throwError $ CustomFailure ConstructCase "internal error: synthZ always returns 'Just', never 'Nothing'"
              Just t -> pure t
  ty <- case ty' of
    TCSynthed t -> pure t
    TCChkedAt _ -> throwError $ CustomFailure ConstructCase "can't take a case on a checkable-only term"
    TCEmb TCBoth{tcSynthed = t} -> pure t
  -- Construct the branches of the case using the type information of the scrutinee
  getTypeDefInfo ty >>= \case
    -- If it's a fully-saturated ADT type, create a branch for each of its constructors.
    Right (TC.TypeDefInfo _ _ (TypeDefAST tydef)) ->
      let f c = do
            -- We replace C[e] with C[case e of D n -> ...], generating names n.
            -- (Here C represents the one-hole context in which the subterm e
            -- sits.)
            -- Thus the available names are the same as in C[EmptyHole].
            -- NB: this is only correct as we know that mkFreshName will never
            -- clash with itself! Technically, the available names for the
            -- second var should exclude the one chosen for the first var.
            freshHole <- emptyHole
            ns <- mapM (\t -> (,Just (TCSynthed t)) <$> mkFreshName (replace freshHole ze)) (valConArgs c)
            branch (valConName c) ns (pure freshHole)
          brs = map f $ astTypeDefConstructors tydef
       in flip replace ze <$> case_ (pure $ target ze) brs
    Left TC.TDIHoleType ->
      asks TC.smartHoles >>= \case
        -- There is a potential mismatch: one can have different SmartHoles
        -- options when running actions and running other TC passes...
        TC.NoSmartHoles -> throwError $ CustomFailure ConstructCase "can only construct case on a term with hole type when using the \"smart\" TC"
        TC.SmartHoles -> flip replace ze <$> case_ (pure $ target ze) []
    _ -> throwError $ CustomFailure ConstructCase ("can only construct case on expression with type a exactly saturated ADT, not " <> show ty)

-- | Replace @x@ with @y@ in @λx. e@
renameLam :: ActionM m => Text -> ExprZ -> m ExprZ
renameLam y ze = case target ze of
  Lam m x e
    | unName (unLocalName x) == y -> pure ze
    | otherwise -> do
        let y' = unsafeMkLocalName y
        case renameLocalVar x y' e of
          Just e' -> pure $ replace (Lam m y' e') ze
          Nothing ->
            throwError NameCapture
  _ ->
    throwError $ CustomFailure (RenameLam y) "the focused expression is not a lambda"

-- | Replace @a@ with @b@ in @Λa. e@
renameLAM :: ActionM m => Text -> ExprZ -> m ExprZ
renameLAM b ze = case target ze of
  LAM m a e
    | unName (unLocalName a) == b -> pure ze
    | otherwise -> do
        let b' = unsafeMkLocalName b
        case renameTyVarExpr a b' e of
          Just e' -> pure $ replace (LAM m b' e') ze
          Nothing ->
            throwError NameCapture
  _ ->
    throwError $ CustomFailure (RenameLAM b) "the focused expression is not a type abstraction"

-- | Replace @x@ with @y@ in @let x = e1 in e2@ or @letrec x : t = e1 in e2@
renameLet :: ActionM m => Text -> ExprZ -> m ExprZ
renameLet y ze = case target ze of
  Let m x e1 e2
    | unName (unLocalName x) == y -> pure ze
    | otherwise -> do
        let y' = unsafeMkLocalName y
        e2' <- rename x y' e2
        pure $ replace (Let m y' e1 e2') ze
  LetType m x t e
    | unName (unLocalName x) == y -> pure ze
    | otherwise -> do
        let y' = unsafeMkLocalName y
        e' <- rename' x y' e
        pure $ replace (LetType m y' t e') ze
  Letrec m x e1 t1 e2
    | unName (unLocalName x) == y -> pure ze
    | otherwise -> do
        let y' = unsafeMkLocalName y
        e1' <- rename x y' e1
        e2' <- rename x y' e2
        pure $ replace (Letrec m y' e1' t1 e2') ze
  _ ->
    throwError $ CustomFailure (RenameLet y) "the focused expression is not a let"
  where
    rename :: ActionM m => LVarName -> LVarName -> Expr -> m Expr
    rename fromName toName e = maybe (throwError NameCapture) pure $ renameLocalVar fromName toName e
    rename' :: ActionM m => TyVarName -> TyVarName -> Expr -> m Expr
    rename' fromName toName e = maybe (throwError NameCapture) pure $ renameTyVarExpr fromName toName e

renameCaseBinding :: forall m. ActionM m => Text -> CaseBindZ -> m CaseBindZ
renameCaseBinding y caseBind = updateCaseBind caseBind $ \bind otherBindings rhs -> do
  let y' = unsafeMkLocalName y

  -- Check that 'y' doesn't clash with any of the other branch bindings
  let otherBindingNames = bindName <$> otherBindings
  when (y' `elem` otherBindingNames) $ throwError $ CaseBindsClash y' otherBindingNames

  -- Apply the rename to the rhs
  rhs' <- case renameLocalVar (bindName bind) y' rhs of
    Just e -> pure e
    Nothing -> throwError NameCapture

  -- Rename the binding
  let bind' = set (typed @LVarName) y' bind

  -- Update the outer expression with these changes
  pure (bind', rhs')

enterType :: ActionM m => ExprZ -> m TypeZ
enterType z = case focusType z of
  Nothing -> throwError $ CustomFailure EnterType "cannot enter type - no type in expression"
  Just zt -> pure zt

constructArrowL :: ActionM m => TypeZ -> m TypeZ
constructArrowL zt = flip replace zt <$> tfun (pure (target zt)) tEmptyHole

constructArrowR :: ActionM m => TypeZ -> m TypeZ
constructArrowR zt = flip replace zt <$> tfun tEmptyHole (pure (target zt))

constructTCon :: ActionM m => QualifiedText -> TypeZ -> m TypeZ
constructTCon c zt = case target zt of
  TEmptyHole{} -> flip replace zt <$> tcon (unsafeMkGlobalName c)
  _ -> throwError $ CustomFailure (ConstructTCon c) "can only construct tcon in hole"

constructTVar :: ActionM m => Text -> TypeZ -> m TypeZ
constructTVar x ast = case target ast of
  TEmptyHole{} -> flip replace ast <$> tvar (unsafeMkLocalName x)
  _ -> throwError $ CustomFailure (ConstructTVar x) "can only construct tvar in hole"

constructTForall :: ActionM m => Maybe Text -> TypeZ -> m TypeZ
constructTForall mx zt = do
  x <- case mx of
    Nothing -> LocalName <$> mkFreshNameTy zt
    Just x -> pure (unsafeMkLocalName x)
  unless (isFreshTy x $ target zt) $ throwError NameCapture
  flip replace zt <$> tforall x C.KType (pure (target zt))

constructTApp :: ActionM m => TypeZ -> m TypeZ
constructTApp zt = flip replace zt <$> tapp (pure (target zt)) tEmptyHole

-- | Replace @a@ with @b@ in @∀a. e@
renameForall :: ActionM m => Text -> TypeZ -> m TypeZ
renameForall b zt = case target zt of
  TForall m a k t
    | unName (unLocalName a) == b -> pure zt
    | otherwise -> do
        let b' = unsafeMkLocalName b
        case renameTyVar a b' t of
          Just t' -> pure $ replace (TForall m b' k t') zt
          Nothing ->
            throwError NameCapture
  _ ->
    throwError $ CustomFailure (RenameForall b) "the focused expression is not a forall type"

-- | Convert a high-level 'Available.NoInputAction' to a concrete sequence of 'ProgAction's.
toProgActionNoInput ::
  DefMap ->
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) ->
  Available.NoInputAction ->
  Either ActionError [ProgAction]
toProgActionNoInput defs def defName mNodeSel = \case
  Available.MakeCase ->
    toProgAction [ConstructCase]
  Available.MakeApp ->
    toProgAction [ConstructApp, Move Child2]
  Available.MakeAPP ->
    toProgAction [ConstructAPP, EnterType]
  Available.MakeAnn ->
    toProgAction [ConstructAnn]
  Available.RemoveAnn ->
    toProgAction [RemoveAnn]
  Available.LetToRec ->
    toProgAction [ConvertLetToLetrec]
  Available.Raise -> do
    id <- mid
    pure [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
  Available.EnterHole ->
    toProgAction [EnterHole]
  Available.RemoveHole ->
    toProgAction [FinishHole]
  Available.DeleteExpr ->
    toProgAction [Delete]
  Available.MakeFun ->
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    toProgAction [ConstructArrowL, Move Child1]
  Available.AddInput -> do
    -- This action traverses the function type and adds a function arrow to the end of it,
    -- resulting in a new argument type. The result type is unchanged.
    -- The cursor location is also unchanged.
    -- e.g. A -> B -> C ==> A -> B -> ? -> C
    id <- mid
    type_ <- case findType id $ astDefType def of
      Just t -> pure t
      Nothing -> case map fst $ findNodeWithParent id $ astDefExpr def of
        Just (TypeNode t) -> pure t
        Just sm -> Left $ NeedType sm
        Nothing -> Left $ IDNotFound id
    l <- case type_ of
      TFun _ a b -> pure $ NE.length $ fst $ unfoldFun a b
      t -> Left $ NeedTFun t
    let moveToLastArg = replicate l (Move Child2)
        moveBack = replicate l (Move Parent)
     in toProgAction $ moveToLastArg <> [ConstructArrowR] <> moveBack
  Available.MakeTApp ->
    toProgAction [ConstructTApp, Move Child1]
  Available.RaiseType -> do
    id <- mid
    pure [MoveToDef defName, CopyPasteSig (defName, id) [SetCursor id, Move Parent, Delete]]
  Available.DeleteType ->
    toProgAction [Delete]
  Available.DuplicateDef ->
    let sigID = getID $ astDefType def
        bodyID = getID $ astDefExpr def
        copyName = uniquifyDefName (qualifiedModule defName) (unName (baseName defName) <> "Copy") defs
     in pure
          [ CreateDef (qualifiedModule defName) (Just copyName)
          , CopyPasteSig (defName, sigID) []
          , CopyPasteBody (defName, bodyID) []
          ]
  Available.DeleteDef ->
    pure [DeleteDef defName]
  where
    toProgAction actions = toProg' actions defName <$> maybeToEither NoNodeSelection mNodeSel
    mid = maybeToEither NoNodeSelection $ snd <$> mNodeSel

-- | Convert a high-level 'Available.InputAction', and associated 'Available.Option',
-- to a concrete sequence of 'ProgAction's.
toProgActionInput ::
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) ->
  Available.Option ->
  Available.InputAction ->
  Either ActionError [ProgAction]
toProgActionInput def defName mNodeSel opt0 = \case
  Available.MakeConSat -> do
    ref <- offerRefined
    opt <- optGlobal
    toProg [if ref then ConstructRefinedCon opt else ConstructSaturatedCon opt]
  Available.MakeInt -> do
    opt <- optNoCxt
    n <- maybeToEither (NeedInt opt0) $ readMaybe opt
    toProg [ConstructPrim $ PrimInt n]
  Available.MakeChar -> do
    opt <- optNoCxt
    case T.uncons opt of
      Just (c, r) | T.null r -> toProg [ConstructPrim $ PrimChar c]
      _ -> Left $ NeedChar opt0
  Available.MakeVar ->
    toProg [ConstructVar optVar]
  Available.MakeVarSat -> do
    ref <- offerRefined
    toProg [if ref then InsertRefinedVar optVar else InsertSaturatedVar optVar]
  Available.MakeLet -> do
    opt <- optNoCxt
    toProg [ConstructLet $ Just opt]
  Available.MakeLetRec -> do
    opt <- optNoCxt
    toProg [ConstructLetrec $ Just opt]
  Available.MakeLam -> do
    opt <- optNoCxt
    toProg [ConstructLam $ Just opt]
  Available.MakeLAM -> do
    opt <- optNoCxt
    toProg [ConstructLAM $ Just opt]
  Available.RenamePattern -> do
    opt <- optNoCxt
    toProg [RenameCaseBinding opt]
  Available.RenameLet -> do
    opt <- optNoCxt
    toProg [RenameLet opt]
  Available.RenameLam -> do
    opt <- optNoCxt
    toProg [RenameLam opt]
  Available.RenameLAM -> do
    opt <- optNoCxt
    toProg [RenameLAM opt]
  Available.MakeTCon -> do
    opt <- optGlobal
    toProg [ConstructTCon opt]
  Available.MakeTVar -> do
    opt <- optNoCxt
    toProg [ConstructTVar opt]
  Available.MakeForall -> do
    opt <- optNoCxt
    toProg [ConstructTForall $ Just opt, Move Child1]
  Available.RenameForall -> do
    opt <- optNoCxt
    toProg [RenameForall opt]
  Available.RenameDef -> do
    opt <- optNoCxt
    pure [RenameDef defName opt]
  where
    mid = maybeToEither NoNodeSelection $ snd <$> mNodeSel
    optVar = case opt0.context of
      Just q -> GlobalVarRef $ unsafeMkGlobalName (q, opt0.option)
      Nothing -> LocalVarRef $ unsafeMkLocalName opt0.option
    -- Note that we use an option with @context = Nothing@ for inputs of
    -- (some offered actions as well as) any @FreeInput@ (including for
    -- insertion of a primitive integer)
    optNoCxt = case opt0.context of
      Just _ -> Left $ NeedGlobal opt0
      Nothing -> pure opt0.option
    optGlobal = case opt0.context of
      Nothing -> Left $ NeedLocal opt0
      Just q -> pure (q, opt0.option)
    toProg actions = toProg' actions defName <$> maybeToEither NoNodeSelection mNodeSel
    offerRefined = do
      id <- mid
      -- If we have a useful type, offer the refine action, otherwise offer the saturate action.
      case findNodeWithParent id $ astDefExpr def of
        Just (ExprNode e, _) -> pure $ case e ^. _exprMetaLens ^? _type % _Just % _chkedAt of
          Just (TEmptyHole _) -> False
          Just (THole _ _) -> False
          Just _ -> True
          _ -> False
        Just (sm, _) -> Left $ NeedType sm
        Nothing -> Left $ IDNotFound id

toProg' :: [Action] -> GVarName -> (NodeType, ID) -> [ProgAction]
toProg' actions defName (nt, id) =
  [ MoveToDef defName
  , (SetCursor id : actions) & case nt of
      SigNode -> SigAction
      BodyNode -> BodyAction
  ]
