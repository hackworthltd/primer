{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Action (
  Action (..),
  ActionError (..),
  Movement (..),
  BranchMove (..),
  ProgAction (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  applyActionsToExpr,
  applyAction',
  moveExpr,
  enterType,
  moveType,
  move,
  uniquifyDefName,
  toProgActionInput,
  toProgActionNoInput,
  applyActionsToField,
  insertSubseqBy,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (Value)
import Data.Bifunctor.Swap qualified as Swap
import Data.Bitraversable (bisequence)
import Data.Functor.Compose (Compose (..))
import Data.Generics.Product (typed)
import Data.List (delete, findIndex, insertBy)
import Data.List.NonEmpty qualified as NE
import Data.Map (insert)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple.Extra ((&&&))
import Optics (over, set, traverseOf, (%), (?~), (^.), (^?), _Just)
import Primer.Action.Actions (Action (..), BranchMove (Fallback, Pattern), Movement (..), QualifiedText)
import Primer.Action.Available qualified as Available
import Primer.Action.Errors (ActionError (..))
import Primer.Action.ProgAction (ProgAction (..))
import Primer.App.Base (
  DefSelection (..),
  NodeSelection (..),
  NodeType (..),
  Selection' (..),
  TypeDefConsFieldSelection (..),
  TypeDefConsSelection (..),
  TypeDefNodeSelection (..),
  TypeDefParamSelection (..),
  TypeDefSelection (..),
 )
import Primer.Core (
  CaseBranch' (CaseBranch),
  CaseFallback' (CaseExhaustive, CaseFallback),
  Expr,
  Expr' (..),
  GVarName,
  HasID,
  HasMetadata (_metadata),
  ID,
  KindMeta,
  LVarName,
  LocalName (LocalName, unLocalName),
  Pattern (PatCon, PatPrim),
  PrimCon (PrimChar, PrimInt),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeMeta,
  ValConName,
  baseName,
  bindName,
  caseBranchName,
  getID,
  qualifiedModule,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _chkedAt,
  _exprMetaLens,
  _type, Kind' (KHole),
 )
import Primer.Core qualified as C
import Primer.Core.DSL (
    ktype,khole,
  aPP,
  ann,
  app,
  apps',
  branch,
  branchPrim,
  caseFB_,
  case_,
  con,
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
  var, khole, kfun,
 )
import Primer.Core.Transform (renameLocalVar, renameTyVar, renameTyVarExpr, unfoldFun)
import Primer.Core.Utils (forgetKindMetadata, forgetTypeMetadata, generateTypeIDs, regenerateExprIDs, _freeTmVars)
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
 )
import Primer.Module (Module (moduleTypes), insertDef)
import Primer.Name (Name, NameCounter, unName, unsafeMkName)
import Primer.Name.Fresh (
  isFresh,
  isFreshTy,
  mkFreshName,
  mkFreshNameTy,
 )
import Primer.Primitives (primConName)
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
  TypeZip,
  down,
  findNodeWithParent,
  focus,
  focusLoc,
  focusOn,
  focusOnlyType,
  focusOnlyKind,
  focusType,
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
  _target, KindZ, KindTZ, unfocusKindT, findTypeOrKind,
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
  m (Either ActionError ([Module], Either TypeZip KindTZ))
applyActionsToTypeSig smartHoles imports (mod, mods) (defName, def) actions =
  runReaderT
    go
    (buildTypingContextFromModules (mod : mods <> imports) smartHoles)
    & runExceptT
  where
    go :: ActionM m => m ([Module], Either TypeZip KindTZ)
    go = do
      zt <- withWrappedType (astDefType def) (\zt -> foldlM (flip applyActionAndSynth) (InType zt) actions)
      let t = target (top $ either identity unfocusKindT zt)
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
    withWrappedType :: ActionM m => Type -> (TypeZ -> m Loc) -> m (Either TypeZip KindTZ)
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
            InType zt -> pure $ Left $ focusOnlyType zt
            InKind zk -> pure $ Right $ focusOnlyKind zk
            -- This probably shouldn't happen, but it may be the case that an action accidentally
            -- exits the type and ends up in the outer expression that we have created as a wrapper.
            -- In this case we just refocus on the top of the type.
            z -> maybe unwrapError (pure . Left . focusOnlyType) (focusType (unfocusLoc z))

applyActionsToField ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [Module] ->
  (Module, [Module]) ->
  (Name, ValConName, Int, ASTTypeDef TypeMeta KindMeta) ->
  [Action] ->
  m (Either ActionError ([Module], TypeZ))
applyActionsToField smartHoles imports (mod, mods) (tyName, conName', index, tyDef) actions =
  runReaderT
    go
    (buildTypingContextFromModules (mod : mods <> imports) smartHoles)
    & runExceptT
  where
    go :: ActionM m => m ([Module], TypeZ)
    go = do
      (tz, cs) <-
        getCompose
          . flip (findAndAdjustA ((== conName') . valConName)) (astTypeDefConstructors tyDef)
          $ Compose . \(ValCon _ ts) -> do
            (tz', cs') <-
              getCompose . flip (adjustAtA index) ts $
                Compose
                  . fmap (First . Just &&& target . top)
                  . flip withWrappedType \tz'' ->
                    foldlM (\l a -> local addParamsToCxt $ applyActionAndSynth a l) (InType tz'') actions
            maybe
              (throwError $ InternalFailure "applyActionsToField: con field index out of bounds")
              (pure . first (First . Just))
              $ bisequence (getFirst tz', ValCon conName' <$> cs')
      (valCons, zt) <-
        maybe (throwError $ InternalFailure "applyActionsToField: con name not found") pure $
          bisequence (cs, getFirst tz)
      let mod' = mod{moduleTypes = insert tyName (TypeDefAST tyDef{astTypeDefConstructors = valCons}) $ moduleTypes mod}
      (,zt) <$> checkEverything smartHoles (CheckEverything{trusted = imports, toCheck = mod' : mods})
    addParamsToCxt :: TC.Cxt -> TC.Cxt
    addParamsToCxt = over #localCxt (<> Map.fromList (map (bimap unLocalName (TC.K . forgetKindMetadata)) (astTypeDefParameters tyDef)))
    withWrappedType :: ActionM m => Type -> (TypeZ -> m Loc) -> m TypeZ
    withWrappedType ty f = do
      wrappedType <- ann emptyHole (pure ty)
      let unwrapError = throwError $ InternalFailure "applyActionsToField: failed to unwrap type"
          wrapError = throwError $ InternalFailure "applyActionsToField: failed to wrap type"
          focusedType = focusType $ focus wrappedType
      case focusedType of
        Nothing -> wrapError
        Just wrappedTy ->
          f wrappedTy >>= \case
            InType zt -> pure zt
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
          InType t -> candidateIDsType t
          InKind k -> [getID k]
          InBind (BindCase ze) -> [getID ze]
  pure . getFirst . mconcat $ fmap (\i -> First $ focusOn i post) candidateIDs
  where
    candidateIDsExpr e =
      getID e : case e of
        Hole _ e' -> candidateIDsExpr e'
        Ann _ e' _ -> candidateIDsExpr e'
        _ -> []
    candidateIDsTypeDown t = case t of
      THole _ t' -> candidateIDsTypeDown t'
      _ -> []
    candidateIDsType tz =
      getID tz
        : candidateIDsTypeDown (target tz)
          -- if we are focused inside a type annotation which gets elided, we
          -- refocus on the expression it was annotating
          <> case (up tz, target $ unfocusType tz) of
            (Nothing, Ann _ e _) -> candidateIDsExpr e
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
applyActionsToExpr :: (MonadFresh ID m, MonadFresh NameCounter m) => SmartHoles -> [Module] -> Expr -> [Action] -> m (Either ActionError Loc)
applyActionsToExpr sh modules expr actions =
  foldlM (flip applyActionAndSynth) (focusLoc expr) actions -- apply all actions
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

-- | The basic building block to apply an action. This does no typechecking, so
-- some actions may result in ill-typed programs, and some others may be
-- well-typed but a pass of smartholes later may still edit it.
--
-- You probably want to use the higher-level 'applyActionsToBody' or
-- 'applyActionsToTypeSig' if possible.
applyAction' :: ActionM m => Action -> Loc -> m Loc
applyAction' a = case a of
  NoOp -> pure
  SetCursor i -> setCursor i . unfocusLoc
  Move m -> \case
    InExpr z -> InExpr <$> moveExpr m z
    InType z -> InType <$> moveType m z
    InKind z -> InKind <$> moveKind m z
    z@(InBind _) -> case m of
      -- If we're moving up from a binding, then shift focus to the nearest parent expression.
      -- This is exactly what 'unfocusLoc' does if the 'Loc' is a binding.
      Parent -> pure . InExpr $ unfocusLoc z
      _ -> throwError $ CustomFailure (Move m) "Can only move up from a binding"
  Delete -> \case
    InExpr ze -> InExpr . flip replace ze <$> emptyHole
    InType zt -> InType . flip replace zt <$> tEmptyHole
    InKind zk -> InKind . flip replace zk <$> khole
    InBind _ -> throwError $ CustomFailure Delete "Cannot delete a binding"
  SetMetadata d -> \case
    InExpr ze -> pure $ InExpr $ setMetadata d ze
    InType zt -> pure $ InType $ setMetadata d zt
    InKind zk -> pure $ InKind $ setMetadata d zk
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
  ConstructLet x -> termAction (constructLet x) "cannot construct let in type"
  ConstructLetrec x -> termAction (constructLetrec x) "cannot construct letrec in type"
  ConvertLetToLetrec -> termAction convertLetToLetrec "cannot convert type to letrec"
  ConstructCase -> termAction constructCase "cannot construct case in type"
  AddCaseBranch c -> termAction (addCaseBranch $ Left c) "cannot add a case branch in type"
  AddCaseBranchPrim c -> termAction (addCaseBranch $ Right c) "cannot add a case branch in type"
  DeleteCaseBranch c -> termAction (deleteCaseBranch $ Left c) "cannot delete a case branch in type"
  DeleteCaseBranchPrim c -> termAction (deleteCaseBranch $ Right c) "cannot delete a case branch in type"
  RenameLam x -> termAction (renameLam x) "cannot rename lam in type"
  RenameLAM x -> termAction (renameLAM x) "cannot rename LAM in type"
  RenameLet x -> termAction (renameLet x) "cannot rename let in type"
  EnterType -> \case
    InExpr ze -> InType <$> enterType ze
    _ -> throwError $ CustomFailure EnterType "cannot enter type - already in type"
  ExitType -> \case
    InType zt -> pure $ InExpr $ unfocusType zt
    _ -> throwError $ CustomFailure ExitType "cannot exit type - not in type"
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
  ConstructKType -> kindAction constructKType "cannot construct the kind 'Type' - not in kind"
  ConstructKFun -> kindAction constructKFun "cannot construct the arrow kind - not in kind"
  where
    termAction f s = \case
      InExpr ze -> InExpr <$> f ze
      _ -> throwError $ CustomFailure a s
    typeAction f s = \case
      InType zt -> InType <$> f zt
      _ -> throwError $ CustomFailure a s
    kindAction f s = \case
      InKind zt -> InKind <$> f zt
      _ -> throwError $ CustomFailure a s

setCursor :: MonadError ActionError m => ID -> ExprZ -> m Loc
setCursor i e = case focusOn i (unfocusExpr e) of
  Just e' -> pure e'
  Nothing -> throwError $ IDNotFound i

-- | Apply a movement to a zipper
moveExpr :: MonadError ActionError m => Movement -> ExprZ -> m ExprZ
moveExpr m@(Branch b) z | Case _ _ brs fb <- target z =
  case b of
    Pattern c ->
      case findIndex ((c ==) . caseBranchName) brs of
        Nothing -> throwError $ CustomFailure (Move m) "Move-to-branch failed: no such branch"
        -- 'down' moves into the scrutinee, 'right' then steps through branch
        -- rhss
        Just i -> case foldr (\_ z' -> right =<< z') (down z) [0 .. i] of
          Just z' -> pure z'
          Nothing -> throwError $ CustomFailure (Move m) "internal error: movement failed, even though branch exists"
    Fallback -> case fb of
      CaseExhaustive -> throwError $ CustomFailure (Move m) "Move-to-branch failed: no fallback branch"
      -- 'down' moves into the scrutinee, 'right' then steps through branch
      -- rhss, and then finally into the fallback branch
      CaseFallback _ -> case foldr (\_ z' -> right =<< z') (down z) [0 .. length brs] of
        Just z' -> pure z'
        Nothing -> throwError $ CustomFailure (Move m) "internal error: movement to fallback branch failed, even though branch exists"
moveExpr m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch failed: this is not a case expression"
moveExpr m@(ConChild n) z | Con{} <- target z =
  -- 'down' moves into the first argument, 'right' steps through the various arguments
  case foldr (\_ z' -> right =<< z') (down z) [1 .. n] of
    Just z' -> pure z'
    Nothing -> throwError $ CustomFailure (Move m) "Move-to-constructor-argument failed: no such argument"
moveExpr m@(ConChild _) _ = throwError $ CustomFailure (Move m) "Move-to-constructor-argument failed: this is not a constructor"
moveExpr Child2 z
  | Case{} <- target z =
      throwError $ CustomFailure (Move Child2) "cannot move to 'Child2' of a case: use Branch instead"
moveExpr m z = move m z

-- | Apply a movement to a type zipper
moveType :: MonadError ActionError m => Movement -> TypeZ -> m TypeZ
moveType m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch unsupported in types (there are no cases in types!)"
moveType m@(ConChild _) _ = throwError $ CustomFailure (Move m) "Move-to-constructor-argument unsupported in types (type constructors do not directly store their arguments)"
moveType m z = move m z

-- | Apply a movement to a kind zipper
moveKind :: MonadError ActionError m => Movement -> KindZ -> m KindZ
moveKind m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch unsupported in kinds (there are no cases in kinds!)"
moveKind m@(ConChild _) _ = throwError $ CustomFailure (Move m) "Move-to-constructor-argument unsupported in kinds (there are no constructors in kinds)"
moveKind m z = move m z

-- TODO: we should have enter/exit kind actions

-- | Apply a movement to a generic zipper - does not support movement to a case
-- branch, or into an argument of a constructor
move :: forall m za a. (MonadError ActionError m, IsZipper za a, HasID za) => Movement -> za -> m za
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

enterHole :: (MonadError ActionError m, MonadFresh ID m) => ExprZ -> m ExprZ
enterHole ze = case target ze of
  EmptyHole{} -> do
    result <- flip replace ze <$> hole emptyHole
    move Child1 result
  e -> throwError $ NeedEmptyHole EnterHole e

finishHole :: MonadError ActionError m => ExprZ -> m ExprZ
finishHole ze = case target ze of
  Hole _ e -> pure $ replace e ze
  e -> throwError $ NeedNonEmptyHole FinishHole e

constructVar :: (MonadError ActionError m, MonadFresh ID m) => TmVarRef -> ExprZ -> m ExprZ
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
    EmptyHole{} ->
      getRefinedApplications cxt vTy tgtTy >>= \case
        -- See Note [No valid refinement]
        Nothing -> flip replace ast <$> hole (mkSaturatedApplication v vTy)
        Just as -> flip replace ast <$> apps' v (Swap.swap . bimap pure pure <$> as)
    e -> throwError $ NeedEmptyHole (InsertRefinedVar x) e

{-
Note [No valid refinement]
Consider having @foo : List Nat -> Bool@ in scope, and being asked to "refine"
@foo@ to fit in a hole of type @Maybe Unit@ (i.e. find some type/term
applications @is@ such that @Maybe Unit ∋ foo is@). This is obviously
impossible. In keeping with our strategy of being permissive and avoiding
showing error messages to students, we will instead return @{? foo ? ?}@, i.e.
saturate the requested function and put it inside a hole when we cannot find a
suitable application spine.
-}

getRefinedApplications ::
  ( MonadError ActionError m
  , MonadFresh NameCounter m
  , MonadFresh ID m
  ) =>
  TC.Cxt ->
  TC.Type ->
  TypeCache ->
  m (Maybe [Either Type Expr])
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

constructApp :: MonadFresh ID m => ExprZ -> m ExprZ
constructApp ze = flip replace ze <$> app (pure (target ze)) emptyHole

constructAPP :: MonadFresh ID m => ExprZ -> m ExprZ
constructAPP ze = flip replace ze <$> aPP (pure (target ze)) tEmptyHole

constructAnn :: MonadFresh ID m => ExprZ -> m ExprZ
constructAnn ze = flip replace ze <$> ann (pure (target ze)) tEmptyHole

removeAnn :: MonadError ActionError m => ExprZ -> m ExprZ
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

constructPrim :: (MonadFresh ID m, MonadError ActionError m) => PrimCon -> ExprZ -> m ExprZ
constructPrim p ze = case target ze of
  EmptyHole{} -> flip replace ze <$> prim p
  e -> throwError $ NeedEmptyHole (ConstructPrim p) e

constructSatCon :: ActionM m => QualifiedText -> ExprZ -> m ExprZ
constructSatCon c ze = case target ze of
  -- Similar comments re smartholes apply as to insertSatVar
  EmptyHole{} -> do
    (_, nTmArgs) <-
      conInfo n >>= \case
        Left err -> throwError $ SaturatedApplicationError $ Left err
        Right t -> pure t
    flip replace ze <$> con n (replicate nTmArgs emptyHole)
  e -> throwError $ NeedEmptyHole (ConstructSaturatedCon c) e
  where
    n = unsafeMkGlobalName c

-- returns
-- - "type" of ctor: the type an eta-expanded version of this constructor would check against
--   e.g. @Cons@'s "type" is @∀a. a -> List a -> List a@.
-- - its arity (number of args required for full saturation)
conInfo ::
  MonadReader TC.Cxt m =>
  ValConName ->
  m (Either Text (TC.Type, Int))
conInfo c =
  asks (flip lookupConstructor c . TC.typeDefs) <&> \case
    Just (vc, tc, td) -> Right (valConType tc td vc, length vc.valConArgs)
    Nothing -> Left $ "Could not find constructor " <> show c

getTypeCache :: MonadError ActionError m => Expr -> m TypeCache
getTypeCache =
  maybeTypeOf <&> \case
    Nothing -> throwError $ RefineError $ Left "Don't have a cached type"
    Just ty -> pure ty

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

-- NB: the errors given by getFocusType assume it is being used on the scrutinee of a case
getFocusType :: ActionM m => ExprZ -> m TypeCache
getFocusType ze = case maybeTypeOf $ target ze of
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
          Just (InKind _) -> throwError $ CustomFailure ConstructCase "internal error when synthesising the type of the scruntinee: focused expression changed into a kind after typechecking"
          Just (InBind _) -> throwError $ CustomFailure ConstructCase "internal error: scrutinee became a binding after synthesis"
          Just (InExpr ze') -> case maybeTypeOf $ target ze' of
            Nothing -> throwError $ CustomFailure ConstructCase "internal error: synthZ always returns 'Just', never 'Nothing'"
            Just t -> pure t

constructCase :: ActionM m => ExprZ -> m ExprZ
constructCase ze = do
  ty <-
    getFocusType ze >>= \case
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
    -- If it's a primitive type, only create a wildcard branch
    Right (TC.TypeDefInfo _ _ TypeDefPrim{}) ->
      flip replace ze <$> caseFB_ (pure $ target ze) [] emptyHole
    Left TC.TDIHoleType ->
      asks TC.smartHoles >>= \case
        -- There is a potential mismatch: one can have different SmartHoles
        -- options when running actions and running other TC passes...
        TC.NoSmartHoles -> throwError $ CustomFailure ConstructCase "can only construct case on a term with hole type when using the \"smart\" TC"
        TC.SmartHoles -> flip replace ze <$> case_ (pure $ target ze) []
    _ -> throwError $ CustomFailure ConstructCase ("can only construct case on expression with type a exactly saturated ADT, not " <> show ty)

addCaseBranch :: ActionM m => Either QualifiedText PrimCon -> ExprZ -> m ExprZ
addCaseBranch rawCon ze = case target ze of
  Case _ _ _ CaseExhaustive -> throwError CaseAlreadyExhaustive
  Case m scrut branches (CaseFallback fallbackBranch) -> do
    let newCon = either (PatCon . unsafeMkGlobalName) PatPrim rawCon
        branchName (CaseBranch n _ _) = n
        branchNames = branchName <$> branches
    when (newCon `elem` branchNames) $ throwError $ CaseBranchAlreadyExists newCon
    ty <-
      move Child1 ze >>= getFocusType >>= \case
        TCSynthed t -> pure t
        TCChkedAt _ -> throwError $ InternalFailure "scrutinees are synthesisable but only had TCChkedAt"
        TCEmb TCBoth{tcSynthed = t} -> pure t
    case newCon of
      PatCon c -> do
        allowedBranches <-
          getTypeDefInfo ty <&> \case
            Right (TC.TypeDefInfo _ _ (TypeDefAST tydef)) -> astTypeDefConstructors tydef
            _ -> []
        newBranch <- case find ((== c) . valConName) allowedBranches of
          Nothing -> throwError $ CaseBranchNotCon newCon ty
          Just vc -> do
            -- make a temporary approximation to the final result, from which we
            -- calculate some names for the new binders
            tmpBranch <- branch c [] $ pure fallbackBranch
            let tmpCase = Case m scrut (tmpBranch : branches) (CaseFallback fallbackBranch)
            binders <- replicateM (length $ valConArgs vc) . mkFreshName =<< moveExpr (Branch $ Pattern newCon) (replace tmpCase ze)
            branch c ((,Nothing) <$> binders) $ regenerateExprIDs fallbackBranch
        -- If we are adding the last constructor, we delete the fallback branch
        let fb =
              if length allowedBranches == length branchNames + 1
                then CaseExhaustive
                else CaseFallback fallbackBranch
        let branches' = insertSubseqBy branchName newBranch (PatCon . valConName <$> allowedBranches) branches
        pure $ replace (Case m scrut branches' fb) ze
      PatPrim c -> do
        unless (TCon () (primConName c) == ty) $ throwError $ CaseBranchNotCon newCon ty
        newBranch <- branchPrim c $ regenerateExprIDs fallbackBranch
        let cmp (PatPrim (PrimInt x)) (PatPrim (PrimInt y)) = compare x y
            cmp (PatPrim (PrimChar x)) (PatPrim (PrimChar y)) = compare x y
            -- We assume the input is well-typed, so all branches are of same
            -- form, and this last case cannot happen
            cmp _ _ = EQ
            branches' = insertBy (cmp `on` caseBranchName) newBranch branches
        pure $ replace (Case m scrut branches' $ CaseFallback fallbackBranch) ze
  _ ->
    throwError $ CustomFailure act "the focused expression is not a case"
  where
    act = either AddCaseBranch AddCaseBranchPrim rawCon

deleteCaseBranch ::
  (MonadFresh ID m, MonadError ActionError m) =>
  Either QualifiedText PrimCon ->
  ExprZ ->
  m ExprZ
deleteCaseBranch c ze = case target ze of
  -- We put the action on the `Case` node, as we cannot currently select the pattern
  -- (we cannot put the action on binders, since nullary constructors have no binders)
  Case m scrut branches fallbackBranch ->
    case find ((c' ==) . caseBranchName) branches of
      Nothing -> throwError $ CaseBranchNotExist c'
      Just br@(CaseBranch _ binds rhs) ->
        let newBranches = delete br branches
            -- If there was no fallback branch, we create a new one whose RHS is
            -- the RHS of the deleted branch, with the now unbound variables
            -- replaced with holes
            bound = bindName <$> binds
            removeBound (m', n) =
              if n `elem` bound
                then emptyHole
                else pure $ Var m' $ LocalVarRef n
         in do
              newFallback <- case fallbackBranch of
                CaseExhaustive -> CaseFallback <$> traverseOf _freeTmVars removeBound rhs
                CaseFallback _ -> pure fallbackBranch
              pure $ replace (Case m scrut newBranches newFallback) ze
  _ ->
    throwError $ CustomFailure act "the focused expression is not a case"
  where
    c' = either (PatCon . unsafeMkGlobalName) PatPrim c
    act = either DeleteCaseBranch DeleteCaseBranchPrim c

-- | Given a sequence @ks@ and @vs@ such that @map f vs@ is a subsequence of
-- @ks@ (this precondition is not checked), @insertSubseqBy f x ks vs@ inserts
-- @x@ at the appropriate position in @vs@.
-- Thus we have
-- - @insertSubseqBy f x ks vs \\ x == vs@
-- - @map f (insertSubseqBy f x ks vs) `isSubsequenceOf` ks@
insertSubseqBy :: Eq k => (v -> k) -> v -> [k] -> [v] -> [v]
insertSubseqBy f x = go
  where
    tgt = f x
    go (k : ks) vvs@(v : vs)
      | k == tgt = x : vvs
      | k == f v = v : go ks vs
      | otherwise = go ks vvs
    go _ _ = [x]

-- | Replace @x@ with @y@ in @λx. e@
renameLam :: MonadError ActionError m => Text -> ExprZ -> m ExprZ
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
renameLAM :: MonadError ActionError m => Text -> ExprZ -> m ExprZ
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
renameLet :: MonadError ActionError m => Text -> ExprZ -> m ExprZ
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
    rename :: MonadError ActionError m => LVarName -> LVarName -> Expr -> m Expr
    rename fromName toName e = maybe (throwError NameCapture) pure $ renameLocalVar fromName toName e
    rename' :: MonadError ActionError m => TyVarName -> TyVarName -> Expr -> m Expr
    rename' fromName toName e = maybe (throwError NameCapture) pure $ renameTyVarExpr fromName toName e

renameCaseBinding :: forall m. MonadError ActionError m => Text -> CaseBindZ -> m CaseBindZ
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

enterType :: MonadError ActionError m => ExprZ -> m TypeZ
enterType z = case focusType z of
  Nothing -> throwError $ CustomFailure EnterType "cannot enter type - no type in expression"
  Just zt -> pure zt

constructArrowL :: MonadFresh ID m => TypeZ -> m TypeZ
constructArrowL zt = flip replace zt <$> tfun (pure (target zt)) tEmptyHole

constructArrowR :: MonadFresh ID m => TypeZ -> m TypeZ
constructArrowR zt = flip replace zt <$> tfun tEmptyHole (pure (target zt))

constructTCon :: (MonadFresh ID m, MonadError ActionError m) => QualifiedText -> TypeZ -> m TypeZ
constructTCon c zt = case target zt of
  TEmptyHole{} -> flip replace zt <$> tcon (unsafeMkGlobalName c)
  _ -> throwError $ CustomFailure (ConstructTCon c) "can only construct tcon in hole"

constructTVar :: (MonadFresh ID m, MonadError ActionError m) => Text -> TypeZ -> m TypeZ
constructTVar x ast = case target ast of
  TEmptyHole{} -> flip replace ast <$> tvar (unsafeMkLocalName x)
  _ -> throwError $ CustomFailure (ConstructTVar x) "can only construct tvar in hole"

constructTForall :: ActionM m => Maybe Text -> TypeZ -> m TypeZ
constructTForall mx zt = do
  x <- case mx of
    Nothing -> LocalName <$> mkFreshNameTy zt
    Just x -> pure (unsafeMkLocalName x)
  unless (isFreshTy x $ target zt) $ throwError NameCapture
  flip replace zt <$> tforall x ktype (pure (target zt))

constructTApp :: MonadFresh ID m => TypeZ -> m TypeZ
constructTApp zt = flip replace zt <$> tapp (pure (target zt)) tEmptyHole

-- | Replace @a@ with @b@ in @∀a. e@
renameForall :: MonadError ActionError m => Text -> TypeZ -> m TypeZ
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

constructKType :: (MonadFresh ID m, MonadError ActionError m) => KindZ -> m KindZ
constructKType zk = case target zk of
  KHole _ -> flip replace zk <$> ktype
  _ -> throwError $ CustomFailure ConstructKType "can only construct the kind 'Type' in hole"

constructKFun :: MonadFresh ID m => KindZ -> m KindZ
constructKFun zk = flip replace zk <$> ktype `kfun` (pure $ target zk)

-- | Convert a high-level 'Available.NoInputAction' to a concrete sequence of 'ProgAction's.
toProgActionNoInput ::
  DefMap ->
  Either (ASTTypeDef TypeMeta KindMeta) ASTDef ->
  Selection' ID ->
  Available.NoInputAction ->
  Either ActionError [ProgAction]
toProgActionNoInput defs def0 sel0 = \case
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
    id <- nodeID
    sel <- termSel
    pure [MoveToDef sel.def, CopyPasteBody (sel.def, id) [SetCursor id, Move Parent, Delete]]
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
    type_ <- case def0 of
      Left def -> do
        (tName, vcName, field) <- conFieldSel
        let id = field.meta
        vc <- maybeToEither (ValConNotFound tName vcName) $ find ((== vcName) . valConName) $ astTypeDefConstructors def
        t <- maybeToEither (FieldIndexOutOfBounds vcName field.index) $ flip atMay field.index $ valConArgs vc
        case findTypeOrKind id t of
          Just (Left t') -> pure $ forgetTypeMetadata t'
          Just (Right k) -> Left $ NeedType $ KindNode k
          Nothing -> Left $ IDNotFound id
      Right def -> do
        id <- nodeID
        forgetTypeMetadata <$> case findTypeOrKind id $ astDefType def of
          Just (Left t) -> pure t
          Just (Right k) -> Left $ NeedType $ KindNode k
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
    id <- nodeID
    sel <- termSel
    pure [MoveToDef sel.def, CopyPasteSig (sel.def, id) [SetCursor id, Move Parent, Delete]]
  Available.DeleteType ->
    toProgAction [Delete]
  Available.DuplicateDef -> do
    sel <- termSel
    def <- termDef
    let sigID = getID $ astDefType def
        bodyID = getID $ astDefExpr def
        copyName = uniquifyDefName (qualifiedModule sel.def) (unName (baseName sel.def) <> "Copy") defs
     in pure
          [ CreateDef (qualifiedModule sel.def) (Just copyName)
          , CopyPasteSig (sel.def, sigID) []
          , CopyPasteBody (sel.def, bodyID) []
          ]
  Available.DeleteDef -> do
    sel <- termSel
    pure [DeleteDef sel.def]
  Available.DeleteTypeDef -> do
    sel <- typeSel
    pure [DeleteTypeDef sel.def]
  Available.DeleteCon -> do
    (t, sel) <- conSel
    pure [DeleteCon t sel.con]
  Available.AddConField -> do
    (defName, sel) <- conSel
    d <- typeDef
    vc <-
      maybe (Left $ ValConNotFound defName sel.con) pure
        . find ((== sel.con) . valConName)
        $ astTypeDefConstructors d
    let index = length $ valConArgs vc -- for now, we always add on to the end
    pure [AddConField defName sel.con index $ TEmptyHole ()]
  Available.DeleteConField -> do
    (t, c, sel) <- conFieldSel
    pure [DeleteConField t c sel.index]
  Available.DeleteTypeParam -> do
    (t, p) <- typeParamSel
    pure [DeleteTypeParam t p.param]
  Available.MakeKType -> do
    toProgAction [ConstructKType]
  Available.MakeKFun -> do
    toProgAction [ConstructKFun]
  Available.DeleteKind -> do
    toProgAction [Delete]
  where
    termSel = case sel0 of
      SelectionDef s -> pure s
      SelectionTypeDef _ -> Left NeedTermDefSelection
    nodeID = do
      sel <- termSel
      maybeToEither NoNodeSelection $ (.meta) <$> sel.node
    typeSel = case sel0 of
      SelectionDef _ -> Left NeedTypeDefSelection
      SelectionTypeDef s -> pure s
    typeNodeSel = do
      sel <- typeSel
      maybe (Left NeedTypeDefNodeSelection) (pure . (sel.def,)) sel.node
    conSel =
      typeNodeSel >>= \case
        (s0, TypeDefConsNodeSelection s) -> pure (s0, s)
        _ -> Left NeedTypeDefConsSelection
    typeParamSel =
      typeNodeSel >>= \case
        (s0, TypeDefParamNodeSelection s) -> pure (s0, s)
        _ -> Left NeedTypeDefParamSelection
    typeParamKindSel =
      typeParamSel >>= \case
        (t, TypeDefParamSelection p (Just id)) -> pure (t, p, id)
        _ -> Left NeedTypeDefParamKindSelection
    conFieldSel = do
      (ty, s) <- conSel
      maybe (Left NeedTypeDefConsFieldSelection) (pure . (ty,s.con,)) s.field
    toProgAction actions = do
      case sel0 of
        SelectionDef sel -> toProg' actions sel.def <$> maybeToEither NoNodeSelection sel.node
        SelectionTypeDef sel -> case sel.node of
          Just (TypeDefParamNodeSelection _) -> do
            (t, p, id) <- typeParamKindSel
            pure [ParamKindAction t p id actions]
          Just (TypeDefConsNodeSelection _) -> do
            (t, c, f) <- conFieldSel
            pure [ConFieldAction t c f.index $ SetCursor f.meta : actions]
          Nothing -> Left NeedTypeDefNodeSelection
    termDef = first (const NeedTermDef) def0
    typeDef = either Right (Left . const NeedTypeDef) def0

-- | Convert a high-level 'Available.InputAction', and associated 'Available.Option',
-- to a concrete sequence of 'ProgAction's.
toProgActionInput ::
  Either (ASTTypeDef a b) ASTDef ->
  Selection' ID ->
  Available.Option ->
  Available.InputAction ->
  Either ActionError [ProgAction]
toProgActionInput def0 sel0 opt0 = \case
  Available.MakeCon -> do
    opt <- optGlobal
    toProg [ConstructSaturatedCon opt]
  Available.MakeInt -> do
    n <- optInt
    toProg [ConstructPrim $ PrimInt n]
  Available.MakeChar -> do
    c <- optChar
    toProg [ConstructPrim $ PrimChar c]
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
  Available.AddBranch -> do
    opt <- optGlobal
    toProg [AddCaseBranch opt]
  Available.AddBranchInt -> do
    n <- optInt
    toProg [AddCaseBranchPrim $ PrimInt n]
  Available.AddBranchChar -> do
    c <- optChar
    toProg [AddCaseBranchPrim $ PrimChar c]
  Available.DeleteBranch -> do
    opt <- optGlobal
    toProg [DeleteCaseBranch opt]
  Available.DeleteBranchInt -> do
    n <- optInt
    toProg [DeleteCaseBranchPrim $ PrimInt n]
  Available.DeleteBranchChar -> do
    c <- optChar
    toProg [DeleteCaseBranchPrim $ PrimChar c]
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
    sel <- termSel
    pure [RenameDef sel.def opt]
  Available.RenameType -> do
    opt <- optNoCxt
    td <- typeSel
    pure [RenameType td.def opt]
  Available.RenameCon -> do
    opt <- optNoCxt
    (defName, sel) <- conSel
    pure [RenameCon defName sel.con opt]
  Available.RenameTypeParam -> do
    opt <- optNoCxt
    (defName, sel) <- typeParamSel
    pure [RenameTypeParam defName sel.param opt]
  Available.AddCon -> do
    opt <- optNoCxt
    sel <- typeSel
    d <- typeDef
    let index = length $ astTypeDefConstructors d -- for now, we always add on the end
    pure [AddCon sel.def index opt]
  Available.AddTypeParam -> do
    opt <- optNoCxt
    sel <- typeSel
    index <- length . astTypeDefParameters <$> typeDef -- for now, we always add on to the end
    pure [AddTypeParam sel.def index opt $ C.KType ()]
  where
    termSel = case sel0 of
      SelectionDef s -> pure s
      SelectionTypeDef _ -> Left NeedTermDefSelection
    nodeID = do
      sel <- termSel
      maybeToEither NoNodeSelection $ (.meta) <$> sel.node
    typeSel = case sel0 of
      SelectionDef _ -> Left NeedTypeDefSelection
      SelectionTypeDef s -> pure s
    typeNodeSel = do
      sel <- typeSel
      maybe (Left NeedTypeDefNodeSelection) (pure . (sel.def,)) sel.node
    typeParamSel =
      typeNodeSel >>= \case
        (s0, TypeDefParamNodeSelection s) -> pure (s0, s)
        _ -> Left NeedTypeDefParamSelection
    conSel =
      typeNodeSel >>= \case
        (s0, TypeDefConsNodeSelection s) -> pure (s0, s)
        _ -> Left NeedTypeDefConsSelection
    termDef = first (const NeedTermDef) def0
    typeDef = either Right (Left . const NeedTypeDef) def0
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
    conFieldSel = do
      (ty, s) <- conSel
      maybe (Left NeedTypeDefConsFieldSelection) (pure . (ty,s.con,)) s.field
    optInt = do
      opt <- optNoCxt
      maybeToEither (NeedInt opt0) $ readMaybe opt
    optChar = do
      opt <- optNoCxt
      case T.uncons opt of
        Just (c, r) | T.null r -> pure c
        _ -> Left $ NeedChar opt0
    toProg actions = do
      case sel0 of
        SelectionDef sel -> toProg' actions sel.def <$> maybeToEither NoNodeSelection sel.node
        SelectionTypeDef _ -> do
          (t, c, f) <- conFieldSel
          pure [ConFieldAction t c f.index $ SetCursor f.meta : actions]
    offerRefined = do
      id <- nodeID
      def <- termDef
      -- If we have a useful type, offer the refine action, otherwise offer the saturate action.
      case findNodeWithParent id $ astDefExpr def of
        Just (ExprNode e, _) -> pure $ case e ^. _exprMetaLens ^? _type % _Just % _chkedAt of
          Just (TEmptyHole _) -> False
          Just (THole _ _) -> False
          Just _ -> True
          _ -> False
        Just (sm, _) -> Left $ NeedType sm
        Nothing -> Left $ IDNotFound id

toProg' :: [Action] -> GVarName -> NodeSelection ID -> [ProgAction]
toProg' actions defName sel =
  [ MoveToDef defName
  , (SetCursor sel.meta : actions) & case sel.nodeType of
      SigNode -> SigAction
      BodyNode -> BodyAction
  ]
