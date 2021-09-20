{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Primer.Action (
  Action (..),
  ActionError (..),
  Movement (..),
  applyActionsToBody,
  applyActionsToTypeSig,
  applyActionsToExpr,
  moveExpr,
  mkAvoidForFreshName,
  mkAvoidForFreshNameTy,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (Value)
import Data.Generics.Product (typed)
import Data.List (delete, findIndex, lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Optics (set, (%), (?~))
import Primer.Core (
  Def (..),
  Expr,
  Expr' (..),
  HasMetadata (_metadata),
  ID,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeDef (typeDefConstructors),
  bindName,
  getID,
  valConArgs,
  valConName,
  valConType,
 )
import qualified Primer.Core as C
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  emptyHole,
  global,
  hole,
  lAM,
  lam,
  let_,
  letrec,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
  var,
 )
import Primer.Core.Transform (renameTyVar, renameTyVarExpr, renameVar)
import Primer.Core.Utils (forgetTypeIDs, generateTypeIDs)
import Primer.JSON
import Primer.Name (Name, NameCounter, freshName, unName, unsafeMkName)
import Primer.Refine (Inst (InstAPP, InstApp, InstUnconstrainedAPP), refine)
import Primer.Subst (freeVars, freeVarsTy)
import Primer.Typecheck (
  SmartHoles,
  TypeError,
  buildTypingContext,
  check,
  checkEverything,
  exprTtoExpr,
  getTypeDefInfo,
  lookupConstructor,
  lookupGlobal,
  maybeTypeOf,
  synth,
 )
import qualified Primer.Typecheck as TC
import Primer.Zipper (
  BindLoc (..),
  CaseBindZ,
  ExprZ,
  IsZipper,
  Loc (..),
  TypeZ,
  bindersAbove,
  bindersAboveTypeZ,
  bindersBelow,
  bindersBelowTy,
  down,
  focus,
  focusLoc,
  focusOn,
  focusOnlyType,
  focusType,
  locToEither,
  replace,
  right,
  target,
  top,
  unfocus,
  unfocusLoc,
  unfocusType,
  up,
  updateCaseBind,
  _target,
 )
import Primer.ZipperCxt (localVariablesInScopeExpr)

-- | Core actions.
--  These describe edits to the core AST.
--  Some of them take Text arguments rather than Name because they represent
--  untrusted input from the frontend.
data Action
  = -- | Do nothing
    NoOp
  | -- | Move the cursor to the expression with this ID
    SetCursor ID
  | -- | Move one step in some direction
    Move Movement
  | -- | Delete the expression under the cursor
    Delete
  | -- | Set metadata under the cursor
    SetMetadata Value
  | -- | Enter the hole under the cursor.
    -- You can only enter an empty hole, changing it into a non-empty hole in the process.
    -- To enter a non-empty hole, use 'Move Child1'
    EnterHole
  | -- | Replace a non-empty hole with its contents
    FinishHole
  | -- | Construct a variable in an empty hole
    ConstructVar Text
  | -- | Construct a global variable (i.e. a reference to a definition) in an empty hole
    ConstructGlobalVar ID
  | -- | Insert a variable, with a saturated spine of term/type applications in an empty hole
    InsertSaturatedVar Text
  | InsertSaturatedGlobalVar ID
  | -- | Insert a variable, with an infered spine of term/type applications in an empty hole
    InsertRefinedVar Text
  | InsertRefinedGlobalVar ID
  | -- | Apply the expression under the cursor
    ConstructApp
  | -- | Apply the expression under the cursor to a type
    ConstructAPP
  | -- | Annotate the expression under the cursor (with a type hole)
    ConstructAnn
  | -- | Remove an annotation, leaving a now non-annotated term
    RemoveAnn
  | -- | Construct a lambda
    ConstructLam (Maybe Text)
  | -- | Construct a type abstraction "big-lambda"
    ConstructLAM (Maybe Text)
  | -- | Put a constructor in an empty hole
    ConstructCon Text
  | -- | Put a constructor applied to a saturated spine in an empty hole
    ConstructSaturatedCon Text
  | -- | Put a constructor in an empty hole, and infer what it should be applied to
    ConstructRefinedCon Text
  | -- | Put a let expression in an empty hole
    ConstructLet (Maybe Text)
  | -- | Put a letrec expression in an empty hole
    ConstructLetrec (Maybe Text)
  | -- | Convert a let to a letrec
    ConvertLetToLetrec
  | -- | Scrutinise the expression under the cursor with a @case@
    ConstructCase
  | -- | Rename a lambda binding
    RenameLam Text
  | -- | Rename a big lambda binding
    RenameLAM Text
  | -- | Rename a let or letrec binding
    RenameLet Text
  | -- | Move from an annotation to its type
    EnterType
  | -- | Move from a type up into the surrounding annotation
    ExitType
  | -- | Construct a function type around the type under the cursor.
    -- The type under the cursor is placed in the domain (left) position.
    ConstructArrowL
  | -- | Construct a function type around the type under the cursor.
    -- The type under the cursor is placed in the range (right) position.
    ConstructArrowR
  | -- | Put a type constructor in a type hole
    ConstructTCon Text
  | -- | Construct a type variable in an empty type hole
    ConstructTVar Text
  | -- | Construct a forall type (only at kind KType for now)
    ConstructTForall (Maybe Text)
  | -- | Construct an application in types
    ConstructTApp
  | -- | Rename a forall binding
    RenameForall Text
  | -- | Rename a case binding
    RenameCaseBinding Text
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Action

-- | Core movements
data Movement = Child1 | Child2 | Parent | Branch Name
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Movement

-- | Errors that may arise when applying an action
-- TODO: convert all CustomFailures to individual constructors
-- https://github.com/hackworthltd/primer/issues/8
data ActionError
  = CustomFailure
      Action
      -- ^ action that caused the error
      Text
      -- ^ the error message
  | InternalFailure Text
  | IDNotFound ID
  | NeedEmptyHole Action Expr
  | NeedNonEmptyHole Action Expr
  | NeedAnn Action Expr
  | TypeError TypeError
  | -- | Both actual and potential, eg renaming the lambda x to y in any of
    -- λx.y     the binder captures the existing y
    -- λx.λy.x  occurance gets captured by the inner binder
    -- λx.λy.y  this would be ok, but we are paranoid and bail out
    NameCapture
  | -- TODO: semantic errors.
    -- https://github.com/hackworthltd/primer/issues/8
    SaturatedApplicationError Text
  | -- | @RefineError@ should never happen unless we use the API wrong or have
    -- a bug. It does not get thrown for "no valid refinement found"
    -- - see Note [No valid refinement]
    RefineError Text
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ActionError

-- | A shorthand for the constraints needed when applying actions
type ActionM m =
  ( Monad m
  , MonadFresh ID m -- can generate fresh IDs
  , MonadFresh NameCounter m -- can generate fresh names
  , MonadError ActionError m -- can raise errors
  , MonadReader TC.Cxt m -- has access to a typing context
  )

-- Apply a sequence of actions to the type signature of a definition
-- We apply the actions to the type, then typecheck the body of the definition against the new type.
-- We must then typecheck the whole program to check any uses of the definition.
applyActionsToTypeSig ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [TypeDef] ->
  Map ID Def ->
  Def ->
  [Action] ->
  m (Either ActionError (Def, Map ID Def, TypeZ))
applyActionsToTypeSig smartHoles typeDefs defs def actions =
  runReaderT go (buildTypingContext typeDefs defs smartHoles)
    & runExceptT
  where
    go :: ActionM m => m (Def, Map ID Def, TypeZ)
    go = do
      zt <- withWrappedType (defType def) (\zt -> foldM (flip applyActionAndSynth) (InType zt) actions)
      let t = target (top zt)
      e <- check (forgetTypeIDs t) (defExpr def)
      let def' = def{defExpr = exprTtoExpr e, defType = t}
          defs' = Map.insert (defID def) def' defs
      -- The actions were applied to the type successfully, and the definition body has been
      -- typechecked against the new type.
      -- Now we need to typecheck the whole program again, to check any uses of the definition
      -- We make sure that the updated type is present in the global context.
      checkedDefs <- checkEverything smartHoles typeDefs defs'
      pure (def', checkedDefs, zt)
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

-- | Apply a sequence of actions to the body of a definition, producing a new Expr or an error if
-- any of the actions failed to apply.
-- After applying the actions, we check the new Expr against the type sig of the definition.
applyActionsToBody ::
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  SmartHoles ->
  [TypeDef] ->
  Map ID Def ->
  Def ->
  [Action] ->
  m (Either ActionError (Def, Loc))
applyActionsToBody sh typeDefs defs def actions =
  go
    & flip runReaderT (buildTypingContext typeDefs defs sh)
    & runExceptT
  where
    go :: ActionM m => m (Def, Loc)
    go = do
      ze <- foldM (flip (applyActionAndCheck (defType def))) (focusLoc (defExpr def)) actions
      let targetID = getID ze
          e = unfocus ze
      e' <- exprTtoExpr <$> check (forgetTypeIDs (defType def)) e
      let def' = def{defExpr = e'}
      case focusOn targetID (focus e') of
        Nothing -> throwError $ InternalFailure "lost ID after typechecking"
        Just z -> pure (def', z)

applyActionAndCheck :: ActionM m => Type -> Action -> Loc -> m Loc
applyActionAndCheck ty action z = do
  z' <- applyAction' action z
  let e = unfocus z'
      targetID = getID z'
  typedAST <- check (forgetTypeIDs ty) e
  -- Refocus on where we were previously
  case focusOn targetID (focus (exprTtoExpr typedAST)) of
    Just z'' -> pure z''
    Nothing -> throwError $ CustomFailure action "internal error: lost ID after typechecking"

-- This is currently only used for tests.
-- We may need it in the future for a REPL, where we want to build standalone expressions.
-- We take a list of the types that should be in scope for the test.
applyActionsToExpr :: (MonadFresh ID m, MonadFresh NameCounter m) => SmartHoles -> [TypeDef] -> Expr -> [Action] -> m (Either ActionError (Either ExprZ TypeZ))
applyActionsToExpr sh typeDefs expr actions =
  foldM (flip applyActionAndSynth) (focusLoc expr) actions -- apply all actions
    <&> locToEither
    & flip runReaderT (buildTypingContext typeDefs mempty sh)
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
synthZ z =
  let e = unfocus z
      targetID = getID z
   in do
        (_, typedAST) <- synth e
        -- Refocus on where we were previously
        pure $ focusOn targetID $ focus $ exprTtoExpr typedAST

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
  ConstructGlobalVar id_ -> termAction (constructGlobalVar id_) "cannot construct global var in type"
  InsertSaturatedVar x -> termAction (insertSatVar $ Left x) "cannot insert var in type"
  InsertSaturatedGlobalVar id_ -> termAction (insertSatVar $ Right id_) "cannot insert var in type"
  InsertRefinedVar x -> termAction (insertRefinedVar $ Left x) "cannot insert var in type"
  InsertRefinedGlobalVar id_ -> termAction (insertRefinedVar $ Right id_) "cannot insert var in type"
  ConstructApp -> termAction constructApp "cannot construct app in type"
  ConstructAPP -> termAction constructAPP "cannot construct term-to-type app in type"
  ConstructAnn -> termAction constructAnn "cannot construct annotation in type"
  RemoveAnn -> termAction removeAnn "there are no annotations in types"
  ConstructLam x -> termAction (constructLam x) "cannot construct function in type"
  ConstructLAM x -> termAction (constructLAM x) "cannot construct function in type"
  ConstructCon c -> termAction (constructCon c) "cannot construct con in type"
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
setCursor i e = case focusOn i (top e) of
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
moveExpr Child2 z
  | Case{} <- target z =
    throwError $ CustomFailure (Move Child2) "cannot move to 'Child2' of a case: use Branch instead"
moveExpr m z = move m z

-- | Apply a movement to a zipper
moveType :: ActionM m => Movement -> TypeZ -> m TypeZ
moveType m@(Branch _) _ = throwError $ CustomFailure (Move m) "Move-to-branch unsupported in types (there are no cases in types!)"
moveType m z = move m z

-- | Apply a movement to a generic zipper - does not support movement to a case
-- branch
move :: forall m za a. (ActionM m, IsZipper za a) => Movement -> za -> m za
move m z = do
  mz' <- move' m z
  case mz' of
    Just z' -> pure z'
    Nothing -> throwError $ CustomFailure (Move m) "movement failed"
  where
    move' :: Movement -> za -> m (Maybe za)
    move' Parent = pure . up
    move' Child1 = pure . down
    move' Child2 = pure . (down >=> right)
    move' (Branch _) = const $ throwError $ CustomFailure (Move m) "internal error: move does not support Branch moves"

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

constructVar :: ActionM m => Text -> ExprZ -> m ExprZ
constructVar x ast = case target ast of
  EmptyHole{} -> flip replace ast <$> var (unsafeMkName x)
  e -> throwError $ NeedEmptyHole (ConstructVar x) e

-- TODO: error if the ID doesn't reference an in-scope definition
constructGlobalVar :: ActionM m => ID -> ExprZ -> m ExprZ
constructGlobalVar id_ ast = case target ast of
  EmptyHole{} -> flip replace ast <$> global id_
  e -> throwError $ NeedEmptyHole (ConstructGlobalVar id_) e

insertSatVar :: ActionM m => Either Text ID -> ExprZ -> m ExprZ
insertSatVar x ast = case target ast of
  -- TODO: for now, rely on SmartHoles to fix up the type. I think we may want
  -- to do this manually to handle running with NoSmartHoles. There is no concern
  -- over "jumpy holes", as we will never emit something synthesing an arrow/forall
  -- type which would force an argument into a hole.
  --
  -- Revisit this once we decide on a smart hole toggle strategy. See:
  -- https://github.com/hackworthltd/primer/issues/9
  EmptyHole{} -> flip replace ast <$> saturatedApplication ast x
  e -> throwError $ NeedEmptyHole (either InsertSaturatedVar InsertSaturatedGlobalVar x) e

-- TODO: Add some tests for this. See:
-- https://github.com/hackworthltd/primer/issues/10
saturatedApplication :: ActionM m => ExprZ -> Either Text ID -> m Expr
saturatedApplication ast v = do
  (appHead, vTy) <-
    getVarType ast v >>= \case
      Left err -> throwError $ SaturatedApplicationError err
      Right t -> pure (either (var . unsafeMkName) global v, t)
  mkSaturatedApplication appHead vTy

getVarType ::
  MonadReader TC.Cxt m =>
  ExprZ ->
  Either Text ID ->
  m (Either Text TC.Type)
getVarType ast = \case
  Left local' ->
    -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
    -- See https://github.com/hackworthltd/primer/issues/11
    let l = unsafeMkName local'
        (tycxt, tmcxt) = localVariablesInScopeExpr (Left ast)
     in pure $ case (lookup l tmcxt, lookup l tycxt) of
          (Just t, _) -> Right t
          (Nothing, Just _) -> Left "That's a type var!"
          (Nothing, Nothing) -> Left "unknown local"
  Right globalId ->
    asks (lookupGlobal globalId) <&> \case
      Just (_, t) -> Right t
      Nothing -> Left "unknown global"

mkSaturatedApplication :: MonadFresh ID m => m Expr -> TC.Type -> m Expr
mkSaturatedApplication e = \case
  TFun _ _ t -> mkSaturatedApplication (e `app` emptyHole) t
  -- possibly we should substitute a type hole for the newly free var in t, but it doesn't matter for this algorithm
  TForall _ _ _ t -> mkSaturatedApplication (e `aPP` tEmptyHole) t
  _ -> e

insertRefinedVar :: ActionM m => Either Text ID -> ExprZ -> m ExprZ
insertRefinedVar x ast = do
  (v, vTy) <-
    getVarType ast x >>= \case
      Left err -> throwError $ RefineError err
      Right t -> pure (either (var . unsafeMkName) global x, t)
  let tgtTyCache = maybeTypeOf $ target ast
  -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
  -- See https://github.com/hackworthltd/primer/issues/11
  -- We only care about the type context for refine
  let (tycxt, _) = localVariablesInScopeExpr (Left ast)
  cxt <- asks $ TC.extendLocalCxtTys tycxt
  case target ast of
    EmptyHole{} -> flip replace ast <$> mkRefinedApplication cxt v vTy tgtTyCache
    e -> throwError $ NeedEmptyHole (either InsertRefinedVar InsertRefinedGlobalVar x) e

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

mkRefinedApplication :: ActionM m => TC.Cxt -> m Expr -> TC.Type -> Maybe TypeCache -> m Expr
mkRefinedApplication cxt e eTy tgtTy' = do
  tgtTy <- case tgtTy' of
    Just (TCChkedAt t) -> pure t
    Just (TCEmb b) -> pure $ tcChkedAt b
    _ -> throwError $ RefineError "Don't have a type we were checked at"
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
    -- 'Tests.Refine.hprop_scoping'). Since we ignore the type of an 'InstApp'
    -- and unconditionally put a hole, we do not have to worry about doing this
    -- substitution.
    f x = \case
      InstApp _ -> x `app` emptyHole
      InstAPP a -> x `aPP` generateTypeIDs a
      InstUnconstrainedAPP _ _ -> x `aPP` tEmptyHole

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
    Just x -> pure (unsafeMkName x)
  unless (isFresh x (target ze)) $ throwError NameCapture
  result <- flip replace ze <$> lam x (pure (target ze))
  moveExpr Child1 result

-- | Similar comments apply here as to 'constructLam'
constructLAM :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLAM mx ze = do
  -- If a name is provided, use that. Otherwise, generate a fresh one.
  x <- case mx of
    Nothing -> mkFreshName ze
    Just x -> pure (unsafeMkName x)
  unless (isFresh x (target ze)) $ throwError NameCapture
  result <- flip replace ze <$> lAM x (pure (target ze))
  moveExpr Child1 result

constructCon :: ActionM m => Text -> ExprZ -> m ExprZ
constructCon c ze = case target ze of
  EmptyHole{} -> flip replace ze <$> con (unsafeMkName c)
  e -> throwError $ NeedEmptyHole (ConstructCon c) e

constructSatCon :: ActionM m => Text -> ExprZ -> m ExprZ
constructSatCon c ze = case target ze of
  -- Similar comments re smartholes apply as to insertSatVar
  EmptyHole{} -> do
    ctorType <-
      getConstructorType n >>= \case
        Left err -> throwError $ SaturatedApplicationError err
        Right t -> pure t
    flip replace ze <$> mkSaturatedApplication (con n) ctorType
  e -> throwError $ NeedEmptyHole (ConstructCon c) e
  where
    n = unsafeMkName c

getConstructorType ::
  MonadReader TC.Cxt m =>
  Name ->
  m (Either Text TC.Type)
getConstructorType c =
  asks (flip lookupConstructor c . TC.typeDefs) <&> \case
    Just (vc, td) -> Right $ valConType td vc
    Nothing -> Left $ "Could not find constructor " <> unName c

constructRefinedCon :: ActionM m => Text -> ExprZ -> m ExprZ
constructRefinedCon c ze = do
  let n = unsafeMkName c
  cTy <-
    getConstructorType n >>= \case
      Left err -> throwError $ RefineError err
      Right t -> pure t
  let tgtTyCache = maybeTypeOf $ target ze
  -- our Cxt in the monad does not care about the local context, we have to extract it from the zipper.
  -- See https://github.com/hackworthltd/primer/issues/11
  -- We only care about the type context for refine
  let (tycxt, _) = localVariablesInScopeExpr (Left ze)
  cxt <- asks $ TC.extendLocalCxtTys tycxt
  case target ze of
    EmptyHole{} -> flip replace ze <$> mkRefinedApplication cxt (con n) cTy tgtTyCache
    e -> throwError $ NeedEmptyHole (ConstructRefinedCon c) e

constructLet :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLet mx ze = case target ze of
  EmptyHole{} -> do
    -- If a name is provided, use that. Otherwise, generate a fresh one.
    x <- case mx of
      Nothing -> mkFreshName ze
      Just x -> pure (unsafeMkName x)
    flip replace ze <$> let_ x emptyHole emptyHole
  e -> throwError $ NeedEmptyHole (ConstructLet mx) e

constructLetrec :: ActionM m => Maybe Text -> ExprZ -> m ExprZ
constructLetrec mx ze = case target ze of
  EmptyHole{} -> do
    -- If a name is provided, use that. Otherwise, generate a fresh one.
    x <- case mx of
      Nothing -> mkFreshName ze
      Just x -> pure (unsafeMkName x)
    flip replace ze <$> letrec x emptyHole tEmptyHole emptyHole
  e -> throwError $ NeedEmptyHole (ConstructLetrec mx) e

convertLetToLetrec :: ActionM m => ExprZ -> m ExprZ
convertLetToLetrec ze = case target ze of
  Let m x e1 e2 -> do
    t1 <- tEmptyHole
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
    Right (TC.TypeDefInfo _ tydef) ->
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
          brs = map f $ typeDefConstructors tydef
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
    | unName x == y -> pure ze
    | otherwise -> do
      let y' = unsafeMkName y
      case renameVar x y' e of
        Just e' -> pure $ replace (Lam m y' e') ze
        Nothing ->
          throwError NameCapture
  _ ->
    throwError $ CustomFailure (RenameLam y) "the focused expression is not a lambda"

-- | Replace @a@ with @b@ in @Λa. e@
renameLAM :: ActionM m => Text -> ExprZ -> m ExprZ
renameLAM b ze = case target ze of
  LAM m a e
    | unName a == b -> pure ze
    | otherwise -> do
      let b' = unsafeMkName b
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
    | unName x == y -> pure ze
    | otherwise -> do
      let y' = unsafeMkName y
      (e1', e2') <- doRename x y' e1 e2
      pure $ replace (Let m y' e1' e2') ze
  Letrec m x e1 t1 e2
    | unName x == y -> pure ze
    | otherwise -> do
      let y' = unsafeMkName y
      (e1', e2') <- doRename x y' e1 e2
      pure $ replace (Letrec m y' e1' t1 e2') ze
  _ ->
    throwError $ CustomFailure (RenameLet y) "the focused expression is not a let"
  where
    -- The renaming logic for lets and letrecs is identical, so we handle both here
    doRename :: ActionM m => Name -> Name -> Expr -> Expr -> m (Expr, Expr)
    doRename fromName toName e1 e2 = case (renameVar fromName toName e1, renameVar fromName toName e2) of
      (Just e1', Just e2') -> pure (e1', e2')
      (Nothing, _) -> throwError NameCapture
      (_, Nothing) -> throwError NameCapture

renameCaseBinding :: ActionM m => Text -> CaseBindZ -> m CaseBindZ
renameCaseBinding y caseBind = updateCaseBind caseBind $ \bind bindings rhs -> do
  let failure = throwError . CustomFailure (RenameCaseBinding y)
  let y' = unsafeMkName y

  -- Check that 'y' doesn't clash with any of the other branch bindings
  let otherBindings = delete bind bindings
  when (y' `elem` map bindName otherBindings) $
    failure $
      "can't rename this binding to "
        <> y
        <> " because it clashes with another binding in the case pattern"

  -- Apply the rename to the rhs
  rhs' <- case renameVar (bindName bind) y' rhs of
    Just e -> pure e
    Nothing ->
      failure $
        "cannot rename this binding to "
          <> y
          <> " because it clashes with another variable bound in the right hand side of the case branch"

  -- Rename the binding
  let bind' = set (typed @Name) y' bind

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

constructTCon :: ActionM m => Text -> TypeZ -> m TypeZ
constructTCon c zt = case target zt of
  TEmptyHole{} -> flip replace zt <$> tcon (unsafeMkName c)
  _ -> throwError $ CustomFailure (ConstructTCon c) "can only construct tcon in hole"

constructTVar :: ActionM m => Text -> TypeZ -> m TypeZ
constructTVar x ast = case target ast of
  TEmptyHole{} -> flip replace ast <$> tvar (unsafeMkName x)
  _ -> throwError $ CustomFailure (ConstructTVar x) "can only construct tvar in hole"

constructTForall :: ActionM m => Maybe Text -> TypeZ -> m TypeZ
constructTForall mx zt = do
  x <- case mx of
    Nothing -> mkFreshNameTy zt
    Just x -> pure (unsafeMkName x)
  unless (isFreshTy x $ target zt) $ throwError NameCapture
  flip replace zt <$> tforall x C.KType (pure (target zt))

constructTApp :: ActionM m => TypeZ -> m TypeZ
constructTApp zt = flip replace zt <$> tapp (pure (target zt)) tEmptyHole

-- | Replace @a@ with @b@ in @∀a. e@
renameForall :: ActionM m => Text -> TypeZ -> m TypeZ
renameForall b zt = case target zt of
  TForall m a k t
    | unName a == b -> pure zt
    | otherwise -> do
      let b' = unsafeMkName b
      case renameTyVar a b' t of
        Just t' -> pure $ replace (TForall m b' k t') zt
        Nothing ->
          throwError NameCapture
  _ ->
    throwError $ CustomFailure (RenameForall b) "the focused expression is not a forall type"

-- Check that a name is fresh for an expression. I.e. it does not
-- occur as a free variables, and thus binding it will not capture
-- a variable.
-- However, it may shadow a binding in more global scope that happens not to
-- be used in the expression, or a binding in the expression may shadow the
-- name.
isFresh :: Name -> Expr -> Bool
isFresh v e = v `S.notMember` freeVars e

isFreshTy :: Name -> Type -> Bool
isFreshTy v t = v `S.notMember` freeVarsTy t

-- We make a fresh name that is appropriate for binding here (i.e. wrapping the
-- target of the zipper).
-- To avoid variable capture we must avoid any name free in the focussed expr;
-- this is important for correctness.
-- To avoid shadowing any other variable we should avoid any more-globally bound
-- name (i.e. "up" in the zipper); this is not a correctness concern, but a
-- usability concern: we don't want automatically generated names inducing
-- shadowing.
-- To avoid being shadowed we should avoid any names bound in the focussed
-- expr; this is also a usability concern only.
--
-- NB: the free names of the target are a subset of the more-globally bound
-- names, so we don't need to explicitly worry about them.
--
-- Because of implementation details, locally bound variables are in a
-- different namespace than top-level definitions and from term/type
-- constructors. However, for the sake of non-confusingness, we don't care
-- about that here. Thus when we avoid more-globally bound names, we will also
-- include globally-scoped things.
mkFreshName :: ActionM m => ExprZ -> m Name
mkFreshName e = freshName =<< mkAvoidForFreshName e

mkAvoidForFreshName :: MonadReader TC.Cxt m => ExprZ -> m (S.Set Name)
mkAvoidForFreshName e = do
  let moreGlobal = bindersAbove e
      moreLocal = bindersBelow e
  globals <- TC.getGlobalNames
  pure $ S.unions [moreGlobal, moreLocal, globals]

mkFreshNameTy :: ActionM m => TypeZ -> m Name
mkFreshNameTy t = freshName =<< mkAvoidForFreshNameTy t

mkAvoidForFreshNameTy :: MonadReader TC.Cxt m => TypeZ -> m (S.Set Name)
mkAvoidForFreshNameTy t = do
  let moreGlobal = bindersAboveTypeZ t
      moreLocal = bindersBelowTy $ focusOnlyType t
  globals <- TC.getGlobalNames
  pure $ S.unions [moreGlobal, moreLocal, globals]
