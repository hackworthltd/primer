{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- This module defines the high level application functions.

module Primer.App (
  module Primer.App.Base,
  MonadEditApp,
  Log (..),
  defaultLog,
  App,
  mkApp,
  mkAppSafe,
  appProg,
  appIdCounter,
  appNameCounter,
  appInit,
  newApp,
  newEmptyApp,
  checkAppWellFormed,
  checkProgWellFormed,
  EditAppM,
  QueryAppM,
  runEditAppM,
  runQueryAppM,
  Prog (..),
  defaultProg,
  newEmptyProg',
  newProg,
  newProg',
  progAllModules,
  progAllDefs,
  progAllTypeDefs,
  progAllTypeDefsMeta,
  allDefs,
  allTypes,
  allValConNames,
  allTyConNames,
  progCxt,
  tcWholeProg,
  tcWholeProgWithImports,
  nextProgID,
  ProgAction (..),
  ProgError (..),
  Question (..),
  handleQuestion,
  handleGetProgramRequest,
  handleMutationRequest,
  handleEditRequest,
  importModules,
  MutationRequest (..),
  lookupASTDef,
  liftError,
) where

import Foreword hiding (mod)

import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.NestedError (MonadNestedError)
import Data.Generics.Uniplate.Operations (transform, transformM)
import Data.Generics.Uniplate.Zipper (
  fromZipper,
 )
import Data.List (intersect, (\\))
import Data.List.Extra (anySame, disjoint)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics (
  Field1 (_1),
  Field2 (_2),
  Field3 (_3),
  ReversibleOptic (re),
  ifoldMap,
  mapped,
  over,
  set,
  traverseOf,
  traversed,
  view,
  (%),
  (%~),
  (.~),
  (?~),
  (^.),
  _Just,
  _Left,
  _Right,
 )
import Optics.State.Operators ((<<%=))
import Primer.Action (
  Action,
  ActionError (..),
  ProgAction (..),
  applyAction',
  applyActionsToBody,
  applyActionsToField,
  applyActionsToTypeSig,
  insertSubseqBy,
 )
import Primer.Action.ProgError (ProgError (..))
import Primer.App.Base (
  DefSelection (..),
  Editable (..),
  Level (..),
  NodeSelection (..),
  NodeType (..),
  Selection,
  Selection' (..),
  TypeDefConsFieldSelection (..),
  TypeDefConsSelection (..),
  TypeDefNodeSelection (..),
  TypeDefSelection (..),
  getTypeDefConFieldType,
 )
import Primer.Core (
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  CaseFallback,
  Expr,
  Expr' (Case, Con, EmptyHole, Var),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID (..),
  LocalName (LocalName, unLocalName),
  Meta (..),
  ModuleName (ModuleName),
  Pattern (PatCon),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  Type,
  Type' (..),
  TypeMeta,
  ValConName,
  caseBranchName,
  getID,
  mkSimpleModuleName,
  qualifyName,
  typesInExpr,
  unModuleName,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _exprMetaLens,
  _typeMetaLens,
 )
import Primer.Core.DSL (S, create, emptyHole, tEmptyHole, tvar)
import Primer.Core.DSL qualified as DSL
import Primer.Core.Transform (renameVar, unfoldTApp)
import Primer.Core.Utils (freeVars, freeVarsTy, generateTypeIDs, regenerateExprIDs, regenerateTypeIDs, _freeTmVars, _freeTyVars, _freeVarsTy)
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
  defAST,
 )
import Primer.Def.Utils (globalInUse, typeInUse)
import Primer.JSON
import Primer.Module (
  Module (Module, moduleDefs, moduleName, moduleTypes),
  builtinModule,
  deleteDef,
  insertDef,
  moduleDefsQualified,
  moduleTypesQualified,
  moduleTypesQualifiedMeta,
  nextModuleID,
  primitiveModule,
  qualifyDefName,
  renameModule,
  renameModule',
 )
import Primer.Name (Name (unName), NameCounter, freshName, unsafeMkName)
import Primer.Prelude (prelude)
import Primer.Questions (
  Question (..),
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.TypeDef (
  ASTTypeDef (..),
  TypeDef (..),
  TypeDefMap,
  ValCon (..),
  generateTypeDefIDs,
  typeDefAST,
 )
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  Cxt,
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError,
  buildTypingContextFromModules,
  checkEverything,
  checkTypeDefs,
  synth,
 )
import Primer.Zipper (
  ExprZ,
  Loc' (InBind, InExpr, InType),
  TypeZ,
  TypeZip,
  current,
  focusLoc,
  focusOn,
  focusOnTy,
  focusOnlyType,
  foldAbove,
  foldAboveTypeZ,
  getBoundHere,
  getBoundHereUp,
  getBoundHereUpTy,
  locToEither,
  replace,
  target,
  unfocusExpr,
  unfocusType,
  _target,
 )

-- | The full program state.
data Prog = Prog
  { progImports :: [Module]
  -- ^ Some immutable imported modules
  , progModules :: [Module]
  -- ^ The editable "home" modules
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log
  -- ^ The sequence of all successful edits performed on the program
  -- since its creation, in order of first edit to last. A successful
  -- undo operation pops the last edit from this log, and pushes it
  -- onto the 'redoLog'.
  , redoLog :: Log
  -- ^ A log of successive undo operations, in order of most recent to
  -- least (i.e., a LIFO-order stack). A successful redo operation
  -- pops the most recent undo from this log. Note that this log is
  -- reset whenever an action is performed; i.e., redos are not
  -- preserved across edits.
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Prog
  deriving anyclass (NFData)

-- | Push a compound action onto the given 'Log', returning the new
-- 'Log'.
push :: [ProgAction] -> Log -> Log
push as l = Log $ as : unlog l

-- | Pop the head of the given 'Log'.
--
-- If the log is not empty, returns 'Just (as, l)' where 'as' is the
-- head of the log and 'l' is the rest of the log.
--
-- If the log is empty, returns 'Nothing'.
pop :: Log -> Maybe ([ProgAction], Log)
pop l = case unlog l of
  [] -> Nothing
  (as : l') -> Just (as, Log l')

-- | The default 'Prog'. It has no imports, no definitions, no current
-- 'Selection', and an empty 'Log'. Smart holes are enabled.
defaultProg :: Prog
defaultProg = Prog mempty mempty Nothing SmartHoles defaultLog defaultLog

progAllModules :: Prog -> [Module]
progAllModules p = progModules p <> progImports p

progAllTypeDefs :: Prog -> Map TyConName (Editable, TypeDef ())
progAllTypeDefs p =
  foldMap' (fmap (Editable,) . moduleTypesQualified) (progModules p)
    <> foldMap' (fmap (NonEditable,) . moduleTypesQualified) (progImports p)

progAllTypeDefsMeta :: Prog -> Map TyConName (Editable, TypeDef TypeMeta)
progAllTypeDefsMeta p =
  foldMap' (fmap (Editable,) . moduleTypesQualifiedMeta) (progModules p)
    <> foldMap' (fmap (NonEditable,) . moduleTypesQualifiedMeta) (progImports p)

progAllDefs :: Prog -> Map GVarName (Editable, Def)
progAllDefs p =
  foldMap' (fmap (Editable,) . moduleDefsQualified) (progModules p)
    <> foldMap' (fmap (NonEditable,) . moduleDefsQualified) (progImports p)

-- Note [Modules]
-- The invariant is that the @progImports@ modules are never edited, but
-- one can insert new ones (and perhaps delete unneeded ones).
--
-- We assume that all Names and IDs are unique within one module, and that
-- module names are unique.
--
-- All modules in a @Prog@ shall be well-typed, in the appropriate scope:
-- all the imports are in one mutual dependency group
-- the @progModule@ has all the imports in scope

-- | A triple of 'Prog', 'ID', and 'NameCounter'.
--
-- The 'Prog' is a program with no imports and an editable module
-- named @Main@. The editable module contains a single top-level
-- definition named @main@ whose type and term are both empty holes.
--
-- The 'ID' is the next available 'ID' in module @Main@.
--
-- The 'NameCounter' is a safe value for seeding an 'App''s name
-- counter when using 'newEmptyProg' as the app's program.
newEmptyProgImporting :: [S Module] -> (Prog, ID, NameCounter)
newEmptyProgImporting imported =
  let defName = "main"
      moduleName = mkSimpleModuleName "Main"
      ((imported', defs), nextID) = create $ do
        mainExpr <- emptyHole
        mainType <- tEmptyHole
        let astDefs =
              Map.singleton
                defName
                ( ASTDef
                    { astDefExpr = mainExpr
                    , astDefType = mainType
                    }
                )
        (,fmap DefAST astDefs) <$> sequence imported
   in ( defaultProg
          { progModules =
              [ Module
                  { moduleName
                  , moduleTypes = mempty
                  , moduleDefs = defs
                  }
              ]
          , progImports = imported'
          , progSelection =
              Just $
                SelectionDef
                  DefSelection
                    { def = qualifyName moduleName defName
                    , node = Nothing
                    }
          }
      , nextID
      , toEnum 0
      )

-- | Like 'newEmptyProg', but drop the 'ID' and 'NameCounter'.
--
-- This value should probably only be used for testing.
newEmptyProg' :: Prog
newEmptyProg' = let (p, _, _) = newEmptyProgImporting [] in p

-- | A triple of 'Prog' and 'ID'.
--
-- The 'Prog' is identical to the one returned by 'newEmptyProg',
-- except that its import list includes all builtin and primitive
-- modules defined by Primer.
--
-- The 'ID' is the next available 'ID' in module @Main@.
--
-- The 'NameCounter' is a safe value for seeding an 'App''s name
-- counter when using 'newProg' as the app's program.
newProg :: (Prog, ID, NameCounter)
newProg =
  let (p, nextID, nc) =
        newEmptyProgImporting
          [ prelude
          , builtinModule
          , pure primitiveModule
          ]
   in ( p
      , nextID
      , nc
      )

-- | Like 'newProg', but drop the 'ID' and 'NameCounter'.
--
-- This value should probably only be used for testing.
newProg' :: Prog
newProg' = let (p, _, _) = newProg in p

-- | Imports some explicitly-given modules, ensuring that they are well-typed
-- (and all their dependencies are already imported)
importModules :: MonadEditApp l ProgError m => [Module] -> m ()
importModules ms = do
  p <- gets appProg
  -- Module names must be unique
  let currentModules = progAllModules p
  let currentNames = moduleName <$> currentModules
  let newNames = moduleName <$> ms
  unless (disjoint currentNames newNames && not (anySame newNames)) $
    throwError $
      ActionError $
        ImportNameClash $
          (currentNames `intersect` newNames) <> (newNames \\ ordNub newNames)
  -- Imports must be well-typed (and cannot depend on the editable modules)
  checkedImports <-
    liftError (ActionError . ImportFailed ()) $
      checkEverything NoSmartHoles $
        CheckEverything{trusted = progImports p, toCheck = ms}
  let p' = p & #progImports %~ (<> checkedImports)
  modify (\a -> a & #currentState % #prog .~ p')

-- | Get all type definitions from all modules (including imports)
allTypes :: Prog -> TypeDefMap
allTypes = fmap snd . progAllTypeDefs

allTypesMeta :: Prog -> Map TyConName (TypeDef TypeMeta)
allTypesMeta = fmap snd . progAllTypeDefsMeta

-- | Get all definitions from all modules (including imports)
allDefs :: Prog -> DefMap
allDefs = fmap snd . progAllDefs

-- | The action log
--  This is the canonical store of the program - we can recreate any current or
--  past program state by replaying this log.
--  Each item is a sequence of Core actions which should be applied atomically.
--  Items are stored in reverse order so it's quick to add new ones.
newtype Log = Log {unlog :: [[ProgAction]]}
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Log
  deriving anyclass (NFData)
  deriving newtype (Semigroup, Monoid)

-- | The default (empty) 'Log'.
defaultLog :: Log
defaultLog = Log mempty

-- | The type of requests which can mutate the application state.
data MutationRequest
  = Undo
  | Redo
  | Edit [ProgAction]
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON MutationRequest
  deriving anyclass (NFData)

-- * Request handlers

-- | Handle a question
-- Note that these only consider the non-imported module as a location of which
-- to ask a question. However, they will return variables which are in scope by
-- dint of being imported.
handleQuestion :: MonadQueryApp m => Question a -> m a
handleQuestion = \case
  VariablesInScope defid exprid -> do
    node <- focusNode' defid exprid
    defs <- asks $ allDefs . appProg
    let (tyvars, termvars, globals) = case node of
          Left zE -> variablesInScopeExpr defs zE
          Right zT -> (variablesInScopeTy zT, [], [])
    pure ((tyvars, termvars), globals)
  GenerateName defid nodeid typeKind -> do
    prog <- asks appProg
    names <-
      focusNode' defid nodeid <&> \case
        Left zE -> generateNameExpr typeKind zE
        Right zT -> generateNameTy typeKind zT
    pure $ runReader names $ progCxt prog
  where
    focusNode' defname nodeid = do
      prog <- asks appProg
      focusNode prog defname nodeid

-- This only looks in the editable modules, not in any imports
focusNode :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNode prog = focusNodeDefs $ foldMap' moduleDefsQualified $ progModules prog

-- This looks in the editable modules and also in any imports
focusNodeImports :: MonadError ProgError m => Prog -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeImports prog = focusNodeDefs $ allDefs prog

focusNodeDefs :: MonadError ProgError m => DefMap -> GVarName -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNodeDefs defs defname nodeid =
  case lookupASTDef defname defs of
    Nothing -> throwError $ DefNotFound defname
    Just def ->
      let mzE = locToEither <$> focusOn nodeid (astDefExpr def)
          mzT = focusOnTy nodeid $ astDefType def
       in case fmap Left mzE <|> fmap Right mzT of
            Nothing -> throwError $ ActionError (IDNotFound nodeid)
            Just x -> pure x

-- | Handle a request to retrieve the current program
handleGetProgramRequest :: MonadReader App m => m Prog
handleGetProgramRequest = asks appProg

-- | Handle a request to mutate the app state
handleMutationRequest :: MonadEditApp l ProgError m => MutationRequest -> m Prog
handleMutationRequest = \case
  Edit as -> handleEditRequest as
  Undo -> handleUndoRequest
  Redo -> handleRedoRequest

-- | Handle an edit request
--
-- Note that a successful edit resets the redo log.
handleEditRequest :: forall m l. MonadEditApp l ProgError m => [ProgAction] -> m Prog
handleEditRequest actions = do
  prog <- gets appProg >>= \p -> foldlM applyProgAction p actions
  let l = progLog prog
  let prog' = prog{progLog = push actions l, redoLog = defaultLog}
  modify (\s -> s & #currentState % #prog .~ prog')
  pure prog'

-- | Handle a 'ProgAction'
applyProgAction :: MonadEdit m ProgError => Prog -> ProgAction -> m Prog
applyProgAction prog = \case
  MoveToDef d -> do
    m <- lookupEditableModule (qualifiedModule d) prog
    case Map.lookup d $ moduleDefsQualified m of
      Nothing -> throwError $ DefNotFound d
      Just _ -> pure $ prog & #progSelection ?~ SelectionDef (DefSelection d Nothing)
  DeleteDef d -> editModuleCross (qualifiedModule d) prog $ \(m, ms) ->
    case deleteDef m d of
      Nothing -> throwError $ DefNotFound d
      Just mod' -> do
        when (globalInUse d $ foldMap' moduleDefs $ mod' : ms) $
          throwError $
            DefInUse d
        pure (mod' : ms, Nothing)
  RenameDef d nameStr -> editModuleOfCross (Just d) prog $ \(m, ms) defName def -> do
    let defs = moduleDefs m
        newNameBase = unsafeMkName nameStr
        newName = qualifyName (moduleName m) newNameBase
    if Map.member newNameBase defs
      then throwError $ DefAlreadyExists newName
      else do
        let m' = m{moduleDefs = Map.insert newNameBase (DefAST def) $ Map.delete defName defs}
        renamedModules <-
          maybe (throwError $ ActionError NameCapture) pure $
            traverseOf
              (traversed % #moduleDefs % traversed % #_DefAST % #astDefExpr)
              (renameVar (GlobalVarRef d) (GlobalVarRef newName))
              (m' : ms)
        pure (renamedModules, Just $ SelectionDef $ DefSelection newName Nothing)
  CreateDef modName n -> editModule modName prog $ \mod -> do
    let defs = moduleDefs mod
    name <- case n of
      Just nameStr ->
        let baseName = unsafeMkName nameStr
            name = qualifyName modName baseName
         in if Map.member baseName defs
              then throwError $ DefAlreadyExists name
              else pure baseName
      Nothing -> freshName $ Map.keysSet defs
    expr <- newExpr
    ty <- newType
    let def = ASTDef expr ty
    pure (insertDef mod name $ DefAST def, Just $ SelectionDef $ DefSelection (qualifyName modName name) Nothing)
  AddTypeDef tc td -> editModule (qualifiedModule tc) prog $ \m -> do
    td' <- generateTypeDefIDs $ TypeDefAST td
    let tydefs' = moduleTypes m <> Map.singleton (baseName tc) td'
    liftError
      -- The frontend should never let this error case happen,
      -- so we just dump out a raw string for debugging/logging purposes
      -- (This is not currently true! We should synchronise the frontend with
      -- the typechecker rules. For instance, the form allows to create
      --   data T (T : *) = T
      -- but the TC rejects it.
      -- see https://github.com/hackworthltd/primer/issues/3)
      (TypeDefError . show @TypeError)
      ( runReaderT
          (checkTypeDefs $ Map.singleton tc (TypeDefAST td))
          (buildTypingContextFromModules (progAllModules prog) NoSmartHoles)
      )
    pure
      ( m{moduleTypes = tydefs'}
      , Just $ SelectionTypeDef $ TypeDefSelection tc Nothing
      )
  DeleteTypeDef d -> editModuleCross (qualifiedModule d) prog $ \(m, ms) ->
    case moduleTypesQualified m Map.!? d of
      Nothing -> throwError $ TypeDefNotFound d
      Just (TypeDefPrim _) -> throwError $ TypeDefIsPrim d
      Just (TypeDefAST td) -> do
        checkTypeNotInUse d td $ m : ms
        let m' = m{moduleTypes = Map.delete (baseName d) (moduleTypes m)}
        pure (m' : ms, Nothing)
  RenameType old (unsafeMkName -> nameRaw) -> editModuleCross (qualifiedModule old) prog $ \(m, ms) -> do
    when (new `elem` allTyConNames prog) $ throwError $ TypeDefAlreadyExists new
    m' <- traverseOf #moduleTypes updateType m
    let renamedInTypes = over (traversed % #moduleTypes) updateRefsInTypes $ m' : ms
    pure
      ( over (traversed % #moduleDefs % traversed % #_DefAST) (updateDefBody . updateDefType) renamedInTypes
      , Just $ SelectionTypeDef $ TypeDefSelection new Nothing
      )
    where
      new = qualifyName (qualifiedModule old) nameRaw
      updateType m = do
        d0 <-
          -- NB We do not allow primitive types to be renamed.
          -- To relax this, we'd have to be careful about how it interacts with type-checking of primitive literals.
          maybe (throwError $ TypeDefIsPrim old) pure . typeDefAST
            =<< maybe (throwError $ TypeDefNotFound old) pure (Map.lookup (baseName old) m)
        when (nameRaw `elem` map (unLocalName . fst) (astTypeDefParameters d0)) $ throwError $ TyConParamClash nameRaw
        pure $ Map.insert nameRaw (TypeDefAST d0) $ Map.delete (baseName old) m
      updateRefsInTypes =
        over
          (traversed % #_TypeDefAST % #astTypeDefConstructors % traversed % #valConArgs % traversed)
          $ transform
          $ over (#_TCon % _2) updateName
      updateDefType =
        over
          #astDefType
          $ transform
          $ over (#_TCon % _2) updateName
      updateDefBody =
        over
          #astDefExpr
          $ transform
          $ over typesInExpr
          $ transform
          $ over (#_TCon % _2) updateName
      updateName n = if n == old then new else n
  RenameCon type_ old (unsafeMkGlobalName . (fmap unName (unModuleName (qualifiedModule type_)),) -> new) ->
    editModuleCross (qualifiedModule type_) prog $ \(m, ms) -> do
      when (new `elem` allValConNames prog) $ throwError $ ConAlreadyExists new
      m' <- updateType m
      pure
        ( over (mapped % #moduleDefs) updateDefs (m' : ms)
        , Just $ SelectionTypeDef $ TypeDefSelection type_ $ Just $ TypeDefConsNodeSelection $ TypeDefConsSelection new Nothing
        )
    where
      updateType =
        alterTypeDef
          ( traverseOf
              #astTypeDefConstructors
              ( maybe (throwError $ ConNotFound old) pure
                  . findAndAdjust ((== old) . valConName) (#valConName .~ new)
              )
          )
          type_
      updateDefs =
        over (traversed % #_DefAST % #astDefExpr) $
          transform $
            over (#_Con % _2) updateName
              . over (#_Case % _3 % traversed % #_CaseBranch % _1 % #_PatCon) updateName
      updateName n = if n == old then new else n
  RenameTypeParam type_ old (unsafeMkLocalName -> new) ->
    editModule (qualifiedModule type_) prog $ \m -> do
      m' <- updateType m
      pure
        ( m'
        , Just $ SelectionTypeDef $ TypeDefSelection type_ $ Just $ TypeDefParamNodeSelection new
        )
    where
      updateType =
        alterTypeDef
          (updateConstructors <=< updateParam)
          type_
      updateParam def = do
        when (new `elem` map fst (astTypeDefParameters def)) $ throwError $ ParamAlreadyExists new
        let nameRaw = unLocalName new
        when (nameRaw == baseName type_) $ throwError $ TyConParamClash nameRaw
        when (nameRaw `elem` map (baseName . valConName) (astTypeDefConstructors def)) $ throwError $ ValConParamClash nameRaw
        def
          & traverseOf
            #astTypeDefParameters
            ( maybe (throwError $ ParamNotFound old) pure
                . findAndAdjust ((== old) . fst) (_1 .~ new)
            )
      updateConstructors =
        traverseOf
          ( #astTypeDefConstructors
              % traversed
              % #valConArgs
              % traversed
          )
          $ traverseOf _freeVarsTy
          $ \(_, v) -> tvar $ updateName v
      updateName n = if n == old then new else n
  AddCon type_ index (unsafeMkGlobalName . (fmap unName (unModuleName (qualifiedModule type_)),) -> con) ->
    editModuleCross (qualifiedModule type_) prog $ \(m, ms) -> do
      when (con `elem` allValConNames prog) $ throwError $ ConAlreadyExists con
      m' <- updateType m
      newTy <- maybe (throwError $ TypeDefNotFound type_) pure $ moduleTypesQualified m' Map.!? type_
      allCons <- maybe (throwError $ TypeDefIsPrim type_) (pure . astTypeDefConstructors) $ typeDefAST newTy
      ms' <-
        traverseOf
          (traversed % #moduleDefs % traversed % #_DefAST % #astDefExpr)
          (updateDefs allCons)
          $ m' : ms
      pure
        ( ms'
        , Just $ SelectionTypeDef $ TypeDefSelection type_ $ Just $ TypeDefConsNodeSelection $ TypeDefConsSelection con Nothing
        )
    where
      updateDefs allCons = transformNamedCaseBranches prog type_ $ \bs -> do
        m' <- DSL.meta
        pure $ insertSubseqBy caseBranchName (CaseBranch (PatCon con) [] (EmptyHole m')) (PatCon . valConName <$> allCons) bs
      updateType =
        alterTypeDef
          ( traverseOf
              #astTypeDefConstructors
              (maybe (throwError $ IndexOutOfRange index) pure . insertAt index (ValCon con []))
          )
          type_
  DeleteCon tdName vcName -> editModuleCross (qualifiedModule tdName) prog $ \(m, ms) -> do
    m' <-
      alterTypeDef
        ( \td -> do
            checkTypeNotInUse tdName td $ m : ms
            traverseOf
              #astTypeDefConstructors
              ( \cons -> do
                  when
                    (vcName `notElem` map valConName cons)
                    (throwError $ ConNotFound vcName)
                  pure $ filter ((/= vcName) . valConName) cons
              )
              td
        )
        tdName
        m
    pure (m' : ms, Just $ SelectionTypeDef $ TypeDefSelection tdName Nothing)
  AddConField type_ con index new ->
    editModuleCross (qualifiedModule type_) prog $ \(m, ms) -> do
      m' <- updateType m
      ms' <- traverseOf (traversed % #moduleDefs) updateDefs (m' : ms)
      pure
        ( ms'
        , Just
            . SelectionTypeDef
            . TypeDefSelection type_
            . Just
            . TypeDefConsNodeSelection
            . TypeDefConsSelection con
            $ Nothing
        )
    where
      updateType =
        alterTypeDef
          ( traverseOf #astTypeDefConstructors $
              maybe (throwError $ ConNotFound con) pure
                <=< findAndAdjustA
                  ((== con) . valConName)
                  ( traverseOf
                      #valConArgs
                      ( maybe (throwError $ IndexOutOfRange index) pure
                          <=< liftA2 (insertAt index) (generateTypeIDs new) . pure
                      )
                  )
          )
          type_
      -- NB: we must updateDecons first, as transformCaseBranches may do
      -- synthesis of the scrutinee's type, using the old typedef. Thus we must
      -- not update the scrutinee before this happens.
      updateDefs = traverseOf (traversed % #_DefAST % #astDefExpr) (updateCons <=< updateDecons)
      updateCons = transformM $ \case
        Con m con' tms | con' == con -> do
          m' <- DSL.meta
          case insertAt index (EmptyHole m') tms of
            Just args' -> pure $ Con m con' args'
            Nothing -> throwError $ ConNotSaturated con
        e -> pure e
      updateDecons = transformNamedCaseBranch prog type_ con $
        \(CaseBranch vc binds e) -> do
          m' <- DSL.meta
          newName <- LocalName <$> freshName (freeVars e)
          binds' <- maybe (throwError $ IndexOutOfRange index) pure $ insertAt index (Bind m' newName) binds
          pure $ CaseBranch vc binds' e
  DeleteConField tdName vcName index -> editModuleCross (qualifiedModule tdName) prog $ \(m, ms) -> do
    m' <-
      alterTypeDef
        ( \td -> do
            checkTypeNotInUse tdName td $ m : ms
            traverseOf
              #astTypeDefConstructors
              ( maybe (throwError $ ConNotFound vcName) pure
                  <=< findAndAdjustA
                    ((== vcName) . valConName)
                    (traverseOf #valConArgs $ maybe (throwError $ IndexOutOfRange index) pure . deleteAt index)
              )
              td
        )
        tdName
        m
    pure
      ( m' : ms
      , Just
          . SelectionTypeDef
          . TypeDefSelection tdName
          . Just
          . TypeDefConsNodeSelection
          $ TypeDefConsSelection vcName Nothing
      )
  AddTypeParam tdName index paramName0 k -> editModuleCross (qualifiedModule tdName) prog $ \(m, ms) -> do
    let paramName = unsafeMkLocalName paramName0
    m' <-
      alterTypeDef
        ( \td -> do
            checkTypeNotInUse tdName td $ m : ms
            traverseOf
              #astTypeDefParameters
              ( \ps -> do
                  when
                    (paramName `elem` map fst ps)
                    (throwError $ ParamAlreadyExists paramName)
                  maybe (throwError $ IndexOutOfRange index) pure $ insertAt index (paramName, k) ps
              )
              td
        )
        tdName
        m
    pure (m' : ms, Just $ SelectionTypeDef $ TypeDefSelection tdName $ Just $ TypeDefParamNodeSelection paramName)
  DeleteTypeParam tdName paramName -> editModuleCross (qualifiedModule tdName) prog $ \(m, ms) -> do
    m' <-
      alterTypeDef
        ( \td -> do
            checkTypeNotInUse tdName td $ m : ms
            when
              (any (elem paramName . freeVarsTy) $ concatMap valConArgs $ astTypeDefConstructors td)
              (throwError $ TypeParamInUse tdName paramName)
            traverseOf
              #astTypeDefParameters
              ( \ps -> do
                  when
                    (paramName `notElem` map fst ps)
                    (throwError $ ParamNotFound paramName)
                  pure $ filter ((/= paramName) . fst) ps
              )
              td
        )
        tdName
        m
    pure
      (m' : ms, Just $ SelectionTypeDef $ TypeDefSelection tdName Nothing)
  BodyAction actions -> editModuleOf mdefName prog $ \m defName def -> do
    let smartHoles = progSmartHoles prog
    res <- applyActionsToBody smartHoles (progAllModules prog) def actions
    case res of
      Left err -> throwError $ ActionError err
      Right (def', z) -> do
        let meta = bimap (view _exprMetaLens . target) (view _typeMetaLens . target) $ locToEither z
        pure
          ( insertDef m defName (DefAST def')
          , Just . SelectionDef $
              DefSelection (qualifyDefName m defName) $
                Just
                  NodeSelection
                    { nodeType = BodyNode
                    , meta
                    }
          )
  SigAction actions -> editModuleOfCross mdefName prog $ \ms@(curMod, _) defName def -> do
    let smartHoles = progSmartHoles prog
    res <- applyActionsToTypeSig smartHoles (progImports prog) ms (defName, def) actions
    case res of
      Left err -> throwError $ ActionError err
      Right (mod', zt) -> do
        let node = target zt
            meta = view _typeMetaLens node
         in pure
              ( mod'
              , Just . SelectionDef $
                  DefSelection (qualifyDefName curMod defName) $
                    Just
                      NodeSelection
                        { nodeType = SigNode
                        , meta = Right meta
                        }
              )
  ConFieldAction tyName con index actions -> editModuleOfCrossType (Just tyName) prog $ \ms defName def -> do
    let smartHoles = progSmartHoles prog
    applyActionsToField smartHoles (progImports prog) ms (defName, con, index, def) actions >>= \case
      Left err -> throwError $ ActionError err
      Right (mod', zt) ->
        pure
          ( mod'
          , Just $
              SelectionTypeDef
                TypeDefSelection
                  { def = tyName
                  , node =
                      Just $
                        TypeDefConsNodeSelection
                          TypeDefConsSelection
                            { con
                            , field =
                                Just
                                  TypeDefConsFieldSelection
                                    { index
                                    , meta = Right $ zt ^. _target % _typeMetaLens
                                    }
                            }
                  }
          )
  SetSmartHoles smartHoles ->
    pure $ prog & #progSmartHoles .~ smartHoles
  CopyPasteSig fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteSig prog fromIds i setup
  CopyPasteBody fromIds setup -> case mdefName of
    Nothing -> throwError NoDefSelected
    Just i -> copyPasteBody prog fromIds i setup
  RenameModule oldName newName -> do
    -- Call editModuleSameSelection solely for checking that 'oldName'
    -- is an editable module
    _ <- editModuleSameSelection oldName prog pure
    let n = ModuleName $ unsafeMkName <$> newName
        curMods = RM{imported = progImports prog, editable = progModules prog}
     in if n == oldName
          then pure prog
          else case renameModule oldName n curMods of
            Nothing -> throwError RenameModuleNameClash
            Just renamedMods ->
              if imported curMods == imported renamedMods
                then
                  pure $
                    prog
                      & #progModules .~ editable renamedMods
                      & #progSelection % _Just %~ renameModule' oldName n
                else
                  throwError $
                    -- It should never happen that the action edits an
                    -- imported module, since the oldName should be distinct
                    -- from the name of any import
                    ActionError $
                      InternalFailure "RenameModule: imported modules were edited by renaming"
  where
    checkTypeNotInUse tdName td ms =
      when
        (typeInUse tdName td (foldMap' moduleTypes ms) (foldMap' moduleDefs ms))
        (throwError $ TypeDefInUse tdName)
    mdefName = case progSelection prog of
      Just (SelectionDef s) -> Just s.def
      _ -> Nothing

-- Helper for RenameModule action
data RenameMods a = RM {imported :: [a], editable :: [a]}
  deriving stock (Functor, Foldable, Traversable)

lookupEditableModule :: MonadError ProgError m => ModuleName -> Prog -> m Module
lookupEditableModule n p =
  lookupModule' n p >>= \case
    MLEditable m -> pure m
    MLImported _ -> throwError $ ModuleReadonly n

-- | Describes return type of successfully looking a module up in the program.
-- We get the module and also whether it is imported or not.
data ModuleLookup = MLEditable Module | MLImported Module

lookupModule' :: MonadError ProgError m => ModuleName -> Prog -> m ModuleLookup
lookupModule' n p = case (find ((n ==) . moduleName) (progModules p), find ((n ==) . moduleName) (progImports p)) of
  (Just m, _) -> pure $ MLEditable m
  (Nothing, Just m) -> pure $ MLImported m
  (Nothing, Nothing) -> throwError $ ModuleNotFound n

editModule ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  (Module -> m (Module, Maybe Selection)) ->
  m Prog
editModule n p f = do
  m <- lookupEditableModule n p
  (m', s) <- f m
  pure $
    p
      { progModules = m' : filter ((/= n) . moduleName) (progModules p)
      , progSelection = s
      }

-- A variant of 'editModule' for actions which can affect multiple modules
editModuleCross ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  ((Module, [Module]) -> m ([Module], Maybe Selection)) ->
  m Prog
editModuleCross n p f = do
  m <- lookupEditableModule n p
  let otherModules = filter ((/= n) . moduleName) (progModules p)
  (m', s) <- f (m, otherModules)
  pure $
    p
      { progModules = m'
      , progSelection = s
      }

editModuleSameSelection ::
  MonadError ProgError m =>
  ModuleName ->
  Prog ->
  (Module -> m Module) ->
  m Prog
editModuleSameSelection n p f = editModule n p (fmap (,progSelection p) . f)

editModuleOf ::
  MonadError ProgError m =>
  Maybe GVarName ->
  Prog ->
  (Module -> Name -> ASTDef -> m (Module, Maybe Selection)) ->
  m Prog
editModuleOf mdefName prog f = case mdefName of
  Nothing -> throwError NoDefSelected
  Just defname -> editModule (qualifiedModule defname) prog $ \m ->
    case Map.lookup (baseName defname) (moduleDefs m) of
      Just (DefAST def) -> f m (baseName defname) def
      _ -> throwError $ DefNotFound defname

-- A variant of 'editModuleOf' for actions which can affect multiple modules
editModuleOfCross ::
  MonadError ProgError m =>
  Maybe GVarName ->
  Prog ->
  ((Module, [Module]) -> Name -> ASTDef -> m ([Module], Maybe Selection)) ->
  m Prog
editModuleOfCross mdefName prog f = case mdefName of
  Nothing -> throwError NoDefSelected
  Just defname -> editModuleCross (qualifiedModule defname) prog $ \ms@(m, _) ->
    case Map.lookup (baseName defname) (moduleDefs m) of
      Just (DefAST def) -> f ms (baseName defname) def
      _ -> throwError $ DefNotFound defname

editModuleOfCrossType ::
  MonadError ProgError m =>
  Maybe TyConName ->
  Prog ->
  ((Module, [Module]) -> Name -> ASTTypeDef TypeMeta -> m ([Module], Maybe Selection)) ->
  m Prog
editModuleOfCrossType mdefName prog f = case mdefName of
  Nothing -> throwError NoTypeDefSelected
  Just defname -> editModuleCross (qualifiedModule defname) prog $ \ms@(m, _) ->
    case Map.lookup (baseName defname) (moduleTypes m) of
      Just (TypeDefAST def) -> f ms (baseName defname) def
      _ -> throwError $ TypeDefNotFound defname

-- | Undo the last block of actions.
--
-- If there are no actions in the log we return the program unchanged.
--
-- Otherwise, we undo by replaying the whole log from the start.
-- Because actions often refer to the IDs of nodes created by previous
-- actions we must reset the ID and name counter to their original
-- state before we replay. We do this by resetting the entire app
-- state.
--
-- If the replay is successful, then we return the new program and
-- push the block of actions that were undone onto the redo log.
--
-- If the replay is unsuccessful, then we throw a 'ProgError' and
-- leave it to the caller to decide how to handle it.
handleUndoRequest :: MonadEditApp l ProgError m => m Prog
handleUndoRequest = do
  prog <- gets appProg
  start <- gets appInit
  case unlog (progLog prog) of
    [] -> pure prog
    (a : as) -> do
      runEditAppM (replay (reverse as)) start >>= \case
        (Right _, app') -> do
          put $ app' & #currentState % #prog % #redoLog .~ push a (redoLog prog)
          gets appProg
        (Left err, _) -> throwError err

-- | Redo the last undo by replaying it from the current program
-- state.
--
-- If the replay is successful, then we return the new program and
-- pop the last undo off the redo log.
--
-- If the replay is unsuccessful, then we throw a 'ProgError' and
-- leave it to the caller to decide how to handle it.
--
-- If the redo log is empty, return the program unchanged.
handleRedoRequest :: (MonadEditApp l ProgError m) => m Prog
handleRedoRequest = do
  prog <- gets appProg
  app <- get
  case pop (redoLog prog) of
    Nothing -> pure prog
    Just (a, redoLog') -> do
      runEditAppM (handleEditRequest a) app >>= \case
        (Right _, app') -> do
          put $ app' & #currentState % #prog % #redoLog .~ redoLog'
          gets appProg
        (Left err, _) -> throwError err

-- Replay a series of actions, updating the app state with the new program
replay :: MonadEditApp l ProgError m => [[ProgAction]] -> m ()
replay = mapM_ handleEditRequest

-- | A shorthand for the constraints we need when performing mutation
-- operations on the application.
--
-- Note we do not want @MonadFresh Name m@, as @fresh :: m Name@ has
-- no way of avoiding student-specified names. Instead, use 'freshName'.
type MonadEditApp l e m = (MonadLog (WithSeverity l) m, MonadEdit m e, MonadState App m)

-- | A shorthand for constraints needed when doing low-level mutation
-- operations which do not themselves update the 'App' contained in a
-- 'State' monad. (Typically interaction with the @State@ monad would
-- be handled by a caller.
type MonadEdit m e = (MonadFresh ID m, MonadFresh NameCounter m, MonadError e m)

-- | A shorthand for the constraints we need when performing read-only
-- operations on the application.
type MonadQueryApp m = (Monad m, MonadReader App m, MonadError ProgError m)

-- | The 'EditApp' monad.
--
-- Actions run in this monad can modify the 'App'. 'ExceptT' wraps
-- state so that an action that throws an error does not modify the
-- state. This is important to ensure that we can reliably replay the
-- log without having ID mismatches.
newtype EditAppM m e a = EditAppM (StateT App (ExceptT e m) a)
  deriving newtype (Functor, Applicative, Monad, MonadState App, MonadError e, MonadLog l)

-- | Run an 'EditAppM' action, returning a result and an updated
-- 'App'.
runEditAppM :: Functor m => EditAppM m e a -> App -> m (Either e a, App)
runEditAppM (EditAppM m) appState =
  runExceptT (runStateT m appState) <&> \case
    Left err -> (Left err, appState)
    Right (res, appState') -> (Right res, appState')

-- | The 'QueryApp' monad.
--
-- Actions run in this monad cannot modify the 'App'. We use 'ExceptT'
-- here for compatibility with 'EditApp'.
newtype QueryAppM a = QueryAppM (ReaderT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader App, MonadError ProgError)

-- | Run a 'QueryAppM' action, returning a result.
runQueryAppM :: QueryAppM a -> App -> Either ProgError a
runQueryAppM (QueryAppM m) appState = case runExcept (runReaderT m appState) of
  Left err -> Left err
  Right res -> Right res

-- | The student's application's state.
--
-- Building an 'App' can be tricky, so we don't export the
-- constructor. See 'mkApp' and 'mkAppSafe'.
data App = App
  { currentState :: AppState
  , initialState :: AppState
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON App
  deriving anyclass (NFData)

-- Internal app state. Note that this type is not exported, as we want
-- to guarantee that the counters are kept in sync with the 'Prog',
-- and this should only be done via the 'MonadFresh' instances in this
-- module.
data AppState = AppState
  { idCounter :: ID
  , nameCounter :: NameCounter
  , prog :: Prog
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON AppState
  deriving anyclass (NFData)

-- | Construct an 'App' from an 'ID' and a 'Prog'.
--
-- Unless you are building a simple 'App' from a 'Prog' with just one
-- module and you happen to already have the next valid 'ID' for that
-- module handy, you should probably use 'mkAppSafe' rather than this
-- function, as 'mkAppSafe' will always do the right thing for more
-- complicated 'Prog's and doesn't expose (as many) 'App'
-- implementation details to the caller. 'mkApp' is chiefly provided
-- for very simple initial programs and for testing purposes.
--
-- The value of the provided 'ID' should be at least one greater than
-- the largest 'ID' in any of the provided 'Prog''s 'progModules'.
-- (See 'nextProgID'.) The 'App' uses this initial 'ID' value to
-- guarantee that newly-created nodes in the program's AST are unique
-- across all editable modules in the 'Prog'. *Note*: 'mkApp' does not
-- enforce or otherwise check that this invariant holds! It is the
-- responsiblity of the caller.
--
-- Also N.B: the requirement that the provided 'ID' value should be
-- greater than the largest 'ID' is an implementation detail, and may
-- change in the future.
--
-- (Strictly speaking, the invariant on the provided 'ID' is
-- overconstrained, as the rest of our implementation depends only on
-- 'ID's being unique *per module*, and not across all editable
-- modules in the 'Prog' as 'App' requires. However, keeping track of
-- a per-module 'ID' would be much less ergonomic, and in practice
-- there's no pressure on the range of 'ID' values, so we can afford
-- to be a bit profligate.)
--
-- A valid value for the provided 'NameCounter' will depend on what
-- names already exist in the provided program, and is rather
-- implementation-dependent at the moment. In most cases, it should be
-- safe to use @toEnum 0@ as the initial value. We will make selecting
-- this value more foolproof, or eliminate it altogether, in the
-- future. See:
--
-- https://github.com/hackworthltd/primer/issues/510
mkApp :: ID -> NameCounter -> Prog -> App
mkApp i n p =
  let s = AppState i n p
   in App s s

-- | A safe(r) version of 'mkApp'. It will only return an 'App' if the
-- provided 'Prog' is well-formed per 'checkAppWellFormed'; otherwise,
-- it returns a 'ProgError'.
--
-- Regarding the provided 'NameCounter', see the corresponding
-- documentation for 'mkApp'.
mkAppSafe :: NameCounter -> Prog -> Either ProgError App
mkAppSafe n p =
  let nextID = nextProgID p
   in checkAppWellFormed (mkApp nextID n p)

-- | Given an 'App', return the next 'ID' that should be used to
-- create a new node.
appIdCounter :: App -> ID
appIdCounter = idCounter . currentState

-- | Given an 'App', return its 'NameCounter'.
appNameCounter :: App -> NameCounter
appNameCounter = nameCounter . currentState

-- | Given an 'App', return its 'Prog'.
appProg :: App -> Prog
appProg = prog . currentState

-- | Given an 'App', return its initial state.
appInit :: App -> App
appInit a =
  let s = initialState a
   in App s s

-- | An initial app whose program is completely empty.
newEmptyApp :: App
newEmptyApp =
  let (p, id_, nc) = newEmptyProgImporting []
   in mkApp id_ nc p

-- | An initial app whose program includes some useful definitions.
newApp :: App
newApp =
  let (p, id_, nc) = newProg
   in mkApp id_ nc p

-- | Ensure the provided 'App' is well-formed.
--
-- Currently, "well-formed" means that the 'App''s 'Prog' typechecks,
-- including both its imported modules and its editable modules. Later
-- versions of this function may perform additional checks, as well.
--
-- In the event that the 'App' is well-formed, then the 'App' that is
-- returned is identical to the one that was provided, except that the
-- modules in its 'Prog' are guaranteed to have up-to-date cached type
-- information.
--
-- If the 'App' is not well-formed, then 'checkAppWellFormed' returns
-- a 'ProgError'.
checkAppWellFormed :: App -> Either ProgError App
checkAppWellFormed app =
  -- Ideally, we would do an additional check here to
  -- ensure that the next name generated by the
  -- 'NameCounter' won't conflict with an existing name.
  -- However, there are bigger issues with our current
  -- automatic name generation scheme which make that
  -- check rather pointless. See:
  --
  -- https://github.com/hackworthltd/primer/issues/510
  runTC app $ traverseOf (#currentState % #prog) (liftError ActionError . checkProgWellFormed) app

newtype M e a = M {unM :: StateT (ID, NameCounter) (Except e) a}
  deriving newtype (Functor, Applicative, Monad, MonadError e)
instance MonadFresh ID (M e) where
  fresh = M $ _1 <<%= succ
instance MonadFresh NameCounter (M e) where
  fresh = M $ _2 <<%= succ
runTC :: App -> M e a -> Either e a
runTC a = runExcept . flip evalStateT (appIdCounter a, appNameCounter a) . unM

checkProgWellFormed ::
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e (ReaderT Cxt m)
  ) =>
  Prog ->
  m Prog
checkProgWellFormed p =
  let
    -- We are careful to turn smartholes off for this check, as
    -- we want to return an error if there is a problem, rather
    -- than try to correct it.
    p' = p{progSmartHoles = NoSmartHoles}
   in
    do
      checkedProg <- tcWholeProgWithImports p'
      -- Ideally, we would do an additional check here to
      -- ensure that the 'ID' is unique across all modules.
      pure $ checkedProg{progSmartHoles = progSmartHoles p}

-- | Construct a new, empty expression
newExpr :: MonadFresh ID m => m Expr
newExpr = do
  id_ <- fresh
  pure $ EmptyHole (Meta id_ Nothing Nothing)

-- | Construct a new, empty type
newType :: MonadFresh ID m => m Type
newType = do
  id_ <- fresh
  pure $ TEmptyHole (Meta id_ Nothing Nothing)

-- | Support for generating fresh IDs
instance Monad m => MonadFresh ID (EditAppM m e) where
  fresh = do
    id_ <- gets appIdCounter
    modify (\s -> s & #currentState % #idCounter .~ id_ + 1)
    pure id_

-- | Support for generating names. Basically just a counter so we don't
-- generate the same automatic name twice.
instance Monad m => MonadFresh NameCounter (EditAppM m e) where
  fresh = do
    nc <- gets appNameCounter
    modify (\s -> s & #currentState % #nameCounter .~ succ nc)
    pure nc

copyPasteSig :: MonadEdit m ProgError => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteSig p (fromDefName, fromTyId) toDefName setup = do
  c' <- focusNodeImports p fromDefName fromTyId
  c <- case c' of
    Left (Left _) -> throwError $ CopyPasteError "tried to copy-paste an expression into a signature"
    Left (Right zt) -> pure $ Left zt
    Right zt -> pure $ Right zt
  let smartHoles = progSmartHoles p
  finalProg <- editModuleOf (Just toDefName) p $ \mod toDefBaseName oldDef -> do
    let otherModules = filter ((/= moduleName mod) . moduleName) (progModules p)
    -- We intentionally throw away any changes in doneSetup other than via 'tgt'
    -- as these could be in other definitions referencing this one, due to
    -- types changing. However, we are going to do a full tc pass anyway,
    -- which will pick up any problems. It is better to do it in one batch,
    -- in case the intermediate state after 'setup' causes more problems
    -- than the final state does.
    doneSetup <- applyActionsToTypeSig smartHoles (progImports p) (mod, otherModules) (toDefBaseName, oldDef) setup
    tgt <- case doneSetup of
      Left err -> throwError $ ActionError err
      Right (_, tgt) -> pure $ focusOnlyType tgt
    let sharedScope =
          if fromDefName == toDefName
            then getSharedScopeTy c $ Right tgt
            else mempty
    -- Delete unbound vars
    let cTgt = either target target c
        f (m, n) =
          if Set.member (unLocalName n) sharedScope
            then pure $ TVar m n
            else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
    cScoped <- traverseOf _freeVarsTy f cTgt
    freshCopy <- regenerateTypeIDs cScoped
    pasted <- case target tgt of
      TEmptyHole _ -> pure $ replace freshCopy tgt
      _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
    let newDef = oldDef{astDefType = fromZipper pasted}
    let newSel = NodeSelection SigNode (pasted ^. _target % _typeMetaLens % re _Right)
    pure (insertDef mod toDefBaseName (DefAST newDef), Just (SelectionDef $ DefSelection toDefName $ Just newSel))
  liftError ActionError $ tcWholeProg finalProg

-- We cannot use bindersAbove as that works on names only, and different scopes
-- may reuse the same names. However, we want to detect that as non-shared.
-- Instead, we rely on fact that IDs are unique.
-- NB: we assume that both arguments belong in the same definition (and thus
-- only require that IDs are unique within one definition -- this is a narrower
-- uniqueness assumption than we currently enforce, see Note [Modules]).
getSharedScopeTy :: Either TypeZ TypeZip -> Either TypeZ TypeZip -> Set.Set Name
getSharedScopeTy = getSharedScope' bindersLocAbove
  where
    bindersLocAbove = either bindersLocAboveTypeZ bindersLocAboveTy
    bindersLocAboveTy :: TypeZip -> Set.Set (ID, Name)
    bindersLocAboveTy = foldAbove (\t -> Set.map ((getID $ current t,) . unLocalName) $ getBoundHereUpTy t)
    bindersLocAboveTypeZ :: TypeZ -> Set.Set (ID, Name)
    bindersLocAboveTypeZ =
      foldAboveTypeZ
        (\t -> Set.map ((getID $ current t,) . unLocalName) $ getBoundHereUpTy t)
        ((\e -> Set.map (getID e,) $ getBoundHere e Nothing) . current)
        (\x -> Set.map (getID $ current x,) $ getBoundHereUp x)

-- TODO: there is a lot of duplicated code for copy/paste, often due to types/terms being different...
getSharedScope :: ExprZ -> ExprZ -> Set.Set Name
getSharedScope = getSharedScope' bindersLocAbove
  where
    bindersLocAbove :: ExprZ -> Set.Set (ID, Name)
    bindersLocAbove = foldAbove (\x -> Set.map (getID $ current x,) $ getBoundHereUp x)

getSharedScope' :: (a -> Set.Set (ID, Name)) -> a -> a -> Set.Set Name
getSharedScope' bindersLocAbove l r =
  let lBinds = bindersLocAbove l
      rBinds = bindersLocAbove r
      common = names $ Set.intersection lBinds rBinds
      onlyLeft = names $ lBinds Set.\\ rBinds
      onlyRight = names $ rBinds Set.\\ lBinds
   in -- NB: 'names' is not injective, thus there can be elements in
      -- both 'common' and 'onlyLeft'/'onlyRight'
      -- We must filter these out since any reference to such a name
      -- would either escape its scope (left) or be captured (right)
      (common Set.\\ onlyLeft) Set.\\ onlyRight
  where
    names :: Set.Set (ID, Name) -> Set.Set Name
    names = Set.map snd

-- | Checks every term and type definition in the editable modules.
-- Does not check imported modules.
tcWholeProg ::
  forall m e.
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e (ReaderT Cxt m)
  ) =>
  Prog ->
  m Prog
tcWholeProg p = do
  mods' <-
    checkEverything
      (progSmartHoles p)
      CheckEverything
        { trusted = progImports p
        , toCheck = progModules p
        }
  let p' = p{progModules = mods'}
  -- We need to update the metadata cached in the selection
  let oldSel = progSelection p
  newSel <- case oldSel of
    Nothing -> pure Nothing
    Just (SelectionDef s) -> do
      let defName_ = s.def
      updatedNode <- case s.node of
        Nothing -> pure Nothing
        Just sel@NodeSelection{nodeType} -> do
          n <- runExceptT $ focusNode p' defName_ $ getID sel
          case (nodeType, n) of
            (BodyNode, Right (Left x)) -> pure $ Just $ NodeSelection BodyNode $ bimap (view _exprMetaLens . target) (view _typeMetaLens . target) x
            (SigNode, Right (Right x)) -> pure $ Just $ NodeSelection SigNode $ x ^. _target % _typeMetaLens % re _Right
            _ -> pure Nothing -- something's gone wrong: expected a SigNode, but found it in the body, or vv, or just not found it
      pure $
        Just . SelectionDef $
          DefSelection
            { def = defName_
            , node = updatedNode
            }
    Just (SelectionTypeDef s) ->
      pure . Just . SelectionTypeDef $
        s & over (#node % mapped % #_TypeDefConsNodeSelection) \conSel ->
          conSel & over #field \case
            Nothing -> Nothing
            Just fieldSel ->
              flip (set #meta) fieldSel . (Right . (^. _typeMetaLens)) <$> do
                -- If something goes wrong in finding the metadata, we just don't set a field selection.
                -- This is similar to what we do when selection is in a term, above.
                td <- Map.lookup s.def $ allTypesMeta p
                tda <- typeDefAST td
                getTypeDefConFieldType tda conSel.con fieldSel.index
  pure $ p'{progSelection = newSel}

-- | Do a full check of a 'Prog', both the imports and the local modules
tcWholeProgWithImports ::
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadNestedError TypeError e (ReaderT Cxt m)
  ) =>
  Prog ->
  m Prog
tcWholeProgWithImports p = do
  imports <- checkEverything (progSmartHoles p) CheckEverything{trusted = mempty, toCheck = progImports p}
  tcWholeProg $ p & #progImports .~ imports

copyPasteBody :: MonadEdit m ProgError => Prog -> (GVarName, ID) -> GVarName -> [Action] -> m Prog
copyPasteBody p (fromDefName, fromId) toDefName setup = do
  src' <- focusNodeImports p fromDefName fromId
  -- reassociate so get Expr+(Type+Type), rather than (Expr+Type)+Type
  let src = case src' of
        Left (Left e) -> Left e
        Left (Right t) -> Right (Left t)
        Right t -> Right (Right t)
  finalProg <- editModuleOf (Just toDefName) p $ \mod toDefBaseName oldDef -> do
    -- We manually use the low-level applyAction', as we do not want to
    -- typecheck intermediate states. There are two reasons for this, both
    -- exemplified by "raising" the type @Bool -> ?@ in
    -- @(λx. case x of {True -> t; False -> ?}) : Bool -> (Bool -> ?)@.
    -- This is implemented via a copy-paste of that subterm (@Bool -> ?@) with a
    -- @setup@ that deletes its parent @Bool -> (Bool -> ?)@.
    -- Note that this expression after @setup@ is
    -- @(λx. case x of {True -> t; False -> ?}) : ?@.
    --
    -- Firstly, if we check without smartholes, then this intermediate
    -- expression is ill-typed: since @x : ?@, we require the case to have
    -- exactly zero branches, which it does not.
    --
    -- Secondly, if we check with smartholes, then we will fix up the
    -- intermediate expression by deleting the branches. Then when we paste we
    -- shall get
    -- @(λx. case x of {}) : Bool -> ?@, which is ill-typed for similar reasons.
    -- Smartholes will again fix it, giving
    -- @(λx. case x of {True -> ?; False -> ?}) : Bool -> ?@.
    -- Note that we have destroyed @t@, the contents of the branch, where it
    -- would be preferable to use smartholes to modify @t@ to ensure
    -- well-typed-ness.
    tgt <-
      foldlM (\l a -> liftError ActionError $ applyAction' a l) (focusLoc (astDefExpr oldDef)) setup
        -- SH not important here, cxt only used to lookup global vars and ctors
        & flip runReaderT (buildTypingContextFromModules (progAllModules p) NoSmartHoles)
    case (src, tgt) of
      (_, InBind _) -> throwError $ CopyPasteError "tried to paste an expression into a binder"
      (Left _, InType _) -> throwError $ CopyPasteError "tried to paste an expression into a type"
      (Right _, InExpr _) -> throwError $ CopyPasteError "tried to paste a type into an expression"
      (Right srcT, InType tgtT) -> do
        let sharedScope =
              if fromDefName == toDefName
                then getSharedScopeTy srcT $ Left tgtT
                else mempty
        -- Delete unbound vars. TODO: we may want to let-bind them?
        let srcSubtree = either target target srcT
            f (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ TVar m n
                else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
        scopedCopy <- traverseOf _freeVarsTy f srcSubtree
        freshCopy <- regenerateTypeIDs scopedCopy
        pasted <- case target tgtT of
          TEmptyHole _ -> pure $ replace freshCopy tgtT
          _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
        let newDef = oldDef{astDefExpr = unfocusExpr $ unfocusType pasted}
        let newSel = NodeSelection BodyNode (pasted ^. _target % _typeMetaLens % re _Right)
        pure (insertDef mod toDefBaseName (DefAST newDef), Just (SelectionDef $ DefSelection toDefName $ Just newSel))
      (Left srcE, InExpr tgtE) -> do
        let sharedScope =
              if fromDefName == toDefName
                then getSharedScope srcE tgtE
                else mempty
        -- Delete unbound vars. TODO: we may want to let-bind them?
        let tm (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ Var m $ LocalVarRef n
                else fresh <&> \i -> EmptyHole (Meta i Nothing Nothing)
            ty (m, n) =
              if Set.member (unLocalName n) sharedScope
                then pure $ TVar m n
                else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
        scopedCopy <- traverseOf _freeTyVars ty =<< traverseOf _freeTmVars tm (target srcE)
        freshCopy <- regenerateExprIDs scopedCopy
        -- TODO: need to care about types and directions here (and write tests for this caring!)
        {-
        - Currently, with smart holes, nothing will go too wrong (i.e. no crashes/rejections happen), but if
        - smartholes were turned off (which currently needs changing in the source code, then things could go wrong, and the TC throws errors.
        - The cases we need to consider are (note that the metadata gives us what type each subtree was chk/syn (could be Nothing, due to our
        - represention, but we can consider that as a hole)
        - NB: as we always paste into a hole, it will always synth ?, but it may also have been checked against a concrete type
        - From    To    Want
        - e ∈ T   ∈ ?   e       if T is ? (or maybe don't special-case this, for consistency?)
        -               {? e ?} otherwise, to avoid "jumpy holes" (paste a 2∈Nat into '? True', would get '2 {? True ?}', but want '{? 2 ?} True', probably?
        - T ∋ t   ∈ ?   t : ?       if T is ? (or maybe don't special-case this, for consistency?)
        -               {? t : T ?} otherwise (avoid jumpy holes, as above)
        - e ∈ T   R ∋   e       if this would TC (i.e. T and R are consistent)
        -               {? e ?} otherwise
        - T ∋ t   R ∋   t           if this would TC (i.e. if T is more specific than R, I expect)
        -               {? t : T ?} otherwise
        -
        - Let's also tabulate what smartholes would give
        -    From    To    Want                   SH gives               Example ('raise' the term in >e<)
        -    e ∈ T   ∈ ?   e       if T is ?      e
        -!!!               {? e ?} otherwise      e, and jumpy holes.    (? : ? -> Bool -> ?) >Succ< True
        -    T ∋ t   ∈ ?   t : ?       if T is ?  t : ?                  ? >λx.?< ?
        -!!!               {? t : T ?} otherwise  t : ?                  (? : (Bool -> Bool) -> ?) >λx.?< ?
        -    e ∈ T   R ∋   e       if  would TC   e                      Bool ∋ not >not True< [using extra not so obv. syn, even if ctors are chk only]
        -                  {? e ?} otherwise      {? e ?}                Bool ∋ >isEven< ?
        -    T ∋ t   R ∋   t         if would TC  t                      Bool -> Bool ∋ (? : (Bool -> Bool) -> ?) >(λx.?)<
        -!!!               {? t : T ?} otherwise  {? t : ? ?}            Bool ∋ (? : (Bool -> Bool) -> ?) >(λx.?)<
        -
        - We could also consider what to do with embeddings: R ∋ e ∈ T: what happens for
        -     Bool ∋ even >(add 0 0)<   [use add so obv. syn, even if ctors are chk only]
        - ?
        -
        - so with SH, we are almost doing well, except we have a case of jumpy holes, and some cases of losing type information,
        - denoted by !!! above
        -}
        pasted <- case target tgtE of
          EmptyHole _ -> pure $ replace freshCopy tgtE
          _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
        let newDef = oldDef{astDefExpr = unfocusExpr pasted}
        let newSel = NodeSelection BodyNode (pasted ^. _target % _exprMetaLens % re _Left)
        pure (insertDef mod toDefBaseName (DefAST newDef), Just (SelectionDef $ DefSelection toDefName $ Just newSel))
  liftError ActionError $ tcWholeProg finalProg

lookupASTDef :: GVarName -> DefMap -> Maybe ASTDef
lookupASTDef name = defAST <=< Map.lookup name

alterTypeDef ::
  MonadError ProgError m =>
  (ASTTypeDef TypeMeta -> m (ASTTypeDef TypeMeta)) ->
  TyConName ->
  Module ->
  m Module
alterTypeDef f type_ m = do
  unless (qualifiedModule type_ == moduleName m) $ throwError $ TypeDefNotFound type_
  traverseOf
    #moduleTypes
    ( Map.alterF
        ( maybe
            (throwError $ TypeDefNotFound type_)
            ( maybe
                (throwError $ TypeDefIsPrim type_)
                (map (Just . TypeDefAST) . f)
                . typeDefAST
            )
        )
        (baseName type_)
    )
    m

-- | Apply a bottom-up transformation to all branches of case expressions on the given type.
transformCaseBranches ::
  MonadEdit m ProgError =>
  Prog ->
  TyConName ->
  (([CaseBranch], CaseFallback) -> m ([CaseBranch], CaseFallback)) ->
  Expr ->
  m Expr
transformCaseBranches prog type_ f = transformM $ \case
  Case m scrut bs fb -> do
    scrutType <-
      fst
        <$> runReaderT
          (liftError (ActionError . TypeError) $ synth scrut)
          (progCxt prog)
    (bs', fb') <-
      if fst (unfoldTApp scrutType) == TCon () type_
        then f (bs, fb)
        else pure (bs, fb)
    pure $ Case m scrut bs' fb'
  e -> pure e

-- | Apply a bottom-up transformation to all non-fallback branches of case
-- expressions on the given type, leaving any fallback branch untouched.
transformNamedCaseBranches ::
  MonadEdit m ProgError =>
  Prog ->
  TyConName ->
  ([CaseBranch] -> m [CaseBranch]) ->
  Expr ->
  m Expr
transformNamedCaseBranches prog type_ f = transformCaseBranches prog type_ (\(bs, fb) -> (,fb) <$> f bs)

-- | Apply a bottom-up transformation to non-fallback case branches matching the
-- given (type and) constructor.
transformNamedCaseBranch ::
  MonadEdit m ProgError =>
  Prog ->
  TyConName ->
  ValConName ->
  -- This only supports ADT case branches, since we cannot edit primitives
  (CaseBranch -> m CaseBranch) ->
  Expr ->
  m Expr
transformNamedCaseBranch prog type_ con f = transformNamedCaseBranches prog type_ $
  traverse $
    \cb -> if caseBranchName cb == PatCon con then f cb else pure cb

progCxt :: Prog -> Cxt
progCxt p = buildTypingContextFromModules (progAllModules p) (progSmartHoles p)

-- | Run a computation in some context whose errors can be promoted to `ProgError`.
liftError :: MonadError ProgError m => (e -> ProgError) -> ExceptT e m b -> m b
liftError = modifyError

allConNames :: Prog -> [(TyConName, [ValConName])]
allConNames p = ifoldMap (\tn td -> pure (tn, valConName <$> typeDefConstructors td)) $ allTypes p
  where
    typeDefConstructors td = maybe [] astTypeDefConstructors $ typeDefAST td

allValConNames :: Prog -> [ValConName]
allValConNames = snd <=< allConNames

allTyConNames :: Prog -> [TyConName]
allTyConNames = fmap fst . allConNames

-- | Given a 'Prog', return the next 'ID' that's safe to use when
-- editing it.
--
-- Note: do not rely on the implementation of this function, as it may
-- change in the future.
nextProgID :: Prog -> ID
nextProgID p = foldl' (\id_ m -> max (nextModuleID m) id_) minBound (progModules p)
