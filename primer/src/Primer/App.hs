{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- This module defines the high level application functions.

module Primer.App (
  Log (..),
  App (..),
  InitialApp (..),
  initialApp,
  newProg,
  newEmptyProg,
  newApp,
  newEmptyApp,
  EditAppM,
  QueryAppM,
  runEditAppM,
  runQueryAppM,
  Prog (..),
  tcWholeProg,
  ProgAction (..),
  ProgError (..),
  Result (..),
  Question (..),
  handleQuestion,
  handleGetProgramRequest,
  handleMutationRequest,
  handleEditRequest,
  handleEvalRequest,
  handleEvalFullRequest,
  MutationRequest (..),
  Selection (..),
  NodeSelection (..),
  NodeType (..),
  EvalReq (..),
  EvalResp (..),
  EvalFullReq (..),
  EvalFullResp (..),
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (
  ToJSON (toEncoding),
  defaultOptions,
  genericToEncoding,
 )
import Data.Bitraversable (bimapM)
import Data.Data (Data)
import Data.Generics.Product (position)
import Data.Generics.Uniplate.Zipper (
  fromZipper,
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics (re, traverseOf, view, (%), (.~), (^.), _Left, _Right)
import Primer.Action (
  Action,
  ActionError (IDNotFound),
  applyActionsToBody,
  applyActionsToTypeSig,
 )
import Primer.Core (
  Def (..),
  Expr,
  Expr' (EmptyHole, Var),
  ExprMeta,
  ID (..),
  Kind,
  Meta (..),
  Type,
  Type' (TEmptyHole, TVar),
  TypeDef,
  TypeMeta,
  defaultTypeDefs,
  getID,
  _exprMeta,
  _exprMetaLens,
  _exprTypeMeta,
  _id,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.Eval (EvalDetail, EvalError)
import qualified Primer.Eval as Eval
import Primer.EvalFull (Dir, EvalFullError (TimedOut), TerminationBound, evalFull)
import Primer.JSON
import Primer.Name (Name, NameCounter, freshName, unsafeMkName)
import Primer.Questions (
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.Subst (_freeTmVars, _freeTyVars, _freeVarsTy)
import Primer.Typecheck (
  Cxt,
  SmartHoles (NoSmartHoles, SmartHoles),
  TypeError,
  buildTypingContext,
  checkDef,
  checkEverything,
  checkTypeDefs,
  mkTypeDefMap,
 )
import Primer.Zipper (
  ExprZ,
  Loc (InBind, InExpr, InType),
  TypeZ,
  TypeZip,
  bindersAbove,
  bindersAboveTy,
  bindersAboveTypeZ,
  current,
  focus,
  focusOn,
  focusOnTy,
  focusOnlyType,
  foldAbove,
  getBoundHere,
  getBoundHereTy,
  locToEither,
  replace,
  target,
  unfocusExpr,
  unfocusType,
  up,
  _target,
 )

-- | The program state, shared between the frontend and backend
--  This is much more info than we need to send - for example we probably don't
--  need to send the log back and forth.
--  But for now, to keep things simple, that's how it works.
data Prog = Prog
  { progTypes :: [TypeDef]
  , progDefs :: Map ID Def -- The current program: a set of definitions indexed by ID
  , progSelection :: Maybe Selection
  , progSmartHoles :: SmartHoles
  , progLog :: Log -- The log of all actions
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Prog

-- | The action log
--  This is the canonical store of the program - we can recreate any current or
--  past program state by replaying this log.
--  Each item is a sequence of Core actions which should be applied atomically.
--  Items are stored in reverse order so it's quick to add new ones.
newtype Log = Log {unlog :: [[ProgAction]]}
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Log

-- | Describes what interface element the user has selected.
-- A definition in the left hand nav bar, and possibly a node in that definition.
data Selection = Selection
  { selectedDef :: Def
  , selectedNode :: Maybe NodeSelection
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON Selection

-- | A selected node, in the body or type signature of some definition.
-- We have the following invariant: @nodeType = SigNode ==> isRight meta@
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , nodeId :: ID
  , meta :: Either ExprMeta TypeMeta
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeSelection

data NodeType = BodyNode | SigNode
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via VJSON NodeType

-- | The API result type
-- This represents the type of all API results.
-- It's just Either with a different name, because we need to define our own
-- Encode/Decode instances on the frontend.
data Result a b
  = Error a
  | Success b
  deriving (Eq, Show, Generic, Data)
  deriving (FromJSON, ToJSON) via VJSON (Result a b)

-- | The type of requests which can mutate the application state.
--
-- Note that `Reset` is not undo-able, as it wipes the log along with
-- all other program state, IDs, etc.
data MutationRequest
  = Undo
  | Reset
  | Edit [ProgAction]
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON MutationRequest

-- | The type of questions which return information about the program, but do not
-- modify it.
data Question a where
  -- Given the ID of a definition and the ID of a type or expression in that
  -- definition, what variables are in scope at the expression?
  -- Nested pairs: to make serialisation to PS work easily
  VariablesInScope :: ID -> ID -> Question (([(Name, Kind)], [(Name, Type' ())]), [(ID, Name, Type' ())])
  GenerateName ::
    ID ->
    ID ->
    Either (Maybe (Type' ())) (Maybe Kind) ->
    Question [Name]

-- | High level actions
-- These actions move around the whole program or modify definitions
data ProgAction
  = -- | Move the cursor to the definition with the given ID
    MoveToDef ID
  | -- | Rename the definition with the given ID
    RenameDef ID Text
  | -- | Create a new definition
    CreateDef (Maybe Text)
  | -- | Delete a new definition
    DeleteDef ID
  | -- | Add a new type definition
    AddTypeDef TypeDef
  | -- | Execute a sequence of actions on the body of the definition
    BodyAction [Action]
  | -- | Execute a sequence of actions on the type annotation of the definition
    SigAction [Action]
  | SetSmartHoles SmartHoles
  | -- | CopyPaste (d,i) as
    --   remembers the tree in def d, node i
    --   runs actions as (in the currently selected def), which should end up in a hole
    --   and then tries to paste the remembered subtree
    --   This rather complex setup enables encoding 'raise' operations,
    --     f s ~> f
    --   where we remember f, then delete f s, then paste f back
    --   as well as allowing cross-definition copy+paste
    --   whilst letting the backend avoid remembering the 'copied' thing in some state.
    --   The cursor is left on the root of the inserted subtree, which may or may not be inside a hole and/or annotation.
    --   At the start of the actions, the cursor starts at the root of the definition's type/expression
    CopyPasteSig (ID, ID) [Action]
  | CopyPasteBody (ID, ID) [Action]
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ProgAction

data ProgError
  = NoDefSelected
  | DefNotFound ID
  | DefAlreadyExists Name ID
  | DefInUse ID
  | ActionError ActionError
  | EvalError EvalError
  | -- | Currently copy/paste is only exposed in the frontend via select
    --   channels, which should never go wrong. Consequently, this is an
    --   "internal error" which should never happen!
    --   If/when we expose it more broadly, we should refactor this to contain
    --   a descriptive ADT, rather than a string.
    CopyPasteError Text
  | -- | Currently one can only add a typedef by a form in the frontend,
    --   which does its own error checking. Thus this is an "internal error"
    --   that should never happen!
    --   (However, this is not entirely true currently, see
    --    https://github.com/hackworthltd/primer/issues/3)
    TypeDefError Text
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ProgError

data EvalReq = EvalReq
  { evalReqExpr :: Expr
  , evalReqRedex :: ID
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalReq

data EvalResp = EvalResp
  { evalRespExpr :: Expr
  , evalRespRedexes :: [ID]
  , evalRespDetail :: EvalDetail
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalResp

data EvalFullReq = EvalFullReq
  { evalFullReqExpr :: Expr
  , evalFullCxtDir :: Dir -- is this expression in a syn/chk context, so we can tell if is an embedding.
  , evalFullMaxSteps :: TerminationBound
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalFullReq

-- If we time out, we still return however far we got
data EvalFullResp
  = EvalFullRespTimedOut Expr
  | EvalFullRespNormal Expr
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalFullResp

-- * Request handlers

-- | Handle a question
handleQuestion :: MonadQueryApp m => Question a -> m a
handleQuestion = \case
  VariablesInScope defid exprid -> do
    node <- focusNode' defid exprid
    progDefs <- asks $ progDefs . appProg
    let (tyvars, termvars, globals) = case node of
          Left zE -> variablesInScopeExpr progDefs zE
          Right zT -> (variablesInScopeTy zT, [], [])
    pure ((tyvars, termvars), globals)
  GenerateName defid nodeid typeKind -> do
    progTypeDefs <- asks $ progTypes . appProg
    progDefs <- asks $ progDefs . appProg
    names <-
      focusNode' defid nodeid <&> \case
        Left zE -> generateNameExpr typeKind zE
        Right zT -> generateNameTy typeKind zT
    -- The choice of SmartHoles is irrelevant here
    pure $ runReader names $ buildTypingContext progTypeDefs progDefs SmartHoles
  where
    focusNode' defid nodeid = do
      prog <- asks appProg
      focusNode prog defid nodeid

focusNode :: MonadError ProgError m => Prog -> ID -> ID -> m (Either (Either ExprZ TypeZ) TypeZip)
focusNode prog defid nodeid =
  case Map.lookup defid (progDefs prog) of
    Nothing -> throwError $ DefNotFound defid
    Just def ->
      let mzE = locToEither <$> focusOn nodeid (focus $ defExpr def)
          mzT = focusOnTy nodeid $ focus $ defType def
       in case fmap Left mzE <|> fmap Right mzT of
            Nothing -> throwError $ ActionError (IDNotFound nodeid)
            Just x -> pure x

-- | Handle a request to retrieve the current program
handleGetProgramRequest :: MonadQueryApp m => m Prog
handleGetProgramRequest = asks appProg

-- | Handle a request to mutate the app state
handleMutationRequest :: MonadEditApp m => MutationRequest -> m Prog
handleMutationRequest = \case
  Edit as -> handleEditRequest as
  Undo -> handleUndoRequest
  Reset -> handleResetRequest

-- | Handle an edit request
handleEditRequest :: forall m. MonadEditApp m => [ProgAction] -> m Prog
handleEditRequest actions = do
  (prog, _) <- gets appProg >>= \p -> foldM go (p, Nothing) actions
  let Log l = progLog prog
  let prog' = prog{progLog = Log (actions : l)}
  modify (\s -> s{appProg = prog'})
  pure prog'
  where
    go :: (Prog, Maybe ID) -> ProgAction -> m (Prog, Maybe ID)
    go (prog, mdef) = applyProgAction prog mdef

-- | Handle an eval request
handleEvalRequest :: MonadEditApp m => EvalReq -> m EvalResp
handleEvalRequest req = do
  prog <- gets appProg
  result <- Eval.step (progDefs prog) (evalReqExpr req) (evalReqRedex req)
  case result of
    Left err -> throwError $ EvalError err
    Right (expr, detail) ->
      pure
        EvalResp
          { evalRespExpr = expr
          , evalRespRedexes = Set.toList $ Eval.redexes expr
          , evalRespDetail = detail
          }

-- | Handle an eval-to-normal-form request
handleEvalFullRequest :: MonadEditApp m => EvalFullReq -> m EvalFullResp
handleEvalFullRequest (EvalFullReq{evalFullReqExpr, evalFullCxtDir, evalFullMaxSteps}) = do
  prog <- gets appProg
  result <- evalFull (mkTypeDefMap $ progTypes prog) (progDefs prog) evalFullMaxSteps evalFullCxtDir evalFullReqExpr
  pure $ case result of
    Left (TimedOut e) -> EvalFullRespTimedOut e
    Right nf -> EvalFullRespNormal nf

applyProgAction :: MonadEditApp m => Prog -> Maybe ID -> ProgAction -> m (Prog, Maybe ID)
applyProgAction prog mdefID = \case
  MoveToDef id_ -> case Map.lookup id_ (progDefs prog) of
    Nothing -> throwError $ DefNotFound id_
    Just _ -> pure (prog, Just id_)
  DeleteDef id_
    | Map.member id_ (progDefs prog) -> do
      let defs = Map.delete id_ (progDefs prog)
          prog' = prog{progDefs = defs, progSelection = Nothing}
      -- Run a full TC solely to ensure that no references to the removed id
      -- remain. This is rather inefficient and could be improved in the
      -- future.
      runExceptT (checkEverything @TypeError NoSmartHoles (progTypes prog) (progDefs prog')) >>= \case
        Left _ -> throwError $ DefInUse id_
        Right _ -> pure ()
      pure (prog', Nothing)
  DeleteDef id_ -> throwError $ DefNotFound id_
  RenameDef id_ nameStr -> case Map.lookup id_ (progDefs prog) of
    Nothing -> throwError $ DefNotFound id_
    Just def -> do
      let name = unsafeMkName nameStr
      let existingName = Map.lookupMin $ Map.filter ((== name) . defName) $ progDefs prog
      case existingName of
        Just (existingID, _) -> throwError $ DefAlreadyExists name existingID
        Nothing -> do
          let def' = def{defName = name}
          let defs = Map.insert (defID def') def' (progDefs prog)
          pure (prog{progDefs = defs, progSelection = Just $ Selection def' Nothing}, mdefID)
  CreateDef n -> do
    name <- case n of
      Just nameStr ->
        let name = unsafeMkName nameStr
            existingName = Map.lookupMin $ Map.filter ((== name) . defName) $ progDefs prog
         in case existingName of
              Just (existingID, _) -> throwError $ DefAlreadyExists name existingID
              Nothing -> pure name
      Nothing -> freshName $ Set.fromList $ map defName $ Map.elems $ progDefs prog
    id_ <- fresh
    expr <- newExpr
    ty <- newType
    let def = Def id_ name expr ty
        defs = Map.insert id_ def (progDefs prog)
    pure (prog{progDefs = defs, progSelection = Just $ Selection def Nothing}, Just id_)
  AddTypeDef td -> do
    runExceptT @TypeError (checkTypeDefs $ progTypes prog <> [td]) >>= \case
      -- The frontend should never let this error case happen,
      -- so we just dump out a raw string for debugging/logging purposes
      -- (This is not currently true! We should synchronise the frontend with
      -- the typechecker rules. For instance, the form allows to create
      --   data T (T : *) = T
      -- but the TC rejects it.
      -- see https://github.com/hackworthltd/primer/issues/3)
      Left err -> throwError $ TypeDefError $ show err
      Right _ -> pure (prog{progTypes = progTypes prog <> [td]}, mdefID)
  BodyAction actions -> do
    withDef mdefID prog $ \def -> do
      smartHoles <- gets $ progSmartHoles . appProg
      res <- applyActionsToBody smartHoles (progTypes prog) (progDefs prog) def actions
      case res of
        Left err -> throwError $ ActionError err
        Right (def', z) -> do
          let defs = Map.insert (defID def) def' (progDefs prog)
              meta = bimap (view (position @1) . target) (view _typeMetaLens . target) $ locToEither z
              nodeId = either getID getID meta
          let prog' =
                prog
                  { progDefs = defs
                  , progSelection =
                      Just $
                        Selection def' $
                          Just
                            NodeSelection
                              { nodeType = BodyNode
                              , nodeId
                              , meta
                              }
                  }
          pure (prog', mdefID)
  SigAction actions -> do
    withDef mdefID prog $ \def -> do
      smartHoles <- gets $ progSmartHoles . appProg
      res <- applyActionsToTypeSig smartHoles (progTypes prog) (progDefs prog) def actions
      case res of
        Left err -> throwError $ ActionError err
        Right (def', defs, zt) -> do
          let node = target zt
              meta = view _typeMetaLens node
              nodeId = getID meta
              prog' =
                prog
                  { progDefs = defs
                  , progSelection =
                      Just $
                        Selection def' $
                          Just
                            NodeSelection
                              { nodeType = SigNode
                              , nodeId
                              , meta = Right meta
                              }
                  }
           in pure (prog', mdefID)
  SetSmartHoles smartHoles ->
    pure
      ( prog & #progSmartHoles .~ smartHoles
      , mdefID
      )
  CopyPasteSig fromIds setup -> case mdefID of
    Nothing -> throwError NoDefSelected
    Just i -> (,mdefID) <$> copyPasteSig prog fromIds i setup
  CopyPasteBody fromIds setup -> case mdefID of
    Nothing -> throwError NoDefSelected
    Just i -> (,mdefID) <$> copyPasteBody prog fromIds i setup

-- Look up the definition by its given ID, then run the given action with it
withDef :: MonadEditApp m => Maybe ID -> Prog -> (Def -> m a) -> m a
withDef mdefID prog f =
  case mdefID of
    Nothing -> throwError NoDefSelected
    Just defid -> do
      case Map.lookup defid (progDefs prog) of
        Nothing -> throwError $ DefNotFound defid
        Just def -> f def

-- | Undo the last block of actions.
-- If there are no actions in the log we return the program unchanged.
-- We undo by replaying the whole log from the start.
-- Because actions often refer to the IDs of nodes created by previous actions
-- we must reset the ID and name counter to their original state before we
-- replay. We do this by resetting the entire app state.
handleUndoRequest :: MonadEditApp m => m Prog
handleUndoRequest = do
  prog <- gets appProg
  start <- gets (initialApp . appInit)
  case unlog (progLog prog) of
    [] -> pure prog
    (_ : as) -> do
      case runEditAppM (replay (reverse as)) start of
        (Success _, app') -> do
          put app'
          gets appProg
        (Error err, _) -> throwError err

-- Replay a series of actions, updating the app state with the new program
replay :: MonadEditApp m => [[ProgAction]] -> m ()
replay = mapM_ handleEditRequest

-- | Reset the entire program state, including any IDs that have been
-- generated.
--
-- This request cannot be undone!
handleResetRequest :: MonadEditApp m => m Prog
handleResetRequest = do
  app <- gets (initialApp . appInit)
  put app
  pure $ appProg app

-- | A shorthand for the constraints we need when performing mutation
-- operations on the application.
--
-- Note we do not want @MonadFresh Name m@, as @fresh :: m Name@ has
-- no way of avoiding user-specified names. Instead, use 'freshName'.
type MonadEditApp m = (Monad m, MonadFresh ID m, MonadFresh NameCounter m, MonadState App m, MonadError ProgError m)

-- | A shorthand for the constraints we need when performing read-only
-- operations on the application.
type MonadQueryApp m = (Monad m, MonadReader App m, MonadError ProgError m)

-- | The 'EditApp' monad.
--
-- Actions run in this monad can modify the 'App'. 'ExceptT' wraps
-- state so that an action that throws an error does not modify the
-- state. This is important to ensure that we can reliably replay the
-- log without having ID mismatches.
newtype EditAppM a = EditAppM (StateT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadState App, MonadError ProgError)

-- | Run an 'EditAppM' action, returning a result and an updated
-- 'App'.
runEditAppM :: EditAppM a -> App -> (Result ProgError a, App)
runEditAppM (EditAppM m) appState = case runExcept (runStateT m appState) of
  Left err -> (Error err, appState)
  Right (res, appState') -> (Success res, appState')

-- | The 'QueryApp' monad.
--
-- Actions run in this monad cannot modify the 'App'. We use 'ExceptT'
-- here for compatibility with 'EditApp'.
newtype QueryAppM a = QueryAppM (ReaderT App (Except ProgError) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader App, MonadError ProgError)

-- | Run a 'QueryAppM' action, returning a result.
runQueryAppM :: QueryAppM a -> App -> Result ProgError a
runQueryAppM (QueryAppM m) appState = case runExcept (runReaderT m appState) of
  Left err -> Error err
  Right res -> Success res

-- | We use this type to remember which "new app" was used to
-- initialize the session. We need this so that program resets and
-- undo know which baseline app to start with when performing their
-- corresponding action.
data InitialApp
  = NewApp
  | NewEmptyApp
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON InitialApp

-- | Given an 'InitialApp', return the corresponding new app instance.
initialApp :: InitialApp -> App
initialApp NewApp = newApp
initialApp NewEmptyApp = newEmptyApp

-- | The global App state
--
-- Note that the 'ToJSON' and 'FromJSON' instances for this type are
-- not used in the frontend, and therefore we can use "Data.Aeson"s
-- generic instances for them.
data App = App
  { appIdCounter :: Int
  , appNameCounter :: NameCounter
  , appProg :: Prog
  , appInit :: InitialApp
  }
  deriving (Eq, Show, Generic)

instance ToJSON App where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON App

-- | An empty initial program.
newEmptyProg :: Prog
newEmptyProg =
  let expr = EmptyHole (Meta 1 Nothing Nothing)
      ty = TEmptyHole (Meta 2 Nothing Nothing)
      def = Def 0 "main" expr ty
   in Prog
        { progTypes = []
        , progDefs = Map.singleton 0 def
        , progSelection = Nothing
        , progSmartHoles = SmartHoles
        , progLog = Log []
        }

-- | An initial app whose program is completely empty.
newEmptyApp :: App
newEmptyApp =
  App
    { appIdCounter = 3
    , appNameCounter = toEnum 0
    , appProg = newEmptyProg
    , appInit = NewEmptyApp
    }

-- | An initial program with some useful typedefs.
newProg :: Prog
newProg = newEmptyProg{progTypes = defaultTypeDefs}

-- | An initial app whose program includes some useful typedefs.
newApp :: App
newApp = newEmptyApp{appProg = newProg, appInit = NewApp}

-- | Construct a new, empty expression
newExpr :: MonadEditApp m => m Expr
newExpr = do
  id_ <- fresh
  pure $ EmptyHole (Meta id_ Nothing Nothing)

-- | Construct a new, empty type
newType :: MonadEditApp m => m Type
newType = do
  id_ <- fresh
  pure $ TEmptyHole (Meta id_ Nothing Nothing)

-- | Support for generating fresh IDs
instance MonadFresh ID EditAppM where
  fresh = do
    idCounter <- gets appIdCounter
    modify (\s -> s{appIdCounter = idCounter + 1})
    pure (ID idCounter)

-- | Support for generating names. Basically just a counter so we don't
-- generate the same automatic name twice.
instance MonadFresh NameCounter EditAppM where
  fresh = do
    nameCounter <- gets appNameCounter
    modify (\s -> s{appNameCounter = succ nameCounter})
    pure nameCounter

copyPasteSig :: MonadEditApp m => Prog -> (ID, ID) -> ID -> [Action] -> m Prog
copyPasteSig p (fromDefId, fromTyId) toDefId setup = do
  c' <- focusNode p fromDefId fromTyId
  c <- case c' of
    Left (Left _) -> throwError $ CopyPasteError "tried to copy-paste an expression into a signature"
    Left (Right zt) -> pure $ Left zt
    Right zt -> pure $ Right zt
  smartHoles <- gets $ progSmartHoles . appProg
  -- We intentionally throw away any changes in doneSetup other than via 'tgt'
  -- as these could be in other definitions referencing this one, due to
  -- types changing. However, we are going to do a full tc pass anyway,
  -- which will pick up any problems. It is better to do it in one batch,
  -- in case the intermediate state after 'setup' causes more problems
  -- than the final state does.
  doneSetup <- withDef (Just toDefId) p $ \def -> applyActionsToTypeSig smartHoles (progTypes p) (progDefs p) def setup
  tgt <- case doneSetup of
    Left err -> throwError $ ActionError err
    Right (_, _, tgt) -> pure $ focusOnlyType tgt
  let sharedScope =
        if fromDefId == toDefId --optimization only
          then getSharedScopeTy c $ Right tgt
          else mempty
  -- Delete unbound vars
  let cTgt = either target target c
      f (m, n) = if Set.member n sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
  cScoped <- traverseOf _freeVarsTy f cTgt
  freshCopy <- traverseOf (_typeMeta % _id) (const fresh) cScoped
  pasted <- case target tgt of
    TEmptyHole _ -> pure $ replace freshCopy tgt
    _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
  let oldDef = progDefs p Map.! toDefId
  let newDef = oldDef{defType = fromZipper pasted}
  let newDefs = Map.insert toDefId newDef $ progDefs p
  let newSel = NodeSelection SigNode (getID $ target pasted) (pasted ^. _target % _typeMetaLens % re _Right)
  let finalProg = p{progDefs = newDefs, progSelection = Just (Selection newDef $ Just newSel)}
  tcWholeProg finalProg

-- We cannot use bindersAbove as that works on names only, and different scopes
-- may reuse the same names. However, we want to detect that as non-shared.
-- Instead, we rely on fact that IDs are unique.
-- We get the scope from the second argument, as that is where we are pasting.
getSharedScopeTy :: Either TypeZ TypeZip -> Either TypeZ TypeZip -> Set.Set Name
getSharedScopeTy l r =
  let idsR = case r of
        Right r' -> getID r' : foldAbove ((: []) . getID . current) r'
        Left r' -> getID r' : foldAbove ((: []) . getID . current) (focusOnlyType r') <> (getID (unfocusType r') : foldAbove ((: []) . getID . current) r')
      -- Replae use of `unsafeHead` here. See:
      -- https://github.com/hackworthltd/primer/issues/147
      rID = unsafeHead idsR
      idsL = case l of
        Right l' -> getID l' : foldAbove ((: []) . getID . current) l'
        Left l' -> getID l' : foldAbove ((: []) . getID . current) (focusOnlyType l') <> (getID (unfocusType l') : foldAbove ((: []) . getID . current) l')
      commonAncestor = getLast $ foldMap (\(il, ir) -> Last $ if il == ir then Just il else Nothing) $ zip (reverse idsL) (reverse idsR)
      rAncestor = do
        a <- commonAncestor
        flip loopM r $ \r' -> if either getID getID r' == a then pure $ Right r' else Left <$> bimapM up up r'
      -- we need to pick up bindings that happen at the ancestor iff it
      -- is an actual ancestor (rather than l being a decendent of r)
      inScope =
        rAncestor <&> \case
          Left ra -> mwhen (rID /= getID ra) (getBoundHereTy $ target ra) <> bindersAboveTypeZ ra
          Right ra -> mwhen (rID /= getID ra) (getBoundHereTy $ target ra) <> bindersAboveTy ra
   in fromMaybe mempty inScope

-- TODO: there is a lot of duplicated code for copy/paste, often due to types/terms being different...
getSharedScope :: ExprZ -> ExprZ -> Set.Set Name
getSharedScope l r =
  let idsR = getID r : foldAbove ((: []) . getID . current) r
      idsL = getID l : foldAbove ((: []) . getID . current) l
      commonAncestor = getLast $ foldMap (\(il, ir) -> Last $ if il == ir then Just il else Nothing) $ zip (reverse idsL) (reverse idsR)
      rAncestorAndPenultimate = do
        a <- commonAncestor
        flip loopM (r, Nothing) $ \(r', p) -> if getID r' == a then pure $ Right (r', p) else Left . (,Just r') <$> up r'
      -- we need to pick up bindings that happen at the ancestor iff it
      -- is an actual ancestor (rather than l being a decendent of r)
      inScope =
        rAncestorAndPenultimate <&> \(ra, rp) ->
          let hereBound = maybe mempty (getBoundHere (target ra) . Just . target) rp -- we have a Just exactly when ra/=r
           in hereBound <> bindersAbove ra
   in fromMaybe mempty inScope

-- | A generalisation of 'when'
mwhen :: Monoid m => Bool -> m -> m
mwhen b m = if b then m else mempty

-- | Iterate until we get a 'Right'
loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM f a =
  f a >>= \case
    Left a' -> loopM f a'
    Right b -> pure b

tcWholeProg :: forall m. MonadEditApp m => Prog -> m Prog
tcWholeProg p =
  let tc :: ReaderT Cxt (ExceptT ActionError m) Prog
      tc = do
        defs' <- mapM checkDef (progDefs p)
        let p' = p{progDefs = defs'}
        -- We need to update the metadata cached in the selection
        let oldSel = progSelection p
        newSel <- case oldSel of
          Nothing -> pure Nothing
          Just s -> do
            let defID = s ^. #selectedDef % #defID
                updatedDef = defs' Map.! defID
            updatedNode <- case s ^. #selectedNode of
              Nothing -> pure Nothing
              Just NodeSelection{nodeType, nodeId} -> do
                n <- runExceptT $ focusNode p' defID nodeId
                case (nodeType, n) of
                  (BodyNode, Right (Left x)) -> pure $ Just $ NodeSelection BodyNode nodeId $ bimap (view (position @1) . target) (view _typeMetaLens . target) x
                  (SigNode, Right (Right x)) -> pure $ Just $ NodeSelection SigNode nodeId $ x ^. _target % _typeMetaLens % re _Right
                  _ -> pure Nothing -- something's gone wrong: expected a SigNode, but found it in the body, or vv, or just not found it
            pure $
              Just $
                Selection
                  { selectedDef = updatedDef
                  , selectedNode = updatedNode
                  }
        pure $ p'{progSelection = newSel}
   in do
        x <- runExceptT $ runReaderT tc $ buildTypingContext (progTypes p) (progDefs p) (progSmartHoles p)
        case x of
          Left e -> throwError $ ActionError e
          Right prog -> pure prog

copyPasteBody :: MonadEditApp m => Prog -> (ID, ID) -> ID -> [Action] -> m Prog
copyPasteBody p (fromDefId, fromId) toDefId setup = do
  src' <- focusNode p fromDefId fromId
  -- reassociate so get Expr+(Type+Type), rather than (Expr+Type)+Type
  let src = case src' of
        Left (Left e) -> Left e
        Left (Right t) -> Right (Left t)
        Right t -> Right (Right t)
  smartHoles <- gets $ progSmartHoles . appProg
  -- The Loc zipper captures all the changes, they are only reflected in the
  -- returned Def, which we thus ignore
  doneSetup <- withDef (Just toDefId) p $ \def -> applyActionsToBody smartHoles (progTypes p) (progDefs p) def setup
  tgt <- case doneSetup of
    Left err -> throwError $ ActionError err
    Right (_, tgt) -> pure tgt
  case (src, tgt) of
    (_, InBind _) -> throwError $ CopyPasteError "tried to paste an expression into a binder"
    (Left _, InType _) -> throwError $ CopyPasteError "tried to paste an expression into a type"
    (Right _, InExpr _) -> throwError $ CopyPasteError "tried to paste a type into an expression"
    (Right srcT, InType tgtT) -> do
      let sharedScope =
            if fromDefId == toDefId --optimization only
              then getSharedScopeTy srcT $ Left tgtT
              else mempty
      -- Delete unbound vars. TODO: we may want to let-bind them?
      let srcSubtree = either target target srcT
          f (m, n) = if Set.member n sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
      scopedCopy <- traverseOf _freeVarsTy f srcSubtree
      freshCopy <- traverseOf (_typeMeta % _id) (const fresh) scopedCopy
      pasted <- case target tgtT of
        TEmptyHole _ -> pure $ replace freshCopy tgtT
        _ -> throwError $ CopyPasteError "copy/paste setup didn't select an empty hole"
      let oldDef = progDefs p Map.! toDefId
      let newDef = oldDef{defExpr = unfocusExpr $ unfocusType pasted}
      let newDefs = Map.insert toDefId newDef $ progDefs p
      let newSel = NodeSelection BodyNode (getID $ target pasted) (pasted ^. _target % _typeMetaLens % re _Right)
      let finalProg = p{progDefs = newDefs, progSelection = Just (Selection newDef $ Just newSel)}
      tcWholeProg finalProg
    (Left srcE, InExpr tgtE) -> do
      let sharedScope =
            if fromDefId == toDefId --optimization only
              then getSharedScope srcE tgtE
              else mempty
      -- Delete unbound vars. TODO: we may want to let-bind them?
      let tm (m, n) = if Set.member n sharedScope then pure $ Var m n else fresh <&> \i -> EmptyHole (Meta i Nothing Nothing)
          ty (m, n) = if Set.member n sharedScope then pure $ TVar m n else fresh <&> \i -> TEmptyHole (Meta i Nothing Nothing)
      scopedCopy <- traverseOf _freeTyVars ty =<< traverseOf _freeTmVars tm (target srcE)
      freshCopy <- traverseOf (_exprTypeMeta % _id) (const fresh) =<< traverseOf (_exprMeta % _id) (const fresh) scopedCopy
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
      let oldDef = progDefs p Map.! toDefId
      let newDef = oldDef{defExpr = unfocusExpr pasted}
      let newDefs = Map.insert toDefId newDef $ progDefs p
      let newSel = NodeSelection BodyNode (getID $ target pasted) (pasted ^. _target % _exprMetaLens % re _Left)
      let finalProg = p{progDefs = newDefs, progSelection = Just (Selection newDef $ Just newSel)}
      tcWholeProg finalProg
