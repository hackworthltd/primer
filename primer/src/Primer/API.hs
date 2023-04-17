{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | The Primer API.
--
-- This module defines the Primer API, which is collection of
-- methods for creating, editing, and evaluating Primer programs.
--
-- While this Primer implementation *may* work for multiple students
-- concurrently editing the same program, at the moment we only
-- guarantee that it works for a single student per program (or
-- "session"). This implementation *is* safe for concurrent requests
-- to different programs, because all API requests are executed
-- transactionally.
module Primer.API (
  Env (..),
  PrimerM (..),
  runPrimerM,
  ReqResp (..),
  APILog (..),
  MonadAPILog,
  PrimerErr (..),
  NewSessionReq (..),
  newSession,
  addSession,
  copySession,
  deleteSession,
  listSessions,
  findSessions,
  getVersion,
  Tree,
  NodeBody (..),
  viewProg,
  Prog (Prog),
  Module (Module),
  Def (Def),
  getProgram,
  getProgram',
  getSessionName,
  renameSession,
  edit,
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
  EvalFullResp (..),
  evalFull',
  flushSessions,
  createDefinition,
  createTypeDef,
  availableActions,
  actionOptions,
  applyActionNoInput,
  applyActionInput,
  ApplyActionBody (..),
  undo,
  redo,
  -- The following are exported only for testing.
  viewTreeType,
  viewTreeExpr,
  getApp,
  Selection (..),
  viewSelection,
  NodeSelection (..),
  viewNodeSelection,
  undoAvailable,
  redoAvailable,
  Name (..),
) where

import Foreword

import Control.Concurrent.STM (
  STM,
  TBQueue,
  TMVar,
  atomically,
  newEmptyTMVar,
  takeTMVar,
  writeTBQueue,
 )
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Log (
  MonadLog,
  Severity (Informational, Warning),
  WithSeverity (WithSeverity),
  logMessage,
 )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Map qualified as Map
import Data.Tuple.Extra (curry3)
import Optics (ifoldr, over, traverseOf, view, (^.))
import Primer.API.NodeFlavor qualified as Flavor
import Primer.API.RecordPair (RecordPair (RecordPair))
import Primer.Action (ActionError, ProgAction, toProgActionInput, toProgActionNoInput)
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  Editable,
  EvalFullReq (..),
  EvalReq (..),
  EvalResp (..),
  Level,
  MutationRequest,
  NodeType (..),
  ProgError,
  QueryAppM,
  Question (GenerateName),
  appProg,
  handleEvalFullRequest,
  handleEvalRequest,
  handleGetProgramRequest,
  handleMutationRequest,
  handleQuestion,
  newApp,
  progAllDefs,
  progAllTypeDefs,
  progCxt,
  progImports,
  progLog,
  progModules,
  progSelection,
  redoLog,
  runEditAppM,
  runQueryAppM,
  unlog,
 )
import Primer.App qualified as App
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName,
  GlobalName (..),
  HasID (..),
  ID,
  Kind (..),
  LVarName,
  ModuleName,
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type,
  Type' (..),
  TypeMeta,
  ValConName,
  getID,
  unLocalName,
  unsafeMkLocalName,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.Core.DSL qualified as DSL
import Primer.Core.Meta (LocalName)
import Primer.Core.Meta qualified as Core
import Primer.Database (
  OffsetLimit,
  OpStatus,
  Page,
  Session,
  SessionData (..),
  SessionId,
  SessionName,
  Sessions,
  Version,
  fromSessionName,
  getCurrentTime,
  newSessionId,
  safeMkSessionName,
 )
import Primer.Database qualified as Database (
  Op (
    DeleteSession,
    FindSessions,
    Insert,
    ListSessions,
    LoadSession,
    UpdateApp,
    UpdateName
  ),
  OpStatus (
    Failure,
    Success
  ),
 )
import Primer.Def (
  ASTDef (..),
  defAST,
 )
import Primer.Def qualified as Def
import Primer.Eval.Redex (Dir (Chk), EvalLog)
import Primer.EvalFull (TerminationBound)
import Primer.JSON (
  CustomJSON (..),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Log (
  ConvertLogMessage (convert),
  PureLog,
  runPureLog,
 )
import Primer.Module (moduleDefsQualified, moduleName, moduleTypesQualified)
import Primer.Name qualified as Name
import Primer.Primitives (primDefType)
import Primer.TypeDef (ASTTypeDef (ASTTypeDef), ValCon (ValCon))
import StmContainers.Map qualified as StmMap

-- | The API environment.
data Env = Env
  { sessions :: Sessions
  , dbOpQueue :: TBQueue Database.Op
  , version :: Version
  }

-- | The Primer API monad transformer.
newtype PrimerM m a = PrimerM {unPrimerM :: ReaderT Env m a}
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadLog l
    , MonadMask
    , MonadReader Env
    , MonadIO
    , MonadFail
    , MonadFix
    , MonadPlus
    , MonadTrans
    , MonadState s
    , MonadWriter w
    , MonadZip
    , MonadCont
    )

-- | Run a 'PrimerM' action with the given 'Env'.
runPrimerM :: PrimerM m a -> Env -> m a
runPrimerM = runReaderT . unPrimerM

-- | Primer exception class.
data PrimerErr
  = DatabaseErr Text
  | UnknownDef GVarName
  | UnexpectedPrimDef GVarName
  | AddDefError ModuleName (Maybe Text) ProgError
  | AddTypeDefError TyConName [ValConName] ProgError
  | ActionOptionsNoID (Maybe (NodeType, ID))
  | ToProgActionError Available.Action ActionError
  | ApplyActionError [ProgAction] ProgError
  | UndoError ProgError
  | RedoError ProgError
  deriving stock (Show)

instance Exception PrimerErr

sessionsTransaction :: (MonadIO m) => (Sessions -> TBQueue Database.Op -> STM a) -> PrimerM m a
sessionsTransaction f = do
  ss <- asks sessions
  q <- asks dbOpQueue
  liftIO $ atomically $ f ss q

data SessionOp l a where
  EditApp :: (App -> PureLog l (a, App)) -> SessionOp l a
  QueryApp :: (App -> a) -> SessionOp l a
  OpGetSessionName :: SessionOp l Text
  GetSessionData :: SessionOp l SessionData
  OpRenameSession :: Text -> SessionOp l Text

-- A note about the implementation here. When the session is missing
-- from the in-memory database, we can't queue the database request to
-- load the session *and* wait for the database thread's asynchronous
-- reply in the same STM transaction. That's because the wait will
-- immediately block until the reply has been received, preventing the
-- transaction from completing, but the database request won't
-- actually be sent until the transaction is complete. This would
-- cause a deadlock!
withSession' :: (MonadIO m, MonadThrow m, MonadLog l m) => SessionId -> SessionOp l a -> PrimerM m a
withSession' sid op = do
  -- Not all operations need this, but we can't do it on demand
  -- because we'll be inside STM. We could check the op type here and
  -- wrap this in a 'Maybe', but then we'd need to assert that it's a
  -- 'Just' when we use it inside the transaction, which complicates
  -- error handling and logging, so that's not without a cost, either.
  --
  -- We can revisit this if the performance impact of 'getCurrentTime'
  -- becomes a problem.
  now <- getCurrentTime
  hndl :: Either (TMVar OpStatus) (a, m ()) <- sessionsTransaction $ \ss q -> do
    query <- StmMap.lookup sid ss
    case query of
      Nothing -> do
        -- The session is not in memory. Ask the database thread to
        -- load the missing session, and return the "callback"
        -- 'TMVar'.
        --
        -- Note: see above for why we don't immediately wait
        -- for the callback.
        callback <- newEmptyTMVar
        writeTBQueue q $ Database.LoadSession sid ss callback
        pure $ Left callback
      Just s@(SessionData appl n _) ->
        -- The session is in memory, let's do this.
        case op of
          EditApp f -> do
            -- We batch the logs, as we are running in STM
            let ((res, appl'), logs) = runPureLog $ f appl
            StmMap.insert (SessionData appl' n now) sid ss
            writeTBQueue q $ Database.UpdateApp sid appl' now
            -- We return an action which, when run, will log the messages
            pure $ Right (res, traverse_ logMessage logs)
          QueryApp f -> pure $ Right (f appl, pure ())
          OpGetSessionName -> pure $ Right (fromSessionName n, pure ())
          GetSessionData -> pure $ Right (s, pure ())
          OpRenameSession n' ->
            let newName = safeMkSessionName n'
             in do
                  StmMap.insert (SessionData appl newName now) sid ss
                  writeTBQueue q $ Database.UpdateName sid newName now
                  pure $ Right (fromSessionName newName, pure ())
  case hndl of
    Left callback -> do
      -- The session was missing from the in-memory database. Once we
      -- get here, we know we've made the database load request, so
      -- now we can wait for the callback.
      dbResult <- liftIO $ atomically $ takeTMVar callback
      case dbResult of
        Database.Failure msg ->
          -- The load failed for some reason.
          throwM $ DatabaseErr msg
        Database.Success ->
          -- The session has been loaded, try the operation again by
          -- recursing.
          --
          -- Note: this should be a tail call as long as we compile
          -- with -O (or at least -floopification), because it's a
          -- known function, it's in tail position, and it's fully
          -- saturated. Ref:
          -- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/haskell-execution/function-calls
          withSession' sid op
    Right (result, logAction) -> do
      -- Actually log the batched messages
      logAction
      -- We performed the session transaction, now return the result.
      pure result

data ReqResp a b = Req a | Resp b
  deriving stock (Show, Read)

data APILog
  = NewSession (ReqResp NewSessionReq SessionId)
  | AddSession (ReqResp (Text, App) SessionId)
  | CopySession (ReqResp SessionId SessionId)
  | DeleteSession (ReqResp SessionId ())
  | ListSessions (ReqResp OffsetLimit (Page Session))
  | FindSessions (ReqResp (Text, OffsetLimit) (Page Session))
  | GetVersion (ReqResp () Version)
  | GetSessionName (ReqResp SessionId Text)
  | RenameSession (ReqResp (SessionId, Text) Text)
  | GetApp (ReqResp SessionId App)
  | GetProgram' (ReqResp SessionId Prog)
  | GetProgram (ReqResp SessionId App.Prog)
  | Edit (ReqResp (SessionId, MutationRequest) (Either ProgError App.Prog))
  | VariablesInScope (ReqResp (SessionId, (GVarName, ID)) (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())])))
  | GenerateNames (ReqResp (SessionId, ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind))) (Either ProgError [Name.Name]))
  | EvalStep (ReqResp (SessionId, EvalReq) (Either ProgError EvalResp))
  | EvalFull (ReqResp (SessionId, EvalFullReq) (Either ProgError App.EvalFullResp))
  | EvalFull' (ReqResp (SessionId, Maybe TerminationBound, GVarName) EvalFullResp)
  | FlushSessions (ReqResp () ())
  | CreateDef (ReqResp (SessionId, ModuleName, Maybe Text) Prog)
  | CreateTypeDef (ReqResp (SessionId, TyConName, [ValConName]) Prog)
  | AvailableActions (ReqResp (SessionId, Level, Selection) [Available.Action])
  | ActionOptions (ReqResp (SessionId, Level, Selection, Available.InputAction) Available.Options)
  | ApplyActionNoInput (ReqResp (SessionId, Selection, Available.NoInputAction) Prog)
  | ApplyActionInput (ReqResp (SessionId, ApplyActionBody, Available.InputAction) Prog)
  | Undo (ReqResp SessionId Prog)
  | Redo (ReqResp SessionId Prog)
  deriving stock (Show, Read)

type MonadAPILog l m = (MonadLog (WithSeverity l) m, ConvertLogMessage APILog l)

-- | A wrapper to log an API call
logAPI :: MonadAPILog l m => (ReqResp a b -> (Severity, APILog)) -> (a -> PrimerM m b) -> a -> PrimerM m b
logAPI c resp req = do
  logMsg $ c $ Req req
  r <- resp req
  logMsg $ c $ Resp r
  pure r
  where
    logMsg = logMessage . uncurry WithSeverity . second convert

-- | A variant of 'logAPI' for actions with no input, and no error possibility
logAPI' :: MonadAPILog l m => (ReqResp () b -> APILog) -> PrimerM m b -> PrimerM m b
logAPI' c m = logAPI (noError c) (const m) ()

noError :: (ReqResp a b -> APILog) -> ReqResp a b -> (Severity, APILog)
noError = ((Informational,) .)

leftResultError :: (ReqResp a (Either e b) -> APILog) -> ReqResp a (Either e b) -> (Severity, APILog)
leftResultError c r@(Resp (Left _)) = (Warning, c r)
leftResultError c r = (Informational, c r)

-- | A new session request.

{- HLINT ignore NewSessionReq "Use newtype instead of data" -}
data NewSessionReq = NewSessionReq
  { name :: Text
  -- ^ The name of the new session. Note that this field is just a
  -- hint: the API may choose a different name if the given name is
  -- invalid.
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON NewSessionReq

-- | Create a new session and return its ID.
--
-- The session's initial program is 'newApp'.
--
-- If the given session name is invalid, it will be replaced iwth a
-- default session name. However, no indication is given to teh caller
-- when this occurs. Query the returned session ID to determine the
-- actual session name that was assigned.
newSession :: (MonadIO m, MonadAPILog l m) => NewSessionReq -> PrimerM m SessionId
newSession = logAPI (noError NewSession) $ \(NewSessionReq n) -> addSession n newApp

-- | Given an 'App' and a proposed session name as 'Text', create a
-- new session with the given app and name, and return the session ID.
--
-- If the given session name is invalid, it will be replaced with a
-- default session name. However, no indication to the caller is given
-- when this occurs. Query the returned session ID to determine the
-- actual session name that was assigned.
--
-- Note: this API method is currently a special case, and we do not
-- expect typical API clients to use it, because it bypasses the edit
-- API by permitting the caller to directly insert an existing 'App'
-- into the database. The chief use case for this API method is to
-- insert pre-made programs built with the Primer Haskell DSL into a
-- new Primer database.
addSession :: (MonadIO m, MonadAPILog l m) => Text -> App -> PrimerM m SessionId
addSession = curry $ logAPI (noError AddSession) $ \(n, a) -> addSession' (safeMkSessionName n) a

addSession' :: (MonadIO m) => SessionName -> App -> PrimerM m SessionId
addSession' n a = do
  nextSID <- liftIO newSessionId
  now <- getCurrentTime
  sessionsTransaction $ \ss q -> do
    StmMap.insert (SessionData a n now) nextSID ss
    writeTBQueue q $ Database.Insert nextSID a n now
    pure nextSID

-- | Copy the given session to a new session, and return the new
-- session's ID.
--
-- We implement this as 2 separate transactions: 1 to retrieve the
-- source session, and 1 to insert the copy. Semantically, this is
-- fine, and it should be more fair on a busy system than a single
-- transaction which takes longer.
--
-- Note that we copy the original session's timestamp to the new
-- session. This is an arbitrary choice and could be changed, if
-- there's some need to do so.
copySession :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m SessionId
copySession = logAPI (noError CopySession) $ \srcId -> do
  copy <- withSession' srcId GetSessionData
  nextSID <- liftIO newSessionId
  sessionsTransaction $ \ss q -> do
    StmMap.insert copy nextSID ss
    writeTBQueue q $ Database.Insert nextSID (sessionApp copy) (sessionName copy) (lastModified copy)
    pure nextSID

deleteSession :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m ()
deleteSession = logAPI (noError DeleteSession) $ \sid -> do
  hndl <- sessionsTransaction $ \ss q -> do
    callback <- newEmptyTMVar
    -- Recall that the in-memory database is just a cache, so we can
    -- safely delete it here without waiting to find out whether the
    -- persistent database deletion succeeds. In fact, to enforce the
    -- invariants expected by the database implementation, we *must*
    -- delete the session from the in-memory database. See the comment
    -- in 'Primer.Database.Op' for more details.
    --
    -- Note that if the session isn't in the in-memory database, then
    -- the 'StmMap.delete' action is a no-op.
    StmMap.delete sid ss
    writeTBQueue q $ Database.DeleteSession sid callback
    pure callback
  -- We need to wait for the result from the database so that we can
  -- inform the client whether the session was actually deleted.
  dbResult <- liftIO $ atomically $ takeTMVar hndl
  case dbResult of
    Database.Failure msg ->
      throwM $ DatabaseErr msg
    Database.Success -> pure ()

-- | Returns a list of all 'Session's in the database.
listSessions :: (MonadIO m, MonadAPILog l m) => OffsetLimit -> PrimerM m (Page Session)
listSessions = logAPI (noError ListSessions) $ \ol -> do
  q <- asks dbOpQueue
  callback <- liftIO $
    atomically $ do
      cb <- newEmptyTMVar
      writeTBQueue q $ Database.ListSessions ol cb
      pure cb
  liftIO $ atomically $ takeTMVar callback

-- | Find sessions whose names contain the given substring.
--
-- Note that this implementation is case-in-sensitive.
findSessions :: (MonadIO m, MonadAPILog l m) => Text -> OffsetLimit -> PrimerM m (Page Session)
findSessions = curry $ logAPI (noError FindSessions) $ \case
  (substr, ol) -> do
    q <- asks dbOpQueue
    callback <- liftIO $
      atomically $ do
        cb <- newEmptyTMVar
        writeTBQueue q $ Database.FindSessions substr ol cb
        pure cb
    liftIO $ atomically $ takeTMVar callback

getVersion :: (MonadAPILog l m) => PrimerM m Version
getVersion = logAPI' GetVersion $ asks version

getSessionName :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m Text
getSessionName = logAPI (noError GetSessionName) $ \sid -> withSession' sid OpGetSessionName

renameSession :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> Text -> PrimerM m Text
renameSession = curry $ logAPI (noError RenameSession) $ \(sid, n) -> withSession' sid $ OpRenameSession n

-- Run an 'EditAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftEditAppM ::
  forall m l e a.
  (MonadIO m, MonadThrow m, MonadLog (WithSeverity l) m) =>
  EditAppM (PureLog (WithSeverity l)) e a ->
  SessionId ->
  PrimerM m (Either e a)
liftEditAppM h sid = withSession' sid (EditApp $ runEditAppM h)

-- Run a 'QueryAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftQueryAppM :: (MonadIO m, MonadThrow m, MonadLog l m) => QueryAppM a -> SessionId -> PrimerM m (Either ProgError a)
liftQueryAppM h sid = withSession' sid (QueryApp $ runQueryAppM h)

-- | Given a 'SessionId', return the session's 'App'.
--
-- Note: this API method is currently a special case, and we do not
-- expect typical API clients to use it. Its primary use is for
-- testing.
getApp :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m App
getApp = logAPI (noError GetApp) $ \sid -> withSession' sid $ QueryApp identity

-- | Given a 'SessionId', return the session's 'Prog'.
--
-- Note that this returns a simplified version of 'App.Prog' intended
-- for use with non-Haskell clients.
getProgram' :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m Prog
getProgram' = logAPI (noError GetProgram') (fmap viewProg . getProgram)

-- | Given a 'SessionId', return the session's 'App.Prog'.
getProgram :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m App.Prog
getProgram = logAPI (noError GetProgram) $ \sid -> withSession' sid $ QueryApp handleGetProgramRequest

-- | A frontend will be mostly concerned with rendering, and does not need the
-- full complexity of our AST for that task. 'Tree' is a simplified view with
-- just enough information to render nicely.
data Tree = Tree
  { nodeId :: Text
  -- ^ a unique identifier
  , body :: NodeBody
  , childTrees :: [Tree]
  , rightChild :: Maybe Tree
  -- ^ a special subtree to be rendered to the right, rather than below - useful for `case` branches
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Tree
  deriving anyclass (NFData)

-- | A local or global name.
-- Field names are intentionally the same as `GlobalName`, so that, unless `qualifiedModule` is `Nothing`,
-- JSON representations are the same, and clients can easily coerce between the two.
data Name = Name
  { qualifiedModule :: Maybe ModuleName
  , baseName :: Name.Name
  }
  deriving stock (Show, Read, Eq, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Name
  deriving anyclass (NFData)

-- | The contents of a node.
data NodeBody
  = -- | A "normal" node, usually with student-generated text, such as a variable or constructor name.
    TextBody (RecordPair Flavor.NodeFlavorTextBody Name)
  | -- | A node containing a value constructor inhabiting a primitive type.
    PrimBody (RecordPair Flavor.NodeFlavorPrimBody PrimCon)
  | -- | A node which contains another tree. Used for rendering pattern matching.
    BoxBody (RecordPair Flavor.NodeFlavorBoxBody Tree)
  | -- | Some simple nodes, like function application, have no body.
    NoBody Flavor.NodeFlavorNoBody
  deriving stock (Show, Read, Eq, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NodeBody
  deriving anyclass (NFData)

-- | This type is the API's view of a 'App.Prog'
data Prog = Prog
  { modules :: [Module]
  , selection :: Maybe Selection
  , undoAvailable :: Bool
  , redoAvailable :: Bool
  }
  deriving stock (Generic, Show, Read)
  deriving (ToJSON, FromJSON) via PrimerJSON Prog
  deriving anyclass (NFData)

-- | This type is the API's view of a 'Module.Module'
-- (this is expected to evolve as we flesh out the API)
data Module = Module
  { modname :: ModuleName
  , editable :: Bool
  , types :: [TyConName]
  , -- We don't use Map Name Def as it is rather redundant since each
    -- Def carries a name field, and it is difficult to enforce that
    -- "the keys of this object match the name field of the
    -- corresponding value".
    defs :: [Def]
  }
  deriving stock (Generic, Show, Read)
  deriving (ToJSON, FromJSON) via PrimerJSON Module
  deriving anyclass (NFData)

-- | This type is the api's view of a 'Primer.Core.Def'
-- (this is expected to evolve as we flesh out the API)
data Def = Def
  { name :: GVarName
  , type_ :: Tree
  , term :: Maybe Tree
  -- ^ definitions with no associated tree are primitives
  }
  deriving stock (Generic, Show, Read)
  deriving (ToJSON, FromJSON) via PrimerJSON Def
  deriving anyclass (NFData)

viewProg :: App.Prog -> Prog
viewProg p =
  Prog
    { modules = map (viewModule True) (progModules p) <> map (viewModule False) (progImports p)
    , selection = viewSelection <$> progSelection p
    , undoAvailable = not $ null $ unlog $ progLog p
    , redoAvailable = not $ null $ unlog $ redoLog p
    }
  where
    viewModule e m =
      Module
        { modname = moduleName m
        , editable = e
        , types = fst <$> Map.assocs (moduleTypesQualified m)
        , defs =
            ( \(name, d) ->
                Def
                  { name
                  , term = viewTreeExpr . astDefExpr <$> defAST d
                  , type_ =
                      case d of
                        Def.DefAST d' -> viewTreeType $ astDefType d'
                        Def.DefPrim d' -> viewTreeType' $ labelNodes $ primDefType d'
                          where
                            labelNodes =
                              flip evalState (0 :: Int) . traverseOf _typeMeta \() -> do
                                n <- get
                                put $ n + 1
                                pure $ "primtype_" <> Name.unName (Core.baseName name) <> "_" <> show n
                  }
            )
              <$> Map.assocs (moduleDefsQualified m)
        }

-- | A simple method to extract 'Tree's from 'Expr's. This is injective.
viewTreeExpr :: Expr -> Tree
viewTreeExpr e0 = case e0 of
  Hole _ e ->
    Tree
      { nodeId
      , body = NoBody Flavor.Hole
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  EmptyHole _ ->
    Tree
      { nodeId
      , body = NoBody Flavor.EmptyHole
      , childTrees = []
      , rightChild = Nothing
      }
  Ann _ e t ->
    Tree
      { nodeId
      , body = NoBody Flavor.Ann
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  App _ e1 e2 ->
    Tree
      { nodeId
      , body = NoBody Flavor.App
      , childTrees = [viewTreeExpr e1, viewTreeExpr e2]
      , rightChild = Nothing
      }
  APP _ e t ->
    Tree
      { nodeId
      , body = NoBody Flavor.APP
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  Con _ c tmApps ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.Con $ globalName c
      , childTrees = map viewTreeExpr tmApps
      , rightChild = Nothing
      }
  Lam _ s e ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.Lam $ localName s
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  LAM _ s e ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.LAM $ localName s
      , childTrees = [viewTreeExpr e]
      , rightChild = Nothing
      }
  Var _ ref ->
    Tree
      { nodeId
      , body = case ref of
          GlobalVarRef n -> TextBody $ RecordPair Flavor.GlobalVar $ globalName n
          LocalVarRef n -> TextBody $ RecordPair Flavor.LocalVar $ localName n
      , childTrees = []
      , rightChild = Nothing
      }
  Let _ s e1 e2 ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.Let $ localName s
      , childTrees = [viewTreeExpr e1, viewTreeExpr e2]
      , rightChild = Nothing
      }
  LetType _ s t e ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.LetType $ localName s
      , childTrees = [viewTreeExpr e, viewTreeType t]
      , rightChild = Nothing
      }
  Letrec _ s e1 t e2 ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.Letrec $ localName s
      , childTrees = [viewTreeExpr e1, viewTreeType t, viewTreeExpr e2]
      , rightChild = Nothing
      }
  Case _ e bs ->
    Tree
      { nodeId
      , body = NoBody Flavor.Case
      , childTrees
      , rightChild
      }
    where
      (childTrees, rightChild) =
        ( [viewTreeExpr e]
        , -- seeing as the inner function always returns a `Just`,
          -- this would only be `Nothing` if the list of branches were empty,
          --  which should only happen when matching on `Void`
          ifoldr
            (\i b next -> Just $ (viewCaseBranch i b){rightChild = next})
            Nothing
            bs
        )
      viewCaseBranch i (CaseBranch con binds rhs) =
        let
          -- these IDs will not clash with any others in the tree,
          -- since node IDs in the input expression are unique,
          -- and don't contain non-numerical characters
          boxId = nodeId <> "P" <> show i
          patternRootId = boxId <> "B"
         in
          Tree
            { nodeId = boxId
            , body =
                BoxBody . RecordPair Flavor.Pattern $
                  ( Tree
                      { nodeId = patternRootId
                      , body = TextBody $ RecordPair Flavor.PatternCon $ globalName con
                      , childTrees =
                          map
                            ( \(Bind m v) ->
                                Tree
                                  { nodeId = show $ getID m
                                  , body = TextBody $ RecordPair Flavor.PatternBind $ localName v
                                  , childTrees = []
                                  , rightChild = Nothing
                                  }
                            )
                            binds
                      , rightChild = Nothing
                      }
                  )
            , childTrees = [viewTreeExpr rhs]
            , rightChild = Nothing
            }
  PrimCon _ pc ->
    Tree
      { nodeId
      , body = PrimBody $ RecordPair Flavor.PrimCon pc
      , childTrees = []
      , rightChild = Nothing
      }
  where
    nodeId = show $ e0 ^. _id

-- | Similar to 'viewTreeExpr', but for 'Type's
viewTreeType :: Type -> Tree
viewTreeType = viewTreeType' . over _typeMeta (show . view _id)

-- | Like 'viewTreeType', but with the flexibility to accept arbitrary textual node identifiers,
-- rather than using the type's numeric IDs.
viewTreeType' :: Type' Text -> Tree
viewTreeType' t0 = case t0 of
  TEmptyHole _ ->
    Tree
      { nodeId
      , body = NoBody Flavor.TEmptyHole
      , childTrees = []
      , rightChild = Nothing
      }
  THole _ t ->
    Tree
      { nodeId
      , body = NoBody Flavor.THole
      , childTrees = [viewTreeType' t]
      , rightChild = Nothing
      }
  TCon _ n ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.TCon $ globalName n
      , childTrees = []
      , rightChild = Nothing
      }
  TFun _ t1 t2 ->
    Tree
      { nodeId
      , body = NoBody Flavor.TFun
      , childTrees = [viewTreeType' t1, viewTreeType' t2]
      , rightChild = Nothing
      }
  TVar _ n ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.TVar $ localName n
      , childTrees = []
      , rightChild = Nothing
      }
  TApp _ t1 t2 ->
    Tree
      { nodeId
      , body = NoBody Flavor.TApp
      , childTrees = [viewTreeType' t1, viewTreeType' t2]
      , rightChild = Nothing
      }
  TForall _ n k t ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.TForall $ localName $ unsafeMkLocalName $ withKindAnn $ Name.unName $ unLocalName n
      , childTrees = [viewTreeType' t]
      , rightChild = Nothing
      }
    where
      -- TODO this is a placeholder
      -- for now we expect all kinds in student programs to be `KType`
      -- but we show something for other kinds, in order to keep rendering injective
      withKindAnn = case k of
        KType -> identity
        _ -> (<> (" :: " <> show k))
  TLet _ n t b ->
    Tree
      { nodeId
      , body = TextBody $ RecordPair Flavor.TLet $ localName n
      , childTrees = [viewTreeType' t, viewTreeType' b]
      , rightChild = Nothing
      }
  where
    nodeId = t0 ^. _typeMetaLens

globalName :: GlobalName k -> Name
globalName n = Name{qualifiedModule = Just $ Core.qualifiedModule n, baseName = Core.baseName n}

localName :: LocalName k -> Name
localName n = Name{qualifiedModule = Nothing, baseName = unLocalName n}

edit ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  MutationRequest ->
  PrimerM m (Either ProgError App.Prog)
edit = curry $ logAPI (leftResultError Edit) $ \(sid, req) -> liftEditAppM (handleMutationRequest req) sid

variablesInScope ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  (GVarName, ID) ->
  PrimerM m (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
variablesInScope = curry $ logAPI (leftResultError VariablesInScope) $ \(sid, (defname, exprid)) ->
  liftQueryAppM (handleQuestion (App.VariablesInScope defname exprid)) sid

generateNames ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind)) ->
  PrimerM m (Either ProgError [Name.Name])
generateNames = curry $ logAPI (leftResultError GenerateNames) $ \(sid, ((defname, exprid), tk)) ->
  liftQueryAppM (handleQuestion $ GenerateName defname exprid tk) sid

evalStep ::
  (MonadIO m, MonadThrow m, MonadAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  EvalReq ->
  PrimerM m (Either ProgError EvalResp)
evalStep = curry $ logAPI (leftResultError EvalStep) $ \(sid, req) ->
  liftEditAppM (handleEvalRequest req) sid

evalFull ::
  (MonadIO m, MonadThrow m, MonadAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  EvalFullReq ->
  PrimerM m (Either ProgError App.EvalFullResp)
evalFull = curry $ logAPI (leftResultError EvalFull) $ \(sid, req) ->
  liftEditAppM (handleEvalFullRequest req) sid

-- | This type is the API's view of a 'App.EvalFullResp
-- (this is expected to evolve as we flesh out the API)
data EvalFullResp
  = EvalFullRespTimedOut Tree
  | EvalFullRespNormal Tree
  deriving stock (Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EvalFullResp

-- | Evaluate some top level definition in a program.
--
-- Note that this is a simplified version of 'evalFull',
-- intended for non-Haskell clients
evalFull' ::
  forall m l.
  (MonadIO m, MonadThrow m, MonadAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  Maybe TerminationBound ->
  GVarName ->
  PrimerM m EvalFullResp
evalFull' = curry3 $ logAPI (noError EvalFull') $ \(sid, lim, d) ->
  noErr <$> liftEditAppM (q lim d) sid
  where
    q ::
      Maybe TerminationBound ->
      GVarName ->
      EditAppM (PureLog (WithSeverity l)) Void EvalFullResp
    q lim d = do
      e <- DSL.gvar d
      x <-
        handleEvalFullRequest $
          EvalFullReq
            { evalFullReqExpr = e
            , evalFullCxtDir = Chk
            , evalFullMaxSteps = fromMaybe 10 lim
            }
      pure $ case x of
        App.EvalFullRespTimedOut e' -> EvalFullRespTimedOut $ viewTreeExpr e'
        App.EvalFullRespNormal e' -> EvalFullRespNormal $ viewTreeExpr e'
    noErr :: Either Void a -> a
    noErr = \case
      Right a -> a
      Left v -> absurd v

flushSessions :: (MonadIO m, MonadAPILog l m) => PrimerM m ()
flushSessions = logAPI' FlushSessions $ do
  sessionsTransaction $ \ss _ -> do
    StmMap.reset ss
  pure ()

createDefinition ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  ModuleName ->
  Maybe Text ->
  PrimerM m Prog
createDefinition =
  curry3 $
    logAPI (noError CreateDef) \(sid, moduleName, mDefName) ->
      edit sid (App.Edit [App.CreateDef moduleName mDefName])
        >>= either (throwM . AddDefError moduleName mDefName) (pure . viewProg)

-- For now, only enumeration types
createTypeDef ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  TyConName ->
  [ValConName] ->
  PrimerM m Prog
createTypeDef =
  curry3 $
    logAPI (noError CreateTypeDef) \(sid, tyconName, valcons) ->
      edit sid (App.Edit [App.AddTypeDef tyconName $ ASTTypeDef [] (map (`ValCon` []) valcons) []])
        >>= either (throwM . AddTypeDefError tyconName valcons) (pure . viewProg)

availableActions ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Level ->
  Selection ->
  PrimerM m [Available.Action]
availableActions = curry3 $ logAPI (noError AvailableActions) $ \(sid, level, selection) -> do
  prog <- getProgram sid
  let allDefs = progAllDefs prog
      allTypeDefs = progAllTypeDefs prog
  (editable, ASTDef{astDefType = type_, astDefExpr = expr}) <- findASTDef allDefs selection.def
  case selection.node of
    Nothing ->
      pure $ Available.forDef (snd <$> allDefs) level editable selection.def
    Just NodeSelection{..} -> do
      pure $ case nodeType of
        SigNode -> Available.forSig level editable type_ id
        BodyNode -> Available.forBody (snd <$> allTypeDefs) level editable expr id

actionOptions ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Level ->
  Selection ->
  Available.InputAction ->
  PrimerM m Available.Options
actionOptions = curry4 $ logAPI (noError ActionOptions) $ \(sid, level, selection, action) -> do
  app <- getApp sid
  let prog = appProg app
      allDefs = progAllDefs prog
      allTypeDefs = progAllTypeDefs prog
      nodeSel = selection.node <&> \s -> (s.nodeType, s.id)
  def' <- snd <$> findASTDef allDefs selection.def
  maybe (throwM $ ActionOptionsNoID nodeSel) pure $
    Available.options
      (snd <$> allTypeDefs)
      (snd <$> allDefs)
      (progCxt prog)
      level
      def'
      nodeSel
      action

findASTDef :: MonadThrow m => Map GVarName (Editable, Def.Def) -> GVarName -> m (Editable, ASTDef)
findASTDef allDefs def = case allDefs Map.!? def of
  Nothing -> throwM $ UnknownDef def
  Just (_, Def.DefPrim _) -> throwM $ UnexpectedPrimDef def
  Just (editable, Def.DefAST d) -> pure (editable, d)

applyActionNoInput ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Selection ->
  Available.NoInputAction ->
  PrimerM m Prog
applyActionNoInput = curry3 $ logAPI (noError ApplyActionNoInput) $ \(sid, selection, action) -> do
  prog <- getProgram sid
  def <- snd <$> findASTDef (progAllDefs prog) selection.def
  actions <-
    either (throwM . ToProgActionError (Available.NoInput action)) pure $
      toProgActionNoInput
        (snd <$> progAllDefs prog)
        def
        selection.def
        (selection.node <&> \s -> (s.nodeType, s.id))
        action
  applyActions sid actions

applyActionInput ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  ApplyActionBody ->
  Available.InputAction ->
  PrimerM m Prog
applyActionInput = curry3 $ logAPI (noError ApplyActionInput) $ \(sid, body, action) -> do
  prog <- getProgram sid
  def <- snd <$> findASTDef (progAllDefs prog) body.selection.def
  actions <-
    either (throwM . ToProgActionError (Available.Input action)) pure $
      toProgActionInput
        def
        body.selection.def
        (body.selection.node <&> \s -> (s.nodeType, s.id))
        body.option
        action
  applyActions sid actions

data ApplyActionBody = ApplyActionBody
  { selection :: Selection
  , option :: Available.Option
  }
  deriving stock (Generic, Show, Read)
  deriving (FromJSON, ToJSON) via PrimerJSON ApplyActionBody

applyActions :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> [ProgAction] -> PrimerM m Prog
applyActions sid actions =
  edit sid (App.Edit actions)
    >>= either
      (throwM . ApplyActionError actions)
      (pure . viewProg)

undo ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  PrimerM m Prog
undo =
  logAPI (noError Undo) \sid ->
    edit sid App.Undo
      >>= either (throwM . UndoError) (pure . viewProg)

redo ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  PrimerM m Prog
redo =
  logAPI (noError Redo) \sid ->
    edit sid App.Redo
      >>= either (throwM . RedoError) (pure . viewProg)

-- | 'App.Selection' without any node metadata.
data Selection = Selection
  { def :: GVarName
  , node :: Maybe NodeSelection
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Selection
  deriving anyclass (NFData)

viewSelection :: App.Selection -> Selection
viewSelection App.Selection{..} = Selection{def = def, node = viewNodeSelection <$> node}

-- | 'App.NodeSelection' without any node metadata.
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , id :: ID
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON NodeSelection
  deriving anyclass (NFData)

viewNodeSelection :: App.NodeSelection (Either ExprMeta TypeMeta) -> NodeSelection
viewNodeSelection sel@App.NodeSelection{nodeType} = NodeSelection{nodeType, id = getID sel}
