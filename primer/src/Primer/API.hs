{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
  APILog (..),
  MonadAPILog,
  PrimerErr (..),
  newSession,
  addSession,
  copySession,
  listSessions,
  getVersion,
  Tree,
  NodeBody (..),
  NodeFlavor,
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
  flushSessions,
  availableActions,
  applyActionNoInput,
  applyActionInput,
  ApplyActionBody (..),
  ExprTreeOpts (..),
  defaultExprTreeOpts,
  -- The following are exported only for testing.
  viewTreeType,
  viewTreeExpr,
  getApp,
  Selection (..),
  convertSelection,
  NodeSelection (..),
  convertNodeSelection,
  inputAction',
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
import Data.Text qualified as T
import Data.Tuple.Extra (curry3)
import ListT qualified (toList)
import Optics (ifoldr, over, traverseOf, view, (^.))
import Primer.API.NodeFlavor (NodeFlavor (..))
import Primer.Action (mkActionInput, mkActionNoInput)
import Primer.Action.Available qualified as Available
import Primer.App (
  App,
  EditAppM,
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  MutationRequest,
  ProgAction,
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
  progModules,
  runEditAppM,
  runQueryAppM,
 )
import Primer.App qualified as App
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (..),
  HasID (..),
  ID,
  Kind (..),
  LVarName,
  Level,
  ModuleName,
  NodeType (..),
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type,
  Type' (..),
  getID,
  moduleNamePretty,
  unLocalName,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.Database (
  OffsetLimit,
  OpStatus,
  Page,
  Session (Session),
  SessionData (..),
  SessionId,
  SessionName,
  Sessions,
  Version,
  defaultSessionName,
  fromSessionName,
  getCurrentTime,
  newSessionId,
  pageList,
  safeMkSessionName,
 )
import Primer.Database qualified as Database (
  Op (
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
import Primer.EvalFull (EvalFullLog)
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
import Primer.Name (Name, unName)
import Primer.Primitives (primDefType)
import StmContainers.Map qualified as StmMap

-- | The API environment.
data Env = Env
  { sessions :: Sessions
  , dbOpQueue :: TBQueue Database.Op
  , version :: Version
  }

-- | The Primer API monad transformer.
newtype PrimerM m a = PrimerM {unPrimerM :: ReaderT Env m a}
  deriving
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
  | InputActionIDNotFound (Maybe ID)
  | MiscPrimerErr Text -- TODO remove
  | ApplyActionError [ProgAction] ProgError -- TODO add more info? e.g. actual types from API call (ProgAction is a bit low-level)
  deriving (Show)

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
        return $ Left callback
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
  deriving (Show)

data APILog
  = NewSession (ReqResp () SessionId)
  | AddSession (ReqResp (Text, App) SessionId)
  | CopySession (ReqResp SessionId SessionId)
  | ListSessions (ReqResp (Bool, OffsetLimit) (Page Session))
  | GetVersion (ReqResp () Version)
  | GetSessionName (ReqResp SessionId Text)
  | RenameSession (ReqResp (SessionId, Text) Text)
  | GetApp (ReqResp SessionId App)
  | GetProgram' (ReqResp (ExprTreeOpts, SessionId) Prog)
  | GetProgram (ReqResp SessionId App.Prog)
  | Edit (ReqResp (SessionId, MutationRequest) (Either ProgError App.Prog))
  | VariablesInScope (ReqResp (SessionId, (GVarName, ID)) (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())])))
  | GenerateNames (ReqResp (SessionId, ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind))) (Either ProgError [Name]))
  | EvalStep (ReqResp (SessionId, EvalReq) (Either ProgError EvalResp))
  | EvalFull (ReqResp (SessionId, EvalFullReq) (Either ProgError EvalFullResp))
  | FlushSessions (ReqResp () ())
  | AvailableActions (ReqResp (SessionId, Level, Selection) [Available.Action])
  deriving (Show)

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

-- | Create a new session and return the session ID.
--
-- The session's initial program is 'newApp'.
newSession :: (MonadIO m, MonadAPILog l m) => PrimerM m SessionId
newSession = logAPI' NewSession $ addSession' defaultSessionName newApp

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

-- If the input is 'False', return all sessions in the database;
-- otherwise, only the in-memory sessions.
--
-- Currently the pagination support is "extract the whole list from the DB,
-- then select a portion". This should be improved to only extract the
-- appropriate section from the DB in the first place.
listSessions :: (MonadIO m, MonadAPILog l m) => Bool -> OffsetLimit -> PrimerM m (Page Session)
listSessions = curry $ logAPI (noError ListSessions) $ \case
  (False, ol) -> do
    q <- asks dbOpQueue
    callback <- liftIO $
      atomically $ do
        cb <- newEmptyTMVar
        writeTBQueue q $ Database.ListSessions ol cb
        return cb
    liftIO $ atomically $ takeTMVar callback
  (_, ol) -> sessionsTransaction $ \ss _ -> do
    kvs' <- ListT.toList $ StmMap.listT ss
    let kvs = (\(i, SessionData _ n t) -> Session i n t) <$> kvs'
    pure $ pageList ol kvs

getVersion :: (MonadAPILog l m) => PrimerM m Version
getVersion = logAPI' GetVersion $ asks version

getSessionName :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m Text
getSessionName = logAPI (noError GetSessionName) $ \sid -> withSession' sid OpGetSessionName

renameSession :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> Text -> PrimerM m Text
renameSession = curry $ logAPI (noError RenameSession) $ \(sid, n) -> withSession' sid $ OpRenameSession n

-- Run an 'EditAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftEditAppM ::
  forall m l a.
  (MonadIO m, MonadThrow m, MonadLog (WithSeverity l) m) =>
  EditAppM (PureLog (WithSeverity l)) a ->
  SessionId ->
  PrimerM m (Either ProgError a)
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
getProgram' :: (MonadIO m, MonadThrow m, MonadAPILog l m) => ExprTreeOpts -> SessionId -> PrimerM m Prog
getProgram' = curry $ logAPI (noError GetProgram') $ \(opts, sid) -> viewProg opts <$> getProgram sid

-- | Given a 'SessionId', return the session's 'App.Prog'.
getProgram :: (MonadIO m, MonadThrow m, MonadAPILog l m) => SessionId -> PrimerM m App.Prog
getProgram = logAPI (noError GetProgram) $ \sid -> withSession' sid $ QueryApp handleGetProgramRequest

-- | A frontend will be mostly concerned with rendering, and does not need the
-- full complexity of our AST for that task. 'Tree' is a simplified view with
-- just enough information to render nicely.
data Tree = Tree
  { nodeId :: Text
  -- ^ a unique identifier
  , flavor :: NodeFlavor
  , body :: NodeBody
  , childTrees :: [Tree]
  , rightChild :: Maybe Tree
  -- ^ a special subtree to be rendered to the right, rather than below - useful for `case` branches
  }
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via PrimerJSON Tree

-- | The contents of a node.
data NodeBody
  = -- | A "normal" node, usually with user-generated text, such as a variable or constructor name.
    TextBody Text
  | -- | A node which contains another tree. Used for rendering pattern matching.
    BoxBody Tree
  | -- | Some simple nodes, like function application, have no body.
    NoBody
  deriving (Show, Eq, Generic)
  deriving (ToJSON) via PrimerJSON NodeBody

-- | This type is the API's view of a 'App.Prog'
-- (this is expected to evolve as we flesh out the API)
newtype Prog = Prog
  { modules :: [Module]
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrimerJSON Prog

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
  deriving (Generic, Show)
  deriving (ToJSON) via PrimerJSON Module

-- | This type is the api's view of a 'Primer.Core.Def'
-- (this is expected to evolve as we flesh out the API)
data Def = Def
  { name :: GVarName
  , type_ :: Tree
  , term :: Maybe Tree
  -- ^ definitions with no associated tree are primitives
  }
  deriving (Generic, Show)
  deriving (ToJSON) via PrimerJSON Def

viewProg :: ExprTreeOpts -> App.Prog -> Prog
viewProg exprTreeOpts p =
  Prog{modules = map (viewModule True) (progModules p) <> map (viewModule False) (progImports p)}
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
                  , term = viewTreeExpr exprTreeOpts . astDefExpr <$> defAST d
                  , type_ =
                      case d of
                        Def.DefAST d' -> viewTreeType $ astDefType d'
                        Def.DefPrim d' -> viewTreeType' $ labelNodes $ primDefType d'
                          where
                            labelNodes =
                              flip evalState (0 :: Int) . traverseOf _typeMeta \() -> do
                                n <- get
                                put $ n + 1
                                pure $ "primtype_" <> show d' <> "_" <> show n
                  }
            )
              <$> Map.assocs (moduleDefsQualified m)
        }

{- HLINT ignore ExprTreeOpts "Use newtype instead of data" -}
data ExprTreeOpts = ExprTreeOpts
  { patternsUnder :: Bool
  -- ^ Some renderers may struggle with aligning subtrees to the right.
  -- This option outputs trees where patterns are direct children of the `match` node instead.
  }
  deriving (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ExprTreeOpts
defaultExprTreeOpts :: ExprTreeOpts
defaultExprTreeOpts =
  ExprTreeOpts
    { patternsUnder = False
    }

-- | A simple method to extract 'Tree's from 'Expr's. This is injective.
viewTreeExpr :: ExprTreeOpts -> Expr -> Tree
viewTreeExpr opts@ExprTreeOpts{patternsUnder} e0 = case e0 of
  Hole _ e ->
    Tree
      { nodeId
      , flavor = FlavorHole
      , body = NoBody
      , childTrees = [viewTreeExpr opts e]
      , rightChild = Nothing
      }
  EmptyHole _ ->
    Tree
      { nodeId
      , flavor = FlavorEmptyHole
      , body = NoBody
      , childTrees = []
      , rightChild = Nothing
      }
  Ann _ e t ->
    Tree
      { nodeId
      , flavor = FlavorAnn
      , body = NoBody
      , childTrees = [viewTreeExpr opts e, viewTreeType t]
      , rightChild = Nothing
      }
  App _ e1 e2 ->
    Tree
      { nodeId
      , flavor = FlavorApp
      , body = NoBody
      , childTrees = [viewTreeExpr opts e1, viewTreeExpr opts e2]
      , rightChild = Nothing
      }
  APP _ e t ->
    Tree
      { nodeId
      , flavor = FlavorAPP
      , body = NoBody
      , childTrees = [viewTreeExpr opts e, viewTreeType t]
      , rightChild = Nothing
      }
  Con _ s ->
    Tree
      { nodeId
      , flavor = FlavorCon
      , body = TextBody $ showGlobal s
      , childTrees = []
      , rightChild = Nothing
      }
  Lam _ s e ->
    Tree
      { nodeId
      , flavor = FlavorLam
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr opts e]
      , rightChild = Nothing
      }
  LAM _ s e ->
    Tree
      { nodeId
      , flavor = FlavorLAM
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr opts e]
      , rightChild = Nothing
      }
  Var _ ref ->
    Tree
      { nodeId
      , flavor
      , body
      , childTrees = []
      , rightChild = Nothing
      }
    where
      (flavor, body) = case ref of
        GlobalVarRef n -> (FlavorGlobalVar, TextBody $ showGlobal n)
        LocalVarRef n -> (FlavorLocalVar, TextBody $ unName $ unLocalName n)
  Let _ s e1 e2 ->
    Tree
      { nodeId
      , flavor = FlavorLet
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr opts e1, viewTreeExpr opts e2]
      , rightChild = Nothing
      }
  LetType _ s t e ->
    Tree
      { nodeId
      , flavor = FlavorLetType
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr opts e, viewTreeType t]
      , rightChild = Nothing
      }
  Letrec _ s e1 t e2 ->
    Tree
      { nodeId
      , flavor = FlavorLetrec
      , body = TextBody $ unName $ unLocalName s
      , childTrees = [viewTreeExpr opts e1, viewTreeType t, viewTreeExpr opts e2]
      , rightChild = Nothing
      }
  Case _ e bs ->
    Tree
      { nodeId
      , flavor = FlavorCase
      , body = NoBody
      , childTrees
      , rightChild
      }
    where
      (childTrees, rightChild) =
        if patternsUnder
          then
            (
              [ viewTreeExpr opts e
              , Tree
                  { nodeId = nodeId <> "W" -- this will not clash with anything (see `boxId` etc.)
                  , flavor = FlavorCaseWith
                  , body = NoBody
                  , childTrees = zipWith viewCaseBranch [0 :: Int ..] bs
                  , rightChild = Nothing
                  }
              ]
            , Nothing
            )
          else
            ( [viewTreeExpr opts e]
            , -- seeing as the inner function always returns a `Just`,
              -- this would only be `Nothing` if the list of branches were empty,
              --  which should only happen when matching on `Void`
              ifoldr
                (\i b next -> Just $ (viewCaseBranch i b){rightChild = next})
                Nothing
                bs
            )
      viewCaseBranch i (CaseBranch con binds rhs) =
        let -- these IDs will not clash with any others in the tree,
            -- since node IDs in the input expression are unique,
            -- and don't contain non-numerical characters
            boxId = nodeId <> "P" <> show i
            patternRootId = boxId <> "B"
            patternBindAppID id = show id <> "A"
         in Tree
              { nodeId = boxId
              , flavor = FlavorPattern
              , body =
                  BoxBody $
                    foldl
                      ( \t (Bind m v) ->
                          let id = m ^. _id
                           in Tree
                                { nodeId = patternBindAppID id
                                , flavor = FlavorPatternApp
                                , body = NoBody
                                , childTrees =
                                    [ t
                                    , Tree
                                        { nodeId = show id
                                        , flavor = FlavorPatternBind
                                        , body = TextBody $ unName $ unLocalName v
                                        , childTrees = []
                                        , rightChild = Nothing
                                        }
                                    ]
                                , rightChild = Nothing
                                }
                      )
                      ( Tree
                          { nodeId = patternRootId
                          , flavor = FlavorPatternCon
                          , body = TextBody $ showGlobal con
                          , childTrees = []
                          , rightChild = Nothing
                          }
                      )
                      binds
              , childTrees = [viewTreeExpr opts rhs]
              , rightChild = Nothing
              }
  PrimCon _ pc ->
    Tree
      { nodeId
      , flavor = FlavorPrimCon
      , body = TextBody $ case pc of
          PrimChar c -> T.singleton c
          PrimInt c -> show c
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
      , flavor = FlavorTEmptyHole
      , body = NoBody
      , childTrees = []
      , rightChild = Nothing
      }
  THole _ t ->
    Tree
      { nodeId
      , flavor = FlavorTHole
      , body = NoBody
      , childTrees = [viewTreeType' t]
      , rightChild = Nothing
      }
  TCon _ n ->
    Tree
      { nodeId
      , flavor = FlavorTCon
      , body = TextBody $ showGlobal n
      , childTrees = []
      , rightChild = Nothing
      }
  TFun _ t1 t2 ->
    Tree
      { nodeId
      , flavor = FlavorTFun
      , body = NoBody
      , childTrees = [viewTreeType' t1, viewTreeType' t2]
      , rightChild = Nothing
      }
  TVar _ n ->
    Tree
      { nodeId
      , flavor = FlavorTVar
      , body = TextBody $ unName $ unLocalName n
      , childTrees = []
      , rightChild = Nothing
      }
  TApp _ t1 t2 ->
    Tree
      { nodeId
      , flavor = FlavorTApp
      , body = NoBody
      , childTrees = [viewTreeType' t1, viewTreeType' t2]
      , rightChild = Nothing
      }
  TForall _ n k t ->
    Tree
      { nodeId
      , flavor = FlavorTForall
      , body = TextBody $ withKindAnn $ unName $ unLocalName n
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
      , flavor = FlavorTLet
      , body = TextBody $ unName $ unLocalName n
      , childTrees = [viewTreeType' t, viewTreeType' b]
      , rightChild = Nothing
      }
  where
    nodeId = t0 ^. _typeMetaLens

showGlobal :: GlobalName k -> Text
showGlobal n = moduleNamePretty (qualifiedModule n) <> "." <> unName (baseName n)

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
  PrimerM m (Either ProgError [Name])
generateNames = curry $ logAPI (leftResultError GenerateNames) $ \(sid, ((defname, exprid), tk)) ->
  liftQueryAppM (handleQuestion $ GenerateName defname exprid tk) sid

evalStep ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  EvalReq ->
  PrimerM m (Either ProgError EvalResp)
evalStep = curry $ logAPI (leftResultError EvalStep) $ \(sid, req) ->
  liftEditAppM (handleEvalRequest req) sid

evalFull ::
  (MonadIO m, MonadThrow m, MonadAPILog l m, ConvertLogMessage EvalFullLog l) =>
  SessionId ->
  EvalFullReq ->
  PrimerM m (Either ProgError EvalFullResp)
evalFull = curry $ logAPI (leftResultError EvalFull) $ \(sid, req) ->
  liftEditAppM (handleEvalFullRequest req) sid

flushSessions :: (MonadIO m, MonadAPILog l m) => PrimerM m ()
flushSessions = logAPI' FlushSessions $ do
  sessionsTransaction $ \ss _ -> do
    StmMap.reset ss
  pure ()

availableActions ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Level ->
  Selection ->
  PrimerM m [Available.Action]
availableActions = curry3 $ logAPI (noError AvailableActions) $ \(sid, level, Selection{..}) -> do
  prog <- getProgram sid
  let allDefs = progAllDefs prog
      allTypeDefs = progAllTypeDefs prog
  (editable, ASTDef{astDefType = type_, astDefExpr = expr}) <- findDef allDefs def
  case node of
    Nothing ->
      pure $ Available.forDef (snd <$> allDefs) level editable def
    Just NodeSelection{..} -> do
      pure $ case nodeType of
        SigNode -> do
          Available.forSig level editable type_ id
        BodyNode -> do
          Available.forBody (snd <$> allTypeDefs) level editable expr id

-- TODO `logAPI`
inputAction' ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Level ->
  Selection ->
  Available.InputAction ->
  PrimerM m Available.Options
inputAction' sid level Selection{..} action = do
  app <- getApp sid
  let prog = appProg app
      allTypeDefs = progAllTypeDefs prog
      allDefs = progAllDefs prog
      id = node <&> \s -> s.id
  def' <- snd <$> findDef allDefs def
  maybe (throwM $ InputActionIDNotFound id) pure $
    Available.options
      (snd <$> allTypeDefs)
      (snd <$> allDefs)
      (progCxt prog)
      level
      def'
      id
      action

findDef :: MonadThrow m => Map GVarName (a, Def.Def) -> GVarName -> m (a, ASTDef)
findDef allDefs def = case allDefs Map.!? def of
  Nothing -> throwM $ UnknownDef def
  Just (_, Def.DefPrim _) -> throwM $ UnexpectedPrimDef def
  Just (editable, Def.DefAST d) -> pure (editable, d)

-- TODO tuple would be nice, but I don't think OpenAPI supports it - find where B previously worked around
data ApplyActionBody = ApplyActionBody
  { selection :: Selection
  , option :: Available.Option
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON ApplyActionBody

-- TODO `logAPI`
applyActionNoInput ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  Selection ->
  Available.NoInputAction ->
  PrimerM m Prog
applyActionNoInput sid selection action = do
  -- TODO DRY with above
  prog <- getProgram sid
  let allDefs = progAllDefs prog
      _allTypeDefs = progAllTypeDefs prog
  case allDefs Map.!? selection.def of
    Nothing -> throwM $ UnknownDef selection.def
    Just (_, Def.DefPrim _) -> throwM $ UnexpectedPrimDef selection.def
    Just (_, Def.DefAST def) -> do
      let patternsUnder = True -- TODO don't hardcode (then again, I expect the option itself to be short-lived)
      actions <-
        either (throwM . MiscPrimerErr) pure $
          mkActionNoInput
            (snd <$> progAllDefs prog)
            def
            selection.def
            (selection.node <&> \s -> (s.nodeType, s.id))
            action
      edit sid (App.Edit actions)
        >>= either
          (throwM . ApplyActionError actions)
          (pure . viewProg (ExprTreeOpts{patternsUnder}))

-- TODO DRY with above
applyActionInput ::
  (MonadIO m, MonadThrow m, MonadAPILog l m) =>
  SessionId ->
  ApplyActionBody ->
  Available.InputAction ->
  PrimerM m Prog
applyActionInput sid ApplyActionBody{selection, option} action = do
  -- TODO DRY with above
  prog <- getProgram sid
  let allDefs = progAllDefs prog
      _allTypeDefs = progAllTypeDefs prog
  case allDefs Map.!? selection.def of
    Nothing -> throwM $ UnknownDef selection.def
    Just (_, Def.DefPrim _) -> throwM $ UnexpectedPrimDef selection.def
    Just (_, Def.DefAST def) -> do
      let patternsUnder = True -- TODO don't hardcode (then again, I expect the option itself to be short-lived)
      actions <-
        either (throwM . MiscPrimerErr) pure $
          mkActionInput
            def
            selection.def
            (selection.node <&> \s -> (s.nodeType, s.id))
            option
            action
      edit sid (App.Edit actions)
        >>= either
          (throwM . ApplyActionError actions)
          (pure . viewProg (ExprTreeOpts{patternsUnder}))

-- | 'App.Selection' without any node metadata.
data Selection = Selection
  { def :: GVarName
  , node :: Maybe NodeSelection
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Selection

convertSelection :: App.Selection -> Selection
convertSelection App.Selection{..} = Selection{def = selectedDef, node = convertNodeSelection <$> selectedNode}

-- | 'App.NodeSelection' without any node metadata.
data NodeSelection = NodeSelection
  { nodeType :: NodeType
  , id :: ID
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON NodeSelection

convertNodeSelection :: App.NodeSelection -> NodeSelection
convertNodeSelection sel@App.NodeSelection{nodeType} = NodeSelection{nodeType, id = getID sel}
