{-# LANGUAGE GADTs #-}

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
  PrimerM,
  PrimerErr (..),
  newSession,
  copySession,
  listSessions,
  getVersion,
  Tree,
  Prog,
  Def,
  getProgram,
  getSessionName,
  renameSession,
  edit,
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
  flushSessions,
  -- viewTree*: only exported for testing
  viewTreeType,
  viewTreeExpr,
) where

import Foreword

import Control.Concurrent.STM (
  STM,
  TBQueue,
  atomically,
  newEmptyTMVar,
  takeTMVar,
  writeTBQueue,
 )
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson (ToJSON)
import Data.Data (showConstr, toConstr)
import qualified Data.Generics.Uniplate.Data as U
import qualified Data.Map as Map
import qualified ListT (toList)
import Primer.App (
  App,
  EditAppM,
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  InitialApp,
  MutationRequest,
  ProgError,
  QueryAppM,
  Question (..),
  handleEvalFullRequest,
  handleEvalRequest,
  handleGetProgramRequest,
  handleMutationRequest,
  handleQuestion,
  initialApp,
  progModule,
  runEditAppM,
  runQueryAppM,
 )
import qualified Primer.App as App
import Primer.Core (
  ASTDef (..),
  Expr,
  Expr' (APP, Ann, LetType, Letrec, PrimCon, Var),
  GVarName,
  ID,
  Kind,
  LVarName,
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type,
  Type' (TForall),
  defAST,
  defName,
  defType,
  getID,
  typeDefName,
  unLocalName,
 )
import Primer.Database (
  OffsetLimit,
  Page,
  Session (Session),
  SessionData (..),
  SessionId,
  Sessions,
  Version,
  defaultSessionName,
  fromSessionName,
  newSessionId,
  pageList,
  safeMkSessionName,
 )
import qualified Primer.Database as Database (
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
import Primer.Module (Module (moduleDefs, moduleTypes))
import Primer.Name (Name, unName)
import qualified StmContainers.Map as StmMap

data Env = Env
  { sessions :: Sessions
  , dbOpQueue :: TBQueue Database.Op
  , version :: Version
  }

type PrimerM m = ReaderT Env m

{- HLINT ignore PrimerErr "Use newtype instead of data" -}

-- | Primer exception class.
data PrimerErr = DatabaseErr Text deriving (Show)

instance Exception PrimerErr

sessionsTransaction :: (MonadIO m) => (Sessions -> TBQueue Database.Op -> STM a) -> PrimerM m a
sessionsTransaction f = do
  ss <- asks sessions
  q <- asks dbOpQueue
  liftIO $ atomically $ f ss q

data SessionOp a where
  EditApp :: (App -> (a, App)) -> SessionOp a
  QueryApp :: (App -> a) -> SessionOp a
  GetSessionName :: SessionOp Text
  GetSessionData :: SessionOp SessionData
  RenameSession :: Text -> SessionOp Text

-- A note about the implementation here. When the session is missing
-- from the in-memory database, we can't queue the database request to
-- load the session *and* wait for the database thread's asynchronous
-- reply in the same STM transaction. That's because the wait will
-- immediately block until the reply has been received, preventing the
-- transaction from completing, but the database request won't
-- actually be sent until the transaction is complete. This would
-- cause a deadlock!
withSession' :: (MonadIO m, MonadThrow m) => SessionId -> SessionOp a -> PrimerM m a
withSession' sid op = do
  hndl <- sessionsTransaction $ \ss q -> do
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
      Just s@(SessionData appl n) ->
        -- The session is in memory, let's do this.
        case op of
          EditApp f -> do
            let (res, appl') = f appl
            StmMap.insert (SessionData appl' n) sid ss
            writeTBQueue q $ Database.UpdateApp sid appl'
            pure $ Right res
          QueryApp f -> pure $ Right $ f appl
          GetSessionName -> pure $ Right (fromSessionName n)
          GetSessionData -> pure $ Right s
          RenameSession n' ->
            let newName = safeMkSessionName n'
             in do
                  StmMap.insert (SessionData appl newName) sid ss
                  writeTBQueue q $ Database.UpdateName sid newName
                  pure $ Right (fromSessionName newName)
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
    Right result ->
      -- We performed the session transaction, now return the result.
      pure result

newSession :: (MonadIO m) => InitialApp -> PrimerM m SessionId
newSession a = do
  nextSID <- liftIO newSessionId
  sessionsTransaction $ \ss q -> do
    let app = initialApp a
    StmMap.insert (SessionData app defaultSessionName) nextSID ss
    writeTBQueue q $ Database.Insert nextSID app defaultSessionName
    pure nextSID

-- | Copy the given session to a new session, and return the new
-- session's ID.
--
-- We implement this as 2 separate transactions: 1 to retrieve the
-- source session, and 1 to insert the copy. Semantically, this is
-- fine, and it should be more fair on a busy system than a single
-- transaction which takes longer.
copySession :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m SessionId
copySession srcId = do
  copy <- withSession' srcId GetSessionData
  nextSID <- liftIO newSessionId
  sessionsTransaction $ \ss q -> do
    StmMap.insert copy nextSID ss
    writeTBQueue q $ Database.Insert nextSID (sessionApp copy) (sessionName copy)
    pure nextSID

-- If the input is 'False', return all sessions in the database;
-- otherwise, only the in-memory sessions.
--
-- Currently the pagination support is "extract the whole list from the DB,
-- then select a portion". This should be improved to only extract the
-- appropriate section from the DB in the first place.
listSessions :: (MonadIO m) => Bool -> OffsetLimit -> PrimerM m (Page Session)
listSessions False ol = do
  q <- asks dbOpQueue
  callback <- liftIO $
    atomically $ do
      cb <- newEmptyTMVar
      writeTBQueue q $ Database.ListSessions ol cb
      return cb
  liftIO $ atomically $ takeTMVar callback
listSessions _ ol = sessionsTransaction $ \ss _ -> do
  kvs' <- ListT.toList $ StmMap.listT ss
  let kvs = uncurry Session . second sessionName <$> kvs'
  pure $ pageList ol kvs

getVersion :: (Monad m) => PrimerM m Version
getVersion = asks version

getSessionName :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m Text
getSessionName sid = withSession' sid GetSessionName

renameSession :: (MonadIO m, MonadThrow m) => SessionId -> Text -> PrimerM m Text
renameSession sid n = withSession' sid $ RenameSession n

-- Run an 'EditAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftEditAppM :: (MonadIO m, MonadThrow m) => EditAppM a -> SessionId -> PrimerM m (Either ProgError a)
liftEditAppM h sid = withSession' sid (EditApp $ runEditAppM h)

-- Run a 'QueryAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftQueryAppM :: (MonadIO m, MonadThrow m) => QueryAppM a -> SessionId -> PrimerM m (Either ProgError a)
liftQueryAppM h sid = withSession' sid (QueryApp $ runQueryAppM h)

getProgram :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m Prog
getProgram sid = withSession' sid $ QueryApp $ viewProg . handleGetProgramRequest

-- | A frontend will be mostly concerned with rendering, and does not need the
-- full complexity of our AST for that task. 'Tree' is a simplified view with
-- just enough information to render nicely.
-- (NB: currently this is just a first draft, and is expected to evolve.)
data Tree = Tree
  { nodeId :: ID
  , label :: Text
  , childTrees :: [Tree]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Tree

-- | This type is the API's view of a 'App.Prog'
-- (this is expected to evolve as we flesh out the API)
data Prog = Prog
  { types :: [TyConName]
  , -- We don't use Map Name Def as it is rather redundant since each
  -- Def carries a name field, and it is difficult to enforce that
  -- "the keys of this object match the name field of the
  -- corresponding value".
  defs :: [Def]
  }
  deriving (Generic)

instance ToJSON Prog

-- | This type is the api's view of a 'Primer.Core.Def'
-- (this is expected to evolve as we flesh out the API)
data Def = Def
  { name :: GVarName
  , type_ :: Tree
  , term :: Maybe Tree
  -- ^ definitions with no associated tree are primitives
  }
  deriving (Generic)

instance ToJSON Def

viewProg :: App.Prog -> Prog
viewProg p =
  Prog
    { types = typeDefName <$> Map.elems (moduleTypes $ progModule p)
    , defs =
        ( \d ->
            Def
              { name = defName d
              , type_ = viewTreeType $ defType d
              , term = viewTreeExpr . astDefExpr <$> defAST d
              }
        )
          <$> Map.elems (moduleDefs $ progModule p)
    }

-- | A simple method to extract 'Tree's from 'Expr's. This is injective.
-- Currently it is designed to be simple and just enough to enable
-- experimenting with rendering on the frontend.
--
-- It is expected to evolve in the future.
viewTreeExpr :: Expr -> Tree
viewTreeExpr = U.para $ \e exprChildren ->
  let c = case e of
        -- We need to disambiguate between local and global references
        -- as using uniplate to extract the names will get the name inside
        -- the TmVarRef, rendering both a local and global as 'Var x' if
        -- we did not have this special case.
        Var _ (LocalVarRef _) -> "LVar"
        Var _ (GlobalVarRef _) -> "GVar"
        _ -> toS $ showConstr $ toConstr e
      n = case e of
        PrimCon _ pc -> case pc of
          PrimChar c' -> show c'
          PrimInt c' -> show c'
        _ -> unwords $ c : map unName (U.childrenBi e)
      -- add info about type children
      allChildren = case e of
        Ann _ _ ty -> exprChildren ++ [viewTreeType ty]
        APP _ _ ty -> exprChildren ++ [viewTreeType ty]
        LetType _ _ ty _ -> viewTreeType ty : exprChildren
        Letrec _ _ _ ty _ -> let (h, t) = splitAt 1 exprChildren in h ++ viewTreeType ty : t
        -- otherwise, no type children
        _ -> exprChildren
   in Tree (getID e) n allChildren

-- | Similar to 'viewTreeExpr', but for 'Type's
viewTreeType :: Type -> Tree
viewTreeType = U.para $ \e allChildren ->
  let c = toS $ showConstr $ toConstr e
      n = case e of
        TForall _ m k _ -> c <> " " <> unName (unLocalName m) <> ":" <> show k
        _ -> unwords $ c : map unName (U.childrenBi e)
   in Tree (getID e) n allChildren

edit :: (MonadIO m, MonadThrow m) => SessionId -> MutationRequest -> PrimerM m (Either ProgError App.Prog)
edit sid req = liftEditAppM (handleMutationRequest req) sid

variablesInScope ::
  (MonadIO m, MonadThrow m) =>
  SessionId ->
  (GVarName, ID) ->
  PrimerM m (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
variablesInScope sid (defname, exprid) =
  liftQueryAppM (handleQuestion (VariablesInScope defname exprid)) sid

generateNames :: (MonadIO m, MonadThrow m) => SessionId -> ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind)) -> PrimerM m (Either ProgError [Name])
generateNames sid ((defname, exprid), tk) =
  liftQueryAppM (handleQuestion $ GenerateName defname exprid tk) sid

evalStep :: (MonadIO m, MonadThrow m) => SessionId -> EvalReq -> PrimerM m (Either ProgError EvalResp)
evalStep sid req =
  liftEditAppM (handleEvalRequest req) sid

evalFull :: (MonadIO m, MonadThrow m) => SessionId -> EvalFullReq -> PrimerM m (Either ProgError EvalFullResp)
evalFull sid req =
  liftEditAppM (handleEvalFullRequest req) sid

flushSessions :: (MonadIO m) => PrimerM m ()
flushSessions = do
  sessionsTransaction $ \ss _ -> do
    StmMap.reset ss
  pure ()
