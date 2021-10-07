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
  getProgram,
  getSessionName,
  renameSession,
  edit,
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
  flushSessions,
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
  Prog,
  ProgError,
  QueryAppM,
  Question (..),
  Result (..),
  handleEvalFullRequest,
  handleEvalRequest,
  handleGetProgramRequest,
  handleMutationRequest,
  handleQuestion,
  initialApp,
  runEditAppM,
  runQueryAppM,
 )
import Primer.Core (
  ID,
  Kind,
  Type',
 )
import Primer.Database (
  Session (Session),
  SessionData (..),
  SessionId,
  Sessions,
  Version,
  defaultSessionName,
  fromSessionName,
  newSessionId,
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
import Primer.Name (Name)
import qualified StmContainers.Map as StmMap

data Env = Env
  { sessions :: Sessions
  , dbOpQueue :: TBQueue Database.Op
  , version :: Version
  }

type PrimerM m = ReaderT Env m

{- HLINT ignore "Use newtype instead of data" -}

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
listSessions :: (MonadIO m) => Bool -> PrimerM m [Session]
listSessions False = do
  q <- asks dbOpQueue
  callback <- liftIO $
    atomically $ do
      cb <- newEmptyTMVar
      writeTBQueue q $ Database.ListSessions cb
      return cb
  liftIO $ atomically $ takeTMVar callback
listSessions _ = sessionsTransaction $ \ss _ -> do
  kvs <- ListT.toList $ StmMap.listT ss
  pure $ uncurry Session . second sessionName <$> kvs

getVersion :: (Monad m) => PrimerM m Version
getVersion = asks version

getSessionName :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m Text
getSessionName sid = withSession' sid GetSessionName

renameSession :: (MonadIO m, MonadThrow m) => SessionId -> Text -> PrimerM m Text
renameSession sid n = withSession' sid $ RenameSession n

-- Run an 'EditAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftEditAppM :: (MonadIO m, MonadThrow m) => EditAppM a -> SessionId -> PrimerM m (Result ProgError a)
liftEditAppM h sid = withSession' sid (EditApp $ runEditAppM h)

-- Run a 'QueryAppM' action, using the given session ID to look up and
-- pass in the app state for that session.
liftQueryAppM :: (MonadIO m, MonadThrow m) => QueryAppM a -> SessionId -> PrimerM m (Result ProgError a)
liftQueryAppM h sid = withSession' sid (QueryApp $ runQueryAppM h)

getProgram :: (MonadIO m, MonadThrow m) => SessionId -> PrimerM m (Result ProgError Prog)
getProgram = liftQueryAppM handleGetProgramRequest

edit :: (MonadIO m, MonadThrow m) => SessionId -> MutationRequest -> PrimerM m (Result ProgError Prog)
edit sid req = liftEditAppM (handleMutationRequest req) sid

variablesInScope :: (MonadIO m, MonadThrow m) => SessionId -> (ID, ID) -> PrimerM m (Result ProgError (([(Name, Kind)], [(Name, Type' ())]), [(ID, Name, Type' ())]))
variablesInScope sid (defid, exprid) =
  liftQueryAppM (handleQuestion (VariablesInScope defid exprid)) sid

generateNames :: (MonadIO m, MonadThrow m) => SessionId -> ((ID, ID), Either (Maybe (Type' ())) (Maybe Kind)) -> PrimerM m (Result ProgError [Name])
generateNames sid ((defid, exprid), tk) =
  liftQueryAppM (handleQuestion $ GenerateName defid exprid tk) sid

evalStep :: (MonadIO m, MonadThrow m) => SessionId -> EvalReq -> PrimerM m (Result ProgError EvalResp)
evalStep sid req =
  liftEditAppM (handleEvalRequest req) sid

evalFull :: (MonadIO m, MonadThrow m) => SessionId -> EvalFullReq -> PrimerM m (Result ProgError EvalFullResp)
evalFull sid req =
  liftEditAppM (handleEvalFullRequest req) sid

flushSessions :: (MonadIO m) => PrimerM m ()
flushSessions = do
  sessionsTransaction $ \ss _ -> do
    StmMap.reset ss
  pure ()
