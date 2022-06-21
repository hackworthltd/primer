{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Primer.Database (
  SessionId,
  -- 'SessionName' is abstract. Do not export its constructors.
  SessionName,
  mkSessionName,
  safeMkSessionName,
  fromSessionName,
  defaultSessionName,
  Session (..),
  SessionData (..),
  Sessions,
  newSessionId,
  LastModified (..),
  getCurrentTime,
  Op (..),
  discardOp,
  OpStatus (..),
  OffsetLimit (..),
  Page (..),
  pageList,
  ServiceCfg (..),
  MonadDb (..),
  DbError (..),
  NullDbT (..),
  runNullDbT,
  NullDb,
  runNullDb,
  runNullDb',
  NullDbException (..),
  Version,
  serve,
) where

import Foreword

import Control.Concurrent.STM (
  TBQueue,
  TMVar,
  peekTBQueue,
  putTMVar,
  readTBQueue,
 )
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Text qualified as Text (
  strip,
  take,
  takeWhile,
 )
import Data.Time.Clock (
  UTCTime,
 )
import Data.Time.Clock qualified as UTC (
  getCurrentTime,
 )
import Data.UUID (UUID)
import Data.UUID qualified as UUID (toText)
import Data.UUID.V4 (nextRandom)
import ListT qualified (toList)
import Optics (
  (.~),
 )
import Primer.App (App)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import StmContainers.Map qualified as StmMap

-- | A Primer version.
--
-- At the moment, this is just a timestamp or Git rev. In the future,
-- it should be a proper version that represents language versions.
type Version = Text

-- | A Primer session ID.
--
-- This type is defined in this module to avoid a circular dependency
-- between it and "Primer.Server".
type SessionId = UUID

-- | A human-friendly Primer session name.
--
-- Some names are more friendly than others. Therefore, this type is
-- abstract and cannot be constructed directly from 'Text'. See
-- 'mkSessionName'.
newtype SessionName = SessionName Text
  deriving (Generic, Eq, Ord, Show, Read)
  deriving newtype (FromJSON, ToJSON)

-- | Given some 'Text', try to convert it to a 'SessionName'.
--
-- This function performs the following operations on the input:
--
--   * Strips leading and trailing whitespace.
--   * Truncates the input at 64 Unicode scalar values.
--   * Truncates the input at the first newline, if any.
--
-- If the resuling text is not a valid 'SessionName', this function
-- returns 'Nothing'.
--
-- Ideally, this function would take a filter function argument that
-- could be tuned to a particular use case or audience. However, this
-- is not as straightforward as it might sound: one implication of
-- such a design would be that we'd need to identify somewhere in the
-- database which filter was used (probably with support for
-- migrations, in case the filter is altered!), so that we know how to
-- transform raw text stored in the database back to a valid
-- 'SessionName'.
--
-- Regardless, any filters done here should be a subset of what our
-- database supports for string values. So long as we're truncating
-- the input string to some reasonable length, we should be in good
-- shape.
mkSessionName :: Text -> Maybe SessionName
mkSessionName "" = Nothing
mkSessionName t =
  let t' = Text.take 64 (Text.strip t)
   in case Text.takeWhile (/= '\n') t' of
        "" -> Nothing
        n -> Just $ SessionName n

-- | Given some 'Text', convert it to a 'SessionName'. If the original
-- 'Text' is not a valid 'SessionName', return the
-- 'defaultSessionName', instead.
safeMkSessionName :: Text -> SessionName
safeMkSessionName t = fromMaybe defaultSessionName (mkSessionName t)

-- | Given a 'SessionName', return its 'Text'.
fromSessionName :: SessionName -> Text
fromSessionName (SessionName t) = t

-- | The default session name.
defaultSessionName :: SessionName
defaultSessionName = SessionName "Untitled Program"

-- | This newtype is a workaround for the fact that our OpenAPI 3
-- implementation serializes 'UTCTime' with a custom string format,
-- rather than using the standard @date-time@ format.
--
-- See:
--
-- https://github.com/hackworthltd/primer/issues/735
-- https://github.com/biocad/openapi3/issues/16
newtype LastModified = LastModified {utcTime :: UTCTime}
  deriving (Generic, Eq, Ord, Show, Read)
  deriving newtype (FromJSON, ToJSON)

-- | Convenience wrapper around 'UTC.getCurrentTime'.
getCurrentTime :: (MonadIO m) => m LastModified
getCurrentTime = LastModified <$> liftIO UTC.getCurrentTime

-- | Bulk-queryable per-session information. See also 'SessionData'.
data Session = Session
  { id :: SessionId
  -- ^ The session ID.
  , name :: SessionName
  -- ^ The session's name.
  , lastModified :: LastModified
  -- ^ The last time the session was modified. See
  -- 'SessionData.lastModified' for details.
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Session

-- | Per-session information.
data SessionData = SessionData
  { sessionApp :: App
  -- ^ The session's 'App'.
  , sessionName :: SessionName
  -- ^ The session's name.
  , lastModified :: LastModified
  -- ^ The last time the session was modified (or when it was created,
  -- if it hasn't yet been modified). This accounts for modifications
  -- either to the session's name, or to its corresponding 'App'.
  }
  deriving (Show, Eq, Generic)

-- | An in-memory cache of sessions. This type maps 'SessionId's to 'App's.
type Sessions = StmMap.Map SessionId SessionData

-- | Create a new, unique session ID.
newSessionId :: IO SessionId
newSessionId = nextRandom

-- | A database operation status, for operations that need to
-- communicate their success or failure to callers.
data OpStatus
  = -- | Success
    Success
  | -- | Failure, with message.
    Failure !Text

-- | A database operation.
--
-- The semantics of (and invariants assumed by) these operations
-- cannot be encoded in the type, so we assume that the functions
-- which act on the 'Op' type have implemented those semantics and
-- invariants faithfully. The 'serve' function provided by this module
-- implements the proper in-order/FIFO semantics of the database
-- operation queue, but relies on implementations of the 'MonadDb'
-- type class to implement the persistent database semantics.
-- 'Primer.API.withSession'' implements the in-memory database and
-- database operation queue invariants, unless otherwise specified.
--
-- Note that operations which modify or create sessions must provide a
-- modification/creation timestamp. For typical operations, generating
-- the timestamp could be deferred to the database engine, but this
-- implementation is more flexible, because it permits replay and
-- restoration of sessions during a migration, from a database dump,
-- etc.
data Op
  = -- | Insert a new session ID and 'App' with the given session name
    -- and creation date.
    Insert !SessionId !App !SessionName !LastModified
  | -- | Update the 'App' associated with the given session ID and
    -- specified timestamp.
    UpdateApp !SessionId !App !LastModified
  | -- | Update the session name associated with the given session ID
    -- and specified timestamp.
    UpdateName !SessionId !SessionName !LastModified
  | -- | Query the database for a session with the given ID. If found,
    -- insert it into the given in-memory database. Then signal the
    -- caller via the supplied 'TMVar' whether the lookup was
    -- successful or not
    LoadSession !SessionId !Sessions !(TMVar OpStatus)
  | -- | Query the database for a session with the given ID. If found,
    -- delete it from the database, then signal the caller via the
    -- supplied 'TMVar' whether the lookup was successful or not.
    --
    -- N.B. for implementers: this operation does *not* delete the
    -- session from the in-memory session database; that is the
    -- responsibility of the caller. So long as the caller atomically
    -- deletes the session from the in-memory database and inserts the
    -- corresponding 'DeleteSession' operation into the database
    -- operation queue, then we can be certain that any subsequent
    -- operations which try to act on the deleted session will fail,
    -- rather than being mistakenly executed, because of the following
    -- invariants:
    --
    -- 1. Any subsequent operation on the deleted session will look up
    -- the session in the in-memory database, fail to find it there,
    -- insert a 'LoadSession' operation into the database operation
    -- queue, and wait for the 'LoadSession' operation to complete.
    --
    -- 2. The 'LoadSession' operation will not be processed until
    -- after the earlier 'DeleteSession' operation, because the
    -- database operation queue is a FIFO.
    --
    -- 3. Therefore, the 'LoadSession' will fail because the session
    -- will have been deleted from the database before the
    -- 'LoadSession' operation is tried.
    --
    -- 4. Furthermore, the operation which is waiting on the
    -- 'LoadSession' will observe that the 'LoadSession' operation
    -- failed, and handle the failure accordingly. In any case,
    -- there's no possibility that the session will have been
    -- mistakenly re-loaded into the in-memory database.
    DeleteSession !SessionId !(TMVar OpStatus)
  | -- | Get the list of all sessions (and their names) in the
    -- database.
    ListSessions !OffsetLimit !(TMVar (Page Session))

-- | A config for the 'serve' computation.
data ServiceCfg = ServiceCfg
  { opQueue :: TBQueue Op
  -- ^ The database operation queue.
  , version :: Version
  -- ^ The running version of Primer.
  }

-- | Discard the next operation in the queue.
--
-- This is useful when handling exceptions.
discardOp :: MonadIO m => TBQueue Op -> m ()
discardOp q = liftIO $ atomically $ void $ readTBQueue q

-- | A 'Page' is a portion of the results of some DB query, along with the
-- total number of results.
data Page a = Page {total :: Int, pageContents :: [a]}
  deriving (Show)

-- | Enable extracting a subset of the results of a query, for later
-- pagination.
data OffsetLimit = OL {offset :: !Int, limit :: Maybe Int}
  deriving (Show)

-- | If one has all the results at hand, it is trivial to extract a page.
pageList :: OffsetLimit -> [a] -> Page a
pageList (OL{offset, limit}) xs =
  Page
    { total = length xs
    , pageContents = maybe identity take limit $ drop offset xs
    }

-- | A monad type class for Primer database operations.
class (Monad m) => MonadDb m where
  -- | Insert a session into the database.
  --
  -- Corresponds to the 'Insert' operation.
  insertSession :: Version -> SessionId -> App -> SessionName -> LastModified -> m ()

  -- | Update an 'App'.
  --
  -- Corresponds to the 'UpdateApp' operation.
  updateSessionApp :: Version -> SessionId -> App -> LastModified -> m ()

  -- | Update a session's name.
  --
  -- Corresponds to the 'UpdateName' operation.
  updateSessionName :: Version -> SessionId -> SessionName -> LastModified -> m ()

  -- | Get a page of the list of all session IDs and their names.
  --
  -- Corresponds to the 'ListSessions' operation.
  listSessions :: OffsetLimit -> m (Page Session)

  -- | Delete the session whose ID is given.
  --
  -- Corresponds to the 'DeleteSession' operation.
  --
  -- Note: this operation returns 'Right ()' for a successful result,
  -- but a subsequent version will return a more informative success
  -- status, once this operation is more flexible (e.g., whether the
  -- deletion was immediate, or will be performed in the future).
  deleteSession :: SessionId -> m (Either DbError ())

  -- | Query a session ID from the database.
  --
  -- Returns 'Left' with a 'DbError' if the query failed (session
  -- doesn't exist), 'Right' with the 'SessionData' if successful.
  querySessionId :: SessionId -> m (Either DbError SessionData)

-- | Routine errors that can occur during 'MonadDb' computations.
--
-- Note that these errors represent database results that should not
-- be considered exceptional. For example, if querying the database
-- for a particular name or session ID returns 0 results, that might
-- indicate that the student has provided (either directly or
-- indirectly) invalid input; e.g., an invalid session ID. However,
-- that result would not be considered worthy of throwing an
-- exception, since it's not indicative of a bug or an impairment of
-- service.
--
-- More serious and/or unusual errors should be handled out-of-band
-- via GHC's exception system; i.e., by throwing an exception. As
-- these sorts of exceptions are typically very
-- implementation-specific, they're considered out of scope for core
-- Primer and are left to the implementer of each specific database
-- connector.
--
-- A good general rule of thumb is this: if the occurrence of the
-- error is something we should be alerted about, then it should
-- probably be an exception, not a 'DbError' value.

{- HLINT ignore DbError "Use newtype instead of data" -}
data DbError
  = -- | A database operation failed because the given 'SessionId'
    -- wasn't found in the database.
    SessionIdNotFound SessionId
  deriving (Eq, Show, Generic)

-- | A "null" database type with no persistent backing store.
--
-- This type is only useful for mocking/testing or "toy" environments.
-- Do not use this type in production!
newtype NullDbT m a = NullDbT {unNullDbT :: ReaderT Sessions m a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader Sessions
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

-- | The 'NullDbT' monad transformer applied to 'IO'.
type NullDb a = NullDbT IO a

-- | A simple 'Exception' type for 'NullDb' computations.
newtype NullDbException = NullDbException Text
  deriving (Eq, Show)

instance Exception NullDbException

instance (MonadThrow m, MonadIO m) => MonadDb (NullDbT m) where
  insertSession _ id_ a n t = do
    ss <- ask
    result <- liftIO $
      atomically $ do
        lookup <- StmMap.lookup id_ ss
        case lookup of
          Nothing -> do
            StmMap.insert (SessionData a n t) id_ ss
            pure $ Right ()
          Just _ -> pure $ Left $ NullDbException "insertSession failed because session already exists"
    case result of
      Left e -> throwM e
      Right _ -> pure ()
  updateSessionApp _ id_ a t = ask >>= updateOrFail id_ (\s -> s & #sessionApp .~ a & #lastModified .~ t)
  updateSessionName _ id_ n t = ask >>= updateOrFail id_ (\s -> s & #sessionName .~ n & #lastModified .~ t)
  listSessions ol = do
    ss <- ask
    kvs <- liftIO $ atomically $ ListT.toList $ StmMap.listT ss
    -- Sorting these by name isn't required by the `MonadDb`
    -- specification, but it's useful for the moment for testing. A
    -- later version of the `MonadDb` interface will support sorting
    -- by various keys. See:
    --
    -- https://github.com/hackworthltd/primer/issues/533
    pure $ pageList ol $ sortOn name $ (\(i, SessionData _ n t) -> Session i n t) <$> kvs
  deleteSession id_ = do
    ss <- ask
    liftIO $ atomically $ do
      lookup <- StmMap.lookup id_ ss
      case lookup of
        Nothing -> pure $ Left $ SessionIdNotFound id_
        Just _ -> do
          StmMap.delete id_ ss
          pure $ Right ()
  querySessionId sid = do
    ss <- ask
    lookup <- liftIO $ atomically $ StmMap.lookup sid ss
    case lookup of
      Nothing -> pure $ Left $ SessionIdNotFound sid
      Just s -> pure $ Right s

updateOrFail :: (MonadThrow m, MonadIO m) => SessionId -> (SessionData -> SessionData) -> Sessions -> m ()
updateOrFail id_ f ss = do
  result <- liftIO $
    atomically $ do
      lookup <- StmMap.lookup id_ ss
      case lookup of
        Nothing -> pure $ Left $ NullDbException "updateSessionName lookup failed"
        Just s -> do
          StmMap.insert (f s) id_ ss
          pure $ Right ()
  case result of
    Left e -> throwM e
    Right _ -> pure ()

-- | Run a 'NullDbT' action in a transformer stack with the given
-- initial 'Sessions' database.
runNullDbT :: Sessions -> NullDbT m a -> m a
runNullDbT ss m = runReaderT (unNullDbT m) ss

-- | Run a 'NullDb' action in 'IO' with the given initial 'Sessions'
-- database.
runNullDb :: Sessions -> NullDb a -> IO a
runNullDb = runNullDbT

-- | Run a 'NullDb' action in 'IO' with an empty initial database.
runNullDb' :: NullDb a -> IO a
runNullDb' m = do
  sessions <- StmMap.newIO
  runNullDb sessions m

-- | The database service computation.
--
-- Because it will block, this computation should be run on its own
-- thread.
serve :: (MonadDb m, MonadIO m) => ServiceCfg -> m Void
serve (ServiceCfg q v) =
  forever $ do
    -- Don't remove the op from the queue until we're certain it
    -- has completed; i.e., if an exception occurs while
    -- performing the corresponding database operation, op should
    -- remain in the queue so that the caller can decide whether
    -- to discard it or retry it.
    op <- liftIO $ atomically $ peekTBQueue q
    perform op
    discardOp q
  where
    perform (Insert s a n t) = insertSession v s a n t
    perform (UpdateApp s a t) = updateSessionApp v s a t
    perform (UpdateName s n t) = updateSessionName v s n t
    perform (ListSessions ol result) = do
      ss <- listSessions ol
      liftIO $ atomically $ putTMVar result ss
    perform (LoadSession sid memdb status) = do
      -- Note that we split the in-memory session insertion (i.e.,
      -- the 'StmMap.insert') and the signal to the caller (i.e.,
      -- the 'putTMVar') across 2 'atomically' blocks. This is
      -- fine, because the signal to the caller doesn't mean that
      -- they have exclusive access to the session, only that they
      -- can try their session transaction again.
      result <- loadSession
      liftIO $ atomically $ putTMVar status result
      where
        loadSession = do
          queryResult <- querySessionId sid
          case queryResult of
            Left (SessionIdNotFound s) ->
              pure $ Failure $ "Couldn't load the requested session: no such session ID " <> UUID.toText s
            Right sd -> do
              liftIO $ atomically $ StmMap.insert sd sid memdb
              pure Success
    perform (DeleteSession sid status) = do
      result <- delete
      liftIO $ atomically $ putTMVar status result
      where
        delete = do
          deletionResult <- deleteSession sid
          case deletionResult of
            Left (SessionIdNotFound s) ->
              pure $ Failure $ "Couldn't delete the requested session: no such session ID " <> UUID.toText s
            Right _ -> pure Success
