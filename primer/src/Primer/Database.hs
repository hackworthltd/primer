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

-- | Bulk-queryable per-session information
-- See also 'SessionData'.
data Session = Session {id :: SessionId, name :: SessionName}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Session

-- | Per-session information.
data SessionData = SessionData
  { sessionApp :: App
  -- ^ The session's 'App'.
  , sessionName :: SessionName
  -- ^ The session's name.
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
data Op
  = -- | Insert a new session ID and 'App' with the given session name.
    Insert !SessionId !App !SessionName
  | -- | Update the 'App' associated with the given session ID.
    UpdateApp !SessionId !App
  | -- | Update the session name associated with the given session ID.
    UpdateName !SessionId !SessionName
  | -- | Query the database for a session with the given ID. If found,
    -- insert it into the given in-memory database. Then signal the
    -- caller via the supplied 'TMVar' whether the lookup was
    -- successful or not
    LoadSession !SessionId !Sessions !(TMVar OpStatus)
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

-- | Enable extracting a subset of the results of a query, for later
-- pagination.
data OffsetLimit = OL {offset :: !Int, limit :: Maybe Int}

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
  insertSession :: Version -> SessionId -> App -> SessionName -> m ()

  -- | Update an 'App'.
  --
  -- Corresponds to the 'UpdateApp' operation.
  updateSessionApp :: Version -> SessionId -> App -> m ()

  -- | Update a session's name.
  --
  -- Corresponds to the 'UpdateName' operation.
  updateSessionName :: Version -> SessionId -> SessionName -> m ()

  -- | Get a page of the list of all session IDs and their names.
  --
  -- Corresponds to the 'ListSessions' operation.
  listSessions :: OffsetLimit -> m (Page Session)

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
  insertSession _ id_ a n = do
    ss <- ask
    result <- liftIO $
      atomically $ do
        lookup <- StmMap.lookup id_ ss
        case lookup of
          Nothing -> do
            StmMap.insert (SessionData a n) id_ ss
            pure $ Right ()
          Just _ -> pure $ Left $ NullDbException "insertSession failed because session already exists"
    case result of
      Left e -> throwM e
      Right _ -> pure ()
  updateSessionApp _ id_ a = ask >>= updateOrFail id_ (\s -> s & #sessionApp .~ a)
  updateSessionName _ id_ n = ask >>= updateOrFail id_ (\s -> s & #sessionName .~ n)
  listSessions ol = do
    ss <- ask
    kvs <- liftIO $ atomically $ ListT.toList $ StmMap.listT ss
    -- Sorting these by name isn't required by the `MonadDb`
    -- specification, but it's useful for the moment for testing. A
    -- later version of the `MonadDb` interface will support sorting
    -- by various keys. See:
    --
    -- https://github.com/hackworthltd/primer/issues/533
    pure $ pageList ol $ sortOn name $ uncurry Session . second sessionName <$> kvs
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
serve :: (MonadDb m, MonadIO m) => ServiceCfg -> m ()
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
    perform (Insert s a n) = insertSession v s a n
    perform (UpdateApp s a) = updateSessionApp v s a
    perform (UpdateName s n) = updateSessionName v s n
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
              return $ Failure $ "Couldn't load the requested session: no such session ID " <> UUID.toText s
            Right sd -> do
              liftIO $ atomically $ StmMap.insert sd sid memdb
              return Success
