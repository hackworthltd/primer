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
  OpStatus (..),
  ServiceCfg (..),
  MonadDb (..),
  NullDbT (..),
  NullDb,
  Version,
  serve,
) where

import Foreword

import Control.Concurrent.STM (
  TBQueue,
  TMVar,
  putTMVar,
  readTBQueue,
 )
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import qualified Data.Text as Text (
  strip,
  take,
  takeWhile,
 )
import Data.UUID (UUID)
import qualified Data.UUID as UUID (toText)
import Data.UUID.V4 (nextRandom)
import qualified ListT (toList)
import Primer.App (App (..))
import Primer.JSON (CustomJSON (CustomJSON), ToJSON, VJSON)
import qualified StmContainers.Map as StmMap

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
  deriving (Generic, Eq, Show, Read)
  deriving newtype (ToJSON)

-- Ugh, this doesn't work. See
-- https://github.com/valderman/selda/issues/13
-- deriving (SqlType) via Text

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
  deriving (Generic)
  deriving (ToJSON) via VJSON Session

-- | Per-session information.
data SessionData = SessionData
  { -- | The session's 'App'.
    sessionApp :: App
  , -- | The session's name.
    sessionName :: SessionName
  }
  deriving (Generic)

-- | An in-memory cache of sessions. This type maps 'SessionId's to 'App's.
type Sessions = StmMap.Map SessionId SessionData

-- | Create a new, unique session ID.
newSessionId :: IO SessionId
-- Note: do not use "Database.Selda"s random UUID generator here. It
-- does not take pains to be cryptographically secure as "Data.UUID"
-- does, which is important for generating unpredictable UUIDs.
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
    ListSessions !(TMVar [Session])

-- | A config for the 'serve' computation.
data ServiceCfg = ServiceCfg
  { -- | The database operation queue.
    opQueue :: TBQueue Op
  , -- | The running version of Primer.
    version :: Version
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

  -- | Get a list of all session IDs and their names.
  --
  -- Corresponds to the 'ListSessions' operation.
  listSessions :: m [Session]

  -- | Query a session ID from the database.
  --
  -- Returns 'Left' with a message if the query failed (session
  -- doesn't exist, version mismatch, etc.), 'Right' with the
  -- 'SessionData' if successful.
  querySessionId :: Version -> SessionId -> m (Either Text SessionData)

-- | A "null" database type that effectively does nothing.
--
-- This type is really only useful for mocking/testing or "toy"
-- environments. It ignores writes, returns an error when you ask it
-- to look up a session ID, and presents the same list of sessions as
-- the in-memory database (i.e., the 'Sessions' type, which is managed
-- in "Primer.API").
--
-- Note that it keeps around a copy of the in-memory database so that
-- it can mock the 'ListSessions' database operation.
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

instance (MonadIO m) => MonadDb (NullDbT m) where
  insertSession _ _ _ _ = pure ()
  updateSessionApp _ _ _ = pure ()
  updateSessionName _ _ _ = pure ()
  listSessions = do
    ss <- ask
    kvs <- liftIO $ atomically $ ListT.toList $ StmMap.listT ss
    pure $ uncurry Session . second sessionName <$> kvs
  querySessionId _ sid = pure $ Left $ "No such session ID " <> UUID.toText sid

-- | The database service computation.
--
-- Because it will block, this computation should be run on its own
-- thread.
serve :: (MonadDb m, MonadIO m) => ServiceCfg -> m ()
serve cfg =
  let q = opQueue cfg
      v = version cfg
   in forever $ do
        op <- liftIO $ atomically $ readTBQueue q
        case op of
          Insert s a n -> insertSession v s a n
          UpdateApp s a -> updateSessionApp v s a
          UpdateName s n -> updateSessionName v s n
          ListSessions result -> do
            ss <- listSessions
            liftIO $ atomically $ putTMVar result ss
          -- Note that we split the in-memory session insertion (i.e.,
          -- the 'StmMap.insert') and the signal to the caller (i.e.,
          -- the 'putTMVar') across 2 'atomically' blocks. This is
          -- fine, because the signal to the caller doesn't mean that
          -- they have exclusive access to the session, only that they
          -- can try their session transaction again.
          LoadSession sid memdb status -> do
            result <- loadSession
            liftIO $ atomically $ putTMVar status result
            where
              loadSession = do
                queryResult <- querySessionId v sid
                case queryResult of
                  Left msg -> return $ Failure msg
                  Right sd -> do
                    liftIO $ atomically $ StmMap.insert sd sid memdb
                    return Success
