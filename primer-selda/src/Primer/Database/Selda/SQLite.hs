{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}

module Primer.Database.Selda.SQLite (
  -- * The "Database.Selda.SQLite" database adapter.
  MonadSeldaSQLiteDb,
  SeldaSQLiteDbT (..),
  runSeldaSQLiteDbT,

  -- * Exported for testing.
  SessionRow (..),
  sessions,
) where

import Foreword hiding ((:*:))

import Control.Monad.Log (
  MonadLog,
  WithSeverity (..),
 )
import Control.Monad.Trans (MonadTrans)
import Data.Aeson qualified as Aeson (
  decode,
  encode,
 )
import Data.ByteString.Lazy as BL hiding (take)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.Selda (
  Assignment ((:=)),
  Attr ((:-)),
  MonadSelda,
  SeldaError,
  SeldaT,
  SqlRow,
  Table,
  ascending,
  deleteFrom,
  descending,
  insert_,
  literal,
  order,
  primary,
  query,
  restrict,
  select,
  table,
  update,
  with,
  (!),
  (.==),
  (:*:) (..),
 )
import Database.Selda qualified as Selda
import Database.Selda.SQLite (
  SQLite,
  withSQLite,
 )
import Primer.Database (
  DbError (AppDecodingError, SessionIdNotFound),
  LastModified (..),
  MonadDb (..),
  OffsetLimit (limit, offset),
  Page (Page, pageContents, total),
  Session (Session),
  SessionData (..),
  Version,
  fromSessionName,
  safeMkSessionName,
 )
import Primer.Database.Selda (
  SeldaDbException (..),
  SeldaDbLogMessage (..),
 )
import Primer.Log (
  ConvertLogMessage (..),
  logError,
 )

-- | A convenience newtype for 'SeldaT' specialized on 'SQLite'.
newtype SeldaSQLiteDbT m a = SeldaSQLiteDbT {unSeldaSQLiteDbT :: SeldaT SQLite m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadFail
    , MonadSelda
    , MonadTrans
    )

instance MonadLog msg m => MonadLog msg (SeldaSQLiteDbT m)

-- | Run an action in the 'SeldaSQLiteDbT' monad with the given SQLite
-- database, expressed as a 'FilePath'.
runSeldaSQLiteDbT :: (MonadIO m, MonadMask m) => FilePath -> SeldaSQLiteDbT m a -> m a
runSeldaSQLiteDbT db m =
  -- Note: any 'SeldaError' that occurs in computation 'm' will be
  -- handled by the appropriate 'MonadDb' instance with a
  -- method-specific 'SeldaDbException' constructor, so any exception
  -- that is handled by this 'handleSeldaDbException' call must have
  -- occurred in 'withSQLite' itself (e.g., SQLite file not found);
  -- hence, we use 'ConnectionFailed' here.
  convertSeldaDbException ConnectionFailed $
    withSQLite db (unSeldaSQLiteDbT m)

-- | A database session table row.
--
-- This table is effectively just a key-value store, where the
-- session's UUID is the primary key and the value is a record
-- consisting of the session's 'App', the git version of Primer that
-- last updated it, and the session's name.
data SessionRow = SessionRow
  { uuid :: UUID
  -- ^ The session's UUID.
  , gitversion :: Version
  -- ^ Primer's git version. We would prefer that this were a git
  -- rev, but for technical reasons, it may also be a last-modified
  -- date.
  , app :: BL.ByteString
  -- ^ The session's 'App'. Note that the 'App' is serialized to
  -- JSON before being stored as a bytestring in the database.
  , name :: Text
  -- ^ The session's name.
  --
  -- This should be of type 'SessionName', but Selda doesn't make it
  -- particularly easy to derive @SqlType@ from a newtype wrapper
  -- around 'Text', so rather than copy-pasting the 'Text' instance,
  -- we just convert back to 'Text' before serializing to the
  -- database.
  --
  -- See:
  -- https://github.com/valderman/selda/issues/152
  , lastmodified :: UTCTime
  -- ^ The session's last modified time.
  }
  deriving stock (Generic)

instance SqlRow SessionRow

-- | The database's sessions table.
sessions :: Table SessionRow
sessions = table "sessions" [#uuid :- primary]

-- | A convenient type alias.
--
-- Note that 'MonadLog' has a functional dependency from 'm' to 'l'.
type MonadSeldaSQLiteDb m l = (ConvertLogMessage SeldaDbLogMessage l, MonadCatch m, MonadThrow m, MonadMask m, MonadIO m, MonadLog (WithSeverity l) m)

-- | A 'MonadDb' instance for 'SeldaT SQLite'.
--
-- This monad throws unexpected database-related exceptions via its
-- 'MonadThrow' instance. These exceptions are represented via the
-- 'SeldaDbException' type. It's the responsibility of the caller to
-- handle them, as opposed to run-of-the-mill exceptions that may
-- occur; e.g., looking up a session ID that doesn't exist in the
-- database. The latter sorts of exceptions are expressed via the
-- types of the 'MonadDb' methods and are handled by Primer
-- internally.
instance MonadSeldaSQLiteDb m l => MonadDb (SeldaSQLiteDbT m) where
  insertSession v s a n t =
    convertSeldaDbException (InsertError s) $
      insert_ sessions [SessionRow s v (Aeson.encode a) (fromSessionName n) (utcTime t)]

  updateSessionApp v s a t = do
    nr <-
      convertSeldaDbException (UpdateAppError s) $
        update
          sessions
          (\session -> session ! #uuid .== literal s)
          (\session -> session `with` [#gitversion := literal v, #app := literal (Aeson.encode a), #lastmodified := literal (utcTime t)])
    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ UpdateAppNonExistentSession s
      1 -> pure ()
      _ -> throwM $ UpdateAppConsistencyError s

  updateSessionName v s n t = do
    nr <-
      convertSeldaDbException (UpdateNameError s) $
        update
          sessions
          (\session -> session ! #uuid .== literal s)
          (\session -> session `with` [#gitversion := literal v, #name := literal (fromSessionName n), #lastmodified := literal (utcTime t)])
    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ UpdateNameNonExistentSession s
      1 -> pure ()
      _ -> throwM $ UpdateNameConsistencyError s

  listSessions ol = convertSeldaDbException ListSessionsError $ do
    n' <- query $
      Selda.aggregate $ do
        session <- select sessions
        pure $ Selda.count $ session ! #uuid
    n <- case n' of
      [n''] -> pure n''
      -- something has gone terribly wrong: selda should never return
      -- the empty list for a 'count' query.
      _ -> throwM ListSessionsSeldaError
    ss <- query $
      Selda.limit (offset ol) (fromMaybe n $ limit ol) $ do
        session <- select sessions
        -- Order by session name (primary) and last-modified
        -- (secondary, newest to oldest). Note that Selda wants these
        -- constraints to be ordered in least-to-most precedence;
        -- i.e., secondary then primary. See:
        -- https://hackage.haskell.org/package/selda-0.5.2.0/docs/Database-Selda.html#v:order
        order (session ! #lastmodified) descending
        order (session ! #name) ascending
        pure (session ! #uuid :*: session ! #name :*: session ! #lastmodified)
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}
    where
      -- See comment in 'querySessionId' re: dealing with invalid
      -- session names loaded from the database.
      safeMkSession (s :*: n :*: t) = Session s (safeMkSessionName n) (LastModified t)

  -- Note: we ignore the stored Primer version for now.
  querySessionId sid = convertSeldaDbException (LoadSessionError sid) $ do
    result <- query $ do
      session <- select sessions
      restrict (session ! #uuid .== literal sid)
      pure (session ! #gitversion :*: session ! #app :*: session ! #name :*: session ! #lastmodified)
    case result of
      [] -> pure $ Left $ SessionIdNotFound sid
      (_ :*: bs :*: n :*: t) : _ ->
        case Aeson.decode bs of
          Nothing -> pure $ Left $ AppDecodingError sid
          Just decodedApp -> do
            -- Note that we have 2 choices here if @n@ is not a valid
            -- 'SessionName': either we can return a failure, or we
            -- can convert it to a valid 'SessionName', possibly
            -- including a helpful message. This situation can only
            -- ever happen if we've made a mistake (e.g., we've
            -- changed the rules on what's a valid 'SessionName' and
            -- didn't run a migration), or if someone has edited the
            -- database directly, without going through the API. In
            -- either case, it would be bad if a student can't load
            -- their session just because a session name was invalid,
            -- so we opt for "convert it to a valid 'SessionName'".
            -- For now, we elide the helpful message.
            let sessionName = safeMkSessionName n
                lastModified = LastModified t
            when (fromSessionName sessionName /= n) $
              logError $
                IllegalSessionName sid n
            pure $ Right (SessionData decodedApp sessionName lastModified)

  deleteSession sid = convertSeldaDbException (DeleteSessionError sid) $ do
    n <- deleteFrom sessions (\session -> session ! #uuid .== literal sid)
    -- This operation should affect at most one row. Note that not
    -- matching any rows is not necessarily indicative of a critcal
    -- error: it could easily occur in a multiplayer situation where
    -- there's a race to delete the session, for example.
    case n of
      0 -> pure $ Left $ SessionIdNotFound sid
      1 -> pure $ Right ()
      _ -> throwM $ DeleteSessionConsistencyError sid

-- Catch 'SeldaError's during 'SeldaT' computations and convert them
-- to 'SeldaDbException's.
convertSeldaDbException :: (MonadCatch m, Exception b) => (SeldaError -> b) -> m a -> m a
convertSeldaDbException exc op =
  catchJust
    justSeldaError
    op
    (throwM . exc)
  where
    justSeldaError :: SeldaError -> Maybe SeldaError
    justSeldaError = Just
