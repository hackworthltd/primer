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
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.UUID.Types (UUID)
import Database.Selda (
  Assignment ((:=)),
  Attr ((:-)),
  Col,
  Inner,
  MonadSelda,
  OuterCols,
  Query,
  Relational,
  Row,
  SeldaError,
  SeldaT,
  SqlRow,
  Table,
  ascending,
  deleteFrom,
  descending,
  insert,
  like,
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
import Database.Selda.Unsafe (rawStm)
import Primer.Database (
  DbError (AppDecodingError, SessionIdNotFound),
  LastModified (..),
  MonadDb (..),
  OffsetLimit (OL),
  Page (Page, pageContents, total),
  Session (Session),
  SessionData (..),
  SessionId,
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
  convertSeldaDbException ConnectionFailed
    $ withSQLite db
    $ do
      -- By default, SQLite has a case-insensitive @LIKE@
      -- implementation. See:
      -- https://www.sqlite.org/pragma.html#pragma_case_sensitive_like
      --
      -- However, we use a pragma upon opening the database connection
      -- just to be sure. (Eventually we'll need to support both
      -- case-sensitive and insensitive, anyway.)
      --
      -- The documentation for the pragma isn't clear on this point,
      -- but the documentation for
      -- https://www.sqlite.org/c3ref/create_function.html, upon which
      -- this pragma is based, is, implies that this pragma needs to
      -- be performed on every connection.
      rawStm "PRAGMA case_sensitive_like = false;"
      unSeldaSQLiteDbT m

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
instance Relational SessionRow

-- | The database's sessions table.
sessions :: Table SessionRow
sessions = table "sessions" [#uuid :- primary]

-- A helper function for creating a 'Session' from a database query.
--
-- Note that we have 2 choices here if the session name that was
-- fetched from the database isn't a valid 'SessionName': either we
-- can return a failure, or we can convert it to a valid
-- 'SessionName'. This situation can only ever happen if we've made a
-- mistake (e.g., we've changed the rules on what's a valid
-- 'SessionName' and didn't run a migration), or if someone has edited
-- the database directly, without going through the API. In either
-- case, it would be bad if a student can't load their session just
-- because a session name was invalid, so we opt for "convert it to a
-- valid 'SessionName'".
--
-- It might be helpful if this function returned an indication of
-- whether the original name was safe, but for now, we convert
-- silently.
safeMkSession :: (SessionId :*: (Text :*: UTCTime)) -> Session
safeMkSession (s :*: n :*: t) =
  Session s (safeMkSessionName n) (LastModified t)

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
  insertSession v s a n t = do
    nr <-
      convertSeldaDbException (InsertError s)
        $ insert sessions [SessionRow s v (Aeson.encode a) (fromSessionName n) (utcTime t)]
    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ InsertZeroRowsAffected s
      1 -> pure ()
      _ -> throwM $ InsertConsistencyError s

  updateSessionApp v s a t = do
    nr <-
      convertSeldaDbException (UpdateAppError s)
        $ update
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
      convertSeldaDbException (UpdateNameError s)
        $ update
          sessions
          (\session -> session ! #uuid .== literal s)
          (\session -> session `with` [#gitversion := literal v, #name := literal (fromSessionName n), #lastmodified := literal (utcTime t)])
    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ UpdateNameNonExistentSession s
      1 -> pure ()
      _ -> throwM $ UpdateNameConsistencyError s

  listSessions ol = convertSeldaDbException ListSessionsError $ do
    n' <- query
      $ Selda.aggregate
      $ do
        session <- allSessions
        pure $ Selda.count $ session ! #uuid
    n <- case n' of
      [n''] -> pure n''
      -- something has gone terribly wrong: selda should never return
      -- the empty list for a 'count' query.
      _ -> throwM ListSessionsSeldaError
    ss <- query $ paginatedSessionMeta ol allSessions
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}

  findSessions substr ol = convertSeldaDbException FindSessionsError $ do
    -- We shouldn't do this, it's very wasteful. However, it'll
    -- require some refactoring. See:
    --
    -- https://github.com/hackworthltd/primer/issues/1037
    n' <- query
      $ Selda.aggregate
      $ do
        session <- sessionByNameSubstr substr
        pure $ Selda.count $ session ! #uuid
    n <- case n' of
      [n''] -> pure n''
      -- something has gone terribly wrong: selda should never return
      -- the empty list for a 'count' query.
      _ -> throwM FindSessionsSeldaError
    ss <- query $ paginatedSessionMeta ol $ sessionByNameSubstr substr
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}

  -- Note: we ignore the stored Primer version for now.
  querySessionId sid = convertSeldaDbException (LoadSessionError sid) $ do
    result <- query $ do
      session <- allSessions
      restrict (session ! #uuid .== literal sid)
      pure (session ! #gitversion :*: session ! #app :*: session ! #name :*: session ! #lastmodified)
    case result of
      [] -> pure $ Left $ SessionIdNotFound sid
      (_ :*: bs :*: n :*: t) : _ ->
        case Aeson.decode bs of
          Nothing -> pure $ Left $ AppDecodingError sid
          Just decodedApp -> do
            -- See comment on 'safeMkSession' regarding how we use
            -- 'safeMkSessionName' here.
            let sessionName = safeMkSessionName n
                lastModified = LastModified t
            when (fromSessionName sessionName /= n)
              $ logError
              $ IllegalSessionName sid n
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

-- "Database.Selda" queries and other operations.

-- All sessions in the database.
allSessions :: Query s (Row s SessionRow)
allSessions = select sessions

-- Select all sessions whose name contains the given substring.
--
-- Note that Selda doesn't support @ESCAPE@ clauses in @LIKE@, so we
-- can't search for strings that contain @%@ or @_@. See:
--
-- https://github.com/hackworthltd/primer/issues/1035
--
-- Because we can't escape them, any occurrences of these characters
-- will be treated as wildcards by SQLite, which seems a bit
-- dangerous. In order to protect against potential SQL injection
-- attacks, we replace any occurrence of @%@ in the search string with
-- @_@. In cases where the student actually wants to search for
-- sessions whose names contain literal @%@, we'll still find those
-- sessions, we'll just potentially return extraneous results, as
-- well.
sessionByNameSubstr :: Text -> Query s (Row s SessionRow)
sessionByNameSubstr substr = do
  session <- allSessions
  restrict (session ! #name `like` literal ("%" <> paranoid substr <> "%"))
  pure session
  where
    paranoid = Text.replace "%" "_"

-- Paginate a query.
paginate :: OffsetLimit -> Query (Inner t) a -> Query t (OuterCols a)
paginate (OL o (Just l)) = Selda.limit o l
paginate (OL o _) = Selda.limit o maxBound

type SessionMeta s = (Col s UUID :*: Col s Text :*: Col s UTCTime)

-- Order by session name (primary) and last-modified (secondary,
-- newest to oldest), and return session metadata.
orderSessionMeta :: Row s SessionRow -> Query s (SessionMeta s)
orderSessionMeta s = do
  -- Note that Selda wants these constraints to be ordered in
  -- least-to-most precedence; i.e., secondary then primary. See:
  -- https://hackage.haskell.org/package/selda-0.5.2.0/docs/Database-Selda.html#v:order
  order (s ! #lastmodified) descending
  order (s ! #name) ascending
  pure (s ! #uuid :*: s ! #name :*: s ! #lastmodified)

-- Paginated session metadata, sorted by session name (primary) and
-- last-modified (secondary, newest to oldest).
paginatedSessionMeta ::
  OffsetLimit ->
  Query (Inner t) (Row (Inner t) SessionRow) ->
  Query t (SessionMeta t)
paginatedSessionMeta ol s =
  paginate ol $ s >>= orderSessionMeta
