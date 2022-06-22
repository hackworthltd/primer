{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
--Module      : Primer.Database.Rel8.Rel8Db
--Description : A database adapter for Primer using "Rel8".
--Copyright   : (c) 2022, Hackworth Ltd
--License     : AGPL 3.0 or later
--Stability   : experimental
--Portability : portable
--
--A "Rel8"- and @Hasql@-based implementation of 'MonadDb'.
module Primer.Database.Rel8.Rel8Db (
  -- * The "Rel8" database adapter
  Rel8DbT (..),
  Rel8Db,
  runRel8DbT,
  runRel8Db,

  -- * Exceptions
  Rel8DbException (..),
) where

import Foreword hiding (filter)

import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import Data.Functor.Contravariant ((>$<))
import Data.UUID (UUID)
import Hasql.Connection (Connection)
import Hasql.Session (
  QueryError,
  run,
  statement,
 )
import Hasql.Statement (Statement)
import Primer.Database (
  DbError (SessionIdNotFound),
  MonadDb (..),
  OffsetLimit (OL),
  Page (Page, pageContents, total),
  Session (Session),
  SessionData (..),
  SessionId,
  fromSessionName,
  safeMkSessionName,
 )
import Primer.Database.Rel8.Schema as Schema (
  SessionRow (..),
  sessionRowSchema,
 )
import Rel8 (
  Expr,
  Insert (Insert, into, onConflict, returning, rows),
  OnConflict (Abort),
  Query,
  Returning (NumberOfRowsAffected),
  Update (Update, from, returning, set, target, updateWhere),
  asc,
  countRows,
  each,
  filter,
  insert,
  limit,
  lit,
  litExpr,
  offset,
  orderBy,
  select,
  update,
  values,
  (==.),
 )

-- | A wrapper type for managing Rel8 operations.
newtype Rel8DbT m a = Rel8DbT {unRel8DbT :: ReaderT Connection m a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadError e
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader Connection
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

-- | The 'Rel8DbT' monad transformer applied to 'IO'.
type Rel8Db a = Rel8DbT IO a

-- | Run an action in the 'Rel8DbT' monad with the given 'Connection'.
runRel8DbT :: Rel8DbT m a -> Connection -> m a
runRel8DbT m = runReaderT (unRel8DbT m)

-- | Run an 'IO' action in the 'Rel8Db' monad with the given
-- 'Connection'.
runRel8Db :: Rel8DbT IO a -> Connection -> IO a
runRel8Db = runRel8DbT

-- | A 'MonadDb' instance for 'Rel8DbT'.
--
-- This monad throws unexpected database-related exceptions via its
-- 'MonadThrow' instance. These exceptions are represented via the
-- 'Rel8DbException' type. It's the responsibility of the caller to
-- handle them, as opposed to run-of-the-mill exceptions that may
-- occur; e.g., looking up a session ID that doesn't exist in the
-- database. The latter sorts of exceptions are expressed via the
-- types of the 'MonadDb' methods and are handled by Primer
-- internally.
instance (MonadThrow m, MonadIO m) => MonadDb (Rel8DbT m) where
  insertSession v s a n =
    runStatement_ (InsertError s) $
      insert
        Insert
          { into = Schema.sessionRowSchema
          , rows =
              values
                [ lit
                    Schema.SessionRow
                      { Schema.uuid = s
                      , Schema.gitversion = v
                      , Schema.app = a
                      , Schema.name = fromSessionName n
                      }
                ]
          , onConflict = Abort
          , returning = NumberOfRowsAffected
          }

  updateSessionApp v s a = do
    nr <-
      runStatement (UpdateAppError s) $
        update
          Update
            { target = Schema.sessionRowSchema
            , from = allSessions
            , set = \_ row ->
                row
                  { Schema.gitversion = lit v
                  , Schema.app = lit a
                  }
            , updateWhere = \_ row -> Schema.uuid row ==. litExpr s
            , returning = NumberOfRowsAffected
            }

    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ UpdateAppNonExistentSession s
      1 -> pure ()
      _ -> throwM $ UpdateAppConsistencyError s

  updateSessionName v s n = do
    nr <-
      runStatement (UpdateNameError s) $
        update
          Update
            { target = Schema.sessionRowSchema
            , from = allSessions
            , set = \_ row ->
                row
                  { Schema.gitversion = lit v
                  , Schema.name = lit $ fromSessionName n
                  }
            , updateWhere = \_ row -> Schema.uuid row ==. litExpr s
            , returning = NumberOfRowsAffected
            }

    -- This operation should affect exactly one row.
    case nr of
      0 -> throwM $ UpdateNameNonExistentSession s
      1 -> pure ()
      _ -> throwM $ UpdateNameConsistencyError s

  listSessions ol = do
    n' <- runStatement ListSessionsError $ select numSessions
    n <- case n' of
      -- Currently, our page size is 'Int', but Rel8 gives
      -- 'Int64'. This needs fixing, but has implications for API
      -- clients, so for now we downcast, as we will not hit 2
      -- billion rows anytime soon. See:
      -- https://github.com/hackworthltd/primer/issues/238
      [n''] -> pure $ fromIntegral n''
      -- This case should never occur, as 'countRows' (used by
      -- 'numSessions' above) should never return the empty list:
      -- https://hackage.haskell.org/package/rel8-1.3.1.0/docs/Rel8.html#v:countRows
      _ -> throwM ListSessionsRel8Error
    ss :: [(UUID, Text)] <- runStatement ListSessionsError $ select $ paginatedSessionMeta ol
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}
    where
      -- See comment in 'querySessionId' re: dealing with invalid
      -- session names loaded from the database.
      safeMkSession (s, n) = Session s (safeMkSessionName n)

  querySessionId sid = do
    result <- runStatement (LoadSessionError sid) $ select $ sessionById sid
    case result of
      [] -> return $ Left $ SessionIdNotFound sid
      (s : _) ->
        -- Note that we have 2 choices here if the session name
        -- returned by the database is not a valid 'SessionName':
        -- either we can return a failure, or we can convert it to
        -- a valid 'SessionName', possibly including a helpful
        -- message. This situation can only ever happen if we've
        -- made a mistake (e.g., we've changed the rules on what's
        -- a valid 'SessionName' and didn't run a migration), or
        -- if someone has edited the database directly, without
        -- going through the API. In either case, it would be bad
        -- if a student can't load their session just because a
        -- session name was invalid, so we opt for the "convert it
        -- to a valid 'SessionName'" strategy. For now, we elide
        -- the helpful message.
        --
        -- We should probably log an event when this occurs. See:
        -- https://github.com/hackworthltd/primer/issues/179
        pure $ Right (SessionData (Schema.app s) (safeMkSessionName $ Schema.name s))

-- | Exceptions that can be thrown by 'Rel8DbT' computations.
--
-- These exceptions are thrown only for truly exceptional errors.
-- Generally speaking, these will not be recoverable by the handler,
-- though in some cases it may be possible to keep retrying the
-- operation until the exceptional condition has been resolved; e.g.,
-- when the connection to the database is temporarily severed.
data Rel8DbException
  = -- | An error occurred during an 'Insert' operation on the given
    -- 'SessionId'.
    InsertError SessionId QueryError
  | -- | An error occurred during an 'UpdateApp' operation on the
    -- given 'SessionId'.
    UpdateAppError SessionId QueryError
  | -- | An attempt was made to 'UpdateApp' using a 'SessionId' that
    -- doesn't exist in the database. (It must be inserted before it
    -- can be updated.)
    UpdateAppNonExistentSession SessionId
  | -- | A database consistency error was detected during an
    -- 'UpdateApp' operation on the given 'SessionId'.
    UpdateAppConsistencyError SessionId
  | -- | An error occurred during an 'UpdateName' operation on the
    -- given 'SessionId'.
    UpdateNameError SessionId QueryError
  | -- | An attempt was made to 'UpdateName' using a 'SessionId' that
    -- doesn't exist in the database. (It must be inserted before it
    -- can be updated.)
    UpdateNameNonExistentSession SessionId
  | -- | A database consistency error was detected during an
    -- 'UpdateName' operation on the given 'SessionId'.
    UpdateNameConsistencyError SessionId
  | -- | An error occurred during a 'LoadSession' operation on the
    -- given 'SessionId'.
    LoadSessionError SessionId QueryError
  | -- | An error occurred during a 'ListSessions' operation.
    ListSessionsError QueryError
  | -- | 'Rel8' returned an unexpected result during a 'ListSessions'
    -- operation. This should never occur unless there's a bug in
    -- 'Rel8'.
    ListSessionsRel8Error
  deriving (Eq, Show)

instance Exception Rel8DbException

-- Helpers to make dealing with "Hasql.Session" easier.
--
-- See the note on 'Rel8DbT's 'MonadDb' instance for an explanation of
-- why we handle "Hasql.Session" exceptions the way we do.

runStatement :: (MonadIO m, MonadThrow m, MonadReader Connection m) => (QueryError -> Rel8DbException) -> Statement () a -> m a
runStatement exc s = do
  conn <- ask
  result <- liftIO $ flip run conn $ statement () s
  case result of
    Left e ->
      -- Something went wrong with the database or database
      -- connection. This is the responsibility of the caller to
      -- handle.
      throwM $ exc e
    Right r -> pure r

runStatement_ :: (MonadIO m, MonadThrow m, MonadReader Connection m) => (QueryError -> Rel8DbException) -> Statement () a -> m ()
runStatement_ exc = void . runStatement exc

-- "Rel8" queries and other operations.

-- All sessions in the database.
allSessions :: Query (Schema.SessionRow Expr)
allSessions = each Schema.sessionRowSchema

-- Select a session by session ID. The session ID is unique, so this
-- should only return at most 1 session, though Hasql's types are not
-- robust enough to represent this invariant.
sessionById :: UUID -> Query (Schema.SessionRow Expr)
sessionById sid =
  allSessions >>= filter \p -> Schema.uuid p ==. litExpr sid

-- Return the number of sessions in the database.
numSessions :: Query (Expr Int64)
numSessions = countRows allSessions

-- Paginate a query.
--
-- Note: the order of operations here is important.
--
-- TODO: review use of 'fromIntegral' here and
-- https://github.com/hackworthltd/primer/issues/238
paginate :: OffsetLimit -> Query a -> Query a
paginate (OL o (Just l)) = limit (fromIntegral l) . offset (fromIntegral o)
paginate (OL o _) = offset (fromIntegral o)

-- Return the metadata (represented as a tuple) for all sessions in
-- the database.
sessionMeta :: Query (Expr UUID, Expr Text)
sessionMeta = do
  s <- allSessions
  return (Schema.uuid s, Schema.name s)

-- Paginated session metadata, sorted by session name.
paginatedSessionMeta :: OffsetLimit -> Query (Expr UUID, Expr Text)
paginatedSessionMeta ol = paginate ol $ orderBy (snd >$< asc) sessionMeta
