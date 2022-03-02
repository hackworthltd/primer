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
  queryError,
  isInsertError,
  isUpdateAppError,
  isUpdateNameError,
  isLoadSessionError,
  isListSessionsError,
) where

import Foreword hiding (filter)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Zip (MonadZip)
import qualified Data.Aeson as Aeson (
  decode,
  encode,
 )
import Data.Functor.Contravariant ((>$<))
import Data.UUID (UUID)
import qualified Data.UUID as UUID (toText)
import Hasql.Connection (Connection)
import Hasql.Session (
  QueryError,
  run,
  statement,
 )
import Hasql.Statement (Statement)
import Primer.Database (
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
-- This monad throws unexpected exceptions via its 'MonadThrow'
-- instance. Unexpected exceptions include any database errors raised
-- by "Hasql.Session". It's the responsibility of the caller to handle
-- them, as opposed to run-of-the-mill exceptions that may occur;
-- e.g., looking up a session ID that doesn't exist in the database.
-- The latter sorts of exceptions are expressed via the types of the
-- 'MonadDb' methods and are handled by Primer internally.
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
                      , Schema.app = Aeson.encode a
                      , Schema.name = fromSessionName n
                      }
                ]
          , onConflict = Abort
          , returning = NumberOfRowsAffected
          }
  updateSessionApp v s a =
    runStatement_ (UpdateAppError s) $
      update
        Update
          { target = Schema.sessionRowSchema
          , from = allSessions
          , set = \_ row ->
              row
                { Schema.gitversion = lit v
                , Schema.app = lit (Aeson.encode a)
                }
          , updateWhere = \_ row -> Schema.uuid row ==. litExpr s
          , returning = NumberOfRowsAffected
          }
  updateSessionName v s n =
    runStatement_ (UpdateNameError s) $
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
  listSessions ol = do
    n' <- runStatement ListSessionsError $ select numSessions
    let n = case n' of
          -- Currently, our page size is 'Int', but Rel8 gives
          -- 'Int64'. This needs fixing, but has implications for API
          -- clients, so for now we downcast, as we will not hit 2
          -- billion rows anytime soon. See:
          -- https://github.com/hackworthltd/primer/issues/238
          [n''] -> fromIntegral n''
          -- This case should never occur, as 'numSessions' should
          -- always return a number equal to or greater than '0' (per
          -- the Rel8 documentation). However, we have no good way to
          -- express this invariant in the type, so we handle this
          -- case with a default value of '0'.
          --
          -- TODO: this should log an error and cause an HTTP 5xx code
          -- to be returned. See:
          -- https://github.com/hackworthltd/primer/issues/179
          _ -> 0
    ss :: [(UUID, Text)] <- runStatement ListSessionsError $ select $ paginatedSessionMeta ol
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}
    where
      -- See comment in 'querySessionId' re: dealing with invalid
      -- session names loaded from the database.
      safeMkSession (s, n) = Session s (safeMkSessionName n)

  -- Note: we ignore the stored Primer version for now.
  --
  -- See https://github.com/hackworthltd/primer/issues/268
  querySessionId _ sid = do
    result <- runStatement (LoadSessionError sid) $ select $ sessionById sid
    case result of
      [] -> return $ Left $ "No such session ID " <> UUID.toText sid
      (s : _) ->
        case Aeson.decode (Schema.app s) of
          Nothing -> pure $ Left $ "Failed to decode stored program for session ID " <> UUID.toText sid
          Just decodedApp -> do
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
            pure $ Right (SessionData decodedApp (safeMkSessionName $ Schema.name s))

-- | Exceptions that can be thrown by 'Rel8DbT' computations.
--
-- This class mainly exists to wrap a @Hasql@ 'QueryError' in an
-- 'Exception' instance, so that @Hasql@ errors can be handled by
-- GHC's exception machinery.
--
-- These exceptions are thrown only for truly exceptional errors.
-- Generally speaking, these will not be recoverable by the handler,
-- though in some cases it may be possible to keep retrying the
-- operation until the exceptional condition has been resolved; e.g.,
-- when the connection to the database is temporarily severed.
--
-- (Non-exceptional errors are handled in-band in the 'MonadDb'
-- interface as an 'Data.Either.Either'.)
data Rel8DbException
  = -- | An error occurred during an 'Insert' operation on the given
    -- 'SessionId'.
    InsertError SessionId QueryError
  | -- | An error occurred during an 'UpdateApp' operation on the
    -- given 'SessionId'.
    UpdateAppError SessionId QueryError
  | -- | An error occurred during an 'UpdateName' operation on the
    -- given 'SessionId'.
    UpdateNameError SessionId QueryError
  | -- | An error occurred during a 'LoadSession' operation on the
    -- given 'SessionId'.
    LoadSessionError SessionId QueryError
  | -- | An error occurred during a 'ListSessions' operation.
    ListSessionsError QueryError
  deriving (Eq, Show)

instance Exception Rel8DbException

-- | Extract the 'QueryError' from a 'Rel8DbException'.
queryError :: Rel8DbException -> QueryError
queryError (ListSessionsError e) = e
queryError (InsertError _ e) = e
queryError (UpdateAppError _ e) = e
queryError (UpdateNameError _ e) = e
queryError (LoadSessionError _ e) = e

-- | 'True' if the 'Rel8DbException' is 'InsertError'.
isInsertError :: Rel8DbException -> Bool
isInsertError (InsertError _ _) = True
isInsertError _ = False

-- | 'True' if the 'Rel8DbException' is 'UpdateAppError'.
isUpdateAppError :: Rel8DbException -> Bool
isUpdateAppError (UpdateAppError _ _) = True
isUpdateAppError _ = False

-- | 'True' if the 'Rel8DbException' is 'UpdateNameError'.
isUpdateNameError :: Rel8DbException -> Bool
isUpdateNameError (UpdateNameError _ _) = True
isUpdateNameError _ = False

-- | 'True' if the 'Rel8DbException' is 'LoadSessionError'.
isLoadSessionError :: Rel8DbException -> Bool
isLoadSessionError (LoadSessionError _ _) = True
isLoadSessionError _ = False

-- | 'True' if the 'Rel8DbException' is 'ListSessionsError'.
isListSessionsError :: Rel8DbException -> Bool
isListSessionsError (ListSessionsError _) = True
isListSessionsError _ = False

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
