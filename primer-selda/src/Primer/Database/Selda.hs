{-# LANGUAGE OverloadedLabels #-}
-- Note: this is on purpose. See 'MonadDb' instance below.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.Database.Selda (
  MonadDb (..),
  sessions, -- exported for testing
  SessionRow (..), -- exported for testing
) where

import Foreword hiding ((:*:))

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
  SeldaM,
  SqlRow,
  Table,
  ascending,
  deleteFrom,
  insert_,
  literal,
  order,
  primary,
  query,
  restrict,
  select,
  table,
  update_,
  with,
  (!),
  (.==),
  (:*:) (..),
 )
import Database.Selda qualified as Selda
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
  , lastmodified :: UTCTime
  -- ^ The session's last modified time.
  --
  -- This should be of type 'SessionName', but Selda doesn't make it
  -- particularly easy to derive @SqlType@ from a newtype wrapper
  -- around 'Text', so rather than copy-pasting the 'Text' instance,
  -- we just convert back to 'Text' before serializing to the
  -- database.
  }
  deriving stock (Generic)

instance SqlRow SessionRow

-- | The database's sessions table.
sessions :: Table SessionRow
sessions = table "sessions" [#uuid :- primary]

-- | A 'MonadDb' instance for 'SeldaM'.
--
-- Note: this is purposely an orphan instance, and it should be fine,
-- since this is the canonical implementation, and is simply factored
-- out of the core Primer package for technical reasons.
instance MonadDb (SeldaM b) where
  insertSession v s a n t =
    insert_ sessions [SessionRow s v (Aeson.encode a) (fromSessionName n) (utcTime t)]

  updateSessionApp v s a t =
    update_
      sessions
      (\session -> session ! #uuid .== literal s)
      (\session -> session `with` [#gitversion := literal v, #app := literal (Aeson.encode a), #lastmodified := literal (utcTime t)])

  updateSessionName v s n t =
    update_
      sessions
      (\session -> session ! #uuid .== literal s)
      (\session -> session `with` [#gitversion := literal v, #name := literal (fromSessionName n), #lastmodified := literal (utcTime t)])

  listSessions ol = do
    n' <- query $
      Selda.aggregate $ do
        session <- select sessions
        pure $ Selda.count $ session ! #uuid
    let n = case n' of
          [n''] -> n''
          -- something has gone terribly wrong: selda will return a singleton
          -- for a 'count' query. For now, return a default value.
          -- TODO: this should log an error and cause a HTTP 5xx code to be,
          -- returned. See https://github.com/hackworthltd/primer/issues/179
          _ -> 0
    ss <- query $
      Selda.limit (offset ol) (fromMaybe n $ limit ol) $ do
        session <- select sessions
        order (session ! #uuid) ascending
        pure (session ! #uuid :*: session ! #name :*: session ! #lastmodified)
    pure $ Page{total = n, pageContents = safeMkSession <$> ss}
    where
      -- See comment in 'querySessionId' re: dealing with invalid
      -- session names loaded from the database.
      safeMkSession (s :*: n :*: t) = Session s (safeMkSessionName n) (LastModified t)

  -- Note: we ignore the stored Primer version for now.
  querySessionId sid = do
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
            pure $ Right (SessionData decodedApp (safeMkSessionName n) (LastModified t))

  deleteSession sid = do
    n <- deleteFrom sessions (\session -> session ! #uuid .== literal sid)
    case n of
      0 -> pure $ Left $ SessionIdNotFound sid
      _ -> pure $ Right ()
