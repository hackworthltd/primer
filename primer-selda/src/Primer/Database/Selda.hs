{-# LANGUAGE OverloadedLabels #-}
-- Note: this is on purpose. See 'MonadDb' instance below.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.Database.Selda
  ( MonadDb (..),
    initialize,
  )
where

import qualified Data.Aeson as Aeson
  ( decode,
    encode,
  )
import Data.ByteString.Lazy as BL hiding (take)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID (toText)
import Database.Selda
  ( Assignment ((:=)),
    Attr ((:-)),
    SeldaM,
    SqlRow,
    Table,
    ascending,
    insert_,
    literal,
    order,
    primary,
    query,
    restrict,
    select,
    table,
    tryCreateTable,
    update_,
    with,
    (!),
    (.==),
    (:*:) (..),
  )
import GHC.Generics (Generic)
import Primer.Database
  ( MonadDb (..),
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
  { -- | The session's UUID.
    uuid :: UUID,
    -- | Primer's git version. We would prefer that this were a git
    -- rev, but for technical reasons, it may also be a last-modified
    -- date.
    gitversion :: Version,
    -- | The session's 'App'. Note that the 'App' is serialized to
    -- JSON before being stored as a bytestring in the database.
    app :: BL.ByteString,
    -- | The session's name.
    --
    -- This should be of type 'SessionName', but Selda doesn't make it
    -- particularly easy to derive @SqlType@ from a newtype wrapper
    -- around 'Text', so rather than copy-pasting the 'Text' instance,
    -- we just convert back to 'Text' before serializing to the
    -- database.
    name :: Text
  }
  deriving (Generic)

instance SqlRow SessionRow

-- | The database's sessions table.
sessions :: Table SessionRow
sessions = table "sessions" [#uuid :- primary]

-- | Initialize the Selda database.
--
-- Note that if the database has already been initialized, the
-- computation simply exits.
initialize :: SeldaM b ()
initialize = do
  tryCreateTable sessions

-- | A 'MonadDb' instance for 'SeldaM'.
--
-- Note: this is purposely an orphan instance, and it should be fine,
-- since this is the canonical implementation, and is simply factored
-- out of the core Primer package for technical reasons.
instance MonadDb (SeldaM b) where
  insertSession v s a n =
    insert_ sessions [SessionRow s v (Aeson.encode a) (fromSessionName n)]

  updateSessionApp v s a =
    update_
      sessions
      (\session -> session ! #uuid .== literal s)
      (\session -> session `with` [#gitversion := literal v, #app := literal (Aeson.encode a)])

  updateSessionName v s n =
    update_
      sessions
      (\session -> session ! #uuid .== literal s)
      (\session -> session `with` [#gitversion := literal v, #name := literal (fromSessionName n)])

  listSessions = do
    ss <- query $ do
      session <- select sessions
      order (session ! #uuid) ascending
      return (session ! #uuid :*: session ! #name)
    pure $ tuple <$> ss
    where
      -- See comment in 'querySessionId' re: dealing with invalid
      -- session names loaded from the database.
      tuple (s :*: n) = (s, safeMkSessionName n)

  -- Note: we ignore the stored Primer version for now.
  querySessionId _ sid = do
    result <- query $ do
      session <- select sessions
      restrict (session ! #uuid .== literal sid)
      return (session ! #gitversion :*: session ! #app :*: session ! #name)
    case result of
      [] -> return $ Left $ "No such session ID " <> UUID.toText sid
      (_ :*: bs :*: n) : _ ->
        case Aeson.decode bs of
          Nothing -> pure $ Left $ "Failed to decode stored program for session ID " <> UUID.toText sid
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
            pure $ Right (SessionData decodedApp (safeMkSessionName n))
