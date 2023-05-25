module Primer.Database.Selda (
  -- * Logging
  SeldaDbLogMessage (..),

  -- * Exceptions
  SeldaDbException (..),
) where

import Foreword

import Database.Selda (
  SeldaError (..),
 )
import Primer.Database (
  SessionId,
 )

-- | Exceptions that can be thrown by 'SeldaT' computations.
--
-- These exceptions are thrown only for truly exceptional errors.
-- Generally speaking, these will not be recoverable by the handler,
-- though in some cases it may be possible to keep retrying the
-- operation until the exceptional condition has been resolved; e.g.,
-- when the connection to the database is temporarily severed.
data SeldaDbException
  = -- | A connection-related error.
    ConnectionFailed SeldaError
  | -- | An error occurred during an 'Insert' operation on the given
    -- 'SessionId'.
    InsertError SessionId SeldaError
  | -- | An 'Insert' operation succeeded on the given 'SessionId', but
    --  the database claimed that zero rows were actually inserted.
    InsertZeroRowsAffected SessionId
  | -- | A database consistency error was detected during an 'Insert'
    -- operation on the given 'SessionId'.
    InsertConsistencyError SessionId
  | -- | An error occurred during a 'DeleteSession' operation on the
    -- given 'SessionId'.
    DeleteSessionError SessionId SeldaError
  | -- | A database consistency error was deletected during a
    -- | 'DeleteSession' operation on the given 'SessionId'.
    DeleteSessionConsistencyError SessionId
  | -- | An error occurred during an 'UpdateApp' operation on the
    -- given 'SessionId'.
    UpdateAppError SessionId SeldaError
  | -- | An attempt was made to 'UpdateApp' using a 'SessionId' that
    -- doesn't exist in the database. (It must be inserted before it
    -- can be updated.)
    UpdateAppNonExistentSession SessionId
  | -- | A database consistency error was detected during an
    -- 'UpdateApp' operation on the given 'SessionId'.
    UpdateAppConsistencyError SessionId
  | -- | An error occurred during an 'UpdateName' operation on the
    -- given 'SessionId'.
    UpdateNameError SessionId SeldaError
  | -- | An attempt was made to 'UpdateName' using a 'SessionId' that
    -- doesn't exist in the database. (It must be inserted before it
    -- can be updated.)
    UpdateNameNonExistentSession SessionId
  | -- | A database consistency error was detected during an
    -- 'UpdateName' operation on the given 'SessionId'.
    UpdateNameConsistencyError SessionId
  | -- | An error occurred during a 'LoadSession' operation on the
    -- given 'SessionId'.
    LoadSessionError SessionId SeldaError
  | -- | An error occurred during a 'ListSessions' operation.
    ListSessionsError SeldaError
  | -- | Selda returned an unexpected result during a 'ListSessions'
    -- operation. This should never occur unless there's a bug in
    -- Selda.
    ListSessionsSeldaError
  | -- | An error occurred during a 'FindSessions' operation.
    FindSessionsError SeldaError
  | -- | Selda returned an unexpected result during a 'FindSessions'
    -- operation. This should never occur unless there's a bug in
    -- Selda.
    FindSessionsSeldaError
  deriving stock (Eq, Show, Generic)

instance Exception SeldaDbException

-- | Selda-related log messages.
data SeldaDbLogMessage
  = -- | An illegal session name was found in the database. This is
    -- probably an indication that a database migration wasn't run
    -- properly, but may also indicate that the database has been
    -- modified outside the API.
    IllegalSessionName SessionId Text
  | -- | A 'SeldaDbException' occurred.
    LogSeldaDbException SeldaDbException
  deriving stock (Eq, Show, Generic)
