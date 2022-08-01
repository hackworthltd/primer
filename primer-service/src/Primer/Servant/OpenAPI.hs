module Primer.Servant.OpenAPI (
  PrimerOpenAPI,
  SessionsOpenAPI (..),
  SessionOpenAPI (..),
) where

import Foreword

import Primer.API qualified as API
import Primer.Database (Session, SessionId)
import Primer.Pagination (Paginated, PaginationParams)
import Servant (
  Capture,
  Get,
  JSON,
  NamedRoutes,
  Post,
  QueryFlag,
  Summary,
  (:>),
 )
import Servant.API.Generic (
  GenericMode ((:-)),
 )
import Servant.OpenApi.OperationId (OpId)

-- | The top-level OpenAPI endpoint.
type PrimerOpenAPI = "api" :> ("sessions" :> NamedRoutes SessionsOpenAPI)

data SessionsOpenAPI mode = SessionsOpenAPI
  { createSession ::
      mode
        :- Summary "Create a new session"
          :> OpId "createSession" Post '[JSON] SessionId
  -- ^ Create a new session on the backend, returning its id
  , getSessionList ::
      mode
        :- QueryFlag "inMemory"
          :> PaginationParams
          :> Summary "List sessions"
          :> OpId "getSessionList" Get '[JSON] (Paginated Session)
  -- ^ Get a list of all sessions and their human-readable names. By
  -- default this returns the list of all sessions in the persistent
  -- database, but optionally it can return just the list of all
  -- sessions in memory, which is mainly useful for testing. Note that
  -- in a production system, this endpoint should obviously be
  -- authentication-scoped and only return the list of sessions that
  -- the caller is authorized to see.
  , withSession ::
      mode
        :- Capture "sessionId" SessionId
          :> NamedRoutes SessionOpenAPI
  -- ^ The rest of the API is scoped to a particular session.
  }
  deriving (Generic)

newtype SessionOpenAPI mode = SessionOpenAPI
  { getProgram ::
      mode
        :- "program"
          :> Summary "Get the current program"
          :> OpId "getProgram" Get '[JSON] API.Prog
  -- ^ Get the current program state.
  }
  deriving (Generic)
