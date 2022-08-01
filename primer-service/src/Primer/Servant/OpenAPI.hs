module Primer.Servant.OpenAPI (
  PrimerOpenAPI,
  RootOpenAPI (..),
  SessionsOpenAPI (..),
  SessionOpenAPI (..),
) where

import Foreword

import Primer.API qualified as API
import Primer.Database (Session, SessionId)
import Primer.Pagination (Paginated, PaginationParams)
import Servant (
  Capture',
  Description,
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
type PrimerOpenAPI =
  "api"
    :> NamedRoutes RootOpenAPI

data RootOpenAPI mode = RootOpenAPI
  { getVersion ::
      mode
        :- "version"
          :> Summary "Get the git version of primer-service."
          :> OpId "getVersion" Get '[JSON] Text
  , sessionsApi ::
      mode
        :- "sessions"
          :> Summary "Sessions API."
          :> NamedRoutes SessionsOpenAPI
  }
  deriving (Generic)

data SessionsOpenAPI mode = SessionsOpenAPI
  { createSession ::
      mode
        :- Summary "Create a new session"
          :> Description
              "Create a new session, returning its session ID."
          :> OpId "createSession" Post '[JSON] SessionId
  , getSessionList ::
      mode
        :- QueryFlag "inMemory"
          :> PaginationParams
          :> Summary "List sessions"
          :> Description
              "Get a list of all sessions, with their human-readable names. \
              \By default this returns the list of all sessions in the persistent \
              \database, but optionally it can return just the list of all \
              \sessions in memory, which is mainly useful for testing. Note that \
              \in a production system, this endpoint should obviously be \
              \authentication-scoped and only return the list of sessions that \
              \the caller is authorized to see."
          :> OpId "getSessionList" Get '[JSON] (Paginated Session)
  , withSession ::
      mode
        :- Capture' '[Description "The session ID"] "sessionId" SessionId
          :> NamedRoutes SessionOpenAPI
  -- ^ The rest of the API is scoped to a particular session.
  }
  deriving (Generic)

newtype SessionOpenAPI mode = SessionOpenAPI
  { getProgram ::
      mode
        :- "program"
          :> Summary "Get the current program"
          :> Description "Get the current program state for the given session ID."
          :> OpId "getProgram" Get '[JSON] API.Prog
  }
  deriving (Generic)
