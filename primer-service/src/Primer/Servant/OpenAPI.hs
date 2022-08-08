-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  Spec,
) where

import Foreword

import Data.OpenApi (OpenApi)
import Primer.API qualified as API
import Primer.Database (
  Session,
  SessionId,
 )
import Primer.OpenAPI ()
import Primer.Pagination (Paginated, PaginationParams)
import Servant (
  Capture',
  Description,
  Get,
  JSON,
  NamedRoutes,
  Post,
  QueryFlag,
  ReqBody,
  Summary,
  (:>),
 )
import Servant.API.Generic (
  GenericMode ((:-)),
 )
import Servant.OpenApi.OperationId (OpId)

-- | Enable clients to obtain the OpenAPI specification.
type Spec = "openapi.json" :> Get '[JSON] OpenApi

-- | The Primer OpenAPI API.
type API = "openapi" :> NamedRoutes RootAPI

data RootAPI mode = RootAPI
  { copySession ::
      mode
        :- "copy-session"
          :> Summary "Copy a session to a new session"
          :> Description
              "Copy the session whose ID is given in the request body to a \
              \new session, and return the new session's ID. Note that this \
              \method can be called at any time and is not part of the \
              \session-specific API, as it's not scoped by the current \
              \session ID like those methods are."
          :> ReqBody '[JSON] SessionId
          :> OpId "copySession" Post '[JSON] SessionId
  , getVersion ::
      mode
        :- "version"
          :> Summary "Get the current server version"
          :> OpId "getVersion" Get '[JSON] Text
  , sessionsAPI ::
      mode
        :- "sessions"
          :> NamedRoutes SessionsAPI
  }
  deriving (Generic)

-- | The Primer OpenAPI sessions API.
--
-- Note: this API is currently incomplete.
data SessionsAPI mode = SessionsAPI
  { createSession ::
      mode
        :- Summary "Create a new session and return its ID"
          :> OpId "createSession" Post '[JSON] SessionId
  , getSessionList ::
      mode
        :- QueryFlag "inMemory"
          :> PaginationParams
          :> Summary "Get the list of sessions"
          :> Description
              "Get a list of all sessions and their \
              \human-readable names. By default, this method returns the list of all \
              \sessions in the persistent database, but optionally it can return \
              \just the list of all sessions in memory, which is mainly useful for \
              \testing. Note that in a production system, this endpoint should \
              \obviously be authentication-scoped and only return the list of \
              \sessions that the caller is authorized to see."
          :> OpId "getSessionList" Get '[JSON] (Paginated Session)
  , sessionAPI ::
      mode
        :- Capture' '[Description "The session ID"] "sessionId" SessionId
          :> NamedRoutes SessionAPI
  }
  deriving (Generic)

-- | The session-specific bits of the API.
newtype SessionAPI mode = SessionAPI
  { getProgram ::
      mode
        :- "program"
          :> Summary "Get the current program state"
          :> OpId "getProgram" Get '[JSON] API.Prog
  }
  deriving (Generic)
