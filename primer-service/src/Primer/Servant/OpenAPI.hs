module Primer.Servant.OpenAPI (
  PrimerOpenAPI,
) where

import Primer.API qualified as API
import Primer.Database (Session, SessionId)
import Primer.Pagination (Paginated, PaginationParams)
import Servant (
  Get,
  JSON,
  Post,
  QueryFlag,
  QueryParam',
  Required,
  Strict,
  Summary,
  (:<|>),
  (:>),
 )
import Servant.OpenApi.OperationId (OpId)

-- | The top-level OpenAPI endpoint.
type PrimerOpenAPI =
  "api"
    :> (
         -- POST /api/sessions
         --   create a new session on the backend, returning its id
         "sessions"
          :> Summary "Create a new session"
          :> OpId "createSession" Post '[JSON] SessionId
          -- GET /api/sessions
          --   Get a list of all sessions and their
          --   human-readable names. By default this returns the list of all
          --   sessions in the persistent database, but optionally it can return
          --   just the list of all sessions in memory, which is mainly useful for
          --   testing. Note that in a production system, this endpoint should
          --   obviously be authentication-scoped and only return the list of
          --   sessions that the caller is authorized to see.
          :<|> QueryFlag "inMemory"
            :> "sessions"
            :> PaginationParams
            :> Summary "List sessions"
            :> OpId "getSessionList" Get '[JSON] (Paginated Session)
          -- The rest of the API is scoped to a particular session
          :<|> QueryParam' '[Required, Strict] "session" SessionId :> SOpenAPI
       )

-- | The session-specific bits of the API.
type SOpenAPI =
  ( -- GET /api/program
    --   Get the current program state
    "program" :> Get '[JSON] API.Prog
  )
