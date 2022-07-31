module Primer.Servant.API.Root (
  PrimerLegacyAPI,
) where

import Foreword

import Primer.App (App)
import Primer.Database (SessionId)
import Primer.Servant.API.Session (SessionAPI)
import Servant (
  Capture,
  Get,
  JSON,
  NoContent,
  Post,
  Put,
  QueryParam',
  Raw,
  ReqBody,
  Required,
  Strict,
  (:<|>),
  (:>),
 )

type PrimerLegacyAPI =
  "api"
    :> (
         -- POST /api/add-session
         --
         -- Exposes the 'addSession' operation.
         "add-session" :> ReqBody '[JSON] App :> Capture "name" Text :> Post '[JSON] SessionId
          -- POST /api/copy-session
          --   Copy the session whose ID is given in the request body to a
          --   new session, and return the new session ID. Note that this
          --   method can be called at any time and is not part of the
          --   session-specific API, as it's not scoped by the current
          --   session ID like those methods are.
          :<|> "copy-session" :> ReqBody '[JSON] SessionId :> Post '[JSON] SessionId
          -- GET /api/app
          --    Exposes the 'getApp' operation. This is useful for testing,
          --    but not much else.
          :<|> "app" :> Capture "id" SessionId :> Get '[JSON] App
          -- GET /api/version
          --   Get the current git version of the server
          :<|> "version" :> Get '[JSON] Text
          -- The rest of the API is scoped to a particular session
          :<|> QueryParam' '[Required, Strict] "session" SessionId :> SessionAPI
       )
    -- PUT /admin/flush-sessions
    --   Flush the in-memory session database.
    --   (All state will be preserved in the persistent database. This is a
    --   non-destructive operation.)
    :<|> "admin" :> ("flush-sessions" :> Put '[JSON] NoContent)
    -- GET /any-path
    --   Get the static file at any-path, if it exists
    :<|> Raw
