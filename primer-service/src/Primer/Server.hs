{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

-- | An HTTP service for the Primer API.
module Primer.Server (
  serve,
  openAPIInfo,
) where

import Foreword hiding (Handler)

import Control.Concurrent.STM (
  TBQueue,
 )

import Data.OpenApi (OpenApi)
import Data.Streaming.Network.Internal (HostPreference (HostIPv4Only))
import Data.Text.Lazy qualified as LT (fromStrict)
import Data.Text.Lazy.Encoding qualified as LT (encodeUtf8)
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp (
  defaultSettings,
  setHost,
  setPort,
 )
import Network.Wai.Handler.Warp qualified as Warp (runSettings)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (..),
  cors,
  corsMethods,
  corsRequestHeaders,
  simpleCorsResourcePolicy,
  simpleHeaders,
  simpleMethods,
 )
import Optics ((%), (.~), (?~))
import Primer.API (
  Env (..),
  PrimerErr (..),
  PrimerIO,
  copySession,
  edit,
  evalFull,
  evalStep,
  flushSessions,
  generateNames,
  getProgram,
  getSessionName,
  getVersion,
  listSessions,
  newSession,
  renameSession,
  runPrimerIO,
  variablesInScope,
 )
import Primer.API qualified as API
import Primer.App (
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  MutationRequest,
  Prog,
  ProgError,
 )
import Primer.Core (
  GVarName,
  ID,
  Kind,
  LVarName,
  TyVarName,
  Type',
 )
import Primer.Database (
  Session,
  SessionId,
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Name (Name)
import Primer.OpenAPI ()
import Primer.Pagination (Paginated, PaginationParams, pagedDefaultClamp)
import Servant (
  Capture,
  Get,
  Handler (..),
  JSON,
  NoContent (..),
  Post,
  Put,
  QueryFlag,
  QueryParam',
  ReqBody,
  Required,
  Server,
  ServerError,
  ServerT,
  Strict,
  Summary,
  err500,
  errBody,
  hoistServer,
  (:<|>) (..),
  (:>),
 )
import Servant qualified (serve)
import Servant.OpenApi (toOpenApi)
import Servant.OpenApi.OperationId (OpId)

-- | The API.
--
-- All endpoints except raw files, new-session, and flush-sessions
-- require a session ID.

-- To be able to format these large types nicely, we disable ormolu and align them manually.
{- ORMOLU_DISABLE -}

type API =
       "openapi.json" :> Get '[JSON] OpenApi
  :<|> PrimerAPI

-- | 'PrimerOpenAPI' is the portion of our API that is documented with an exported
-- OpenAPI3 spec.
-- 'PrimerLegacyAPI' is everything else.
-- Over time, the 'PrimerLegacyAPI' should shrink as we improve our documentation.
type PrimerAPI = PrimerOpenAPI :<|> PrimerLegacyAPI

type PrimerOpenAPI =
  "api" :> "sessions" :> (
    -- POST /api/sessions
    --   create a new session on the backend, returning its id
    Summary "Create a new session" :>
    OpId "createSession" Post '[JSON] SessionId

    -- GET /api/sessions
    --   Get a list of all sessions and their
    --   human-readable names. By default this returns the list of all
    --   sessions in the persistent database, but optionally it can return
    --   just the list of all sessions in memory, which is mainly useful for
    --   testing. Note that in a production system, this endpoint should
    --   obviously be authentication-scoped and only return the list of
    --   sessions that the caller is authorized to see.
  :<|> QueryFlag "inMemory" :>
    PaginationParams :>
    Summary "List sessions" :>
    OpId "getSessionList" Get '[JSON] (Paginated Session)

    -- The rest of the API is scoped to a particular session
  :<|> Capture "sessionId" SessionId :> SOpenAPI
  )

type PrimerLegacyAPI =
  "api" :> (
    -- POST /api/copy-session
    --   Copy the session whose ID is given in the request body to a
    --   new session, and return the new session ID. Note that this
    --   method can be called at any time and is not part of the
    --   session-specific API, as it's not scoped by the current
    --   session ID like those methods are.
    "copy-session" :> ReqBody '[JSON] SessionId :> Post '[JSON] SessionId

    -- GET /api/version
    --   Get the current git version of the server
  :<|> "version" :> Get '[JSON] Text

    -- The rest of the API is scoped to a particular session
  :<|> QueryParam' '[Required, Strict] "session" SessionId :> SAPI
  )

    -- PUT /admin/flush-sessions
    --   Flush the in-memory session database.
    --   (All state will be preserved in the persistent database. This is a
    --   non-destructive operation.)
  :<|> "admin" :> ("flush-sessions" :> Put '[JSON] NoContent)

-- | The session-specific bits of the api
type SOpenAPI = (
    -- GET /api/sessions/:sessionId/program
    --   Get the current program state
    "program" :>
    Summary "Get the current program state" :>
    OpId "getProgram" Get '[JSON] API.Prog
  )

-- | The session-specific bits of the api
-- (legacy version)
type SAPI =
    -- GET /api/session-name
    --   Get the current session name.
       "session-name" :> Get '[JSON] Text

    -- PUT /api/session-name
    --   Attempt to set the current session name. Returns the new
    --   session name. (Note that this may differ from the name
    --   provided.)
  :<|> "session-name" :> ReqBody '[JSON] Text :> Put '[JSON] Text

    -- POST /api/edit
    --   Submit an action, returning an updated program state
  :<|> "edit" :> ReqBody '[JSON] MutationRequest :> Post '[JSON] (Either ProgError Prog)

    -- POST /question
    --   Submit a qestion, returning the answer or an error.
    --
    -- Ideally we'd model questions as a GADT, I don't know how to integrate that with Servant.
    -- Instead we just write out the types fully here.
  :<|> "question" :> (

    -- POST /question/variables-in-scope
    --   Ask what variables are in scope for the given node ID
    "variables-in-scope"
      :> ReqBody '[JSON] (GVarName, ID)
      :> Post '[JSON] (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))

    -- POST /question/generate-names
    --   Ask for a list of possible names for a binding at the given location.
    -- This method would be GET (since it doesn't modify any state) but we need to provide a request
    -- body, which isn't well supported for GET requests.
    :<|> "generate-names"
      :> ReqBody '[JSON] ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind))
      :> Post '[JSON] (Either ProgError [Name])
    )

    -- POST /eval-step
    --   Perform one step of evaluation on the given expression.
  :<|> "eval-step" :> ReqBody '[JSON] EvalReq :> Post '[JSON] (Either ProgError EvalResp)

    -- POST /eval
    --   Evaluate the given expression to normal form (or time out).
   :<|> "eval" :> ReqBody '[JSON] EvalFullReq :> Post '[JSON] (Either ProgError EvalFullResp)

{- ORMOLU_ENABLE -}

openAPIInfo :: OpenApi
openAPIInfo =
  toOpenApi (Proxy :: Proxy PrimerOpenAPI)
    & #info % #title .~ "Primer backend API"
    & #info % #description ?~ "A backend service implementing a pedagogic functional programming language."
    & #info % #version .~ "0.7"

primerApi :: Proxy PrimerAPI
primerApi = Proxy

api :: Proxy API
api = Proxy

hoistPrimer :: Env -> Server PrimerAPI
hoistPrimer e = hoistServer primerApi nt primerServer
  where
    nt :: PrimerIO a -> Handler a
    nt m = Handler $ ExceptT $ catch (Right <$> runPrimerIO m e) handler
    -- Catch exceptions from the API and convert them to Servant
    -- errors via 'Either'.
    handler :: PrimerErr -> IO (Either ServerError a)
    handler (DatabaseErr msg) = pure $ Left $ err500{errBody = (LT.encodeUtf8 . LT.fromStrict) msg}

primerServer :: ServerT PrimerAPI PrimerIO
primerServer = openAPIServer :<|> legacyServer
  where
    openAPIServer :: ServerT PrimerOpenAPI PrimerIO
    openAPIServer =
      newSession
        :<|> (\b p -> pagedDefaultClamp 100 p $ listSessions b)
        :<|> getProgram
    legacyServer :: ServerT PrimerLegacyAPI PrimerIO
    legacyServer =
      ( copySession
          :<|> getVersion
          :<|> ( \sid ->
                  getSessionName sid
                    :<|> renameSession sid
                    :<|> edit sid
                    :<|> (variablesInScope sid :<|> generateNames sid)
                    :<|> evalStep sid
                    :<|> evalFull sid
               )
      )
        :<|> flushSessions'
    -- We need to convert '()' from the API to 'NoContent'
    flushSessions' = flushSessions >> pure NoContent

server :: Env -> Server API
server e = pure openAPIInfo :<|> hoistPrimer e

-- | CORS settings for the Primer API. Note that this policy will not
-- work with credentialed requests because the origin is implicitly
-- "*". See:
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS#credentialed_requests_and_wildcards
apiCors :: CorsResourcePolicy
apiCors =
  simpleCorsResourcePolicy
    { corsMethods = simpleMethods <> ["PUT", "OPTIONS"]
    , corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
    }

serve :: Sessions -> TBQueue Database.Op -> Version -> Int -> IO ()
serve ss q v port = do
  putText $ "Listening on port " <> show port
  Warp.runSettings warpSettings $ noCache $ cors (const $ Just apiCors) $ Servant.serve api $ server $ Env ss q v
  where
    -- By default Warp will try to bind on either IPv4 or IPv6, whichever is
    -- available.
    -- This can be confusing because you can have two copies of the server
    -- running, bound to the same port, one on IPv4 and one on IPv6.
    -- Since we don't need IPv6 support right now, force the server to connect
    -- over IPv4.  This ensures there's only ever one running at once.
    warpSettings = defaultSettings & setPort port & setHost HostIPv4Only

    noCache :: WAI.Middleware
    noCache = WAI.modifyResponse $ WAI.mapResponseHeaders (("Cache-Control", "no-store") :)
