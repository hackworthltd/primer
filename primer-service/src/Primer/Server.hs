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
  edit,
  listSessions,
  newSession,
  renameSession,
  runPrimerIO,
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
  SessionId,
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Name (Name)
import Primer.Pagination (pagedDefaultClamp)
import Primer.Servant.OpenAPI qualified as OpenAPI
import Servant (
  Description,
  Get,
  Handler (..),
  JSON,
  NamedRoutes,
  NoContent (..),
  Post,
  Put,
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
import Servant.API.Generic (
  GenericMode ((:-)),
 )
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServerT)

-- | The full API, plus the OpenAPI specification.
type API = OpenAPI.Spec :<|> PrimerAPI

-- | 'OpenAPI.API' is the portion of our API that is documented with
-- an exported OpenAPI3 spec. 'PrimerLegacyAPI' is everything else.
-- Over time, the 'PrimerLegacyAPI' should shrink as we improve our
-- documentation.
type PrimerAPI = OpenAPI.API :<|> PrimerLegacyAPI

type PrimerLegacyAPI = "api" :> NamedRoutes LegacyAPI

data LegacyAPI mode = LegacyAPI
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
          :> Post '[JSON] SessionId
  , getVersion ::
      mode
        :- "version"
          :> Summary "Get the current server version"
          :> Get '[JSON] Text
  , sessionAPI ::
      mode
        :- QueryParam' '[Required, Strict] "session" SessionId
          :> NamedRoutes SessionAPI
  , adminAPI ::
      mode
        :- "admin"
          :> NamedRoutes AdminAPI
  }
  deriving (Generic)

newtype AdminAPI mode = AdminAPI
  { flushSessions ::
      mode
        :- "flush-sessions"
          :> Summary "Flush the in-memory session database"
          :> Description
              "Flush the in-memory session database. Note that \
              \all dirty state will be saved to the persistent \
              \database before it's discarded from memory; i.e., \
              \this is a non-destructive operation."
          :> Put '[JSON] NoContent
  }
  deriving (Generic)

-- | The session-specific bits of the API (legacy version).
data SessionAPI mode = SessionAPI
  { getSessionName ::
      mode
        :- "session-name"
          :> Summary "Get the specified session's name"
          :> Get '[JSON] Text
  , setSessionName ::
      mode
        :- "session-name"
          :> Summary "Set the specified session's name"
          :> Description
              "Attempt to set the current session name. Returns the actual \
              \new session name. (Note that this may differ from the name \
              \provided.)"
          :> ReqBody '[JSON] Text
          :> Put '[JSON] Text
  , editSession ::
      mode
        :- "edit"
          :> Summary "Edit the program"
          :> Description "Submit an action, returning the updated program state."
          :> ReqBody '[JSON] MutationRequest
          :> Post '[JSON] (Either ProgError Prog)
  , questionAPI ::
      mode
        :- "question"
          :> NamedRoutes QuestionAPI
  , evalStep ::
      mode
        :- "eval-step"
          :> Summary "Perform one step of evaluation on the given expression"
          :> ReqBody '[JSON] EvalReq
          :> Post '[JSON] (Either ProgError EvalResp)
  , evalFull ::
      mode
        :- "eval"
          :> Summary "Evaluate the given expression to normal form (or time out)"
          :> ReqBody '[JSON] EvalFullReq
          :> Post '[JSON] (Either ProgError EvalFullResp)
  }
  deriving (Generic)

data QuestionAPI mode = QuestionAPI
  { variablesInScope ::
      mode
        :- "variables-in-scope"
          :> Summary "Ask what variables are in scope for the given node ID"
          :> ReqBody '[JSON] (GVarName, ID)
          :> Post '[JSON] (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
  , generateNames ::
      mode
        :- "generate-names"
          :> Summary "Ask for a list of possible names at the given location"
          :> Description
              "Ask for a list of possible names for a binding \
              \at the given location. This method would be GET \
              \(since it doesn't modify any state) but we need \
              \to provide a request body, which isn't well \
              \supported for GET requests."
          :> ReqBody '[JSON] ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind))
          :> Post '[JSON] (Either ProgError [Name])
  }
  deriving (Generic)

openAPIInfo :: OpenApi
openAPIInfo =
  toOpenApi (Proxy :: Proxy OpenAPI.API)
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

openAPIServer :: OpenAPI.SessionsAPI (AsServerT PrimerIO)
openAPIServer =
  OpenAPI.SessionsAPI
    { OpenAPI.createSession = newSession
    , OpenAPI.getSessionList = \b p -> pagedDefaultClamp 100 p $ listSessions b
    , OpenAPI.withSession = openAPISessionServer
    }

openAPISessionServer :: SessionId -> OpenAPI.SessionAPI (AsServerT PrimerIO)
openAPISessionServer sid =
  OpenAPI.SessionAPI
    { OpenAPI.getProg = API.getProgram sid
    }

legacyAPIServer :: LegacyAPI (AsServerT PrimerIO)
legacyAPIServer =
  LegacyAPI
    { copySession = API.copySession
    , getVersion = API.getVersion
    , sessionAPI = sessionAPIServer
    , adminAPI = adminAPIServer
    }

sessionAPIServer :: SessionId -> SessionAPI (AsServerT PrimerIO)
sessionAPIServer sid =
  SessionAPI
    { getSessionName = API.getSessionName sid
    , setSessionName = renameSession sid
    , editSession = edit sid
    , questionAPI = questionAPIServer sid
    , evalStep = API.evalStep sid
    , evalFull = API.evalFull sid
    }

questionAPIServer :: SessionId -> QuestionAPI (AsServerT PrimerIO)
questionAPIServer sid =
  QuestionAPI
    { variablesInScope = API.variablesInScope sid
    , generateNames = API.generateNames sid
    }

adminAPIServer :: AdminAPI (AsServerT PrimerIO)
adminAPIServer =
  AdminAPI
    { flushSessions = API.flushSessions >> pure NoContent
    }

primerServer :: ServerT PrimerAPI PrimerIO
primerServer = openAPIServer :<|> legacyAPIServer

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
