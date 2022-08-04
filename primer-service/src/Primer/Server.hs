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
import Primer.Database (
  SessionId,
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Pagination (pagedDefaultClamp)
import Primer.Servant.API qualified as S
import Primer.Servant.OpenAPI qualified as OpenAPI
import Servant (
  Handler (..),
  NoContent (..),
  Server,
  ServerError,
  ServerT,
  err500,
  errBody,
  hoistServer,
  (:<|>) (..),
 )
import Servant qualified (serve)
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServerT)

-- | All available API endpoints, plus the OpenAPI specification.
type API = OpenAPI.Spec :<|> PrimerAPI

-- | All available API endpoints.
--
-- 'OpenAPI.API' is the portion of our API that is implemented via an
-- OpenAPI 3-compliant specification. It mostly uses simpler types
-- than Primer's full core types, and is intended to be used with
-- clients that don't need to, or want to, know much about Primer
-- technical details. These clients are expected to focus mainly on
-- presentation and interaction, and leave the heavy lifting to the
-- backend service.
--
-- 'S.API' is a bespoke Servant API, and exposes the full core Primer
-- types, plus methods to act upon them, query them, etc. It is
-- probably most useful to clients written in Haskell that need to
-- access the full Primer API over HTTP.
type PrimerAPI = OpenAPI.API :<|> S.API

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

apiServer :: S.RootAPI (AsServerT PrimerIO)
apiServer =
  S.RootAPI
    { S.copySession = API.copySession
    , S.getVersion = API.getVersion
    , S.sessionAPI = sessionAPIServer
    , S.adminAPI = adminAPIServer
    }

sessionAPIServer :: SessionId -> S.SessionAPI (AsServerT PrimerIO)
sessionAPIServer sid =
  S.SessionAPI
    { S.getSessionName = API.getSessionName sid
    , S.setSessionName = renameSession sid
    , S.editSession = edit sid
    , S.questionAPI = questionAPIServer sid
    , S.evalStep = API.evalStep sid
    , S.evalFull = API.evalFull sid
    }

questionAPIServer :: SessionId -> S.QuestionAPI (AsServerT PrimerIO)
questionAPIServer sid =
  S.QuestionAPI
    { S.variablesInScope = API.variablesInScope sid
    , S.generateNames = API.generateNames sid
    }

adminAPIServer :: S.AdminAPI (AsServerT PrimerIO)
adminAPIServer =
  S.AdminAPI
    { S.flushSessions = API.flushSessions >> pure NoContent
    }

primerServer :: ServerT PrimerAPI PrimerIO
primerServer = openAPIServer :<|> apiServer

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
