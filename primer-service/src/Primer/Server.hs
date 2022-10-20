{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- | An HTTP service for the Primer API.
module Primer.Server (
  serve,
  openAPIInfo,
) where

import Foreword hiding (Handler)

import Control.Concurrent.STM (
  TBQueue,
 )

import Data.Map ((!?))
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
  ExprTreeOpts (..),
  NodeSelection (..),
  PrimerErr (..),
  PrimerIO,
  Selection (..),
  edit,
  getProgram,
  listSessions,
  newSession,
  renameSession,
  runPrimerIO,
  viewProg,
 )
import Primer.API qualified as API
import Primer.Action.Available (ActionRequest (ActionRequestSimple), OfferedAction (..), SomeAction (..), actionsForDef, actionsForDefBody, actionsForDefSig, inputAction, inputActionQualified, mkAction)
import Primer.App (MutationRequest (Edit), NodeType (..), progAllDefs, progAllTypeDefs)
import Primer.Core (globalNamePretty)
import Primer.Database (
  SessionId,
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Def (ASTDef (..), Def (..))
import Primer.Pagination (pagedDefaultClamp)
import Primer.Servant.API qualified as S
import Primer.Servant.OpenAPI qualified as OpenAPI
import Servant (
  Handler (Handler),
  NoContent (NoContent),
  ServerError (errBody),
  err400,
  err404,
  err500,
 )
import Servant.API.Generic (GenericMode ((:-)))
import Servant.OpenApi (toOpenApi)
import Servant.Server.Generic (AsServerT, genericServeT)

openAPIInfo :: OpenApi
openAPIInfo =
  toOpenApi (Proxy @OpenAPI.API)
    & #info % #title .~ "Primer backend API"
    & #info % #description ?~ "A backend service implementing a pedagogic functional programming language."
    & #info % #version .~ "0.7"

openAPIServer :: OpenAPI.RootAPI (AsServerT PrimerIO)
openAPIServer =
  OpenAPI.RootAPI
    { OpenAPI.copySession = API.copySession
    , OpenAPI.getVersion = API.getVersion
    , OpenAPI.sessionsAPI = openAPISessionsServer
    }

openAPISessionsServer :: OpenAPI.SessionsAPI (AsServerT PrimerIO)
openAPISessionsServer =
  OpenAPI.SessionsAPI
    { OpenAPI.createSession = newSession
    , OpenAPI.getSessionList = \b p -> pagedDefaultClamp 100 p $ listSessions b
    , OpenAPI.sessionAPI = openAPISessionServer
    }

openAPISessionServer :: SessionId -> OpenAPI.SessionAPI (AsServerT PrimerIO)
openAPISessionServer sid =
  OpenAPI.SessionAPI
    { OpenAPI.getProgram = \patternsUnder -> API.getProgram' (ExprTreeOpts{patternsUnder}) sid
    , OpenAPI.getSessionName = API.getSessionName sid
    , OpenAPI.setSessionName = renameSession sid
    , OpenAPI.actions = openAPIActionServer sid
    }

openAPIActionServer :: SessionId -> OpenAPI.ActionAPI (AsServerT PrimerIO)
openAPIActionServer sid =
  OpenAPI.ActionAPI
    { available = \level Selection{..} -> do
        prog <- getProgram sid
        let allDefs = progAllDefs prog
            allTypeDefs = progAllTypeDefs prog
        actions <- case node of
          Nothing ->
            pure $ actionsForDef level allDefs def
          Just NodeSelection{..} -> do
            case allDefs !? def of
              Nothing -> throwM $ UnknownDef def
              Just (_, DefPrim _) -> throwM $ UnexpectedPrimDef def
              Just (editable, DefAST ASTDef{astDefType = type_, astDefExpr = expr}) ->
                pure $ case nodeType of
                  SigNode -> do
                    actionsForDefSig level def editable id type_
                  BodyNode -> do
                    actionsForDefBody (snd <$> allTypeDefs) level def editable id expr
        pure $
          actions <&> \case
            NoInputAction a -> NoInputRequired a
            InputAction a -> inputAction level a
            InputActionQualified a -> inputActionQualified level a
    , apply = \OpenAPI.ApplyActionBody{selection, action} -> do
        -- TODO DRY with above
        prog <- getProgram sid
        let allDefs = progAllDefs prog
            _allTypeDefs = progAllTypeDefs prog
        case allDefs !? selection.def of
          Nothing -> throwM $ UnknownDef selection.def
          Just (_, DefPrim _) -> throwM $ UnexpectedPrimDef selection.def
          Just (_, DefAST def) -> do
            let patternsUnder = True -- TODO don't hardcode (then again, I expect the option itself to be short-lived)
            actions <-
              either (throwM . MiscPrimerErr) pure $
                mkAction
                  (snd <$> progAllDefs prog)
                  def
                  selection.def
                  (selection.node <&> \s -> (s.nodeType, s.id))
                  action
            edit sid (Edit actions)
              >>= either
                (throwM . ApplyActionError actions)
                (pure . viewProg (ExprTreeOpts{patternsUnder}))
    }

apiServer :: S.RootAPI (AsServerT PrimerIO)
apiServer =
  S.RootAPI
    { S.copySession = API.copySession
    , S.getVersion = API.getVersion
    , S.adminAPI = adminAPIServer
    , S.sessionsAPI = sessionsAPIServer
    }

sessionsAPIServer :: S.SessionsAPI (AsServerT PrimerIO)
sessionsAPIServer =
  S.SessionsAPI
    { S.createSession = newSession
    , S.getSessionList = \b p -> pagedDefaultClamp 100 p $ listSessions b
    , S.addSession = API.addSession
    , S.sessionAPI = sessionAPIServer
    }

sessionAPIServer :: SessionId -> S.SessionAPI (AsServerT PrimerIO)
sessionAPIServer sid =
  S.SessionAPI
    { S.getProgram = API.getProgram sid
    , S.getApp = API.getApp sid
    , S.getSessionName = API.getSessionName sid
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

-- | All available API endpoints, plus the OpenAPI specification.
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
data API mode = API
  { getSpec :: mode :- OpenAPI.Spec
  , openAPI :: mode :- OpenAPI.API
  , servantAPI :: mode :- S.API
  }
  deriving (Generic)

server :: API (AsServerT PrimerIO)
server =
  API
    { getSpec = pure openAPIInfo
    , openAPI = openAPIServer
    , servantAPI = apiServer
    }

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
  Warp.runSettings warpSettings $
    noCache $
      cors (const $ Just apiCors) $
        genericServeT nt server
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

    nt :: PrimerIO a -> Handler a
    nt m = Handler $ ExceptT $ catch (Right <$> runPrimerIO m (Env ss q v)) handler

    -- Catch exceptions from the API and convert them to Servant
    -- errors via 'Either'.
    handler :: PrimerErr -> IO (Either ServerError a)
    handler =
      pure . Left . \case
        DatabaseErr msg -> err500{errBody = encode msg}
        UnknownDef d -> err404{errBody = "Unknown definition: " <> encode (globalNamePretty d)}
        UnexpectedPrimDef d -> err400{errBody = "Unexpected primitive definition: " <> encode (globalNamePretty d)}
        ApplyActionError as e -> err400{errBody = "Error while applying action"}
        MiscPrimerErr t -> err400{errBody = "Misc error: " <> encode t}
      where
        encode = LT.encodeUtf8 . LT.fromStrict
