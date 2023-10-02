{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}

-- | An HTTP service for the Primer API.
module Primer.Server (
  API (..),
  serve,
  ServantLog (..),
  ConvertServerLogs,
  openAPIInfo,
  CorsAllowedOrigins (..),
  parseCorsAllowedOrigins,
  prettyPrintCorsAllowedOrigins,

  -- * Re-exported from "Network.Wai.Middleware.Cors"
  Origin,
) where

import Foreword hiding (Handler)

import Control.Concurrent.STM (
  TBQueue,
 )

import Control.Monad.Log (LoggingT, WithSeverity, runLoggingT)
import Control.Monad.Log qualified as Log
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.OpenApi (OpenApi, Reference (Reference), Referenced (Inline, Ref), ToSchema, toSchema)
import Data.Streaming.Network.Internal (HostPreference (HostIPv4Only))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT (fromStrict)
import Data.Text.Lazy.Encoding qualified as LT (encodeUtf8)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.Method (
  StdMethod (DELETE, OPTIONS, PUT),
  renderStdMethod,
 )
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp (
  defaultSettings,
  setHost,
  setPort,
 )
import Network.Wai.Handler.Warp qualified as Warp (runSettings)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (..),
  Origin,
  cors,
  corsMethods,
  corsRequestHeaders,
  simpleCorsResourcePolicy,
  simpleHeaders,
  simpleMethods,
 )
import Network.Wai.Middleware.Prometheus qualified as P
import Optics (mapped, (%), (%~), (.~), (?~), (^.))
import Primer.API (
  APILog,
  Env (..),
  PrimerErr (..),
  PrimerM,
  actionOptions,
  applyActionInput,
  applyActionNoInput,
  availableActions,
  createDefinition,
  createTypeDef,
  edit,
  evalFull',
  findSessions,
  listSessions,
  newSession,
  redo,
  renameSession,
  runPrimerM,
  undo,
 )
import Primer.API qualified as API
import Primer.Action.Available (InputAction, NoInputAction)
import Primer.App (Level)
import Primer.Core (globalNamePretty, moduleNamePretty, qualifyName)
import Primer.Database (
  SessionId,
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Eval (EvalLog)
import Primer.Finite (getFinite)
import Primer.Log (ConvertLogMessage, logInfo, logWarning)
import Primer.Name (unsafeMkName)
import Primer.Pagination (pagedDefault)
import Primer.Servant.API qualified as S
import Primer.Servant.OpenAPI (CreateTypeDefBody (CreateTypeDefBody))
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
import Type.Reflection (typeRep)

type Primer l = (PrimerM (LoggingT (WithSeverity l) IO))

type ConvertServerLogs l =
  ( ConvertLogMessage APILog l
  , ConvertLogMessage EvalLog l
  )

openAPIInfo :: OpenApi
openAPIInfo =
  toOpenApi (Proxy @OpenAPI.API)
    & #info
    % #title
    .~ "Primer backend API"
    & #info
    % #description
    ?~ "A backend service implementing a pedagogic functional programming language."
    & #info
    % #version
    .~ "0.7"
    & refParamSchemas @Level
      [ ("/openapi/sessions/{sessionId}/action/available", "level")
      , ("/openapi/sessions/{sessionId}/action/options", "level")
      ]
    & refParamSchemas @InputAction
      [ ("/openapi/sessions/{sessionId}/action/apply/input", "action")
      ]
    & refParamSchemas @NoInputAction
      [ ("/openapi/sessions/{sessionId}/action/apply/simple", "action")
      ]
  where
    {- This is a workaround for an upstream issue: https://github.com/biocad/servant-openapi3/issues/37.
    Given a type, and some query parameters of that type,
    this ensures that the specification of each parameter references a common schema,
    instead of inlining it.
    This could be made more general, (visit non-POST endpoints,
    modify `show . typeRep` output when it contains ` ` to match `openapi3`,
    search by schema shape so that we don't have to manually enumerate use sites),
    but _hopefully_ this will just be fixed upstream before we have to worry about any of that.
    -}
    refParamSchemas :: forall a. ToSchema a => [(FilePath, Text)] -> OpenApi -> OpenApi
    refParamSchemas params api =
      api
        & #components
        % #schemas
        %~ IOHM.insert name (toSchema $ Proxy @a)
        & #paths
        %~ composeList (map (uncurry $ flip adjustParam) params)
      where
        composeList = appEndo . foldMap' Endo
        adjustParam paramName =
          IOHM.adjust
            $ #post
            % mapped
            % #parameters
            % mapped
            %~ \case
              Inline x | x ^. #name == paramName -> Inline $ x & #schema ?~ Ref (Reference name)
              p -> p
        name = show $ typeRep @a

openAPIServer :: ConvertServerLogs l => OpenAPI.RootAPI (AsServerT (Primer l))
openAPIServer =
  OpenAPI.RootAPI
    { OpenAPI.copySession = API.copySession
    , OpenAPI.getVersion = API.getVersion
    , OpenAPI.sessionsAPI = openAPISessionsServer
    }

openAPISessionsServer :: ConvertServerLogs l => OpenAPI.SessionsAPI (AsServerT (Primer l))
openAPISessionsServer =
  OpenAPI.SessionsAPI
    { OpenAPI.createSession = newSession
    , OpenAPI.getSessionList = \s p -> pagedDefault 100 p $ maybe listSessions findSessions s
    , OpenAPI.sessionAPI = openAPISessionServer
    }

openAPISessionServer :: ConvertServerLogs l => SessionId -> OpenAPI.SessionAPI (AsServerT (Primer l))
openAPISessionServer sid =
  OpenAPI.SessionAPI
    { OpenAPI.deleteSession = API.deleteSession sid >> pure NoContent
    , OpenAPI.getProgram = API.getProgram' sid
    , OpenAPI.getSessionName = API.getSessionName sid
    , OpenAPI.setSessionName = renameSession sid
    , OpenAPI.getTypeOrKind = API.getSelectionTypeOrKind sid
    , OpenAPI.createDefinition = createDefinition sid
    , OpenAPI.typeDef = openAPITypeDefServer sid
    , OpenAPI.actions = openAPIActionServer sid
    , OpenAPI.evalFull = evalFull' sid . fmap getFinite
    , OpenAPI.undo = undo sid
    , OpenAPI.redo = redo sid
    }

openAPITypeDefServer :: ConvertServerLogs l => SessionId -> OpenAPI.TypeDefAPI (AsServerT (Primer l))
openAPITypeDefServer sid =
  OpenAPI.TypeDefAPI
    { create = \CreateTypeDefBody{moduleName, typeName, ctors} ->
        createTypeDef
          sid
          (qualifyName moduleName $ unsafeMkName typeName)
          (map (qualifyName moduleName . unsafeMkName) ctors)
    }

openAPIActionServer :: ConvertServerLogs l => SessionId -> OpenAPI.ActionAPI (AsServerT (Primer l))
openAPIActionServer sid =
  OpenAPI.ActionAPI
    { available = availableActions sid
    , options = actionOptions sid
    , apply =
        OpenAPI.ApplyActionAPI
          { simple = applyActionNoInput sid
          , input = applyActionInput sid
          }
    }

apiServer :: ConvertServerLogs l => S.RootAPI (AsServerT (Primer l))
apiServer =
  S.RootAPI
    { S.copySession = API.copySession
    , S.getVersion = API.getVersion
    , S.adminAPI = adminAPIServer
    , S.sessionsAPI = sessionsAPIServer
    }

sessionsAPIServer :: ConvertServerLogs l => S.SessionsAPI (AsServerT (Primer l))
sessionsAPIServer =
  S.SessionsAPI
    { S.createSession = newSession
    , S.getSessionList = \s p -> pagedDefault 100 p $ maybe listSessions findSessions s
    , S.addSession = API.addSession
    , S.sessionAPI = sessionAPIServer
    }

sessionAPIServer :: ConvertServerLogs l => SessionId -> S.SessionAPI (AsServerT (Primer l))
sessionAPIServer sid =
  S.SessionAPI
    { S.deleteSession = API.deleteSession sid >> pure NoContent
    , S.getProgram = API.getProgram sid
    , S.getApp = API.getApp sid
    , S.getSessionName = API.getSessionName sid
    , S.setSessionName = renameSession sid
    , S.editSession = edit sid
    , S.questionAPI = questionAPIServer sid
    , S.evalStep = API.evalStep sid
    , S.evalFull = API.evalFull sid
    }

questionAPIServer :: ConvertServerLogs l => SessionId -> S.QuestionAPI (AsServerT (Primer l))
questionAPIServer sid =
  S.QuestionAPI
    { S.variablesInScope = API.variablesInScope sid
    , S.generateNames = API.generateNames sid
    }

adminAPIServer :: ConvertServerLogs l => S.AdminAPI (AsServerT (Primer l))
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
  deriving stock (Generic)

server :: ConvertServerLogs l => API (AsServerT (Primer l))
server =
  API
    { getSpec = pure openAPIInfo
    , openAPI = openAPIServer
    , servantAPI = apiServer
    }

-- | CORS allowed origins.
data CorsAllowedOrigins
  = -- | Allow any origin. Equivalent to @Access-Control-Allow-Origin: *@.
    AllowAnyOrigin
  | -- | Allow one or more specific origins.
    Allow [Origin]
  deriving stock (Show, Eq, Generic)

-- | Pretty-print a 'CorsAllowedOrigins' value as a 'Text'.
prettyPrintCorsAllowedOrigins :: CorsAllowedOrigins -> Text
prettyPrintCorsAllowedOrigins = \case
  AllowAnyOrigin -> "*"
  Allow origins -> T.intercalate ", " $ map decodeUtf8 origins

-- | Parse a 'CorsAllowedOrigins' value from a list of 'Text'.
--
-- Each element of the list should be formatted as an [RFC
-- 6454](https://www.ietf.org/rfc/rfc6454.html) web origin, though for
-- practical reasons, the parser doesn't validate this.
--
-- Note that the literal string @*@ is not a valid origin. If you want
-- to specify that any origin is allowed, pass the empty list.
--
-- Also note that we do not allow the literal string @null@, per [the
-- W3C's
-- advice](https://w3c.github.io/webappsec-cors-for-developers/#avoid-returning-access-control-allow-origin-null).
--
-- If parsing is successful, the function returns a value of type
-- 'CorsAllowedOrigins' in a 'Right'. If parsing fails, the function
-- returns a 'Text' error message in a 'Left'.
parseCorsAllowedOrigins :: [Text] -> Either Text CorsAllowedOrigins
parseCorsAllowedOrigins [] = Right AllowAnyOrigin
parseCorsAllowedOrigins xs = case traverse parse xs of
  Left err -> Left err
  Right [] -> Right AllowAnyOrigin
  Right origins -> Right $ Allow $ encodeUtf8 <$> origins
  where
    parse "" = Left "The empty string is not a valid origin"
    parse "*" = Left "'*' is not a valid origin"
    parse "null" = Left "'null' is not permitted as an origin"
    parse x = Right x

-- | CORS settings for the Primer API.
apiCors :: CorsAllowedOrigins -> CorsResourcePolicy
apiCors origins =
  simpleCorsResourcePolicy
    { corsOrigins = toCorsOrigins origins
    , corsMethods = simpleMethods <> (renderStdMethod <$> [PUT, OPTIONS, DELETE])
    , corsRequestHeaders = simpleHeaders <> [hAuthorization]
    , -- Per:
      -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin#cors_and_caching
      corsVaryOrigin = origins /= AllowAnyOrigin
    , -- If we're running with any origin, then there's no point in
      -- requiring that the client send one.
      corsRequireOrigin = origins /= AllowAnyOrigin
    }
  where
    toCorsOrigins = \case
      AllowAnyOrigin -> Nothing
      Allow os -> Just (os, True)

data ServantLog
  = RequestStart
  deriving stock (Show, Read)

serve ::
  forall l.
  ( ConvertLogMessage PrimerErr l
  , ConvertServerLogs l
  , ConvertLogMessage ServantLog l
  ) =>
  Sessions ->
  TBQueue Database.Op ->
  Version ->
  Int ->
  CorsAllowedOrigins ->
  Log.Handler IO (Log.WithSeverity l) ->
  IO ()
serve ss q v port origins logger = do
  Warp.runSettings warpSettings
    $ noCache
    $
    -- It may make sense to allow access to some resources
    -- regardless of origin, but for now, we use a blanket CORS
    -- policy for every resource, hence the 'const' function here.
    cors (const $ Just $ apiCors origins)
    $ metrics
    $ genericServeT nt server
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

    metrics :: WAI.Middleware
    metrics = P.prometheus P.def

    nt :: Primer l a -> Handler a
    nt m =
      Handler
        $ ExceptT
        $ flip runLoggingT logger
        $ do
          -- This is not guaranteed to be consecutive with the logs from the action in the case of concurrent actions
          -- (unlikely in a dev environment, except perhaps a getProgram&getActions request)
          logInfo RequestStart
          catch (Right <$> runPrimerM m (Env ss q v)) handler

    -- Catch exceptions from the API and convert them to Servant
    -- errors via 'Either'.
    handler :: PrimerErr -> LoggingT (WithSeverity l) IO (Either ServerError a)
    handler e = do
      logWarning e
      pure . Left $ case e of
        DatabaseErr msg -> err500{errBody = encode msg}
        UnknownDef d -> err404{errBody = "Unknown definition: " <> encode (globalNamePretty d)}
        UnexpectedPrimDef d -> err400{errBody = "Unexpected primitive definition: " <> encode (globalNamePretty d)}
        UnknownTypeDef d -> err404{errBody = "Unknown type definition: " <> encode (globalNamePretty d)}
        UnexpectedPrimTypeDef d -> err400{errBody = "Unexpected primitive type definition: " <> encode (globalNamePretty d)}
        AddDefError m md pe -> err400{errBody = "Error while adding definition (" <> s <> "): " <> show pe}
          where
            s = encode $ case md of
              Just d -> globalNamePretty (qualifyName m $ unsafeMkName d)
              Nothing -> moduleNamePretty m
        AddTypeDefError tc vcs pe ->
          err400
            { errBody =
                "Error while adding type definition ("
                  <> encode (globalNamePretty tc)
                  <> " with constructors "
                  <> encode (T.intercalate ", " $ globalNamePretty <$> vcs)
                  <> "): "
                  <> show pe
            }
        ActionOptionsNoID id -> err404{errBody = "ID not found for action input options: " <> show id}
        ApplyActionError as pe -> err400{errBody = "Error while applying actions (" <> show as <> "): " <> show pe}
        ToProgActionError a ae -> err400{errBody = "Error while converting action (" <> show a <> "): " <> show ae}
        UndoError pe -> err500{errBody = "Undo failed: " <> show pe}
        RedoError pe -> err500{errBody = "Redo failed: " <> show pe}
        GetTypeOrKindError sel pe -> err400{errBody = "Error while getting type/kind (" <> show sel <> "): " <> show pe}
      where
        encode = LT.encodeUtf8 . LT.fromStrict
