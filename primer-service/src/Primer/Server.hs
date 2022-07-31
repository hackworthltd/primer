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
  addSession,
  copySession,
  edit,
  evalFull,
  evalStep,
  flushSessions,
  generateNames,
  getApp,
  getProgram,
  getSessionName,
  getVersion,
  listSessions,
  newSession,
  renameSession,
  runPrimerIO,
  variablesInScope,
 )
import Primer.Action (
  Action (Move),
  ActionError (TypeError),
  Movement (Child1),
 )
import Primer.App (
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  Log (..),
  ProgAction (BodyAction, MoveToDef),
  ProgError (NoDefSelected),
  newProg',
 )
import Primer.Builtins (boolDef)
import Primer.Core (
  ASTDef (..),
  Def (..),
  Kind (KFun, KType),
  Type' (TEmptyHole),
  TypeCache (..),
  TypeCacheBoth (..),
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  app,
  branch',
  case_,
  create',
  emptyHole,
  tEmptyHole,
  tfun,
 )
import Primer.Database (
  Sessions,
  Version,
 )
import Primer.Database qualified as Database (
  Op,
 )
import Primer.Eval (BetaReductionDetail (..), EvalDetail (..))
import Primer.EvalFull (Dir (Syn))
import Primer.OpenAPI ()
import Primer.Pagination (pagedDefaultClamp)
import Primer.Servant.API (
  API,
  PrimerAPI,
  api,
  primerApi,
 )
import Primer.Servant.API.Root (PrimerLegacyAPI)
import Primer.Servant.API.Test (TestAPI)
import Primer.Servant.OpenAPI (PrimerOpenAPI)
import Primer.Typecheck (TypeError (TypeDoesNotMatchArrow))
import Servant (
  Handler (..),
  NoContent (..),
  Raw,
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
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (MaxAge (NoMaxAge), StaticSettings (ssIndices, ssMaxAge, ssRedirectToIndex), unsafeToPiece)

openAPIInfo :: OpenApi
openAPIInfo =
  toOpenApi (Proxy :: Proxy PrimerOpenAPI)
    & #info % #title .~ "Primer backend API"
    & #info % #description ?~ "A backend service implementing a pedagogic functional programming language."
    & #info % #version .~ "0.7"

serveStaticFiles :: ServerT Raw PrimerIO
serveStaticFiles =
  -- Static file settings. Sane defaults, plus:
  -- - if the user requests a directory (like /), look for an index.html
  --   file in that directory and redirect to it.
  -- - disable caching, because it's unhelpful during development.
  let settings =
        (defaultWebAppSettings ".")
          { ssIndices = [unsafeToPiece "index.html"]
          , ssRedirectToIndex = True
          , ssMaxAge = NoMaxAge
          }
   in serveDirectoryWith settings

-- These endpoints (de)serialize different types in the API, to help
-- with testing (de)serialization code.
testEndpoints :: ServerT TestAPI PrimerIO
testEndpoints =
  mkTest Child1
    :<|> mkTest (Move Child1)
    :<|> mkTest (TypeError (TypeDoesNotMatchArrow (TEmptyHole ())))
    :<|> mkTest "x"
    :<|> mkTest (create' (tfun tEmptyHole tEmptyHole))
    :<|> mkTest (TCSynthed $ TEmptyHole ())
    :<|> mkTest (TCBoth (TEmptyHole ()) (TEmptyHole ()))
    :<|> mkTest (create' (app emptyHole emptyHole))
    :<|> mkTest (create' $ case_ emptyHole [])
    :<|> mkTest (create' $ case_ emptyHole [branch' ("M" :| [], "C") [("x", Nothing)] emptyHole])
    :<|> mkTest (KFun KType KType)
    :<|> mkTest 0
    :<|> mkTest (Log [[BodyAction [Move Child1]]])
    :<|> mkTest newProg'
    :<|> mkTest (MoveToDef $ qualifyName (mkSimpleModuleName "M") "main")
    :<|> mkTest NoDefSelected
    :<|> mkTest (DefAST $ ASTDef expr ty)
    :<|> mkTest boolDef
    :<|> mkTest EvalReq{evalReqExpr = expr, evalReqRedex = 0}
    :<|> mkTest EvalResp{evalRespExpr = expr, evalRespRedexes = [0, 1], evalRespDetail = reductionDetail}
    :<|> mkTest EvalFullReq{evalFullReqExpr = expr, evalFullMaxSteps = 10, evalFullCxtDir = Syn}
    :<|> mkTest (EvalFullRespNormal expr)
  where
    mkTest x = pure x :<|> pure
    expr = create' emptyHole
    ty = create' tEmptyHole
    reductionDetail =
      BetaReduction
        BetaReductionDetail
          { betaBefore = expr
          , betaAfter = expr
          , betaBindingName = "x"
          , betaLambdaID = 0
          , betaLetID = 0
          , betaArgID = 0
          , betaBodyID = 0
          , betaTypes = Just (ty, ty)
          }

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
      ( addSession
          :<|> copySession
          :<|> getApp
          :<|> getVersion
          :<|> ( \sid ->
                  getSessionName sid
                    :<|> renameSession sid
                    :<|> edit sid
                    :<|> (variablesInScope sid :<|> generateNames sid)
                    :<|> evalStep sid
                    :<|> evalFull sid
                    :<|> testEndpoints
               )
      )
        :<|> flushSessions'
        :<|> serveStaticFiles
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
