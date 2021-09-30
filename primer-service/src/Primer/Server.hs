{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | An HTTP service for the Primer API.
module Primer.Server (
  serve,
) where

import Control.Concurrent.STM (
  TBQueue,
 )

--import Control.Exception (try)
import Control.Monad.Catch (catch)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader (runReaderT)
import Data.Function ((&))
import Data.Streaming.Network.Internal (HostPreference (HostIPv4Only))
import Data.Text (Text)
import qualified Data.Text.Lazy as LT (fromStrict)
import qualified Data.Text.Lazy.Encoding as LT (encodeUtf8)
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp (
  defaultSettings,
  setHost,
  setPort,
 )
import qualified Network.Wai.Handler.Warp as Warp (runSettings)
import Primer.API (
  Env (..),
  PrimerErr (..),
  PrimerM,
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
  InitialApp (..),
  Log (..),
  MutationRequest,
  Prog,
  ProgAction (BodyAction, MoveToDef),
  ProgError (NoDefSelected),
  Result (..),
  newProg,
 )
import Primer.Core (
  Def (..),
  Expr,
  ID,
  Kind (KFun, KType),
  Type,
  Type' (TEmptyHole),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeDef,
  boolDef,
 )
import Primer.Core.DSL (app, branch, case_, create, emptyHole, tEmptyHole, tfun)
import Primer.Database (
  SessionId,
  Sessions,
  Version,
 )
import qualified Primer.Database as Database (
  Op,
 )
import Primer.Eval (BetaReductionDetail (..), EvalDetail (..))
import Primer.EvalFull (Dir (Syn))
import Primer.Name (Name)
import Primer.Typecheck (TypeError (TypeDoesNotMatchArrow))
import Servant (
  Get,
  Handler (..),
  JSON,
  NoContent (..),
  Post,
  Proxy (Proxy),
  Put,
  QueryFlag,
  QueryParam',
  Raw,
  ReqBody,
  Required,
  Server,
  ServerError,
  ServerT,
  Strict,
  err500,
  errBody,
  hoistServer,
  (:<|>) (..),
  (:>),
 )
import qualified Servant (serve)
import Servant.Server.StaticFiles (serveDirectoryWith)
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (MaxAge (NoMaxAge), StaticSettings (ssIndices, ssMaxAge, ssRedirectToIndex), unsafeToPiece)

-- | The API.
--
-- All endpoints except raw files, new-session, and flush-sessions
-- require a session ID.

-- To be able to format these large types nicely, we disable ormolu and align them manually.
{- ORMOLU_DISABLE -}

-- | 'OpenAPI' is the portion of our API that is documented with an exported
-- OpenAPI3 spec.
-- 'LegacyAPI' is everything else.
-- Over time, the 'LegacyAPI' should shrink as we improve our documentation.
type API = OpenAPI :<|> LegacyAPI

type OpenAPI =
  "api" :> (
    -- POST /api/sessions
    --   create a new session on the backend, returning its id
    "sessions" :> ReqBody '[JSON] InitialApp :> Post '[JSON] SessionId

    -- GET /api/sessions
    --   Get a list of all sessions and their
    --   human-readable names. By default this returns the list of all
    --   sessions in the persistent database, but optionally it can return
    --   just the list of all sessions in memory, which is mainly useful for
    --   testing. Note that in a production system, this endpoint should
    --   obviously be authentication-scoped and only return the list of
    --   sessions that the caller is authorized to see.
  :<|> QueryFlag "inMemory" :> "sessions" :> Get '[JSON] [(SessionId, Text)])

type LegacyAPI =
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

    -- GET /any-path
    --   Get the static file at any-path, if it exists
  :<|> Raw

-- | The session-specific bits of the api
type SAPI = (

    -- GET /api/program
    --   Get the current program state
    "program" :> Get '[JSON] (Result ProgError Prog)

    -- GET /api/session-name
    --   Get the current session name.
  :<|> "session-name" :> Get '[JSON] Text

    -- PUT /api/session-name
    --   Attempt to set the current session name. Returns the new
    --   session name. (Note that this may differ from the name
    --   provided.)
  :<|> "session-name" :> ReqBody '[JSON] Text :> Put '[JSON] Text

    -- POST /api/edit
    --   Submit an action, returning an updated program state
  :<|> "edit" :> ReqBody '[JSON] MutationRequest :> Post '[JSON] (Result ProgError Prog)

    -- POST /question
    --   Submit a qestion, returning the answer or an error.
    --
    -- Ideally we'd model questions as a GADT, I don't know how to integrate that with Servant.
    -- Instead we just write out the types fully here.
  :<|> "question" :> (

    -- POST /question/variables-in-scope
    --   Ask what variables are in scope for the given node ID
    "variables-in-scope"
      :> ReqBody '[JSON] (ID, ID)
      :> Post '[JSON] (Result ProgError (([(Name, Kind)], [(Name, Type' ())]), [(ID, Name, Type' ())]))

    -- POST /question/generate-names
    --   Ask for a list of possible names for a binding at the given location.
    -- This method would be GET (since it doesn't modify any state) but we need to provide a request
    -- body, which isn't well supported for GET requests.
    :<|> "generate-names"
      :> ReqBody '[JSON] ((ID, ID), Either (Maybe (Type' ())) (Maybe Kind))
      :> Post '[JSON] (Result ProgError [Name])
    )

    -- POST /eval-step
    --   Perform one step of evaluation on the given expression.
  :<|> "eval-step" :> ReqBody '[JSON] EvalReq :> Post '[JSON] (Result ProgError EvalResp)

    -- POST /eval
    --   Evaluate the given expression to normal form (or time out).
   :<|> "eval" :> ReqBody '[JSON] EvalFullReq :> Post '[JSON] (Result ProgError EvalFullResp)

    -- GET /api/test/<type>
    --   Get an arbitrary value of that type
    -- POST /api/test/<type>
    --   Post an arbitrary value of that type, responding with the value
  :<|> "test" :> TestAPI
  )

-- | API endpoints that we use for integration tests
type TestAPI = (
       "movement"      :> Test Movement
  :<|> "action"        :> Test Action
  :<|> "actionerror"   :> Test ActionError
  :<|> "name"          :> Test Name
  :<|> "type"          :> Test Type
  :<|> "typecache"     :> Test TypeCache
  :<|> "typecacheboth" :> Test TypeCacheBoth
  :<|> "expr"          :> Test Expr
  :<|> "exprCaseEmpty" :> Test Expr
  :<|> "exprCaseFull"  :> Test Expr
  :<|> "kind"          :> Test Kind
  :<|> "id"            :> Test ID
  :<|> "log"           :> Test Log
  :<|> "program"       :> Test Prog
  :<|> "progaction"    :> Test ProgAction
  :<|> "progerror"     :> Test ProgError
  :<|> "def"           :> Test Def
  :<|> "typeDef"       :> Test TypeDef
  :<|> "evalReq"       :> Test EvalReq
  :<|> "evalResp"      :> Test EvalResp
  :<|> "evalFullReq"   :> Test EvalFullReq
  :<|> "evalFullResp"  :> Test EvalFullResp
  )

{- ORMOLU_ENABLE -}

-- | A type for a pair of test endpoints.
-- The first endpoint returns a fixture of the given type.
-- The second endpoint accepts a value of the given type as JSON and
-- responds with the value it was given, re-encoded as JSON.
type Test a = Get '[JSON] a :<|> (ReqBody '[JSON] a :> Post '[JSON] a)

serveStaticFiles :: ServerT Raw (PrimerM IO)
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

-- These endpoints (de)serialise different types in the API, to help
-- with testing (de)serialisation code.
testEndpoints :: ServerT TestAPI (PrimerM IO)
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
    :<|> mkTest (create' $ case_ emptyHole [branch "C" [("x", Nothing)] emptyHole])
    :<|> mkTest (KFun KType KType)
    :<|> mkTest 0
    :<|> mkTest (Log [[BodyAction [Move Child1]]])
    :<|> mkTest newProg
    :<|> mkTest (MoveToDef 0)
    :<|> mkTest NoDefSelected
    :<|> mkTest (Def 1 "main" expr ty)
    :<|> mkTest boolDef
    :<|> mkTest EvalReq{evalReqExpr = expr, evalReqRedex = 0}
    :<|> mkTest EvalResp{evalRespExpr = expr, evalRespRedexes = [0, 1], evalRespDetail = reductionDetail}
    :<|> mkTest EvalFullReq{evalFullReqExpr = expr, evalFullMaxSteps = 10, evalFullCxtDir = Syn}
    :<|> mkTest (EvalFullRespNormal expr)
  where
    mkTest x = pure x :<|> pure
    create' = fst . create
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

api :: Proxy API
api = Proxy

hoistPrimer :: Env -> Server API
hoistPrimer e = hoistServer api nt server
  where
    nt :: PrimerM IO a -> Handler a
    nt m = Handler $ ExceptT $ catch (Right <$> runReaderT m e) handler
    -- Catch exceptions from the API and convert them to Servant
    -- errors via 'Either'.
    handler :: PrimerErr -> IO (Either ServerError a)
    handler (DatabaseErr msg) = pure $ Left $ err500{errBody = (LT.encodeUtf8 . LT.fromStrict) msg}

server :: ServerT API (PrimerM IO)
server = openAPIServer :<|> legacyServer
  where
    openAPIServer = newSession :<|> listSessions
    legacyServer =
      ( copySession
          :<|> getVersion
          :<|> ( \sid ->
                  getProgram sid
                    :<|> getSessionName sid
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

serve :: Sessions -> TBQueue Database.Op -> Version -> Int -> IO ()
serve ss q v port = do
  putStrLn $ "Starting server on port " <> show port
  Warp.runSettings warpSettings $ noCache $ Servant.serve api $ hoistPrimer $ Env ss q v
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
