-- | A Primer Servant API client.
--
-- This module exposes the full Primer API over HTTP.
module Primer.Client (
  defaultAPIPath,
  apiClient,
  copySession,
  getVersion,
  flushSessions,
  createSession,
  listSessions,
  addSession,
  deleteSession,
  getProgram,
  getApp,
  getSessionName,
  renameSession,
  edit,
  undo,
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
  getProgramOpenApi,
  availableActionsOpenAPI,
  actionOptionsOpenAPI,
  applyActionNoInputOpenAPI,
  applyActionInputOpenAPI,
) where

import Foreword

import Data.String (String)
import Primer.API (
  ExprTreeOpts (ExprTreeOpts),
  NewSessionReq,
 )
import Primer.API qualified
import Primer.Action.Available (Action, InputAction, NoInputAction, Options)
import Primer.App (
  App,
  EvalFullReq,
  EvalFullResp,
  EvalReq,
  EvalResp,
  Level,
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
 )
import Primer.Name (Name)
import Primer.Pagination (
  Paginated,
  Pagination,
 )
import Primer.Servant.API qualified as API
import Primer.Servant.OpenAPI qualified as OpenAPI
import Primer.Server (API (openAPI, servantAPI))
import Servant (NoContent)
import Servant.Client (
  AsClientT,
  ClientM,
  (//),
  (/:),
 )
import Servant.Client.Generic (genericClient)

-- | The base path for the Primer API.
--
-- Use this in any 'Servant.Client.Core.BaseUrl's that you construct
-- for use with the Primer API.  (Note that both the "full" api and
-- the "OpenAPI" apis are hosted below this path -- the functions in
-- this module will add the appropriate prefixes)
defaultAPIPath :: String
defaultAPIPath = ""

-- | A client for the full Primer API.
apiClient :: API.RootAPI (AsClientT ClientM)
apiClient = genericClient // servantAPI

-- | A client for the full Primer API.
openAPIClient :: OpenAPI.RootAPI (AsClientT ClientM)
openAPIClient = genericClient // openAPI

-- | As 'Primer.API.copySession'.
copySession :: SessionId -> ClientM SessionId
copySession = apiClient // API.copySession

-- | As 'Primer.API.getVersion'.
getVersion :: ClientM Text
getVersion = apiClient // API.getVersion

-- | As 'Primer.API.flushSessions'.
flushSessions :: ClientM ()
flushSessions = void $ apiClient // API.adminAPI // API.flushSessions

-- | As 'Primer.API.createSession'.
createSession :: NewSessionReq -> ClientM SessionId
createSession req = apiClient // API.sessionsAPI // API.createSession /: req

-- | As 'Primer.API.listSessions'.
listSessions :: Bool -> Pagination -> ClientM (Paginated Session)
listSessions inMemory pp = apiClient // API.sessionsAPI // API.getSessionList /: inMemory /: pp

-- | As 'Primer.API.addSession'.
addSession :: Text -> App -> ClientM SessionId
addSession name app = apiClient // API.sessionsAPI // API.addSession /: name /: app

-- | As 'Primer.API.deleteSession'.
deleteSession :: SessionId -> ClientM NoContent
deleteSession sid = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.deleteSession

-- | As 'Primer.API.getProgram'.
getProgram :: SessionId -> ClientM Prog
getProgram sid = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.getProgram

-- | As 'Primer.API.getProgram'.
getProgramOpenApi :: SessionId -> Primer.API.ExprTreeOpts -> ClientM Primer.API.Prog
getProgramOpenApi sid (ExprTreeOpts patternsUnder) = openAPIClient // OpenAPI.sessionsAPI // OpenAPI.sessionAPI /: sid // OpenAPI.getProgram $ patternsUnder

-- | As 'Primer.API.getApp'.
getApp :: SessionId -> ClientM App
getApp sid = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.getApp

-- | As 'Primer.API.getSessionName'.
getSessionName :: SessionId -> ClientM Text
getSessionName sid = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.getSessionName

-- | As 'Primer.API.renameSession'.
renameSession :: SessionId -> Text -> ClientM Text
renameSession sid name = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.setSessionName /: name

-- | As 'Primer.API.edit'.
edit :: SessionId -> MutationRequest -> ClientM (Either ProgError Prog)
edit sid req = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.editSession /: req

-- | As 'Primer.API.undo'.
undo :: SessionId -> MutationRequest -> ClientM (Either ProgError Prog)
undo sid req = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.undoSession /: req

-- | As 'Primer.API.variablesInScope'.
variablesInScope ::
  SessionId ->
  (GVarName, ID) ->
  ClientM (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
variablesInScope sid ctx = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.questionAPI // API.variablesInScope /: ctx

-- | As 'Primer.API.generateNames'.
generateNames ::
  SessionId ->
  ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind)) ->
  ClientM (Either ProgError [Name])
generateNames sid ctx = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.questionAPI // API.generateNames /: ctx

-- | As 'Primer.API.evalStep'.
evalStep :: SessionId -> EvalReq -> ClientM (Either ProgError EvalResp)
evalStep sid req = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.evalStep /: req

-- | As 'Primer.API.evalFull'.
evalFull :: SessionId -> EvalFullReq -> ClientM (Either ProgError EvalFullResp)
evalFull sid req = apiClient // API.sessionsAPI // API.sessionAPI /: sid // API.evalFull /: req

availableActionsOpenAPI :: SessionId -> Level -> Primer.API.Selection -> ClientM [Action]
availableActionsOpenAPI sid = openAPIClient // OpenAPI.sessionsAPI // OpenAPI.sessionAPI /: sid // OpenAPI.actions // OpenAPI.available

actionOptionsOpenAPI :: SessionId -> Level -> Primer.API.Selection -> InputAction -> ClientM Options
actionOptionsOpenAPI sid = openAPIClient // OpenAPI.sessionsAPI // OpenAPI.sessionAPI /: sid // OpenAPI.actions // OpenAPI.options

applyActionNoInputOpenAPI ::
  SessionId ->
  Primer.API.ExprTreeOpts ->
  Primer.API.Selection ->
  NoInputAction ->
  ClientM Primer.API.Prog
applyActionNoInputOpenAPI sid (ExprTreeOpts patternsUnder) =
  openAPIClient
    // OpenAPI.sessionsAPI
    // OpenAPI.sessionAPI
    /: sid
    // OpenAPI.actions
    // OpenAPI.apply
    // OpenAPI.simple
    $ patternsUnder

applyActionInputOpenAPI ::
  SessionId ->
  Primer.API.ExprTreeOpts ->
  Primer.API.ApplyActionBody ->
  InputAction ->
  ClientM Primer.API.Prog
applyActionInputOpenAPI sid (ExprTreeOpts patternsUnder) =
  openAPIClient
    // OpenAPI.sessionsAPI
    // OpenAPI.sessionAPI
    /: sid
    // OpenAPI.actions
    // OpenAPI.apply
    // OpenAPI.input
    $ patternsUnder
