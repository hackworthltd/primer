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
  variablesInScope,
  generateNames,
  evalStep,
  evalFull,
) where

import Foreword

import Data.String (String)
import Primer.App (
  App,
  EvalFullReq,
  EvalFullResp,
  EvalReq,
  EvalResp,
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
import Servant (NoContent)
import Servant.Client (
  AsClientT,
  ClientM,
  (//),
  (/:),
 )
import Servant.Client.Generic (genericClient)

-- | The base path for the full Primer API.
--
-- Use this in any 'Servant.Client.Core.BaseUrl's that you construct
-- for use with the full Primer API.
defaultAPIPath :: String
defaultAPIPath = "/api"

-- | A client for the full Primer API.
apiClient :: API.RootAPI (AsClientT ClientM)
apiClient = genericClient

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
createSession :: Text -> ClientM SessionId
createSession name = apiClient // API.sessionsAPI // API.createSession /: name

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
