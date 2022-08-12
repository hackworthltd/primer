-- | A Servant-generated HTTP service for the Primer API.
module Primer.Servant.API (
  API,
  RootAPI (..),
  SessionAPI (..),
  SessionsAPI (..),
  QuestionAPI (..),
  AdminAPI (..),
) where

import Foreword

import Primer.App (
  App,
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
 )
import Primer.Name (Name)
import Primer.Servant.Types (
  CopySession,
  CreateSession,
  GetSessionList,
  GetSessionName,
  GetVersion,
  SetSessionName,
 )
import Servant (
  Capture',
  Description,
  Get,
  JSON,
  NamedRoutes,
  NoContent (..),
  Post,
  Put,
  ReqBody,
  Summary,
  (:>),
 )
import Servant.API.Generic (
  GenericMode ((:-)),
 )

-- | The Primer API.
type API = "api" :> NamedRoutes RootAPI

data RootAPI mode = RootAPI
  { copySession :: CopySession mode
  , getVersion :: GetVersion mode
  , adminAPI ::
      mode
        :- "admin"
          :> NamedRoutes AdminAPI
  , sessionsAPI ::
      mode
        :- "sessions"
          :> NamedRoutes SessionsAPI
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

-- | The sessions API.
data SessionsAPI mode = SessionsAPI
  { createSession :: CreateSession mode
  , getSessionList :: GetSessionList mode
  , addSession ::
      mode
        :- "add-session"
          :> Summary "Directly add a session to the database"
          :> Description
              "Given an existing app and a proposed session name, \
              \create a new session and return its session ID. If the \
              \given session name is invalid, it will be replaced with \
              \a default session name. However, this is not reflected \
              \in the returned status code. Query the returned session ID \
              \to determine the actual session name that was assigned."
          :> Capture' '[Description "The session's name"] "name" Text
          :> ReqBody '[JSON] App
          :> Post '[JSON] SessionId
  , sessionAPI ::
      mode
        :- Capture' '[Description "The session ID"] "sessionId" SessionId
          :> NamedRoutes SessionAPI
  }
  deriving (Generic)

-- | The per-session bits of the API.
data SessionAPI mode = SessionAPI
  { getProgram ::
      mode
        :- "program"
          :> Summary "Get the current program program state"
          :> Get '[JSON] Prog
  , getSessionName :: GetSessionName mode
  , setSessionName :: SetSessionName mode
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
