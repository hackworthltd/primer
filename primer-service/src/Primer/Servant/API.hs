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
 )
import Primer.Name (Name)
import Primer.Pagination (
  Paginated,
  PaginationParams,
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
  QueryFlag,
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
  { createSession ::
      mode
        :- Summary "Create a new session and return its ID"
          :> Post '[JSON] SessionId
  , getSessionList ::
      mode
        :- QueryFlag "inMemory"
          :> PaginationParams
          :> Summary "Get the list of sessions"
          :> Description
              "Get a list of all sessions and their human-readable names. By \
              \default, this method returns the list of all sessions in the \
              \persistent database, but optionally it can return just the list \
              \of all sessions in memory, which is mainly useful for \
              \testing. Note that in a production system, this endpoint should \
              \obviously be authentication-scoped and only return the list of \
              \sessions that the caller is authorized to see."
          :> Get '[JSON] (Paginated Session)
  , sessionAPI ::
      mode
        :- Capture' '[Description "The session ID"] "sessionId" SessionId
          :> NamedRoutes SessionAPI
  }
  deriving (Generic)

-- | The per-session bits of the API.
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
