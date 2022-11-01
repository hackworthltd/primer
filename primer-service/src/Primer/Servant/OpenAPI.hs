{-# LANGUAGE DuplicateRecordFields #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  ActionAPI (..),
  Spec,
) where

import Foreword

import Data.OpenApi (OpenApi)
import Primer.API (ApplyActionBody, Selection)
import Primer.API qualified as API
import Primer.Action (Level)
import Primer.Action.Available (
  ActionOptions,
  InputAction,
  NoInputAction,
  OfferedAction,
 )
import Primer.Database (
  SessionId,
 )
import Primer.OpenAPI ()
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
  Post,
  QueryFlag,
  QueryParam',
  ReqBody,
  Required,
  Strict,
  Summary,
  (:>),
 )
import Servant.API.Generic (
  GenericMode ((:-)),
 )
import Servant.OpenApi.OperationId (OperationId)

-- | Enable clients to obtain the OpenAPI specification.
type Spec = "openapi.json" :> Get '[JSON] OpenApi

-- | The Primer OpenAPI API.
type API = "openapi" :> NamedRoutes RootAPI

data RootAPI mode = RootAPI
  { copySession :: CopySession mode
  , getVersion :: GetVersion mode
  , sessionsAPI ::
      mode
        :- "sessions"
        :> NamedRoutes SessionsAPI
  }
  deriving (Generic)

-- | The Primer OpenAPI sessions API.
--
-- Note: this API is currently incomplete.
data SessionsAPI mode = SessionsAPI
  { createSession :: CreateSession mode
  , getSessionList :: GetSessionList mode
  , sessionAPI ::
      mode
        :- Capture' '[Description "The session ID"] "sessionId" SessionId
        :> NamedRoutes SessionAPI
  }
  deriving (Generic)

-- | The session-specific bits of the API.
data SessionAPI mode = SessionAPI
  { getProgram ::
      mode
        :- "program"
        :> Summary "Get the current program state"
        :> QueryFlag "patternsUnder"
        :> OperationId "getProgram"
        :> Get '[JSON] API.Prog
  , getSessionName :: GetSessionName mode
  , setSessionName :: SetSessionName mode
  , actions ::
      mode
        :- "action"
        :> NamedRoutes ActionAPI
  }
  deriving (Generic)

{- HLINT ignore ActionAPI "Use newtype instead of data" -}
data ActionAPI mode = ActionAPI
  { available ::
      mode
        :- "available"
        :> Summary "Get available actions for the definition, or a node within it"
        :> QueryParam' '[Required, Strict] "level" Level
        :> ReqBody '[JSON] Selection
        :> OperationId "getAvailableActions"
        :> Post '[JSON] [OfferedAction]
  , options :: -- TODO name?
      mode
        :- "options"
        -- :> Summary ""
        :> QueryParam' '[Required, Strict] "level" Level
        :> ReqBody '[JSON] Selection
        :> QueryParam' '[Required, Strict] "action" InputAction
        :> OperationId "getActionOptions"
        :> Post '[JSON] ActionOptions
  , apply :: -- NB this is only really for "action panel" actions - I suppose constructing type definitions (etc.) will have its own API, and we'll keep the old actions as a lower-level implementation detail, away from the API
      mode
        :- "apply"
        -- :> Summary "Get available actions for the definition, or a node within it"
        :> ReqBody '[JSON] Selection
        :> QueryParam' '[Required, Strict] "action" NoInputAction
        :> OperationId "applyAction"
        :> Post '[JSON] API.Prog -- TODO return prog? or get from separate call? in the long run, this will return some kind of patch/diff
  , applyWithInput :: -- TODO can we somehow say that whether to expect input depends on which set `action` param is in? and thus combine this endpoint with the above?
      mode
        :- "apply1" -- TODO name
        -- :> Summary ""
        :> ReqBody '[JSON] ApplyActionBody
        :> QueryParam' '[Required, Strict] "action" InputAction
        :> OperationId "applyActionWithInput"
        :> Post '[JSON] API.Prog
  }
  deriving (Generic)
