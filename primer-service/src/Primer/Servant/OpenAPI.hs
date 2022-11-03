{-# LANGUAGE DuplicateRecordFields #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  ActionAPI (..),
  ApplyActionAPI (..),
  Spec,
) where

import Foreword

import Data.OpenApi (OpenApi)
import Primer.API (ApplyActionBody, Selection)
import Primer.API qualified as API
import Primer.Action.Available qualified as Available
import Primer.Database (
  SessionId,
 )
import Primer.Level (Level (..))
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

data ActionAPI mode = ActionAPI
  { available ::
      mode
        :- "available"
          :> Summary "Get available actions for the definition, or a node within it, sorted by priority"
          :> QueryParam' '[Required, Strict] "level" Level
          :> ReqBody '[JSON] Selection
          :> OperationId "getAvailableActions"
          :> Post '[JSON] [Available.Action]
  , options ::
      mode
        :- "options"
          :> Summary "Get the input options for an action"
          :> QueryParam' '[Required, Strict] "level" Level
          :> ReqBody '[JSON] Selection
          :> QueryParam' '[Required, Strict] "action" Available.InputAction
          :> OperationId "getActionOptions"
          :> Post '[JSON] Available.Options
  , apply ::
      mode
        :- "apply"
          :> NamedRoutes ApplyActionAPI
  }
  deriving (Generic)

data ApplyActionAPI mode = ApplyActionAPI
  { simple ::
      mode
        :- "simple"
          :> Summary "Apply a simple action i.e. one which requires no further input"
          :> QueryFlag "patternsUnder"
          :> ReqBody '[JSON] Selection
          :> QueryParam' '[Required, Strict] "action" Available.NoInputAction
          :> OperationId "applyAction"
          :> Post '[JSON] API.Prog
  , input ::
      mode
        :- "input"
          :> Summary "Apply an action with some additional input"
          :> QueryFlag "patternsUnder"
          :> ReqBody '[JSON] ApplyActionBody
          :> QueryParam' '[Required, Strict] "action" Available.InputAction
          :> OperationId "applyActionWithInput"
          :> Post '[JSON] API.Prog
  }
  deriving (Generic)
