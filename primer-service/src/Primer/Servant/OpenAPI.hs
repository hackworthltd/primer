{-# LANGUAGE DuplicateRecordFields #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  ActionAPI (..),
  Spec,
  ApplyActionBody (..),
  AvailableActionResult (..),
) where

import Foreword

import Data.OpenApi (OpenApi, ToSchema)
import Primer.API (Selection)
import Primer.API qualified as API
import Primer.Action (Level)
import Primer.Action.Available (InputAction, NoInputAction, OfferedAction)
import Primer.Database (
  SessionId,
 )
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
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
        :> Post '[JSON] [AvailableActionResult]
  , apply ::
      mode
        :- "apply"
        -- :> Summary "Get available actions for the definition, or a node within it"
        :> ReqBody '[JSON] ApplyActionBody
        :> OperationId "applyAction"
        :> Post '[JSON] API.Prog -- TODO return prog? or get from separate call? in the long run, this will return some kind of patch/diff
  }
  deriving (Generic)

-- TODO tuple would be nice, but I don't think OpenAPI supports it - find where B previously worked around
data ApplyActionBody = ApplyActionBody
  { selection :: Selection
  , action :: NoInputAction
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema) via PrimerJSON ApplyActionBody

-- only temporary
data AvailableActionResult = AvailableActionResult
  { extra :: OfferedAction -- TODO eventually this will be just the input field
  , action :: Either NoInputAction InputAction
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON, ToSchema) via PrimerJSON AvailableActionResult
