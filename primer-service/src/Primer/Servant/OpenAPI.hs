{-# LANGUAGE DuplicateRecordFields #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  ActionAPI (..),
  AvailableActionsAPIBody (..),
  SigOrBodyID (..),
  Spec,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (OpenApi)
import Data.OpenApi.Schema (ToSchema)
import Primer.API qualified as API
import Primer.Action (Level)
import Primer.Core
import Primer.Database (
  SessionId,
 )
import Primer.JSON (CustomJSON (..), PrimerJSON)
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
  ReqBody,
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
          :> "available"
          :> NamedRoutes ActionAPI
  }
  deriving (Generic)

{- HLINT ignore ActionAPI "Use newtype instead of data" -}
data ActionAPI mode = ActionAPI
  { available ::
      mode
        :- "available"
          :> Summary "Get available actions for the definition, or a node within it"
          :> ReqBody '[JSON] AvailableActionsAPIBody
          :> OperationId "getAvailableActions"
          :> Post '[JSON] [API.OfferedAction]
  }
  deriving (Generic)
data AvailableActionsAPIBody = AvailableActionsAPIBody
  { def :: GVarName
  , id :: Maybe SigOrBodyID
  , level :: Level
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via PrimerJSON AvailableActionsAPIBody
data SigOrBodyID
  = SigID ID
  | BodyID ID
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via PrimerJSON SigOrBodyID
