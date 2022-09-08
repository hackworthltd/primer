{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  AvailableActionsAPI (..),
  Spec,
) where

import Foreword

import Data.OpenApi (OpenApi)
import Primer.API qualified as API
import Primer.Action (Level)
import Primer.App (Mutability)
import Primer.Core
import Primer.Database (
  SessionId,
 )
import Primer.Name (Name)
import Primer.OpenAPI ()
import Primer.Servant.Types (
  CopySession,
  CreateSession,
  GetSessionList,
  GetSessionName,
  GetVersion,
  SetSessionName,
 )
import Servant (Capture, Capture', Description, Get, JSON, NamedRoutes, QueryFlag, Summary, (:>))
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
  , availableActionsAPI ::
      mode
        :- "action"
        :> "available"
        :> NamedRoutes AvailableActionsAPI
  }
  deriving (Generic)

-- POST needed for body, but lose caching etc.: https://stackoverflow.com/a/29210375 (and elsewhere)
-- :> Get '[JSON] [API.OfferedAction]
data AvailableActionsAPI mode = AvailableActionsAPI
  { getBodyActions ::
      mode
        :- "body"
        :> Summary "Get available actions at the given body node"
        -- :> QueryParam "level" Level
        -- :> QueryParam "mut" Mutability
        -- :> QueryParam "id" ID
        -- :> QueryParam "def" Name
        :> Capture "level" Level
        :> Capture "mut" Mutability
        :> Capture "id" ID
        :> Capture "def" Name
        -- :> Capture' '[] "level" Level
        -- :> Capture' '[] "mut" Mutability
        -- :> Capture' '[] "id" ID
        -- :> Capture' '[] "def" Name
        :> Capture "module" Name
        :> Get '[JSON] [API.OfferedAction]
        -- , getTypeActions :: ()
  }
  deriving (Generic)
