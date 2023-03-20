{-# LANGUAGE DuplicateRecordFields #-}

-- | An OpenAPI service for the Primer API.
module Primer.Servant.OpenAPI (
  API,
  RootAPI (..),
  SessionsAPI (..),
  SessionAPI (..),
  TypeDefAPI (..),
  CreateTypeDefBody (..),
  ActionAPI (..),
  ApplyActionAPI (..),
  Spec,
) where

import Foreword

import Data.OpenApi (OpenApi, ToSchema)
import Primer.API (ApplyActionBody, EvalFullResp, Prog, Selection)
import Primer.Action.Available qualified as Available
import Primer.App (Level)
import Primer.Core (GVarName, ModuleName)
import Primer.Database (
  SessionId,
 )
import Primer.Finite (Finite)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.OpenAPI ()
import Primer.Servant.Types (
  CopySession,
  CreateSession,
  DeleteSession,
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
  QueryParam,
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
  deriving stock (Generic)

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
  deriving stock (Generic)

-- | A static bound on the maximum requested timeout for evaluation endpoint
type EvalFullStepLimit = 100

-- | The session-specific bits of the API.
data SessionAPI mode = SessionAPI
  { deleteSession :: DeleteSession mode
  , getProgram ::
      mode
        :- "program"
          :> Summary "Get the current program state"
          :> OperationId "getProgram"
          :> Get '[JSON] Prog
  , getSessionName :: GetSessionName mode
  , setSessionName :: SetSessionName mode
  , createDefinition ::
      mode
        :- "def"
          :> Summary "Create a new definition"
          :> ReqBody '[JSON] ModuleName
          :> QueryParam "name" Text
          :> OperationId "createDefinition"
          :> Post '[JSON] Prog
  , typeDef :: mode :- "typedef" :> NamedRoutes TypeDefAPI
  , actions ::
      mode
        :- "action"
          :> NamedRoutes ActionAPI
  , evalFull ::
      mode
        :- "eval"
          :> Summary "Evaluate the named definition to normal form (or time out)"
          :> OperationId "eval-full"
          :> QueryParam "stepLimit" (Finite 0 EvalFullStepLimit)
          :> ReqBody '[JSON] GVarName
          :> Post '[JSON] EvalFullResp
  , undo ::
      mode
        :- "undo"
          :> Summary "Undo the last action"
          :> OperationId "undo"
          :> Post '[JSON] Prog
  }
  deriving stock (Generic)

newtype TypeDefAPI mode = TypeDefAPI
  { create ::
      mode
        :- Summary "Create a new type definition"
          :> OperationId "createTypeDef"
          :> ReqBody '[JSON] CreateTypeDefBody
          :> Post '[JSON] Prog
  }
  deriving stock (Generic)

data CreateTypeDefBody = CreateTypeDefBody
  { moduleName :: ModuleName
  , typeName :: Text
  , ctors :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON CreateTypeDefBody
  deriving (ToSchema) via PrimerJSON CreateTypeDefBody

data ActionAPI mode = ActionAPI
  { available ::
      mode
        :- "available"
          :> Summary "Get available actions for the definition, or a node within it, sorted by priority"
          :> LevelParam
          :> ReqBody '[JSON] Selection
          :> OperationId "getAvailableActions"
          :> Post '[JSON] [Available.Action]
  , options ::
      mode
        :- "options"
          :> Summary "Get the input options for an action"
          :> LevelParam
          :> ReqBody '[JSON] Selection
          :> QueryParam' '[Required, Strict] "action" Available.InputAction
          :> OperationId "getActionOptions"
          :> Post '[JSON] Available.Options
  , apply ::
      mode
        :- "apply"
          :> NamedRoutes ApplyActionAPI
  }
  deriving stock (Generic)

data ApplyActionAPI mode = ApplyActionAPI
  { simple ::
      mode
        :- "simple"
          :> Summary "Apply a simple action i.e. one which requires no further input"
          :> ReqBody '[JSON] Selection
          :> QueryParam' '[Required, Strict] "action" Available.NoInputAction
          :> OperationId "applyAction"
          :> Post '[JSON] Prog
  , input ::
      mode
        :- "input"
          :> Summary "Apply an action with some additional input"
          :> ReqBody '[JSON] ApplyActionBody
          :> QueryParam' '[Required, Strict] "action" Available.InputAction
          :> OperationId "applyActionWithInput"
          :> Post '[JSON] Prog
  }
  deriving stock (Generic)

type LevelParam = QueryParam' '[Required, Strict] "level" Level
