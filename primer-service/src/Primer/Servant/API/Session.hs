module Primer.Servant.API.Session (
  SessionAPI,
) where

import Foreword

import Primer.App (
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
import Primer.Name (Name)
import Primer.Servant.API.Test (TestAPI)
import Servant (
  Get,
  JSON,
  Post,
  Put,
  ReqBody,
  (:<|>),
  (:>),
 )

-- | The session-specific bits of the legacy API.
type SessionAPI =
  ( -- GET /api/session-name
    --   Get the current session name.
    "session-name" :> Get '[JSON] Text
      -- PUT /api/session-name
      --   Attempt to set the current session name. Returns the new
      --   session name. (Note that this may differ from the name
      --   provided.)
      :<|> "session-name" :> ReqBody '[JSON] Text :> Put '[JSON] Text
      -- POST /api/edit
      --   Submit an action, returning an updated program state
      :<|> "edit" :> ReqBody '[JSON] MutationRequest :> Post '[JSON] (Either ProgError Prog)
      -- POST /question
      --   Submit a qestion, returning the answer or an error.
      --
      -- Ideally we'd model questions as a GADT, I don't know how to integrate that with Servant.
      -- Instead we just write out the types fully here.
      :<|> "question"
        :> (
             -- POST /question/variables-in-scope
             --   Ask what variables are in scope for the given node ID
             "variables-in-scope"
              :> ReqBody '[JSON] (GVarName, ID)
              :> Post '[JSON] (Either ProgError (([(TyVarName, Kind)], [(LVarName, Type' ())]), [(GVarName, Type' ())]))
              -- POST /question/generate-names
              --   Ask for a list of possible names for a binding at the given location.
              -- This method would be GET (since it doesn't modify any state) but we need to provide a request
              -- body, which isn't well supported for GET requests.
              :<|> "generate-names"
                :> ReqBody '[JSON] ((GVarName, ID), Either (Maybe (Type' ())) (Maybe Kind))
                :> Post '[JSON] (Either ProgError [Name])
           )
      -- POST /eval-step
      --   Perform one step of evaluation on the given expression.
      :<|> "eval-step" :> ReqBody '[JSON] EvalReq :> Post '[JSON] (Either ProgError EvalResp)
      -- POST /eval
      --   Evaluate the given expression to normal form (or time out).
      :<|> "eval" :> ReqBody '[JSON] EvalFullReq :> Post '[JSON] (Either ProgError EvalFullResp)
      -- GET /api/test/<type>
      --   Get an arbitrary value of that type
      -- POST /api/test/<type>
      --   Post an arbitrary value of that type, responding with the value
      :<|> "test" :> TestAPI
  )
