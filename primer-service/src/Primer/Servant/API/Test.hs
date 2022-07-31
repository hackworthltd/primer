module Primer.Servant.API.Test (
  Test,
  TestAPI,
) where

import Primer.Action (
  Action,
  ActionError,
  Movement,
 )
import Primer.App (
  EvalFullReq (..),
  EvalFullResp (..),
  EvalReq (..),
  EvalResp (..),
  Log (..),
  Prog,
  ProgAction,
  ProgError,
 )
import Primer.Core (
  ASTTypeDef,
  Def (..),
  Expr,
  ID,
  Kind,
  Type,
  TypeCache (..),
  TypeCacheBoth (..),
 )
import Primer.Name (Name)
import Servant (
  Get,
  JSON,
  Post,
  ReqBody,
  (:<|>) (..),
  (:>),
 )

-- | A type for a pair of test endpoints.
--
-- The first endpoint returns a fixture of the given type. The second
-- endpoint accepts a value of the given type as JSON and responds
-- with the value it was given, re-encoded as JSON.
type Test a = Get '[JSON] a :<|> (ReqBody '[JSON] a :> Post '[JSON] a)

-- | API endpoints that we use for integration tests.
type TestAPI =
  ( "movement" :> Test Movement
      :<|> "action" :> Test Action
      :<|> "actionerror" :> Test ActionError
      :<|> "name" :> Test Name
      :<|> "type" :> Test Type
      :<|> "typecache" :> Test TypeCache
      :<|> "typecacheboth" :> Test TypeCacheBoth
      :<|> "expr" :> Test Expr
      :<|> "exprCaseEmpty" :> Test Expr
      :<|> "exprCaseFull" :> Test Expr
      :<|> "kind" :> Test Kind
      :<|> "id" :> Test ID
      :<|> "log" :> Test Log
      :<|> "program" :> Test Prog
      :<|> "progaction" :> Test ProgAction
      :<|> "progerror" :> Test ProgError
      :<|> "def" :> Test Def
      :<|> "typeDef" :> Test ASTTypeDef
      :<|> "evalReq" :> Test EvalReq
      :<|> "evalResp" :> Test EvalResp
      :<|> "evalFullReq" :> Test EvalFullReq
      :<|> "evalFullResp" :> Test EvalFullResp
  )
