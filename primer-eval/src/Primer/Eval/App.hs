{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- This module defines the high level application functions for
-- program evaluation.

module Primer.Eval.App (
  handleEvalRequest,
  handleEvalFullRequest,
  EvalReq (..),
  EvalResp (..),
  EvalFullReq (..),
  EvalFullResp (..),
) where

import Foreword hiding (mod)

import Control.Monad.NestedError (MonadNestedError, throwError')
import Primer.App (
  MonadEditApp,
  allDefs,
  allTypes,
  appProg,
 )
import Primer.Core (
  Expr,
  ID (..),
 )
import Primer.Eval qualified as Eval
import Primer.Eval.Detail (EvalDetail)
import Primer.Eval.Redex (EvalLog)
import Primer.EvalFull (Dir (Syn), EvalFullError (TimedOut), TerminationBound, evalFull)
import Primer.JSON
import Primer.Log (ConvertLogMessage)

data EvalReq = EvalReq
  { evalReqExpr :: Expr
  , evalReqRedex :: ID
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalReq

data EvalResp = EvalResp
  { evalRespExpr :: Expr
  , evalRespRedexes :: [ID]
  , evalRespDetail :: EvalDetail
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalResp

data EvalFullReq = EvalFullReq
  { evalFullReqExpr :: Expr
  , evalFullCxtDir :: Dir -- is this expression in a syn/chk context, so we can tell if is an embedding.
  , evalFullMaxSteps :: TerminationBound
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalFullReq

-- If we time out, we still return however far we got
data EvalFullResp
  = EvalFullRespTimedOut Expr
  | EvalFullRespNormal Expr
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalFullResp

-- * Request handlers

-- | Handle an eval request (we assume that all such requests are implicitly in a synthesisable context)
handleEvalRequest ::
  ( MonadEditApp l e m
  , MonadNestedError Eval.EvalError e m
  , ConvertLogMessage EvalLog l
  ) =>
  EvalReq ->
  m EvalResp
handleEvalRequest req = do
  prog <- gets appProg
  result <- Eval.step (allTypes prog) (allDefs prog) (evalReqExpr req) Syn (evalReqRedex req)
  case result of
    Left err -> throwError' err
    Right (expr, detail) -> do
      redexes <- Eval.redexes (allTypes prog) (allDefs prog) Syn expr
      pure
        EvalResp
          { evalRespExpr = expr
          , evalRespRedexes = redexes
          , evalRespDetail = detail
          }

-- | Handle an eval-to-normal-form request
handleEvalFullRequest :: (MonadEditApp l e m, ConvertLogMessage EvalLog l) => EvalFullReq -> m EvalFullResp
handleEvalFullRequest (EvalFullReq{evalFullReqExpr, evalFullCxtDir, evalFullMaxSteps}) = do
  prog <- gets appProg
  result <- evalFull (allTypes prog) (allDefs prog) evalFullMaxSteps evalFullCxtDir evalFullReqExpr
  pure $ case result of
    Left (TimedOut e) -> EvalFullRespTimedOut e
    Right nf -> EvalFullRespNormal nf
