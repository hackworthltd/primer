{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | The Primer evaluation API.
--
-- This module defines the Primer evaluation API, which extends the
-- core Primer API with methods for evaluating Primer programs.
module Primer.Eval.API (
  EvalAPILog (..),
  MonadEvalAPILog,
  evalStep,
  evalFull,
  EvalFullResp (..),
  evalFull',
) where

import Foreword

import Control.Monad.Log (
  MonadLog,
  Severity (Informational, Warning),
  WithSeverity (WithSeverity),
  logMessage,
 )
import Data.Tuple.Extra (curry3)
import Primer.App (EditAppM)
import Primer.API (
  liftEditAppM,
  viewTreeExpr,
 )
import Primer.Eval.App qualified as App
import Primer.Eval.App (
  EvalFullReq (..),
  EvalReq (..),
  EvalResp (..),
  handleEvalRequest,
  handleEvalFullRequest,
 )
import Primer.Eval.EvalError (
  EvalError (..),
 )
import Primer.API (
  PrimerM,
  ReqResp (..),
  Tree,
 )
import Primer.Core (
  GVarName,
 )
import Primer.Core.DSL qualified as DSL
import Primer.Database (
  SessionId,
 )
import Primer.Eval.Redex (Dir (Chk), EvalLog)
import Primer.EvalFull (TerminationBound)
import Primer.JSON (
  CustomJSON (..),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Log (
  ConvertLogMessage (convert),
  PureLog,
 )

data EvalAPILog
  = EvalStep (ReqResp (SessionId, EvalReq) (Either EvalError EvalResp))
  | EvalFull (ReqResp (SessionId, EvalFullReq) (Either EvalError App.EvalFullResp))
  | EvalFull' (ReqResp (SessionId, Maybe TerminationBound, GVarName) EvalFullResp)
  deriving stock (Show, Read)

type MonadEvalAPILog l m = (MonadLog (WithSeverity l) m, ConvertLogMessage EvalAPILog l)

-- | A wrapper to log an API call
logAPI :: MonadEvalAPILog l m => (ReqResp a b -> (Severity, EvalAPILog)) -> (a -> PrimerM m b) -> a -> PrimerM m b
logAPI c resp req = do
  logMsg $ c $ Req req
  r <- resp req
  logMsg $ c $ Resp r
  pure r
  where
    logMsg = logMessage . uncurry WithSeverity . second convert

noError :: (ReqResp a b -> EvalAPILog) -> ReqResp a b -> (Severity, EvalAPILog)
noError = ((Informational,) .)

leftResultError :: (ReqResp a (Either e b) -> EvalAPILog) -> ReqResp a (Either e b) -> (Severity, EvalAPILog)
leftResultError c r@(Resp (Left _)) = (Warning, c r)
leftResultError c r = (Informational, c r)

evalStep ::
  (MonadIO m, MonadThrow m, MonadEvalAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  EvalReq ->
  PrimerM m (Either EvalError EvalResp)
evalStep = curry $ logAPI (leftResultError EvalStep) $ \(sid, req) ->
  liftEditAppM (handleEvalRequest req) sid

evalFull ::
  (MonadIO m, MonadThrow m, MonadEvalAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  EvalFullReq ->
  PrimerM m (Either EvalError App.EvalFullResp)
evalFull = curry $ logAPI (leftResultError EvalFull) $ \(sid, req) ->
  liftEditAppM (handleEvalFullRequest req) sid

-- | This type is the API's view of a 'App.EvalFullResp'
-- (this is expected to evolve as we flesh out the API)
data EvalFullResp
  = EvalFullRespTimedOut Tree
  | EvalFullRespNormal Tree
  deriving stock (Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EvalFullResp

-- | Evaluate some top level definition in a program.
--
-- Note that this is a simplified version of 'evalFull',
-- intended for non-Haskell clients
evalFull' ::
  forall m l.
  (MonadIO m, MonadThrow m, MonadEvalAPILog l m, ConvertLogMessage EvalLog l) =>
  SessionId ->
  Maybe TerminationBound ->
  GVarName ->
  PrimerM m EvalFullResp
evalFull' = curry3 $ logAPI (noError EvalFull') $ \(sid, lim, d) ->
  noErr <$> liftEditAppM (q lim d) sid
  where
    q ::
      Maybe TerminationBound ->
      GVarName ->
      EditAppM (PureLog (WithSeverity l)) Void EvalFullResp
    q lim d = do
      e <- DSL.gvar d
      x <-
        handleEvalFullRequest $
          EvalFullReq
            { evalFullReqExpr = e
            , evalFullCxtDir = Chk
            , evalFullMaxSteps = fromMaybe 10 lim
            }
      pure $ case x of
        App.EvalFullRespTimedOut e' -> EvalFullRespTimedOut $ viewTreeExpr e'
        App.EvalFullRespNormal e' -> EvalFullRespNormal $ viewTreeExpr e'
    noErr :: Either Void a -> a
    noErr = \case
      Right a -> a
      Left v -> absurd v
