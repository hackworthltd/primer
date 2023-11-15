module Primer.EvalFullStep (
  Dir (..),
  EvalFullError (..),
  TerminationBound,
  evalFull,
  evalFullStepCount,
  EvalLog (..),
) where

-- TODO: ensure do sane things to metadata
-- (Perhaps we should just run a TC pass after each step?)
-- See https://github.com/hackworthltd/primer/issues/6

import Foreword

import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Data (Data)
import Numeric.Natural (Natural)
import Primer.Core (
  Expr,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.NormalOrder (NormalOrderOptions, RedexWithContext (RExpr, RType), findRedex)
import Primer.Eval.Redex (
  Dir (Chk, Syn),
  EvalLog (..),
  MonadEval,
  RunRedexOptions,
  ViewRedexOptions,
  runRedex,
  runRedexTy,
 )
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  replace,
  unfocusExpr,
  unfocusType,
 )

newtype EvalFullError
  = TimedOut Expr
  -- We assume the input is type-correct, and don't even detect the most egregious flouting of that assumption
  deriving stock (Eq, Show, Data, Generic)
  deriving anyclass (NFData)

-- Currently just a step limit
type TerminationBound = Natural

-- A naive implementation of normal-order reduction
evalFull ::
  MonadEval l m =>
  NormalOrderOptions ->
  ViewRedexOptions ->
  RunRedexOptions ->
  TypeDefMap ->
  DefMap ->
  TerminationBound ->
  Dir ->
  Expr ->
  m (Either EvalFullError Expr)
evalFull optsN optsV optsR tydefs env n d expr = snd <$> evalFullStepCount optsN optsV optsR tydefs env n d expr

-- | As 'evalFull', but also returns how many reduction steps were taken.
-- (This is mostly useful for testing purposes.)
-- Note that we only detect termination when we fail to take a step, thus if
--
-- > evalFullStepCount _ _ m _ e = (s,Right _)
--
-- we have @m >= s+1@, as we do @s@ reductions, and then need to attempt one
-- more to notice termination.
evalFullStepCount ::
  MonadEval l m =>
  NormalOrderOptions ->
  ViewRedexOptions ->
  RunRedexOptions ->
  TypeDefMap ->
  DefMap ->
  TerminationBound ->
  Dir ->
  Expr ->
  m (Natural, Either EvalFullError Expr)
evalFullStepCount optsN optsV optsR tydefs env n d = go 0
  where
    go s expr
      | s >= n = pure (s, Left $ TimedOut expr)
      | otherwise =
          runMaybeT (step optsN optsV optsR tydefs env d expr) >>= \case
            Nothing -> pure (s, Right expr) -- this is a normal form
            Just e -> go (s + 1) e

-- The 'Dir' argument only affects what happens if the root is an annotation:
-- do we keep it (Syn) or remove it (Chk). I.e. is an upsilon reduction allowed
-- at the root?
step ::
  MonadEval l m =>
  NormalOrderOptions ->
  ViewRedexOptions ->
  RunRedexOptions ->
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  MaybeT m Expr
step optsN optsV optsR tydefs g d e =
  findRedex optsN optsV tydefs g d e >>= \case
    RExpr ez r -> lift $ unfocusExpr . flip replace ez . fst <$> runRedex optsR r
    RType et r -> lift $ unfocusExpr . unfocusType . flip replace et . fst <$> runRedexTy optsR r
