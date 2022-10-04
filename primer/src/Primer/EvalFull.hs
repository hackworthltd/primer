-- STATUS:
-- re-implemented evalfull to be factored nicer
--   - viewing redex is  disentangled from finding normal order
-- everything compiles, primer-test test suite passes
-- EvalFull tests take ~14s now (default options, under -O0, as reported by tasty)
-- previously they took (on log-move-failure) ~15s
-- with --hedgehog-tests 0, is 0.69s vs 0.79s
-- no idea why... maybe agressive elision is a minor win even though have to look for free vars more often?
--                or maybe less monadic (fresh name/id) overhead?
-- With -O1 we get
--  new: 7s ; 0.31s
--  old: 7s ; 0.35s
--
-- Next steps:
--   - PR this?
--   - common up Eval
--   - push lets down
--   - audit

module Primer.EvalFull (
  Dir (..),
  EvalFullError (..),
  TerminationBound,
  evalFull,
  evalFullStepCount,
) where

-- TODO: share code with Primer.Eval
-- I hope to reuse this code in Eval - the current implementation does some weird things with annotations and metadata
-- but that will come later

-- TODO: ensure do sane things to metadata
-- (Perhaps we should just run a TC pass after each step?)
-- See https://github.com/hackworthltd/primer/issues/6

import Foreword hiding (hoistAccum)

import Control.Monad.Fresh (MonadFresh)
import Numeric.Natural (Natural)
import Primer.Core (
  Expr,
  ID,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Name (NameCounter)
import Primer.TypeDef (
  TypeDefMap,
 )
import Primer.Zipper (
  replace,
  unfocusExpr,
  unfocusType,
 )
import Primer.Log (ConvertLogMessage)
import Control.Monad.Log (MonadLog, WithSeverity)
import Primer.Eval.Redex (Dir(Chk,Syn), runRedex, runRedexTy)
import Primer.Eval.NormalOrder (RedexWithContext (RExpr, RType), findRedex)

newtype EvalFullError
  = TimedOut Expr
  -- We assume the input is type-correct, and don't even detect the most egregious flouting of that assumption
  deriving (Eq, Show)

-- Currently just a step limit
type TerminationBound = Natural

-- A naive implementation of normal-order reduction
evalFull :: (MonadFresh NameCounter m, MonadFresh ID m, MonadLog (WithSeverity l) m, ConvertLogMessage Text l)
  => TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> m (Either EvalFullError Expr)
evalFull tydefs env n d expr = snd <$> evalFullStepCount tydefs env n d expr

-- | As 'evalFull', but also returns how many reduction steps were taken.
-- (This is mostly useful for testing purposes.)
-- Note that we only detect termination when we fail to take a step, thus if
--
-- > evalFullStepCount _ _ m _ e = (s,Right _)
--
-- we have @m >= s+1@, as we do @s@ reductions, and then need to attempt one
-- more to notice termination.
evalFullStepCount ::
  (MonadFresh NameCounter m, MonadFresh ID m, MonadLog (WithSeverity l) m, ConvertLogMessage Text l) =>
  TypeDefMap ->
  DefMap ->
  TerminationBound ->
  Dir ->
  Expr ->
  m (Natural, Either EvalFullError Expr)
evalFullStepCount tydefs env n d = go 0
  where
    go s expr
      | s >= n = pure (s, Left $ TimedOut expr)
      | otherwise = case step tydefs env d expr of
          Nothing -> pure (s, Right expr) -- this is a normal form
          Just me -> me >>= go (s + 1)

-- The 'Dir' argument only affects what happens if the root is an annotation:
-- do we keep it (Syn) or remove it (Chk). I.e. is an upsilon reduction allowed
-- at the root?
step :: (MonadFresh NameCounter m, MonadFresh ID m, MonadLog (WithSeverity l) m, ConvertLogMessage Text l) => TypeDefMap -> DefMap -> Dir -> Expr -> Maybe (m Expr)
step tydefs g d e = case findRedex tydefs g d e of
  Nothing -> Nothing
  Just (RExpr ez r) -> Just $ unfocusExpr . flip replace ez  <$> runRedex r
  Just (RType et r) -> Just $ unfocusExpr . unfocusType . flip replace et . fst <$> runRedexTy r
