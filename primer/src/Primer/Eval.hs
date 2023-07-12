{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Primer.Eval (
  -- The public API of this module
  step,
  redexes,
  EvalLog (..),
  EvalError (..),
  EvalDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  LocalVarInlineDetail (..),
  CaseReductionDetail (..),
  CaseReductionTrivialDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  ApplyPrimFunDetail (..),
  PushLetDetail (..),
  -- Only exported for testing
  Cxt (Cxt),
  singletonCxt,
  lookupEnclosingLet,
  tryReduceExpr,
  tryReduceType,
  findNodeByID,
  Dir (..),
) where

import Foreword hiding (until)

import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import ListT (ListT (ListT))
import ListT qualified
import Primer.Core (
  Expr,
  ID,
  Type,
  getID,
 )
import Primer.Def (DefMap)
import Primer.Eval.Detail (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  CaseReductionDetail (..),
  CaseReductionTrivialDetail (..),
  EvalDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LocalVarInlineDetail (..),
  PushLetDetail (..),
 )
import Primer.Eval.EvalError (EvalError (..))
import Primer.Eval.NormalOrder (
  FMExpr (FMExpr, expr, ty),
  foldMapExpr,
  singletonCxt,
 )
import Primer.Eval.Redex (
  Cxt (Cxt),
  Dir (..),
  EvalLog (..),
  MonadEval,
  lookupEnclosingLet,
  runRedex,
  runRedexTy,
  viewRedex,
  viewRedexType,
 )
import Primer.Log (ConvertLogMessage)
import Primer.TypeDef (TypeDefMap)
import Primer.Zipper (
  ExprZ,
  TypeZ,
  replace,
  target,
  unfocusExpr,
  unfocusType,
 )

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  MonadEval l m =>
  TypeDefMap ->
  DefMap ->
  Expr ->
  Dir ->
  ID ->
  m (Either EvalError (Expr, EvalDetail))
step tydefs globals expr d i = runExceptT $ do
  (cxt, nodeZ) <- maybe (throwError (NodeNotFound i)) pure (findNodeByID i d expr)
  case nodeZ of
    Left (d', z) -> do
      (node', detail) <- tryReduceExpr tydefs globals cxt d' (target z)
      let expr' = unfocusExpr $ replace node' z
      pure (expr', detail)
    Right z -> do
      (node', detail) <- tryReduceType globals cxt (target z)
      let expr' = unfocusExpr $ unfocusType $ replace node' z
      pure (expr', detail)

-- | Search for the given node by its ID.
-- Collect all immediately-surrounding let bindings and return them
-- (these are the ones we may push into this node)
-- along with the focused node.
-- Returns Nothing if the node is a binding, (note that no reduction rules can apply there).
findNodeByID :: ID -> Dir -> Expr -> Maybe (Cxt, Either (Dir, ExprZ) TypeZ)
findNodeByID i =
  foldMapExpr
    FMExpr
      { expr = \ez d c -> if getID ez == i then Just (c, Left (d, ez)) else Nothing
      , ty = \tz c -> if getID tz == i then Just (c, Right tz) else Nothing
      }

-- | Return the IDs of nodes which are reducible.
-- We assume that the expression is well scoped. There are no
-- guarantees about whether we will claim that an ill-sorted variable
-- is inlinable, e.g. @lettype a = _ in case a of ...@.
redexes ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  m [ID]
redexes tydefs globals =
  (ListT.toList .)
    . foldMapExpr
      FMExpr
        { expr = \ez d -> liftMaybeT . runReaderT (getID ez <$ viewRedex tydefs globals d (target ez))
        , ty = \tz -> runReader (whenJust (getID tz) <$> viewRedexType (target tz))
        }
  where
    liftMaybeT :: Monad m' => MaybeT m' a -> ListT m' a
    liftMaybeT m = ListT $ fmap (,mempty) <$> runMaybeT m
    -- whenJust :: Alternative f => a -> Maybe b -> f a
    whenJust = maybe empty . const . pure

-- | Given a context of local and global variables and an expression, try to reduce that expression.
-- Expects that the expression is redex and will throw an error if not.
tryReduceExpr ::
  forall l m.
  (MonadEval l m, MonadError EvalError m) =>
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Dir ->
  Expr ->
  m (Expr, EvalDetail)
tryReduceExpr tydefs globals cxt dir expr =
  runMaybeT (flip runReaderT cxt $ viewRedex tydefs globals dir expr) >>= \case
    Just r -> runRedex r
    _ -> throwError NotRedex

tryReduceType ::
  ( MonadEval l m
  , MonadError EvalError m
  ) =>
  DefMap ->
  Cxt ->
  Type ->
  m (Type, EvalDetail)
tryReduceType _globals cxt =
  flip runReader cxt . viewRedexType <&> \case
    Just r -> runRedexTy r
    _ -> throwError NotRedex
