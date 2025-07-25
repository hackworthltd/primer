{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Primer.Eval (
  -- The public API of this module
  step,
  redexes,
  findRedex,
  AvoidShadowing (..),
  RunRedexOptions (..),
  ViewRedexOptions (..),
  NormalOrderOptions (..),
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
  NormalOrderOptions (..),
  findRedex,
  foldMapExpr,
  singletonCxt,
 )
import Primer.Eval.Redex (
  Cxt (Cxt),
  Dir (..),
  EvalLog (..),
  MonadEval,
  RunRedexOptions (RunRedexOptions, pushAndElide),
  ViewRedexOptions (ViewRedexOptions, aggressiveElision, avoidShadowing, groupedLets),
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
  Loc' (InBind, InKind),
  TypeZ,
  focusOn,
  replace,
  target,
  unfocusExpr,
  unfocusType,
 )

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  MonadEval l m =>
  AvoidShadowing ->
  TypeDefMap ->
  DefMap ->
  Expr ->
  Dir ->
  ID ->
  m (Either EvalError (Expr, EvalDetail))
step as tydefs globals expr d i = runExceptT $
  case findNodeByID i d expr of
    Just (cxt, Left (d', z)) -> do
      (node', detail) <- tryReduceExpr as tydefs globals cxt d' (target z)
      let expr' = unfocusExpr $ replace node' z
      pure (expr', detail)
    Just (cxt, Right z) -> do
      (node', detail) <- tryReduceType as globals cxt (target z)
      let expr' = unfocusExpr $ unfocusType $ replace node' z
      pure (expr', detail)
    Nothing -> case focusOn i expr of
      -- This is expected to be very rare, as clients will probably
      -- pass in IDs that they think are redexes (probably from the output
      -- of @redexes@).
      -- However, we attempt to give non-confusing errors in this case.
      Just (InKind{}) -> throwError NotRedex
      Just (InBind{}) -> throwError NotRedex
      -- This matches @Nothing@, which is unambiguously a "NodeNotFound",
      -- but also @Just InExpr@ and @Just InType@, which should not happen
      -- as @findNodeByID@ failed.
      _ -> throwError $ NodeNotFound i

-- | Search for the given node by its ID.
-- Collect all immediately-surrounding let bindings and return them
-- (these are the ones we may push into this node)
-- along with the focused node.
-- Returns Nothing if the node is a binding, (note that no reduction rules can apply there).
findNodeByID :: ID -> Dir -> Expr -> Maybe (Cxt, Either (Dir, ExprZ) TypeZ)
findNodeByID i =
  foldMapExpr
    UnderBinders
    FMExpr
      { expr = \ez d c -> if getID ez == i then Just (c, Left (d, ez)) else Nothing
      , ty = \tz c -> if getID tz == i then Just (c, Right tz) else Nothing
      }

-- We hardcode a permissive set of options for the interactive eval
-- (i.e. these see more redexes)
evalOpts :: AvoidShadowing -> ViewRedexOptions
evalOpts as =
  ViewRedexOptions
    { groupedLets = True
    , aggressiveElision = True
    , avoidShadowing = case as of AvoidShadowing -> True; NoAvoidShadowing -> False
    }

data AvoidShadowing = AvoidShadowing | NoAvoidShadowing
  deriving stock (Show, Bounded, Enum)

-- | Return the IDs of nodes which are reducible.
-- We assume that the expression is well scoped. There are no
-- guarantees about whether we will claim that an ill-sorted variable
-- is inlinable, e.g. @lettype a = _ in case a of ...@.
--
-- NB: we return /all/ redexes, even those under binders. We ignore any
-- `NormalOrderOptions`. This means that more redexes may be returned than an
-- EvalFull would actually reduce (depending on the NormalOrderOptions given to
-- EvalFull).
redexes ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  AvoidShadowing ->
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  m [ID]
redexes as tydefs globals =
  (ListT.toList .)
    . foldMapExpr
      UnderBinders
      FMExpr
        { expr = \ez d -> liftMaybeT . runReaderT (getID ez <$ viewRedex (evalOpts as) tydefs globals d (target ez))
        , ty = \tz -> runReader (whenJust (getID tz) <$> viewRedexType (evalOpts as) (target tz))
        }
  where
    liftMaybeT :: Monad m' => MaybeT m' a -> ListT m' a
    liftMaybeT m = ListT $ fmap (,mempty) <$> runMaybeT m
    -- whenJust :: Alternative f => a -> Maybe b -> f a
    whenJust = maybe empty . const . pure

-- We hardcode a particular set of reduction options for the interactive evaluator
reductionOpts :: RunRedexOptions
reductionOpts =
  RunRedexOptions
    { -- For intearctive use, we think combining these two steps is too confusing.
      -- The choice of hardcoding this makes this feature slightly harder to test,
      -- see https://github.com/hackworthltd/primer/pull/736#discussion_r1293290757
      -- for some tests that we would like to have added, if it were simple to test
      -- a single step of pushAndElide.
      pushAndElide = False
    }

-- | Given a context of local and global variables and an expression, try to reduce that expression.
-- Expects that the expression is redex and will throw an error if not.
tryReduceExpr ::
  forall l m.
  (MonadEval l m, MonadError EvalError m) =>
  AvoidShadowing ->
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Dir ->
  Expr ->
  m (Expr, EvalDetail)
tryReduceExpr as tydefs globals cxt dir expr =
  runMaybeT (flip runReaderT cxt $ viewRedex (evalOpts as) tydefs globals dir expr) >>= \case
    Just r -> runRedex reductionOpts r
    _ -> throwError NotRedex

tryReduceType ::
  ( MonadEval l m
  , MonadError EvalError m
  ) =>
  AvoidShadowing ->
  DefMap ->
  Cxt ->
  Type ->
  m (Type, EvalDetail)
tryReduceType as _globals cxt =
  flip runReader cxt . viewRedexType (evalOpts as) <&> \case
    Just r -> runRedexTy reductionOpts r
    _ -> throwError NotRedex
