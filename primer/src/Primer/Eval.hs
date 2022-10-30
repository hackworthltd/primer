{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Eval (
  -- The public API of this module
  step,
  redexes,
  EvalError (..),
  EvalDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  LocalVarInlineDetail (..),
  CaseReductionDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  PushAppIntoLetrecDetail (..),
  ApplyPrimFunDetail (..),
  -- Only exported for testing
  Locals,
  LocalLet (..),
  tryReduceExpr,
  tryReduceType,
  findNodeByID,
  singletonLocal,
  RHSCaptured (..),
  Dir (..),
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import ListT (ListT (ListT))
import ListT qualified
import Optics (
  filtered,
  getting,
  notElemOf,
  to,
  view,
  (%),
  (^.),
  (^..),
  _1,
  _2,
 )
import Primer.Core (
  Expr,
  Expr' (..),
  HasID (_id),
  ID,
  LocalName (unLocalName),
  Type,
  Type' (..),
  getID,
 )
import Primer.Core.DSL.Type (tlet)
import Primer.Core.Utils (
  freeVars,
  freeVarsTy,
  regenerateTypeIDs,
  _freeVarsTy,
 )
import Primer.Def (DefMap)
import Primer.Eval.Detail (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  BindRenameDetail (..),
  CaseReductionDetail (..),
  EvalDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LocalLet (LLet, LLetRec, LLetType),
  LocalVarInlineDetail (..),
  Locals,
  PushAppIntoLetrecDetail (..),
  RHSCaptured (Capture, NoCapture),
  tryCaseReduction,
  tryInlineGlobal,
  tryInlineLocal,
  tryLetRemoval,
  tryReduceBETA,
  tryReduceBeta,
  tryReducePrim,
  tryReducePush,
 )
import Primer.Eval.EvalError (EvalError (..))
import Primer.Eval.NormalOrder (FMExpr (FMExpr, expr, subst, substTy, ty), foldMapExpr)
import Primer.Eval.Redex (Dir (..), EvalFullLog, viewRedex, viewRedexType)
import Primer.Eval.Utils (makeSafeTLetBinding)
import Primer.Log (ConvertLogMessage)
import Primer.Name (Name)
import Primer.TypeDef (TypeDefMap)
import Primer.Zipper (
  ExprZ,
  FoldAbove,
  Loc' (InBind, InExpr, InType),
  TypeZ,
  current,
  focusOn,
  foldAbove,
  foldAboveTypeZ,
  getBoundHereUp,
  getBoundHereUpTy,
  replace,
  target,
  unfocusExpr,
  unfocusType,
 )

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  MonadFresh ID m =>
  DefMap ->
  Expr ->
  ID ->
  m (Either EvalError (Expr, EvalDetail))
step globals expr i = runExceptT $ do
  (locals, nodeZ) <- maybe (throwError (NodeNotFound i)) pure (findNodeByID i expr)
  case nodeZ of
    Left z -> do
      (node', detail) <- tryReduceExpr globals locals (target z)
      let expr' = unfocusExpr $ replace node' z
      pure (expr', detail)
    Right z -> do
      (node', detail) <- tryReduceType globals locals (target z)
      let expr' = unfocusExpr $ unfocusType $ replace node' z
      pure (expr', detail)

-- | Search for the given node by its ID.
-- Collect all local let, letrec and lettype bindings in scope and return them
-- along with the focused node.
-- Returns Nothing if the node is a binding, because no reduction rules can apply there.
findNodeByID :: ID -> Expr -> Maybe (Locals, Either ExprZ TypeZ)
findNodeByID i expr = do
  loc <- focusOn i expr
  case loc of
    InExpr z ->
      let fls = foldAbove collectBinding z
       in pure (dropCache $ lets fls, Left z)
    InType z ->
      let fls' =
            foldAboveTypeZ
              collectBindingTy
              -- Since nothing both contains a type and binds a variable, we
              -- can write (const mempty) for the "border" argument,
              (const mempty)
              collectBinding
              z
       in pure (dropCache $ lets fls', Right z)
    InBind{} -> Nothing
  where
    collectBinding :: FoldAbove Expr -> FindLet
    collectBinding a = case (getBoundHereUp a, current a) of
      (bs, _) | Set.null bs -> mempty
      (bs, Let m x e _) | Set.member (unLocalName x) bs -> flLet (unLocalName x) (view _id m) (LLet e)
      -- Note that because @x@ is in scope in @e1@, we will allow @e1@ to be reduced even if this
      -- reduction may never terminate.
      -- See https://github.com/hackworthltd/primer/issues/4
      -- @x@ is not in scope in @t@.
      (_bs, Letrec m x e _t _bdy) ->
        --  | Set.member (unLocalName x) bs -- NB: this is always true: x is in
        -- scope for both e and bdy
        flLet (unLocalName x) (view _id m) (LLetRec e)
      (_bs, LetType m x t _bdy) ->
        --  | Set.member (unLocalName x) bs -- This guard is always true: we only
        -- can have prior::Expr, so we must be considering binders for bdy, not t
        flLet (unLocalName x) (view _id m) (LLetType t)
      (bs, _) -> flOthers bs
    collectBindingTy :: FoldAbove Type -> FindLet
    collectBindingTy a = case (getBoundHereUpTy a, current a) of
      (bs, _) | Set.null bs -> mempty
      (_bs, TLet m v t _bdy) -> flLet (unLocalName v) (view _id m) (LLetType t)
      (bs, _) -> flOthers $ Set.map unLocalName bs

-- Helper for findNodeByID
data FindLet = FL
  { -- Let bindings in scope (let, letrec and lettype)
    -- This is essentially @lets :: Locals@, except
    -- we cache the free vars of the expression (as an optimisation)
    lets :: Map Name (ID, LocalLet, Set Name, RHSCaptured)
  , -- Other bindings in scope (lambdas etc), which may shadow outer 'let's
    others :: Set Name
  }
flLet :: Name -> ID -> LocalLet -> FindLet
flLet n i l = FL{lets = singletonLocal' n (i, l), others = mempty}
flOthers :: Set Name -> FindLet
flOthers = FL mempty
instance Semigroup FindLet where
  inner <> outer =
    let allInners = Map.keysSet (lets inner) <> others inner
        f (i, e, fvs, Capture) = (i, e, fvs, Capture)
        f (i, e, fvs, NoCapture) = (i, e, fvs, if Set.disjoint allInners fvs then NoCapture else Capture)
     in FL
          (lets inner <> (f <$> lets outer `Map.withoutKeys` others inner))
          (others inner <> others outer)
instance Monoid FindLet where
  mempty = FL mempty mempty

dropCache :: Map Name (ID, LocalLet, Set Name, RHSCaptured) -> Locals
dropCache = fmap $ \(j, l, _, c) -> (j, l, c)

-- This is a wrapper exported for testing
singletonLocal :: Name -> (ID, LocalLet) -> Locals
singletonLocal n = dropCache . singletonLocal' n

-- This is used internally, and returns an result augmented with a cache of the free vars of the RHS
singletonLocal' :: Name -> (ID, LocalLet) -> Map Name (ID, LocalLet, Set Name, RHSCaptured)
singletonLocal' n (i, l) = Map.singleton n (i, l, fvs, c)
  where
    (fvs, c) = case l of
      LLet e -> let fvs' = freeVars e in (fvs', if Set.member n fvs' then Capture else NoCapture)
      LLetRec e -> (freeVars e, NoCapture)
      LLetType t ->
        let fvs' = Set.map unLocalName $ freeVarsTy t
         in (fvs', if Set.member n fvs' then Capture else NoCapture)

-- | Return the IDs of nodes which are reducible.
-- We assume that the expression is well scoped. There are no
-- guarantees about whether we will claim that an ill-sorted variable
-- is inlinable, e.g. @lettype a = _ in case a of ...@.
redexes ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalFullLog l) =>
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
        , subst = Nothing
        , substTy = Nothing
        }
  where
    liftMaybeT :: Monad m' => MaybeT m' a -> ListT m' a
    liftMaybeT m = ListT $ fmap (,mempty) <$> runMaybeT m
    -- whenJust :: Alternative f => a -> Maybe b -> f a
    whenJust = maybe empty . const . pure

-- | Given a context of local and global variables and an expression, try to reduce that expression.
-- Expects that the expression is redex and will throw an error if not.
tryReduceExpr ::
  (MonadFresh ID m, MonadError EvalError m) =>
  DefMap ->
  Locals ->
  Expr ->
  m (Expr, EvalDetail)
tryReduceExpr globals locals = \case
  (tryReduceBeta -> Just m) -> second BetaReduction <$> m
  (tryReduceBETA -> Just m) -> second BETAReduction <$> m
  (tryReducePush -> Just m) -> second PushAppIntoLetrec <$> m
  (tryReducePrim globals -> Just m) -> second ApplyPrimFun <$> m
  (tryInlineLocal locals -> Just m) -> second LocalVarInline <$> m
  (tryInlineGlobal globals -> Just m) -> second GlobalVarInline <$> m
  (tryLetRemoval -> Just m) -> second (either BindRename LetRemoval) <$> m
  (tryCaseReduction -> Just m) -> second CaseReduction <$> m
  _ -> throwError NotRedex

tryReduceType ::
  (MonadFresh ID m, MonadError EvalError m) =>
  DefMap ->
  Locals ->
  Type ->
  m (Type, EvalDetail)
tryReduceType _globals locals = \case
  -- Inline local type variable
  -- x=t |- x ==> t
  -- If the variable is not in the local set, that's fine - it just means it is bound by a big
  -- lambda that hasn't yet been reduced.
  TVar mTVar x
    | Just (i, LLetType t, NoCapture) <- Map.lookup (unLocalName x) locals -> do
        -- Since we're duplicating @t@, we must regenerate all its IDs.
        t' <- regenerateTypeIDs t
        pure
          ( t'
          , LocalTypeVarInline
              LocalVarInlineDetail
                { letID = i
                , varID = mTVar ^. _id
                , valueID = t ^. _id
                , bindingName = x
                , replacementID = t' ^. _id
                , isTypeVar = True
                }
          )
  ty@(TLet meta x t body)
    -- Redundant let removal
    -- tlet x = t1 in t2 ==> t2   if x not free in t2
    | notElemOf (getting _freeVarsTy % _2) x body ->
        pure
          ( body
          , TLetRemoval $
              LetRemovalDetail
                { before = ty
                , after = body
                , bindingName = unLocalName x
                , letID = meta ^. _id
                , bodyID = body ^. _id
                }
          )
    -- Renaming a potentially self-capturing let
    -- tlet x = f[x] in g[x] ==> tlet y = f[x] in g[y]
    | otherwise -> do
        let (y, body') = makeSafeTLetBinding x (freeVarsTy t) body
        ty' <- tlet y (pure t) (pure body')
        pure
          ( ty'
          , TBindRename $
              BindRenameDetail
                { before = ty
                , after = ty'
                , bindingNamesOld = [unLocalName x]
                , bindingNamesNew = [unLocalName y]
                , bindersOld = [meta ^. _id]
                , bindersNew = [getID ty']
                , bindingOccurrences = t ^.. getting _freeVarsTy % to (first getID) % filtered ((== x) . snd) % _1
                , bodyID = body ^. _id
                }
          )
  _ -> throwError NotRedex
