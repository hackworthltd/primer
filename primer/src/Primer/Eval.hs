{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Eval (
  -- The public API of this module
  step,
  redexes,
  EvalError (..),
  EvalDetail (..),
  BetaReductionDetail (..),
  LocalVarInlineDetail (..),
  CaseReductionDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LetRenameDetail (..),
  PushAppIntoLetrecDetail (..),
  ApplyPrimFunDetail (..),
  -- Only exported for testing
  Locals,
  LocalLet (..),
  Cxt,singletonCxtLet,singletonCxtLetType,singletonCxtLetrec,
  tryReduceExpr,
  tryReduceType,
  findNodeByID,
  singletonLocal,
  RHSCaptured (..),
  Dir(..),
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Optics (
  elemOf,
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
  CaseBranch' (..),
  Expr,
  Expr' (..),
  GVarName,
  HasID (_id),
  ID,
  LVarName,
  LocalName (LocalName, unLocalName),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (..),
  bindName,
  getID,
 )
import Primer.Core.DSL.Type (tlet)
import Primer.Core.Transform (removeAnn, unfoldAPP, unfoldApp)
import Primer.Core.Utils (
  freeVars,
  freeVarsTy,
  regenerateTypeIDs,
  _freeTmVars,
  _freeTyVars,
  _freeVarsTy,
 )
import Primer.Def (DefMap)
import Primer.Eval.Detail (
  ApplyPrimFunDetail (..),
  BetaReductionDetail (..),
  CaseReductionDetail (..),
  EvalDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  LetRenameDetail (..),
  LocalLet (LLet, LLetRec, LLetType),
  LocalVarInlineDetail (..),
  Locals,
  PushAppIntoLetrecDetail (..),
  RHSCaptured (Capture, NoCapture),
  tryCaseReduction,
  tryInlineGlobal,
  tryInlineLocal,
  tryLetRemoval,
  tryPrimFun,
  tryReduceBETA,
  tryReduceBeta,
  tryReducePrim,
  tryReducePush,
 )
import Primer.Eval.EvalError (EvalError (..))
import Primer.Eval.Utils (makeSafeTLetBinding)
import Primer.Name (Name, NameCounter)
import Primer.Name.Fresh (isFresh, isFreshTy)
import Primer.Primitives.PrimDef (PrimDef)
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
import Primer.Eval.Redex (Dir(..), viewRedex, viewRedexType, runRedexTy, Cxt(Cxt))
import Primer.TypeDef (TypeDefMap)
import Primer.Eval.NormalOrder (foldMapExpr, FMExpr (FMExpr, subst, substTy, expr, ty)
                               ,singletonCxtLet,singletonCxtLetType,singletonCxtLetrec)
import Control.Monad.Log (WithSeverity, MonadLog)
import Primer.Log (ConvertLogMessage)

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  (MonadFresh ID m, MonadFresh NameCounter m
  , MonadLog (WithSeverity l) m, ConvertLogMessage Text l) =>
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
    Right _ -> do
      case findNodeByID' i expr of
        Just (cxt,Right z) -> do
          (node', detail) <- tryReduceType globals cxt (target z)
          let expr' = unfocusExpr $ unfocusType $ replace node' z
          pure (expr', detail)
        _ -> throwError $ NodeNotFound i

-- TODO: docs
findNodeByID' :: ID -> Expr -> Maybe (Cxt, Either ExprZ TypeZ)
findNodeByID' i  = foldMapExpr (FMExpr {
  expr = \ez _ c -> if getID ez == i then Just (c,Left ez) else Nothing
  ,ty = \tz c -> if getID tz == i then Just (c,Right tz) else Nothing
  ,subst = Nothing
  ,substTy = Nothing
  }) Syn -- the direction does not actually matter, as it is ignored everywhere
  

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
-- We assume the expression is well scoped, and do not e.g. check whether
-- @e@ refers to a type variable @x@ when deciding if we can reduce a
-- @let x = _ in e@ (we of course check whether @e@ refers to a term variable
-- @x@)
-- TODO/REVIEW: review comment^
redexes :: TypeDefMap ->
  DefMap ->
  Dir ->
  Expr -> Seq ID
redexes tydefs globals = foldMapExpr $ FMExpr {
    expr = \ez d -> runReader (maybe mempty (const $ pure $ getID ez) <$> viewRedex tydefs globals d (target ez))
  , ty = \tz -> runReader (whenJust (getID tz) <$> viewRedexType (target tz))
  , subst = Nothing
  , substTy = Nothing
                                               }
  where
    --whenJust :: Alternative f => a -> Maybe b -> f a
    whenJust = maybe empty . const . pure
{-
redexes :: Map GVarName PrimDef -> Expr -> Set ID
redexes primDefs = go mempty
  where
    -- letTm and letTy track the set of local variables we have a definition for,
    -- and the free vars of their RHSs, to tell if we go under a capturing binder
    go locals@(letTm, letTy) expr =
      -- A set containing just the ID of this expression
      let self = Set.singleton (expr ^. _id)
          freeTmVar = elemOf $ getting _freeTmVars % _2
          freeTyVar = elemOf $ getting _freeTyVars % _2
       in case expr of
            -- Application nodes are reducible only if their left child is a λ node or an annotation
            -- wrapping a λ node.
            -- (λ ...) x
            App _ e1@Lam{} e2 -> self <> go locals e1 <> go locals e2
            -- (λ ... : T) x
            App _ e1@(Ann _ Lam{} _) e2 -> self <> go locals e1 <> go locals e2
            -- (letrec x : T = t in λ ...) e  ~>  letrec x : T = t in ((λ...) e)
            -- We can reduce an application across a letrec as long as x isn't a free variable in e.
            -- If it was, it would be a different x and we'd cause variable capture if we
            -- substituted e into the λ body.
            App _ e1@(Letrec _ x _ _ Lam{}) e4 ->
              mwhen (isFresh x e4) self <> go locals e1 <> go locals e4
            -- Application of a primitive (fully-applied, with all arguments in normal form).
            App{} | Just _ <- tryPrimFun primDefs expr -> self
            -- f x
            App _ e1 e2 -> go locals e1 <> go locals e2
            APP _ e@LAM{} t -> self <> go locals e <> goType letTy t
            APP _ e@(Ann _ LAM{} _) t -> self <> go locals e <> goType letTy t
            -- (letrec x : T = t in Λ ...) e  ~>  letrec x : T = t in ((Λ ...) e)
            -- This is the same as the letrec case above, but for Λ
            APP _ e1@(Letrec _ x _ _ LAM{}) e4 ->
              mwhen (isFreshTy x e4) self <> go locals e1 <> goType letTy e4
            APP _ e t -> go locals e <> goType letTy t
            Var _ (LocalVarRef x)
              | Map.member x letTm -> self
              | otherwise -> mempty
            Var _ (GlobalVarRef x)
              | Map.member x primDefs -> mempty
              | otherwise -> self
            -- Note that x is in scope in e2 but not e1.
            Let _ x e1 e2 ->
              -- If we have something like let x = f x in (x,x), then we cannot
              -- substitute each occurrence individually, since the let would
              -- capture the new 'x' in 'f x'. We first rename to
              -- let y = f x in (y,y)
              let selfCapture = not $ isFresh x e1
                  locals' =
                    ( if selfCapture then letTm else insertTm x e1 letTm
                    , letTy
                    )
               in go locals e1 <> go locals' e2 <> munless (not selfCapture && freeTmVar x e2) self
            -- Whereas here, x is in scope in both e1 and e2.
            Letrec _ x e1 t e2 ->
              let locals' = (insertTm x e1 letTm, letTy)
               in go locals' e1 <> go locals' e2 <> goType letTy t <> munless (freeTmVar x e2) self
            -- As with Let, x is in scope in e but not in t
            LetType _ x t e ->
              -- We need to be careful that the LetType will not capture a
              -- variable occurrence arising from any potential substitution
              -- of itself. See the comment on 'Let' above for an example.
              let selfCapture = not $ isFreshTy x t
                  locals' = (removeTmTy x letTm, if selfCapture then letTy else insertTy x t letTy)
               in goType (snd locals) t <> go locals' e <> munless (not selfCapture && freeTyVar x e) self
            Lam _ x e -> go (removeTm x letTm, letTy) e
            LAM _ x e -> go (letTm, removeTy x letTy) e
            EmptyHole{} -> mempty
            Hole _ e -> go locals e
            Ann _ e t -> go locals e <> goType letTy t
            Con{} -> mempty
            Case _ e branches ->
              let branchRedexes (CaseBranch _ binds rhs) =
                    let locals' = (removeAll (map bindName binds) letTm, letTy)
                     in go locals' rhs
                  scrutRedex = case unfoldAPP $ fst $ unfoldApp $ removeAnn e of
                    (Con{}, _) -> self
                    _ -> mempty
               in scrutRedex <> go locals e <> mconcat (map branchRedexes branches)
            PrimCon{} -> mempty
    goType locals ty =
      -- A set containing just the ID of this type
      let self = Set.singleton (ty ^. _id)
       in case ty of
            TEmptyHole _ -> mempty
            THole _ t -> goType locals t
            TVar _ x
              | Map.member x locals -> self
              | otherwise -> mempty
            TCon _ _ -> mempty
            TFun _ a b -> goType locals a <> goType locals b
            TApp _ a b -> goType locals a <> goType locals b
            TForall _ x _ t -> goType (removeTy x locals) t
            TLet _ x t b ->
              -- As with term-level lets, we need to be careful about capture
              let selfCapture = not $ isFreshTy x t
                  locals' = if selfCapture then locals else insertTy x t locals
                  freeTyVar = elemOf $ getting _freeVarsTy % _2
               in goType locals t <> goType locals' b <> munless (not selfCapture && freeTyVar x b) self
    -- When going under a binder, outer binders of that name go out of scope,
    -- and any outer let bindings mentioning that name are not available for
    -- substitution (as the binder we are going under would capture such a
    -- reference)
    removeTm :: LVarName -> Map LVarName (Set Name) -> Map LVarName (Set Name)
    removeTm x = Map.filter (Set.notMember $ unLocalName x) . Map.delete x
    removeTy :: TyVarName -> Map TyVarName (Set TyVarName) -> Map TyVarName (Set TyVarName)
    removeTy x = Map.filter (Set.notMember x) . Map.delete x
    removeTmTy :: TyVarName -> Map LVarName (Set Name) -> Map LVarName (Set Name)
    removeTmTy x = Map.filter (Set.notMember $ unLocalName x) . Map.delete (LocalName $ unLocalName x)
    removeAll :: [LVarName] -> Map LVarName (Set Name) -> Map LVarName (Set Name)
    removeAll xs' =
      let xs = Set.fromList xs'
          xsNames = Set.fromList $ unLocalName <$> xs'
       in Map.filter (Set.disjoint xsNames) . flip Map.withoutKeys xs
    -- insert does not deal with self shadowing (as don't know let vs letrec)
    insertTm :: LVarName -> Expr -> Map LVarName (Set Name) -> Map LVarName (Set Name)
    insertTm x e = Map.insert x (freeVars e) . removeTm x
    insertTy :: TyVarName -> Type -> Map TyVarName (Set TyVarName) -> Map TyVarName (Set TyVarName)
    insertTy x t = Map.insert x (freeVarsTy t) . removeTy x
-}

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
  (tryLetRemoval -> Just m) -> second (either LetRename LetRemoval) <$> m
  (tryCaseReduction -> Just m) -> second CaseReduction <$> m
  _ -> throwError NotRedex

tryReduceType ::
  (MonadFresh ID m, MonadFresh NameCounter m
  , MonadError EvalError m, MonadLog (WithSeverity l) m, ConvertLogMessage Text l) =>
  DefMap ->
  Cxt ->
  Type ->
  m (Type, EvalDetail)
tryReduceType _globals cxt = flip runReader cxt . viewRedexType <&> \case
  Just r -> runRedexTy r
  _ -> throwError NotRedex
{-
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
          , TLetRename $
              LetRenameDetail
                { before = ty
                , after = ty'
                , bindingNameOld = unLocalName x
                , bindingNameNew = unLocalName y
                , letID = meta ^. _id
                , bindingOccurrences = t ^.. getting _freeVarsTy % to (first getID) % filtered ((== x) . snd) % _1
                , bodyID = body ^. _id
                }
          )
-}
