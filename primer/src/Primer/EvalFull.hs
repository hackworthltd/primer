{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}

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

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Data (Data)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set.Optics (setOf)
import GHC.Err (error)
import Numeric.Natural (Natural)
import Optics (Fold, anyOf, getting, hasn't, set, summing, (%), _1, _2)
import Primer.Core (
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  Def (defExpr, defType),
  Expr,
  Expr' (
    APP,
    Ann,
    App,
    Case,
    Con,
    GlobalVar,
    Hole,
    LAM,
    Lam,
    Let,
    LetType,
    Letrec,
    PrimCon,
    Var
  ),
  ExprMeta,
  ID,
  Kind,
  PrimFun (PrimFun, primFunDef, primFunType),
  Type,
  Type' (
    TApp,
    TCon,
    TEmptyHole,
    TForall,
    TFun,
    THole,
    TVar
  ),
  TypeDef (typeDefName, typeDefParameters),
  TypeMeta,
  bindName,
  _typeMeta,
 )
import Primer.Core.DSL (ann, create, letType, let_, letrec, var)
import Primer.Core.Transform (unfoldAPP, unfoldApp, unfoldFun)
import Primer.Core.Utils (generateTypeIDs, noHoles)
import Primer.Eval (regenerateExprIDs, regenerateTypeIDs)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, ToJSON, VJSON)
import Primer.Name (Name, NameCounter, freshName)
import Primer.Primitives (globalPrims)
import Primer.Subst (freeVars, freeVarsTy, _freeVars, _freeVarsTy)
import Primer.Typecheck (instantiateValCons', lookupConstructor)
import Primer.Zipper (
  ExprZ,
  TypeZ,
  down,
  focus,
  focusType,
  getBoundHere,
  replace,
  right,
  target,
  unfocusExpr,
  unfocusType,
  up,
 )

newtype EvalFullError
  = TimedOut Expr
  -- We assume the input is type-correct, and don't even detect the most egregious flouting of that assumption
  deriving (Eq, Show)

data Redex
  = -- f  ~>  e : T  where we have  f : T ; f = e  in (global) scope
    InlineGlobal ID Def
  | -- x  ~>  e   where we are inside the scope of a  let x = e in ...
    InlineLet Name Expr
  | -- x  ~>  letrec x:T=t in t:T   where we are inside the scope of a  letrec x : T = t in ...
    InlineLetrec Name Expr Type
  | -- let(rec/type) x = e in t  ~>  t  if x does not appear in t
    ElideLet Name Local Expr
  | -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
    Beta Name Expr Type Type Expr
  | -- (Λa.t : ∀b.T) S  ~>  lettype b = S in (lettype a = S in t) : T
    BETA Name Expr Name Type Type
  | -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- also the non-annotated case, as we consider constructors to be synthesisable
    -- case C as of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- (This is the natural rule if we consider non-annotated constructors to
    -- be shorthand for a annotated-lambda wrapping, and combine a few
    -- reduction steps. E.g.
    --     cons ==  (Λa λx λxs. Cons @a x xs) : ∀a. a -> List a -> List a
    -- )
    CaseRedex Name [(Expr, Type)] (Either Type (Type' ())) [Name] Expr
  | -- [ t : T ]  ~>  t  writing [_] for the embedding of syn into chk
    -- This only fires for concrete (non-holey, no free vars) T, as otherwise the
    -- annotation can act as a type-changing cast:
    --   Nat ∋ [ True : ? ] checks, but Nat ∋ True fails
    Upsilon Expr Type
  | -- λy.t  ~>  λz.let y = z in t (and similar for other binding forms, but not
    -- let - that would create an infinite loop)
    -- This only fires when trying to do a substitution x|->y, and we need to go
    -- under a binder and avoid variable capture.
    -- We only record what names to avoid, and do the renaming in runRedex
    RenameBindingsLam ExprMeta Name Expr (S.Set Name)
  | RenameBindingsLAM ExprMeta Name Expr (S.Set Name)
  | RenameBindingsCase ExprMeta Expr [CaseBranch] (S.Set Name)
  | ApplyPrimFun PrimFun [Expr]

-- there are only trivial redexes in types.
-- Note that the let must appear in the surrounding Expr (not in a type itself)
data RedexType
  = InlineLetInType Name Type
  | -- ∀a:k.t  ~>  ∀b:k.t[b/a]  for fresh b, avoiding the given set
    RenameForall TypeMeta Name Kind Type (S.Set Name)

-- Currently just a step limit
type TerminationBound = Natural

-- A naive implementation of normal-order reduction
evalFull :: (MonadFresh NameCounter m, MonadFresh ID m) => M.Map Name TypeDef -> M.Map ID Def -> TerminationBound -> Dir -> Expr -> m (Either EvalFullError Expr)
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
  (MonadFresh NameCounter m, MonadFresh ID m) =>
  M.Map Name TypeDef ->
  M.Map ID Def ->
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
step :: (MonadFresh NameCounter m, MonadFresh ID m) => M.Map Name TypeDef -> M.Map ID Def -> Dir -> Expr -> Maybe (m Expr)
step tydefs g d e = case findRedex tydefs g d e of
  Nothing -> Nothing
  Just mr ->
    Just $
      mr >>= \case
        RExpr ez r -> unfocusExpr . flip replace ez <$> runRedex r
        RType et r -> unfocusExpr . unfocusType . flip replace et <$> runRedexTy r

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

data Local
  = LLet Expr
  | LLetrec Expr Type
  | LLetType Type
  deriving (Generic)

_freeVarsLocal :: Fold Local Name
_freeVarsLocal =
  #_LLet % _freeVars % _2
    `summing` #_LLetrec % (_1 % _freeVars % _2 `summing` _2 % getting _freeVarsTy % _2)
    `summing` #_LLetType % getting _freeVarsTy % _2

data Dir = Syn | Chk
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Dir

-- What is the direction from the context?
-- i.e. are we in the head of an elimination (or inside a hole)?
focusDir :: Dir -> ExprZ -> Dir
focusDir dirIfTop ez = case up ez of
  Nothing -> dirIfTop
  Just z -> case target z of
    App _ f _ | f == target ez -> Syn
    APP _ f _ | f == target ez -> Syn
    Case _ scrut _ | scrut == target ez -> Syn
    Hole _ _ -> Syn
    _ -> Chk

concreteTy :: Data b => Type' b -> Bool
concreteTy ty = hasn't (getting _freeVarsTy) ty && noHoles ty

viewLet :: ExprZ -> Maybe (Name, Local, ExprZ)
viewLet ez = case target ez of
  -- the movements return Maybe but will obviously not fail in this scenario
  Let _ x e _t -> (x,LLet e,) <$> (right =<< down ez)
  Letrec _ x e ty _t -> (x,LLetrec e ty,) <$> (right =<< down ez)
  LetType _ a ty _t -> (a,LLetType ty,) <$> down ez
  _ -> Nothing

viewCaseRedex :: (MonadFresh ID m, MonadFresh NameCounter m) => M.Map Name TypeDef -> Expr -> Maybe (m Redex)
viewCaseRedex tydefs = \case
  -- The patterns in the case branch have a Maybe TypeCache attached, but we
  -- should not assume that this has been filled in correctly, so we record
  -- the type of the scrutinee, and reconstruct the types of the pattern
  -- variables. This is especially important, as we do not (yet?) take care of
  -- metadata correctly in this evaluator (for instance, substituting when we
  -- do a BETA reduction)!
  Case _ (Ann _ expr ty) brs
    | Just (c, _, as, xs, e) <- extract expr brs
    , Just argTys <- instantiateCon ty c ->
        formCaseRedex (Left ty) c argTys as xs e
  -- In the constructors-are-synthesisable case, we don't have the benefit of
  -- an explicit annotation, and have to work out the type based off the name
  -- of the constructor.
  Case _ expr brs
    | Just (c, tyargs, args, patterns, br) <- extract expr brs
    , Just (_, tydef) <- lookupConstructor tydefs c
    , ty <- foldl (\t a -> TApp () t $ set _typeMeta () a) (TCon () (typeDefName tydef)) (take (length $ typeDefParameters tydef) tyargs)
    , Just argTys <- instantiateCon ty c ->
        formCaseRedex (Right ty) c argTys args patterns br
  _ -> Nothing
  where
    extract expr brs =
      -- NB: constructors never have mixed App and APPs: they are always of the
      -- form C @a @b ... x y ...
      let (h, as) = unfoldApp expr
          (h', params) = unfoldAPP h
       in case h' of
            Con _ c -> do
              CaseBranch _ xs e <- find (\(CaseBranch n _ _) -> n == c) brs
              pure (c, params, as, xs, e)
            _ -> Nothing
    instantiateCon ty c
      | Right (_, instVCs) <- instantiateValCons' tydefs $ set _typeMeta () ty
      , Just (_, argTys) <- find ((== c) . fst) instVCs =
          Just argTys
      | otherwise = Nothing
    formCaseRedex ty c argTys args patterns br = pure $ do
      argTys' <- sequence argTys
      -- TODO: we are putting trivial metadata in here...
      -- See https://github.com/hackworthltd/primer/issues/6
      argTys'' <- traverse generateTypeIDs argTys'
      pure $ CaseRedex c (zip args argTys'') ty (map bindName patterns) br

-- This spots all redexs other than InlineLet
viewRedex :: (MonadFresh ID m, MonadFresh NameCounter m) => M.Map Name TypeDef -> M.Map ID Def -> Dir -> Expr -> Maybe (m Redex)
viewRedex tydefs globals dir = \case
  GlobalVar _ x | Just y <- x `M.lookup` globals -> pure $ pure $ InlineGlobal x y
  App _ (Ann _ (Lam _ x t) (TFun _ src tgt)) s -> pure $ pure $ Beta x t src tgt s
  e@App{}
    | (Var _ fName, args) <- unfoldApp e
      , Just f@PrimFun{primFunType} <- M.lookup fName globalPrims
      , TFun _ lhs rhs <- fst $ create primFunType
      , length args == length (fst $ unfoldFun lhs rhs)
      , all isNormalForm args ->
      pure $ pure $ ApplyPrimFun f args
    where
      isNormalForm = \case
        PrimCon _ _ -> True
        Con _ _ -> True
        App _ f x -> isNormalForm f && isNormalForm x
        _ -> False
  APP _ (Ann _ (LAM _ a t) (TForall _ b _ ty1)) ty2 -> pure $ pure $ BETA a t b ty1 ty2
  e | Just r <- viewCaseRedex tydefs e -> Just r
  Ann _ t ty | Chk <- dir, concreteTy ty -> pure $ pure $ Upsilon t ty
  _ -> Nothing

-- We find the normal-order redex.
-- Annoyingly this is not quite leftmost-outermost wrt our Expr type, as we
-- are using 'let's to encode something similar to explicit substitution, and
-- reduce them by substituting one occurrance at a time, removing the 'let'
-- when there are no more substitutions to be done. Note that the 'let' itself
-- doesn't get "pushed down" in the tree.
-- example:
--   lettype a = Bool -> Bool in (λx.not x : a) True
-- reduces to
--   lettype a = Bool -> Bool in (λx.not x : Bool -> Bool) True
-- and then to
--   (λx.not x : Bool -> Bool) True
-- This can be seen as "leftmost-outermost" if you consider the location of the
-- "expand a" redex to be the 'lettype' rather than the variable occurrance.
--
-- This is unfortunately annoying to implement, because our Zipper doesn't mesh
-- well here (movements are Maybe, I know what should happen, but cannot
-- express the moves nicely...)
findRedex ::
  forall m.
  (MonadFresh ID m, MonadFresh NameCounter m) =>
  M.Map Name TypeDef ->
  M.Map ID Def ->
  Dir ->
  Expr ->
  Maybe (m RedexWithContext)
findRedex tydefs globals dir = go . focus
  where
    -- it may be nice to use First and write <> rather than <<||>>, but down returns a maybe, so the monadic binds work better if go returns Maybe rather than First...
    Nothing <<||>> y = y
    x@(Just _) <<||>> _ = x
    eachChild z f = case down z of
      Nothing -> Nothing
      Just z' ->
        let children = z' : unfoldr (fmap (\x -> (x, x)) . right) z'
         in foldr ((<<||>>) . f) Nothing children
    eachChildWithBinding z f = case down z of
      Nothing -> Nothing
      Just z' ->
        let children = z' : unfoldr (fmap (\x -> (x, x)) . right) z'
         in foldr (\c acc -> f (getBoundHere (target z) (Just $ target c)) c <<||>> acc) Nothing children
    go ez
      | Just (n, l, bz) <- viewLet ez = pure <$> goLet n l bz
      | Just mr <- viewRedex tydefs globals (focusDir dir ez) (target ez) = Just $ RExpr ez <$> mr
      | otherwise = eachChild ez go
    -- This should always return Just
    -- It finds either this let is redundant, or somewhere to substitute it
    -- or something inside that we need to rename to unblock substitution
    goLet :: Name -> Local -> ExprZ -> Maybe RedexWithContext
    goLet n l ez =
      goSubst n l ez
        <<||>> (up ez <&> \letz -> RExpr letz $ ElideLet n l (target ez))
    goSubst :: Name -> Local -> ExprZ -> Maybe RedexWithContext
    goSubst n l ez = case target ez of
      -- We've found one
      Var _ x | x == n -> case l of
        LLet le -> pure $ RExpr ez $ InlineLet n le
        LLetrec le lt -> pure $ RExpr ez $ InlineLetrec n le lt
        -- This case should have caught by the TC: a term var is bound by a lettype
        LLetType _ -> Nothing
      -- We have found something like
      --   let x=y in let y=z in t
      -- to substitute the 'x' inside 't' we would need to rename the 'let y'
      -- binding, but that is implemented in terms of let:
      --   let x=y in let w=z in let y=w in t
      -- and this doesn't make progress for let! (c.f. if the 'y' was bound by a
      -- lambda). Instead, we swap to reducing the let y. Similarly for lettype.
      -- LetRec can make progress, but we treat it the same as let, for
      -- consistency. We are careful to not start substituting the inner let in
      --   letrec x = x:T in let x=True in x
      -- as we prefer to elide the outer. This is important to avoid an growing
      -- expansion when evaluating letrec x = x:T in x.
      _
        | Just (m, l', bz') <- viewLet ez, m /= n, anyOf _freeVarsLocal (== m) l -> goLet m l' bz'
        -- Otherwise recurse into subexpressions (including let bindings) and types (if appropriate)
        | LLetType t <- l -> eachChildWithBinding ez rec <<||>> (focusType ez >>= goSubstTy n t)
        | otherwise -> eachChildWithBinding ez rec
      where
        rec bs z
          -- Don't go under binding of 'n': those won't be the 'n's we are looking for
          | n `S.member` bs = Nothing
          -- If we are substituting x->y in e.g. λy.x, we rename the y to avoid capture
          -- This may recompute the FV set of l quite a lot. We could be more efficient here!
          | fvs <- setOf _freeVarsLocal l
          , not $ S.null $ fvs `S.intersection` bs =
              up z <&> \z' -> case target z' of
                Lam m x e -> RExpr z' $ RenameBindingsLam m x e fvs
                LAM m x e -> RExpr z' $ RenameBindingsLAM m x e fvs
                Case m s brs -> RExpr z' $ RenameBindingsCase m s brs fvs
                -- We should replace this with a proper exception. See:
                -- https://github.com/hackworthltd/primer/issues/148
                e -> error $ "Internal Error: something other than Lam/LAM/Case was a binding: " ++ show e
          | otherwise = goSubst n l z
    goSubstTy :: Name -> Type -> TypeZ -> Maybe RedexWithContext
    goSubstTy n t tz = case target tz of
      -- found one
      TVar _ x | x == n -> pure $ RType tz $ InlineLetInType n t
      -- The only binding form is a forall
      -- Don't go under bindings of 'n'
      (TForall i m k s)
        | n == m -> Nothing
        -- If we are substituting x->y in forall y.s, we rename the y to avoid capture
        -- As we don't have 'let's in types, this is a big step
        | fvs <- freeVarsTy t
        , m `S.member` fvs ->
            pure $ RType tz $ RenameForall i m k s fvs
      _ -> eachChild tz (goSubstTy n t)

-- TODO: deal with metadata. https://github.com/hackworthltd/primer/issues/6
runRedex :: (MonadFresh ID m, MonadFresh NameCounter m) => Redex -> m Expr
runRedex = \case
  InlineGlobal _ def -> ann (regenerateExprIDs $ defExpr def) (regenerateTypeIDs $ defType def)
  InlineLet _ e -> regenerateExprIDs e
  InlineLetrec x e t -> letrec x (regenerateExprIDs e) (regenerateTypeIDs t) $ ann (regenerateExprIDs e) (regenerateTypeIDs t)
  -- let(rec/type) x = e in t  ~>  t  if e does not appear in t
  ElideLet _ _ t -> pure t
  -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
  Beta x t tyS tyT s -> let_ x (pure s `ann` pure tyS) (pure t) `ann` pure tyT
  -- (Λa.t : ∀b.T) S  ~>  lettype b = S in (lettype a = S in t) : T
  BETA a t b tyT tyS
    | a == b -> letType a (pure tyS) $ pure t `ann` pure tyT
    | otherwise -> letType b (pure tyS) $ letType a (pure tyS) (pure t) `ann` pure tyT
  -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
  -- (and also the non-annotated-constructor case)
  CaseRedex _ as _ xs e -> foldrM (\(x, (a, tyA)) t -> let_ x (pure a `ann` pure tyA) (pure t)) e (zip xs as)
  -- [ t : T ]  ~>  t  writing [_] for the embedding of syn into chk
  Upsilon e _ -> pure e
  -- λy.t  ~>  λz.let y = z in t (and similar for other binding forms, except let)
  RenameBindingsLam m x e avoid -> do
    y <- freshName (avoid <> freeVars e)
    Lam m y <$> let_ x (var y) (pure e)
  RenameBindingsLAM m x e avoid -> do
    y <- freshName (avoid <> freeVars e)
    LAM m y <$> let_ x (var y) (pure e)
  RenameBindingsCase m s brs avoid
    | (brs0, CaseBranch ctor binds rhs : brs1) <- break (\(CaseBranch _ bs _) -> any ((`S.member` avoid) . bindName) bs) brs ->
        let bns = map bindName binds
            avoid' = avoid <> freeVars rhs <> S.fromList bns
         in do
              rn <- traverse (\b -> if b `S.member` avoid then Right . (b,) <$> freshName avoid' else pure $ Left b) bns
              let f b@(Bind i _) = \case Left _ -> b; Right (_, w) -> Bind i w
              let binds' = zipWith f binds rn
              rhs' <- foldrM (\(v, w) -> let_ v (var w) . pure) rhs $ rights rn
              pure $ Case m s $ brs0 ++ CaseBranch ctor binds' rhs' : brs1
    -- We should replace this with a proper exception. See:
    -- https://github.com/hackworthltd/primer/issues/148
    | otherwise -> error "Internal Error: RenameBindingsCase found no applicable branches"
  ApplyPrimFun PrimFun{primFunDef} args ->
    case primFunDef args of
      Left err -> error $ show err
      Right x -> x

runRedexTy :: (MonadFresh ID m, MonadFresh NameCounter m) => RedexType -> m Type
runRedexTy (InlineLetInType _ t) = regenerateTypeIDs t
runRedexTy (RenameForall m a k s avoid) = do
  b <- freshName (avoid <> freeVarsTy s)
  TForall m b k <$> renameTy a b s

-- This is very similar to Subst.substTy
-- However, we work over Type rather than Type' (), as we don't need to worry
-- about how/if to duplicate metadata etc
renameTy :: MonadFresh NameCounter m => Name -> Name -> Type -> m Type
renameTy a b = go
  where
    go = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go t
      t@TCon{} -> pure t
      TFun m s t -> TFun m <$> go s <*> go t
      t@(TVar m c)
        | c == a -> pure $ TVar m b
        | otherwise -> pure t
      TApp m s t -> TApp m <$> go s <*> go t
      t@(TForall m c k s)
        | c == a -> pure t
        | c == b ->
            freshName (S.singleton a <> S.singleton b <> freeVarsTy s) >>= \c' ->
              renameTy c c' s >>= fmap (TForall m c' k) . go
        | otherwise -> TForall m c k <$> go s
