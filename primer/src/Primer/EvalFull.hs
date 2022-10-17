{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Primer.EvalFull (
  Dir (..),
  EvalFullError (..),
  TerminationBound,
  evalFull,
  evalFullStepCount,
  EvalFullLog (..),
) where

-- TODO: share code with Primer.Eval
-- I hope to reuse this code in Eval - the current implementation does some weird things with annotations and metadata
-- but that will come later

-- TODO: ensure do sane things to metadata
-- (Perhaps we should just run a TC pass after each step?)
-- See https://github.com/hackworthltd/primer/issues/6

import Foreword

import Control.Monad.Extra (untilJustM)
import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (MonadLog, WithSeverity)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (thd3)
import GHC.Err (error)
import Numeric.Natural (Natural)
import Optics (AffineFold, Fold, afolding, elemOf, getting, summing, to, (%), _2, _3)
import Primer.Core (
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (
    APP,
    Ann,
    App,
    Case,
    Con,
    Hole,
    LAM,
    Lam,
    Let,
    LetType,
    Letrec,
    Var
  ),
  ExprMeta,
  GVarName,
  ID,
  Kind,
  LVarName,
  LocalName (unLocalName),
  LocalNameKind (ATmVar, ATyVar),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (
    TForall,
    TFun,
    TLet,
    TVar
  ),
  TypeMeta,
  ValConName,
  bindName,
 )
import Primer.Core.DSL (ann, letType, let_, letrec, lvar, tlet, tvar)
import Primer.Core.Transform (renameTyVar, unfoldAPP, unfoldApp)
import Primer.Core.Utils (
  concreteTy,
  forgetTypeMetadata,
  freeVars,
  freeVarsTy,
  freshLocalName,
  freshLocalName',
  generateTypeIDs,
  regenerateExprIDs,
  regenerateTypeIDs,
  _freeVars,
  _freeVarsTy,
 )
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
  defPrim,
 )
import Primer.Eval.Prim (tryPrimFun)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Log (ConvertLogMessage (convert), logWarning)
import Primer.Name (Name, NameCounter)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefParameters),
  TypeDefMap,
 )
import Primer.Typecheck.Utils (instantiateValCons', lookupConstructor, mkTAppCon)
import Primer.Zipper (
  ExprZ,
  TypeZ,
  bindersBelowTy,
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

newtype EvalFullLog = InvariantFailure Text
  deriving newtype (Show, Eq)

instance ConvertLogMessage EvalFullLog EvalFullLog where
  convert = identity

newtype EvalFullError
  = TimedOut Expr
  -- We assume the input is type-correct, and don't even detect the most egregious flouting of that assumption
  deriving (Eq, Show)

data Redex
  = -- f  ~>  e : T  where we have  f : T ; f = e  in (global) scope
    InlineGlobal GVarName ASTDef
  | -- x  ~>  e   where we are inside the scope of a  let x = e in ...
    InlineLet LVarName Expr
  | -- x  ~>  letrec x:T=t in t:T   where we are inside the scope of a  letrec x : T = t in ...
    InlineLetrec LVarName Expr Type
  | -- let(rec/type) x = e in t  ~>  t  if x does not appear in t
    ElideLet SomeLocal Expr
  | -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
    Beta LVarName Expr Type Type Expr
  | -- (Λa.t : ∀b.T) S  ~>  (lettype a = S in t) : (lettype b = S in T)
    BETA TyVarName Expr TyVarName Type Type
  | -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- also the non-annotated case, as we consider constructors to be synthesisable
    -- case C as of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- (This is the natural rule if we consider non-annotated constructors to
    -- be shorthand for a annotated-lambda wrapping, and combine a few
    -- reduction steps. E.g.
    --     cons ==  (Λa λx λxs. Cons @a x xs) : ∀a. a -> List a -> List a
    -- )
    CaseRedex ValConName (forall m. MonadFresh NameCounter m => [(Expr, m (Type' ()))]) (Either Type (Type' ())) [LVarName] Expr
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
    RenameBindingsLam ExprMeta LVarName Expr (S.Set Name)
  | RenameBindingsLAM ExprMeta TyVarName Expr (S.Set Name)
  | RenameBindingsCase ExprMeta Expr [CaseBranch] (S.Set Name)
  | -- let x = f x in g x x  ~>  let y = f x in let x = y in g x x
    -- Note that we cannot substitute the let in the initial term, since
    -- we only substitute one occurence at a time, and the 'let' would capture the 'x'
    -- in the expansion if we did a substitution.
    RenameSelfLet LVarName Expr Expr
  | -- As RenameSelfLet, but for LetType. (Note that it is unnecessary for letrec.)
    RenameSelfLetType TyVarName Type Expr
  | ApplyPrimFun (forall m. MonadFresh ID m => m Expr)

data RedexType
  = InlineLetInType TyVarName Type
  | -- let a = s in t  ~>  t  if a does not appear in t
    ElideLetInType (Local 'ATyVar) Type
  | -- let a = s a in t a a  ~>  let b = s a in let a = b in t a a
    -- Note that we cannot substitute the let in the initial term, since
    -- we only substitute one occurence at a time, and the 'let' would capture the 'a'
    -- in the expansion if we did a substitution.
    RenameSelfLetInType TyVarName Type Type
  | -- ∀a:k.t  ~>  ∀b:k.t[b/a]  for fresh b, avoiding the given set
    RenameForall TypeMeta TyVarName Kind Type (S.Set TyVarName)

-- Currently just a step limit
type TerminationBound = Natural

-- A naive implementation of normal-order reduction
evalFull :: MonadEvalFull l m => TypeDefMap -> DefMap -> TerminationBound -> Dir -> Expr -> m (Either EvalFullError Expr)
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
  MonadEvalFull l m =>
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
step :: MonadEvalFull l m => TypeDefMap -> DefMap -> Dir -> Expr -> Maybe (m Expr)
step tydefs g d e = case findRedex tydefs g d e of
  Nothing -> Nothing
  Just (RExpr ez r) -> Just $ unfocusExpr . flip replace ez <$> runRedex r
  Just (RType et r) -> Just $ unfocusExpr . unfocusType . flip replace et <$> runRedexTy r

-- We don't really want a zipper here, but a one-hole context, but it is easier
-- to just reuse the zipper machinery and ignore the target of the zipper.
-- The ExprZ/TypeZ is a zipper focussed on a subterm which is a Redex as described by the second argument.
data RedexWithContext
  = RExpr ExprZ Redex
  | RType TypeZ RedexType

data Local k where
  LLet :: LVarName -> Expr -> Local 'ATmVar
  LLetrec :: LVarName -> Expr -> Type -> Local 'ATmVar
  LLetType :: TyVarName -> Type -> Local 'ATyVar

localName :: Local k -> Name
localName = \case
  LLet n _ -> unLocalName n
  LLetrec n _ _ -> unLocalName n
  LLetType n _ -> unLocalName n

_LLet :: AffineFold (Local k) (LVarName, Expr)
_LLet = afolding $ \case LLet n e -> pure (n, e); _ -> Nothing

_LLetrec :: AffineFold (Local k) (LVarName, Expr, Type)
_LLetrec = afolding $ \case LLetrec n e t -> pure (n, e, t); _ -> Nothing

_LLetType :: AffineFold (Local k) (TyVarName, Type)
_LLetType = afolding $ \case LLetType n t -> pure (n, t); _ -> Nothing

data SomeLocal where
  LSome :: Local k -> SomeLocal

_freeVars' :: Fold (Expr' a b) Name
_freeVars' = _freeVars % to (either (unLocalName . snd) (unLocalName . snd))

_freeVarsTy' :: Fold (Type' b) Name
_freeVarsTy' = getting _freeVarsTy % _2 % to unLocalName

_freeVarsLocal :: Fold (Local k) Name
_freeVarsLocal =
  _LLet % _2 % _freeVars'
    `summing` _LLetrec % (_2 % _freeVars' `summing` _3 % _freeVarsTy')
    `summing` _LLetType % _2 % _freeVarsTy'

data Dir = Syn | Chk
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Dir

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

viewLet :: ExprZ -> Maybe (SomeLocal, ExprZ)
viewLet ez = case target ez of
  -- the movements return Maybe but will obviously not fail in this scenario
  Let _ x e _t -> (LSome $ LLet x e,) <$> (right =<< down ez)
  Letrec _ x e ty _t -> (LSome $ LLetrec x e ty,) <$> (right =<< down ez)
  LetType _ a ty _t -> (LSome $ LLetType a ty,) <$> down ez
  _ -> Nothing

viewCaseRedex :: TypeDefMap -> Expr -> Maybe Redex
viewCaseRedex tydefs = \case
  -- The patterns in the case branch have a Maybe TypeCache attached, but we
  -- should not assume that this has been filled in correctly, so we record
  -- the type of the scrutinee, and reconstruct the types of the pattern
  -- variables. This is especially important, as we do not (yet?) take care of
  -- metadata correctly in this evaluator (for instance, substituting when we
  -- do a BETA reduction)!
  Case m expr'@(Ann _ expr ty) brs
    | Just (c, _, as, xs, e) <- extract expr brs
    , Just argTys <- instantiateCon ty c ->
        renameBindings m expr' brs [ty] as xs
          <|> formCaseRedex (Left ty) c argTys as xs e
  -- In the constructors-are-synthesisable case, we don't have the benefit of
  -- an explicit annotation, and have to work out the type based off the name
  -- of the constructor.
  Case m expr brs
    | Just (c, tyargs, args, patterns, br) <- extract expr brs
    , Just (_, tc, tydef) <- lookupConstructor tydefs c
    , ty <- mkTAppCon tc (forgetTypeMetadata <$> take (length $ astTypeDefParameters tydef) tyargs)
    , Just argTys <- instantiateCon ty c ->
        renameBindings m expr brs tyargs args patterns
          <|> formCaseRedex (Right ty) c argTys args patterns br
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
    instantiateCon :: Type' a -> ValConName -> Maybe (forall m. MonadFresh NameCounter m => [m (Type' ())])
    instantiateCon ty c
      | Right (_, _, instVCs) <- instantiateValCons' tydefs $ forgetTypeMetadata ty
      , Just (_, argTys) <- find ((== c) . fst) instVCs =
          Just argTys
      | otherwise = Nothing
    {- Note [Case reduction and variable capture]
       There is a subtlety here around variable capture.
       Consider
         case C s t : T A B of C a b -> e
       We would like to reduce this to
         let a = s : S; let b = t : T in e
       where we have annotated `s` and `t` with their types, which will be
       built from `A` and `B` according to the definition of the type `T`
       (for reasons of bidirectionality).
       Note that the binding of `a` may capture a reference in `t`
       or (assuming type and term variables can shadow) in `T`.
       We must catch this case and rename the case binders as a first step.
       Note that the free vars in `t : T` are a subset of the free vars in the
       arguments of the scrutinee (s, t) plus the arguments to its type
       annotation (A, B). (In the non-annotated case, we instead look at the
       type arguments of the scrutinee).
       We shall be conservative and rename all binders in every branch apart
       from these free vars.
       (We could get away with only renaming within the matching branch, only
       avoiding those FVs that actually occur, and in a "telescope" fashion:
       the first binder needs to avoid the FVs of all except the first
       argument, the second needs to avoid all but the first two args, ...,
       the last doesn't need any renaming.)
    -}
    renameBindings m expr brs tyargs args patterns =
      let avoid = foldMap (S.map unLocalName . freeVarsTy) tyargs <> foldMap freeVars args
          binders = S.fromList $ map (unLocalName . bindName) patterns
       in if S.disjoint avoid binders
            then Nothing
            else Just $ RenameBindingsCase m expr brs avoid
    formCaseRedex ::
      Either Type (Type' ()) ->
      ValConName ->
      (forall m. MonadFresh NameCounter m => [m (Type' ())]) ->
      [Expr] ->
      [Bind' a] ->
      Expr ->
      Maybe Redex
    formCaseRedex ty c argTys args patterns br =
      Just $
        CaseRedex c (zip args argTys) ty (map bindName patterns) br

-- This spots all redexs other than InlineLet
viewRedex :: TypeDefMap -> DefMap -> Dir -> Expr -> Maybe Redex
viewRedex tydefs globals dir = \case
  Var _ (GlobalVarRef x) | Just (DefAST y) <- x `M.lookup` globals -> pure $ InlineGlobal x y
  App _ (Ann _ (Lam _ x t) (TFun _ src tgt)) s -> pure $ Beta x t src tgt s
  e@App{} -> ApplyPrimFun . thd3 <$> tryPrimFun (M.mapMaybe defPrim globals) e
  -- (Λa.t : ∀b.T) S  ~> (letType a = S in t) : (letType b = S in T)
  APP _ (Ann _ (LAM _ a t) (TForall _ b _ ty1)) ty2 -> pure $ BETA a t b ty1 ty2
  e | Just r <- viewCaseRedex tydefs e -> Just r
  Ann _ t ty | Chk <- dir, concreteTy ty -> pure $ Upsilon t ty
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
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  Maybe RedexWithContext
findRedex tydefs globals dir = go . focus
  where
    eachChild z f = case down z of
      Nothing -> Nothing
      Just z' ->
        let children = z' : unfoldr (fmap (\x -> (x, x)) . right) z'
         in foldr ((<|>) . f) Nothing children
    eachChildWithBinding z f = case down z of
      Nothing -> Nothing
      Just z' ->
        let children = z' : unfoldr (fmap (\x -> (x, x)) . right) z'
         in foldr (\c acc -> f (getBoundHere (target z) (Just $ target c)) c <|> acc) Nothing children
    go ez
      | Just (LSome l, bz) <- viewLet ez = goLet l ez bz
      | Just mr <- viewRedex tydefs globals (focusDir dir ez) (target ez) = Just $ RExpr ez mr
      | otherwise =
          -- We reduce any types first, as computation in types is simple (just inlining)
          (focusType ez >>= goType) <|> eachChild ez go
    goType tz
      | TLet _ a t _body <- target tz = down tz >>= right >>= goTLet (LLetType a t) tz
      | otherwise = eachChild tz goType
    -- This should always return Just
    -- It finds either this let is redundant, or somewhere to substitute it
    -- or something inside that we need to rename to unblock substitution
    goLet :: Local k -> ExprZ -> ExprZ -> Maybe RedexWithContext
    goLet l letz bodyz =
      case (l, elemOf _freeVarsLocal (localName l) l) of
        -- We have something like λx.let x = f x in g x (NB: non-recursive let)
        -- We cannot substitute this let as we would get λx. let x = f x in g (f x)
        -- where a variable has been captured
        (LLet x e, True) -> pure $ RExpr letz $ RenameSelfLet x e (target bodyz)
        (LLetType a ty, True) -> pure $ RExpr letz $ RenameSelfLetType a ty (target bodyz)
        _ ->
          goSubst l bodyz
            <|> Just (RExpr letz $ ElideLet (LSome l) (target bodyz))
    -- As goLet, but for TLet
    goTLet :: Local 'ATyVar -> TypeZ -> TypeZ -> Maybe RedexWithContext
    goTLet l@(LLetType a s) tletz bodyz =
      if elemOf (getting _freeVarsTy % _2) a s
        then -- We have something like Λa. _ : tlet a = s a in t a
        -- We cannot substitute this let as we would get Λa. _ : tlet a = s a in t (s a)
        -- where a variable has been captured
          pure $ RType tletz $ RenameSelfLetInType a s (target bodyz)
        else
          goSubstTy a s bodyz
            <|> Just (RType tletz $ ElideLetInType l (target bodyz))
    goSubst :: Local k -> ExprZ -> Maybe RedexWithContext
    goSubst l ez = case target ez of
      -- We've found one
      Var _ (LocalVarRef x) | unLocalName x == localName l -> case l of
        LLet n le -> pure $ RExpr ez $ InlineLet n le
        LLetrec n le lt -> pure $ RExpr ez $ InlineLetrec n le lt
        -- This case should have caught by the TC: a term var is bound by a lettype
        LLetType _ _ -> Nothing
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
        | Just (LSome l', bz') <- viewLet ez
        , localName l' /= localName l
        , elemOf _freeVarsLocal (localName l') l ->
            goLet l' ez bz'
        -- Otherwise recurse into subexpressions (including let bindings) and types (if appropriate)
        | LLetType n t <- l -> eachChildWithBinding ez rec <|> (focusType ez >>= goSubstTy n t)
        | otherwise -> eachChildWithBinding ez rec
      where
        rec bs z
          -- Don't go under binding of 'n': those won't be the 'n's we are looking for
          | localName l `S.member` bs = Nothing
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
          | otherwise = goSubst l z
    goSubstTy :: TyVarName -> Type -> TypeZ -> Maybe RedexWithContext
    goSubstTy n t tz = case target tz of
      -- found one
      TVar _ x | x == n -> pure $ RType tz $ InlineLetInType n t
      -- Swap to an inner let, as long as it would make progress,
      -- but prefer eliding an outer binder if possible
      TLet _ m s _body
        | m /= n
        , elemOf (getting _freeVarsTy % _2) m t ->
            down tz >>= right >>= goTLet (LLetType m s) tz
      -- The only other binding form is a forall
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
runRedex :: MonadEvalFull l m => Redex -> m Expr
runRedex = \case
  InlineGlobal _ def -> ann (regenerateExprIDs $ astDefExpr def) (regenerateTypeIDs $ astDefType def)
  InlineLet _ e -> regenerateExprIDs e
  InlineLetrec x e t -> letrec x (regenerateExprIDs e) (regenerateTypeIDs t) $ ann (regenerateExprIDs e) (regenerateTypeIDs t)
  -- let(rec/type) x = e in t  ~>  t  if e does not appear in t
  ElideLet _ t -> pure t
  -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
  Beta x t tyS tyT s -> let_ x (pure s `ann` pure tyS) (pure t) `ann` pure tyT
  -- (Λa.t : ∀b.T) S  ~>  (lettype a = S in t) : (lettype b = S in T)
  BETA a t b tyT tyS -> letType a (pure tyS) (pure t) `ann` tlet b (regenerateTypeIDs tyS) (pure tyT)
  -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
  -- (and also the non-annotated-constructor case)
  -- Note that when forming the CaseRedex we checked that the variables @xs@ were fresh for @as@ and @As@,
  -- so this will not capture any variables.
  CaseRedex _ as _ xs e -> do
    -- TODO: we are putting trivial metadata in here...
    -- See https://github.com/hackworthltd/primer/issues/6
    foldrM (\(x, (a, tyA)) t -> let_ x (pure a `ann` (generateTypeIDs =<< tyA)) (pure t)) e (zip xs as)
  -- [ t : T ]  ~>  t  writing [_] for the embedding of syn into chk
  Upsilon e _ -> pure e
  -- λy.t  ~>  λz.let y = z in t (and similar for other binding forms, except let)
  RenameBindingsLam m x e avoid -> do
    y <- freshLocalName' (avoid <> freeVars e)
    Lam m y <$> let_ x (lvar y) (pure e)
  RenameBindingsLAM m x e avoid -> do
    y <- freshLocalName' (avoid <> freeVars e)
    LAM m y <$> letType x (tvar y) (pure e)
  RenameBindingsCase m s brs avoid
    | (brs0, CaseBranch ctor binds rhs : brs1) <- break (\(CaseBranch _ bs _) -> any ((`S.member` avoid) . unLocalName . bindName) bs) brs ->
        let bns = map bindName binds
            avoid' = avoid <> freeVars rhs <> S.fromList (map unLocalName bns)
         in do
              rn <- traverse (\b -> if unLocalName b `S.member` avoid then Right . (b,) <$> freshLocalName' avoid' else pure $ Left b) bns
              let f b@(Bind i _) = \case Left _ -> b; Right (_, w) -> Bind i w
              let binds' = zipWith f binds rn
              rhs' <- foldrM (\(v, w) -> let_ v (lvar w) . pure) rhs $ rights rn
              pure $ Case m s $ brs0 ++ CaseBranch ctor binds' rhs' : brs1
    -- We should replace this with a proper exception. See:
    -- https://github.com/hackworthltd/primer/issues/148
    | otherwise -> error "Internal Error: RenameBindingsCase found no applicable branches"
  -- let x = f x in g x x  ~>  let y = f x in let x = y in g x x
  RenameSelfLet x e body -> do
    y <- freshLocalName' (freeVars e <> freeVars body)
    let_ y (pure e) $ let_ x (lvar y) $ pure body
  -- As RenameSelfLet, but for LetType
  RenameSelfLetType a ty body -> do
    b <- freshLocalName' (S.map unLocalName (freeVarsTy ty) <> freeVars body)
    letType b (pure ty) $ letType a (tvar b) $ pure body
  ApplyPrimFun e -> e

runRedexTy :: MonadEvalFull l m => RedexType -> m Type
runRedexTy (InlineLetInType _ t) = regenerateTypeIDs t
-- let a = s in t  ~>  t  if a does not appear in t
runRedexTy (ElideLetInType _ t) = pure t
-- let a = s a in t a a  ~>  let b = s a in let a = b in t a a
runRedexTy (RenameSelfLetInType a s t) = do
  b <- freshLocalName (freeVarsTy s <> freeVarsTy t)
  tlet b (pure s) $ tlet a (tvar b) $ pure t
runRedexTy (RenameForall m a k s avoid) = do
  -- It should never be necessary to try more than once, since
  -- we pick a new name disjoint from any that appear in @s@
  -- thus renaming will never capture (so @renameTyVar@ will always succeed).
  -- However, the type system does not know about this.
  -- We explicitly try once, and log if that fails before trying again.
  -- We do not log on retries
  let rename = do
        b <- freshLocalName (avoid <> freeVarsTy s <> bindersBelowTy (focus s))
        pure (b, TForall m b k <$> renameTyVar a b s)
  rename >>= \case
    (_, Just t') -> pure t'
    (b, Nothing) -> do
      logWarning $
        InvariantFailure $
          "runRedexTy.RenameForall: initial name choice was not fresh enough: chose "
            <> show b
            <> " for "
            <> show @_ @Text (m, a, k, s, avoid)
      untilJustM $ snd <$> rename

type MonadEvalFull l m =
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadLog (WithSeverity l) m
  , ConvertLogMessage EvalFullLog l
  )
