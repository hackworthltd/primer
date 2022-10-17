{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}

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

import Foreword hiding (hoistAccum)
import Foreword qualified

import Control.Monad.Extra (untilJustM)
import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Morph (generalize)
import Control.Monad.Trans.Accum (Accum, AccumT, add, evalAccumT, look, readerToAccumT)
import Data.List (zip3)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (snd3, thd3)
import GHC.Err (error)
import Numeric.Natural (Natural)
import Optics (
  AffineFold,
  Fold,
  afolding,
  allOf,
  elemOf,
  folded,
  getting,
  ifiltered,
  isnd,
  notElemOf,
  summing,
  to,
  (%),
  (<%),
  _1,
  _2,
  _Just,
 )
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
  HasID,
  ID,
  Kind,
  LVarName,
  LocalName (unLocalName),
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
  getID,
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
  IsZipper,
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  TypeZ,
  bindersBelowTy,
  down,
  focus,
  focusType,
  getBoundHere,
  getBoundHere',
  getBoundHereDn,
  getBoundHereTy,
  letBindingName,
  replace,
  right,
  target,
  unfocusExpr,
  unfocusType,
  up,
 )
import Primer.Zipper.Type (
  LetTypeBinding,
  LetTypeBinding' (LetTypeBind),
  getBoundHereTy',
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
    ElideLet LetBinding Expr
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
    CaseRedex ValConName [Expr] (forall m. MonadFresh NameCounter m => [m (Type' ())]) (Type' ()) [LVarName] Expr
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
    ElideLetInType LetTypeBinding Type
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

_LetBind :: AffineFold LetBinding (LVarName, Expr)
_LetBind = afolding $ \case LetBind n e -> pure (n, e); _ -> Nothing

_LetrecBind :: AffineFold LetBinding (LVarName, Expr, Type)
_LetrecBind = afolding $ \case LetrecBind n e t -> pure (n, e, t); _ -> Nothing

_LetTyBind :: AffineFold LetBinding LetTypeBinding
_LetTyBind = afolding $ \case LetTyBind b -> pure b; _ -> Nothing

_LetTypeBind :: AffineFold LetTypeBinding (TyVarName, Type)
_LetTypeBind = afolding $ \case LetTypeBind n t -> pure (n, t)

_freeVars' :: Fold (Expr' a b) Name
_freeVars' = _freeVars % to (either (unLocalName . snd) (unLocalName . snd))

_freeVarsTy' :: Fold (Type' b) Name
_freeVarsTy' = getting _freeVarsTy % _2 % to unLocalName

-- | Fold over the free variables in the unfolding of this definition.
-- Note that this differs from "the variables that occur free in the
-- RHS of the definition" for recursive bindings
-- (e.g. @letrec x = C x y : T@ would have @y@ free but not @x@).
_freeVarsLetBinding :: Fold LetBinding Name
_freeVarsLetBinding =
  (_LetBind % _2 % _freeVars')
    -- Since letrec bound variables expand with a local letrec,
    -- we don't consider the recursively-bound variable free
    -- (since it will not be in the inlining)
    `summing` ( _LetrecBind
                  <% ( to (\(a, b, c) -> (a, (b, c)))
                        % isnd
                        % (_1 % _freeVars' `summing` _2 % _freeVarsTy')
                        & ifiltered ((/=) . unLocalName)
                     )
              )
    `summing` (_LetTyBind % _LetTypeBind % _2 % _freeVarsTy')

data Dir = Syn | Chk
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Dir

viewLet :: ExprZ -> Maybe (LetBinding, Accum Cxt ExprZ)
viewLet ez = case (target ez, exprChildren ez) of
  (Let _ x e _b, [_, bz]) -> Just (LetBind x e, bz)
  (Letrec _ x e ty _b, [_, bz]) -> Just (LetrecBind x e ty, bz)
  (LetType _ a ty _b, [bz]) -> bz `seq` Just (LetTyBind $ LetTypeBind a ty, bz)
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
          <|> formCaseRedex (forgetTypeMetadata ty) c argTys as xs e
  -- In the constructors-are-synthesisable case, we don't have the benefit of
  -- an explicit annotation, and have to work out the type based off the name
  -- of the constructor.
  Case m expr brs
    | Just (c, tyargs, args, patterns, br) <- extract expr brs
    , Just (_, tc, tydef) <- lookupConstructor tydefs c
    , ty <- mkTAppCon tc (forgetTypeMetadata <$> take (length $ astTypeDefParameters tydef) tyargs)
    , Just argTys <- instantiateCon ty c ->
        renameBindings m expr brs tyargs args patterns
          <|> formCaseRedex ty c argTys args patterns br
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
    instantiateCon :: Type' a -> ValConName -> Maybe (forall m'. MonadFresh NameCounter m' => [m' (Type' ())])
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
      Type' () ->
      ValConName ->
      (forall m'. MonadFresh NameCounter m' => [m' (Type' ())]) ->
      [Expr] ->
      [Bind' a] ->
      Expr ->
      Maybe Redex
    formCaseRedex ty c argTys args patterns br =
      Just $
        CaseRedex c args argTys ty (map bindName patterns) br

-- We record each binder, along with its let-bound RHS (if any)
-- and its original binding location and  context (to be able to detect capture)
-- Invariant: lookup x c == Just (Just l,_,_) ==> letBindingName l == x
newtype Cxt = Cxt (M.Map Name (Maybe LetBinding, ID, Cxt))
  -- We want right-biased mappend, as we will use this with 'Accum'
  -- and want later 'add's to overwrite earlier (more-global) context entries
  deriving (Semigroup, Monoid) via Dual (M.Map Name (Maybe LetBinding, ID, Cxt))

lookup :: Name -> Cxt -> Maybe (Maybe LetBinding, ID, Cxt)
lookup n (Cxt cxt) = M.lookup n cxt

-- We only care about LLetType if we are looking up tyvars,
-- as we assume that the input is well-typed, and the only things
-- that tyvars can refer to are lettype, or foralls
lookupTy :: TyVarName -> Cxt -> Maybe (Maybe LetTypeBinding, ID, Cxt)
lookupTy n c = case lookup (unLocalName n) c of
  Just (Just (LetTyBind b), i, c') -> Just (Just b, i, c')
  _ -> Nothing

-- This notices all redexes
-- Note that if a term is not a redex, but stuck on some sub-term,
-- then it is either
-- - a let (of some flavor)
-- - stuck on its left-most child
-- - stuck on the type annotation on its left-most child
-- - stuck on expression under the type annotation in its left-most child
viewRedex ::
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  Reader Cxt (Maybe Redex)
viewRedex tydefs globals dir = \case
  Var _ (GlobalVarRef x) | Just (DefAST y) <- x `M.lookup` globals -> purer $ InlineGlobal x y
  Var _ (LocalVarRef v) -> do
    getNonCapturedLocal v <&> \x -> do
      case x of
        Just (LetBind _ e) -> pure $ InlineLet v e
        Just (LetrecBind _ e t) -> pure $ InlineLetrec v e t
        _ -> Nothing
  Let _ v e1 e2
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | unLocalName v `S.notMember` freeVars e2 -> purer $ ElideLet (LetBind v e1) e2
    | unLocalName v `S.member` freeVars e1 -> purer $ RenameSelfLet v e1 e2
    | otherwise -> pure Nothing
  LetType _ v t e
    | unLocalName v `S.notMember` freeVars e -> purer $ ElideLet (LetTyBind $ LetTypeBind v t) e
    | v `S.member` freeVarsTy t -> purer $ RenameSelfLetType v t e
    | otherwise -> pure Nothing
  Letrec _ v e1 t e2
    | unLocalName v `S.notMember` freeVars e2 -> purer $ ElideLet (LetrecBind v e1 t) e2
    | otherwise -> pure Nothing
  l@(Lam m v e) -> do
    fvcxt <- fvCxt $ freeVars l
    pure $
      if unLocalName v `S.member` fvcxt
        then pure $ RenameBindingsLam m v e fvcxt
        else Nothing
  l@(LAM m v e) -> do
    fvcxt <- fvCxt $ freeVars l
    pure $
      if unLocalName v `S.member` fvcxt
        then pure $ RenameBindingsLAM m v e fvcxt
        else Nothing
  App _ (Ann _ (Lam _ x t) (TFun _ src tgt)) s -> purer $ Beta x t src tgt s
  e@App{} -> pure $ ApplyPrimFun . thd3 <$> tryPrimFun (M.mapMaybe defPrim globals) e
  -- (Λa.t : ∀b.T) S  ~> (letType a = S in t) : (letType b = S in T)
  APP _ (Ann _ (LAM _ a t) (TForall _ b _ ty1)) ty2 -> purer $ BETA a t b ty1 ty2
  APP{} -> pure Nothing
  e@(Case m s brs) -> do
    fvcxt <- fvCxt $ freeVars e
    -- TODO: we arbitrarily decide that renaming takes priority over reducing the case
    -- This is good for evalfull, but bad for interactive use.
    -- Maybe we want to offer both. See
    -- https://github.com/hackworthltd/primer/issues/734
    if getBoundHereDn e `S.disjoint` fvcxt
      then case viewCaseRedex tydefs e of
        Nothing -> pure Nothing
        Just r -> purer r
      else pure $ pure $ RenameBindingsCase m s brs fvcxt
  Ann _ t ty | Chk <- dir, concreteTy ty -> purer $ Upsilon t ty
  _ -> pure Nothing

viewRedexType :: Type -> Reader Cxt (Maybe RedexType)
viewRedexType = \case
  TVar _ v ->
    getNonCapturedLocal v <&> \case
      Just (LetTyBind (LetTypeBind _ t)) -> pure $ InlineLetInType v t
      _ -> Nothing
  TLet _ v s t
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | notElemOf (getting _freeVarsTy % _2) v t -> purer $ ElideLetInType (LetTypeBind v s) t
    | elemOf (getting _freeVarsTy % _2) v s -> purer $ RenameSelfLetInType v s t
    | otherwise -> pure Nothing
  fa@(TForall m v s t) -> do
    fvcxt <- fvCxtTy $ freeVarsTy fa
    pure $
      if v `S.member` fvcxt
        then -- If anything we may substitute would cause capture, we should rename this binder
          pure $ RenameForall m v s t fvcxt
        else Nothing
  _ -> pure Nothing

-- Get the let-bound definition of this variable, if some such exists
-- and is substitutible in the current context
getNonCapturedLocal :: LocalName k -> Reader Cxt (Maybe LetBinding)
getNonCapturedLocal v = do
  def <- asks (lookup $ unLocalName v)
  curCxt <- ask
  pure $ do
    (def', _, origCxt) <- def
    def'' <- def'
    let uncaptured x = ((==) `on` fmap snd3 . lookup x) origCxt curCxt
    if allOf _freeVarsLetBinding uncaptured def''
      then Just def''
      else Nothing

-- What are the FVs of the RHS of these bindings?
fvCxt :: S.Set Name -> Reader Cxt (S.Set Name)
fvCxt vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _freeVarsLetBinding) . flip lookup cxt) vs

fvCxtTy :: S.Set TyVarName -> Reader Cxt (S.Set TyVarName)
fvCxtTy vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _LetTypeBind % _2 % getting _freeVarsTy % _2) . flip lookupTy cxt) vs

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
findRedex ::
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  Maybe RedexWithContext
findRedex tydefs globals topDir = flip evalAccumT mempty . go . focus
  where
    -- What is the direction from the context?
    -- i.e. are we in the head of an elimination (or inside a hole)?
    focusDir :: ExprZ -> Dir
    focusDir ez = case up ez of
      Nothing -> topDir
      Just z -> case target z of
        App _ f _ | f == target ez -> Syn
        APP _ f _ | f == target ez -> Syn
        Case _ scrut _ | scrut == target ez -> Syn
        Hole _ _ -> Syn
        _ -> Chk
    focusType' :: ExprZ -> AccumT Cxt Maybe TypeZ
    -- Note that nothing in Expr binds a variable which scopes over a type child
    -- so we don't need to 'add' anything
    focusType' = lift . focusType
    hoistAccum :: Accum Cxt a -> AccumT Cxt Maybe a
    hoistAccum = Foreword.hoistAccum generalize
    go :: ExprZ -> AccumT Cxt Maybe RedexWithContext
    go ez = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir ez) (target ez)) >>= \case
        Just r -> pure $ RExpr ez r
        Nothing
          | Just (l, bz) <- viewLet ez -> goSubst l =<< hoistAccum bz
          -- Since stuck things other than lets are stuck on the first child or
          -- its type annotation, we can handle them all uniformly
          | otherwise ->
              msum $
                (goType =<< focusType' ez)
                  : map (go <=< hoistAccum) (exprChildren ez)
    goType :: TypeZ -> AccumT Cxt Maybe RedexWithContext
    goType tz = do
      hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
        Just r -> pure $ RType tz r
        Nothing
          | TLet _ a t _body <- target tz
          , [_, bz] <- typeChildren tz ->
              goSubstTy a t =<< hoistAccum bz
          | otherwise -> msum $ map (goType <=< hoistAccum) $ typeChildren tz
    goSubst :: LetBinding -> ExprZ -> AccumT Cxt Maybe RedexWithContext
    goSubst l ez = do
      hoistAccum (readerToAccumT $ viewRedex tydefs globals (focusDir ez) $ target ez) >>= \case
        -- We should inline such 'v' (note that we will not go under any 'v' binders)
        Just r@(InlineLet w _) | letBindingName l == unLocalName w -> pure $ RExpr ez r
        Just r@(InlineLetrec w _ _) | letBindingName l == unLocalName w -> pure $ RExpr ez r
        -- Elide a let only if it blocks the reduction
        Just r@(ElideLet w _) | elemOf _freeVarsLetBinding (letBindingName w) l -> pure $ RExpr ez r
        -- Rename a binder only if it blocks the reduction
        Just r@(RenameBindingsLam _ w _ _) | elemOf _freeVarsLetBinding (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameBindingsLAM _ w _ _) | elemOf _freeVarsLetBinding (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameBindingsCase _ _ brs _)
          | not $ S.disjoint (setOf _freeVarsLetBinding l) (setOf (folded % #_CaseBranch % _2 % folded % to bindName % to unLocalName) brs) ->
              pure $ RExpr ez r
        Just r@(RenameSelfLet w _ _) | elemOf _freeVarsLetBinding (unLocalName w) l -> pure $ RExpr ez r
        Just r@(RenameSelfLetType w _ _) | elemOf _freeVarsLetBinding (unLocalName w) l -> pure $ RExpr ez r
        -- Switch to an inner let if substituting under it would cause capture
        Nothing
          | Just (l', bz) <- viewLet ez
          , letBindingName l' /= letBindingName l
          , elemOf _freeVarsLetBinding (letBindingName l') l ->
              goSubst l' =<< hoistAccum bz
        -- We should not go under 'v' binders, but otherwise substitute in each child
        _ ->
          let substChild c = do
                guard $ S.notMember (letBindingName l) $ getBoundHere (target ez) (Just $ target c)
                goSubst l c
              substTyChild c = case l of
                LetTyBind (LetTypeBind v t) -> goSubstTy v t c
                _ -> mzero
           in msum @[] $ (substTyChild =<< focusType' ez) : map (substChild <=< hoistAccum) (exprChildren ez)
    goSubstTy :: TyVarName -> Type -> TypeZ -> AccumT Cxt Maybe RedexWithContext
    goSubstTy v t tz =
      let isFreeIn = elemOf (getting _freeVarsTy % _2)
       in do
            hoistAccum (readerToAccumT $ viewRedexType $ target tz) >>= \case
              -- We should inline such 'v' (note that we will not go under any 'v' binders)
              Just r@(InlineLetInType w _) | w == v -> pure $ RType tz r
              -- Elide a let only if it blocks the reduction
              Just r@(ElideLetInType (LetTypeBind w _) _) | w `isFreeIn` t -> pure $ RType tz r
              -- Rename a binder only if it blocks the reduction
              Just r@(RenameSelfLetInType w _ _) | w `isFreeIn` t -> pure $ RType tz r
              Just r@(RenameForall _ w _ _ _) | w `isFreeIn` t -> pure $ RType tz r
              -- We switch to an inner let if substituting under it would cause capture
              Nothing
                | TLet _ w s _ <- target tz
                , [_, bz] <- typeChildren tz
                , v /= w
                , w `isFreeIn` t ->
                    goSubstTy w s =<< hoistAccum bz
              -- We should not go under 'v' binders, but otherwise substitute in each child
              _ ->
                let substChild c = do
                      guard $
                        S.notMember (unLocalName v) $
                          S.map unLocalName $
                            getBoundHereTy (target tz) (Just $ target c)
                      goSubstTy v t c
                 in msum $ map (substChild <=< hoistAccum) (typeChildren tz)

children' :: IsZipper za a => za -> [za]
children' z = case down z of
  Nothing -> mempty
  Just z' -> z' : unfoldr (fmap (\x -> (x, x)) . right) z'

exprChildren :: ExprZ -> [Accum Cxt ExprZ]
exprChildren ez =
  children' ez <&> \c -> do
    let bs = getBoundHere' (target ez) (Just $ target c)
    addBinds ez bs
    pure c

typeChildren :: TypeZ -> [Accum Cxt TypeZ]
typeChildren tz =
  children' tz <&> \c -> do
    let bs = getBoundHereTy' (target tz) (Just $ target c)
    addBinds tz $ bimap unLocalName LetTyBind <$> bs
    pure c

addBinds :: HasID i => i -> [Either Name LetBinding] -> Accum Cxt ()
addBinds i' bs = do
  let i = getID i'
  cxt <- look
  add $
    Cxt $
      M.fromList $
        bs <&> \case
          Left n -> (n, (Nothing, i, cxt))
          Right l -> (letBindingName l, (Just l, i, cxt))

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
  CaseRedex _ as aTys' _ xs e -> do
    aTys <- sequence aTys'
    -- TODO: we are putting trivial metadata in here...
    -- See https://github.com/hackworthltd/primer/issues/6
    foldrM (\(x, a, tyA) t -> let_ x (pure a `ann` generateTypeIDs tyA) (pure t)) e (zip3 xs as aTys)
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
