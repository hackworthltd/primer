{-# LANGUAGE ImpredicativeTypes #-}

module Primer.Eval.Redex (
  Redex (..),
  viewRedex,
  runRedex,
  RedexType (..),
  viewRedexType,
  runRedexTy,
  Dir (Syn, Chk),
  Cxt (Cxt),
  _freeVarsLetBinding,
  EvalFullLog (..),
  MonadEvalFull,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.List.Extra (zip3)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (snd3, thd3)
import GHC.Err (error)
import Optics (
  AffineFold,
  Fold,
  afolding,
  allOf,
  elemOf,
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
  TmVarRef (..),
  TyConName,
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
import Primer.Core.Transform (removeAnn, unfoldAPP, unfoldApp)
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
  TypeDefMap,
 )
import Primer.Typecheck.Utils (
  TypeDefError (TDIHoleType, TDINotADT, TDINotSaturated, TDIUnknown),
  instantiateValCons',
  lookupConstructor,
  mkTAppCon,
 )
import Primer.Zipper (
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  bindersBelowTy,
  focus,
  getBoundHereDn,
 )
import Primer.Zipper.Type (
  LetTypeBinding,
  LetTypeBinding' (LetTypeBind),
 )

data EvalFullLog
  = -- | Found something that may have been a case redex,
    -- but the scrutinee's head is an out-of-scope constructor.
    -- This should not happen if the expression is type correct.
    CaseRedexUnknownCtor ValConName
  | -- | Found something that may have been a case redex,
    -- but there is no branch matching the constructor at the head of the scrutinee.
    -- This should not happen if the expression is type correct.
    CaseRedexMissingBranch ValConName
  | -- | Found something that may have been a case redex,
    -- but the scrutinee's type is not in scope
    -- This should not happen if the expression is type correct.
    CaseRedexUnknownType TyConName
  | -- | Found something that may have been a case redex,
    -- but the scrutinee's type is either under or over saturated.
    -- This should not happen if the expression is type correct.
    CaseRedexNotSaturated (Type' ())
  | -- | Found something that may have been a case redex,
    -- but the scrutinee's head (value) constructor does not construct a member of the scrutinee's type.
    -- This should not happen if the expression is type correct.
    CaseRedexCtorMismatch TyConName ValConName
  | -- | Found something that may have been a case redex,
    -- but the number of arguments in the scrutinee differs from the number of bindings in the corresponding branch.
    -- (Or the number of arguments expected from the scrutinee's type differs from either of these.)
    -- This should not happen if the expression is type correct.
    CaseRedexWrongArgNum ValConName [Expr] [Type' ()] [LVarName]
  | InvariantFailure Text
  deriving (Show, Eq)

instance ConvertLogMessage EvalFullLog EvalFullLog where
  convert = identity

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
  | -- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
    RenameForall TypeMeta TyVarName Kind Type (S.Set TyVarName)

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

viewCaseRedex ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalFullLog l) =>
  TypeDefMap ->
  Expr ->
  MaybeT m Redex
viewCaseRedex tydefs = \case
  -- The patterns in the case branch have a Maybe TypeCache attached, but we
  -- should not assume that this has been filled in correctly, so we record
  -- the type of the scrutinee, and reconstruct the types of the pattern
  -- variables. This is especially important, as we do not (yet?) take care of
  -- metadata correctly in this evaluator (for instance, substituting when we
  -- do a BETA reduction)!
  Case m expr brs -> do
    (c, tyargs, args) <- extractCon (removeAnn expr)
    ty <- case expr of
      Ann _ _ ty' -> pure $ forgetTypeMetadata ty'
      _ -> do
        -- In the constructors-are-synthesisable case, we don't have the benefit of
        -- an explicit annotation, and have to work out the type based off the name
        -- of the constructor.
        case lookupConstructor tydefs c of
          Nothing -> do
            logWarning $ CaseRedexUnknownCtor c
            mzero
          Just (_, tc, _) -> do
            pure $ mkTAppCon tc (forgetTypeMetadata <$> tyargs)
    -- Style note: unfortunately do notation does not work well with polytyped binds on ghc 9.2.4
    -- Thus we write this with an explicit bind instead.
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/18324
    -- and https://gitlab.haskell.org/ghc/ghc/-/issues/20020
    instantiateCon ty c >>= \argTys -> do
      (patterns, br) <- extractBranch c brs
      renameBindings m expr brs [ty] args patterns
        <|> pure (formCaseRedex ty c argTys args patterns br)
  _ -> mzero
  where
    extractCon expr =
      -- NB: constructors never have mixed App and APPs: they are always of the
      -- form C @a @b ... x y ...
      let (h, as) = unfoldApp expr
          (h', params) = unfoldAPP h
       in case h' of
            Con _ c -> pure (c, params, as)
            _ -> mzero
    extractBranch c brs =
      case find (\(CaseBranch n _ _) -> n == c) brs of
        Nothing -> do
          logWarning $ CaseRedexMissingBranch c
          mzero
        Just (CaseBranch _ xs e) -> pure (xs, e)

    instantiateCon :: Type' () -> ValConName -> MaybeT m (forall m'. MonadFresh NameCounter m' => [m' (Type' ())])
    instantiateCon ty c =
      let fail err = logWarning err >> mzero
       in case instantiateValCons' tydefs $ forgetTypeMetadata ty of
            Left (TDIUnknown t) -> fail $ CaseRedexUnknownType t
            Left TDINotSaturated -> fail $ CaseRedexNotSaturated ty
            Left TDIHoleType -> mzero -- this is not a redex, but not unexpected
            Left TDINotADT -> mzero -- note that this could happen if we had a type let, in which case this is not unexpected
            Right (t, _, instVCs) -> case find ((== c) . fst) instVCs of
              Nothing -> fail $ CaseRedexCtorMismatch t c
              Just (_, argTys) -> pure argTys
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
       in hoistMaybe $
            if S.disjoint avoid binders
              then Nothing
              else Just $ RenameBindingsCase m expr brs avoid
    formCaseRedex ::
      Type' () ->
      ValConName ->
      (forall m'. MonadFresh NameCounter m' => [m' (Type' ())]) ->
      [Expr] ->
      [Bind' a] ->
      Expr ->
      Redex
    formCaseRedex ty c argTys args patterns =
      CaseRedex c args argTys ty (map bindName patterns)

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
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalFullLog l) =>
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  ReaderT Cxt (MaybeT m) Redex
viewRedex tydefs globals dir = \case
  Var _ (GlobalVarRef x) | Just (DefAST y) <- x `M.lookup` globals -> pure $ InlineGlobal x y
  Var _ (LocalVarRef v) -> do
    runMaybeT (getNonCapturedLocal v) >>= \x -> do
      case x of
        Just (LetBind _ e) -> pure $ InlineLet v e
        Just (LetrecBind _ e t) -> pure $ InlineLetrec v e t
        _ -> mzero
  Let _ v e1 e2
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | unLocalName v `S.notMember` freeVars e2 -> pure $ ElideLet (LetBind v e1) e2
    | unLocalName v `S.member` freeVars e1 -> pure $ RenameSelfLet v e1 e2
    | otherwise -> mzero
  LetType _ v t e
    | unLocalName v `S.notMember` freeVars e -> pure $ ElideLet (LetTyBind $ LetTypeBind v t) e
    | v `S.member` freeVarsTy t -> pure $ RenameSelfLetType v t e
    | otherwise -> mzero
  Letrec _ v e1 t e2
    | unLocalName v `S.notMember` freeVars e2 -> pure $ ElideLet (LetrecBind v e1 t) e2
    | otherwise -> mzero
  l@(Lam m v e) -> do
    fvcxt <- fvCxt $ freeVars l
    if unLocalName v `S.member` fvcxt
      then pure $ RenameBindingsLam m v e fvcxt
      else mzero
  l@(LAM m v e) -> do
    fvcxt <- fvCxt $ freeVars l
    if unLocalName v `S.member` fvcxt
      then pure $ RenameBindingsLAM m v e fvcxt
      else mzero
  App _ (Ann _ (Lam _ x t) (TFun _ src tgt)) s -> pure $ Beta x t src tgt s
  e@App{} -> lift $ hoistMaybe $ ApplyPrimFun . thd3 <$> tryPrimFun (M.mapMaybe defPrim globals) e
  -- (Λa.t : ∀b.T) S  ~> (letType a = S in t) : (letType b = S in T)
  APP _ (Ann _ (LAM _ a t) (TForall _ b _ ty1)) ty2 -> pure $ BETA a t b ty1 ty2
  APP{} -> mzero
  e@(Case m s brs) -> do
    fvcxt <- fvCxt $ freeVars e
    -- TODO: we arbitrarily decide that renaming takes priority over reducing the case
    -- This is good for evalfull, but bad for interactive use.
    -- Maybe we want to offer both. See
    -- https://github.com/hackworthltd/primer/issues/734
    if getBoundHereDn e `S.disjoint` fvcxt
      then lift $ viewCaseRedex tydefs e
      else pure $ RenameBindingsCase m s brs fvcxt
  Ann _ t ty | Chk <- dir, concreteTy ty -> pure $ Upsilon t ty
  _ -> mzero

viewRedexType :: Type -> Reader Cxt (Maybe RedexType)
viewRedexType = \case
  TVar _ v ->
    runMaybeT (getNonCapturedLocal v) <&> \case
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
getNonCapturedLocal :: MonadReader Cxt m => LocalName k -> MaybeT m LetBinding
getNonCapturedLocal v = do
  def <- asks (lookup $ unLocalName v)
  curCxt <- ask
  hoistMaybe $ do
    (def', _, origCxt) <- def
    def'' <- def'
    let uncaptured x = ((==) `on` fmap snd3 . lookup x) origCxt curCxt
    if allOf _freeVarsLetBinding uncaptured def''
      then Just def''
      else Nothing

-- What are the FVs of the RHS of these bindings?
fvCxt :: MonadReader Cxt m => S.Set Name -> m (S.Set Name)
fvCxt vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _freeVarsLetBinding) . flip lookup cxt) vs

fvCxtTy :: S.Set TyVarName -> Reader Cxt (S.Set TyVarName)
fvCxtTy vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _LetTypeBind % _2 % getting _freeVarsTy % _2) . flip lookupTy cxt) vs

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
  CaseRedex c as aTys' _ xs e -> do
    aTys <- sequence aTys'
    unless (length as == length aTys && length as == length xs) $
      logWarning $
        CaseRedexWrongArgNum c as aTys xs
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
-- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
runRedexTy (RenameForall m a k s avoid) = do
  b <- freshLocalName (avoid <> freeVarsTy s <> bindersBelowTy (focus s))
  TForall m b k <$> tlet a (tvar b) (pure s)

type MonadEvalFull l m =
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadLog (WithSeverity l) m
  , ConvertLogMessage EvalFullLog l
  )