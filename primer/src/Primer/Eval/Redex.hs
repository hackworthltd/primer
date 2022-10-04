{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Primer.Eval.Redex
  ( Redex(..),    viewRedex,
    runRedex,
    RedexType(..),
    viewRedexType,
    runRedexTy,
    Dir(Syn,Chk),
    Cxt(Cxt),
    SomeLocal(LSome),Local(..),
    localName, _freeVarsLocal,
  )
  where

-- TODO: share code with Primer.Eval
-- I hope to reuse this code in Eval - the current implementation does some weird things with annotations and metadata
-- but that will come later

-- TODO: ensure do sane things to metadata
-- (Perhaps we should just run a TC pass after each step?)
-- See https://github.com/hackworthltd/primer/issues/6

import Foreword hiding (hoistAccum)

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (thd3, snd3)
import Optics (AffineFold,
               Fold, afolding, getting, summing, to, (%), _2, notElemOf, elemOf, _Just, Field1 (_1), foldVL, traverseOf_, allOf, isnd, ifiltered, (<%))
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
import Primer.Core.Transform (unfoldAPP, unfoldApp, renameTyVar)
import Primer.Core.Utils (
  concreteTy,
  forgetTypeMetadata,
  freeVars,
  freeVarsTy,
  _freeVars,
  _freeVarsTy, regenerateExprIDs, regenerateTypeIDs, generateTypeIDs, freshLocalName', freshLocalName,
 )
import Primer.Def (
  ASTDef (..),
  Def (..),
  DefMap,
  defPrim,
 )
import Primer.Eval.Prim (tryPrimFun)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name, NameCounter)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefParameters),
  TypeDefMap,
 )
import Primer.Typecheck.Utils (instantiateValCons', lookupConstructor, mkTAppCon)
import Primer.Zipper (
  getBoundHereDn, bindersBelowTy, focus,
 )
import Primer.Core.DSL (letrec, ann, let_, letType, tlet, tvar, lvar)
import Prelude (error)
import Control.Monad.Extra (untilJustM)
import Primer.Log (ConvertLogMessage, logError)
import Control.Monad.Log (MonadLog, WithSeverity)

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

{-
instance Show Redex where
  showsPrec _ = \case
    InlineGlobal x _ -> showString "InlineGlobal " <> shows x
    InlineLet x _ -> showString "InlineLet " <> shows x
    InlineLetrec x _ _ -> showString "InlineLetrec " <> shows x
    ElideLet l e -> showString "ElideLet"
    Beta _ _ _ _ _ -> showString "Beta"
    BETA _ _ _ _ _ -> showString "BETA"
    CaseRedex _ _ _ _ _ -> showString "Case"
    Upsilon _ _ -> showString "Upsilon"
    RenameBindingsLam _ _ _ _ -> showString "RenameBindingsLam"
    RenameBindingsLAM _ _ _ _ -> showString "RenameBindingsLAM"
    RenameBindingsCase _ _ _ _ -> showString "RenameBindingsCase"
    RenameSelfLet x _ _ -> showString "RenameSelfLet " <> shows x
    RenameSelfLetType x _ _ -> showString "RenameSelfLetType " <> shows x
    ApplyPrimFun _ -> showString "ApplyPrimFun"
-}
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
{-
deriving Show
deriving instance Show (Local k)
-}

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
  -- Since letrec bound variables expand with a local letrec,
  -- we don't consider the recursively-bound variable free
  -- (since it will not be in the inlining)
    `summing` _LLetrec <% (to (\(a,b,c) -> (a,(b,c)))
                         % isnd
                         % (_1 % _freeVars' `summing` _2 % _freeVarsTy')
                         & ifiltered ((/=) . unLocalName))
    `summing` _LLetType % _2 % _freeVarsTy'

_freeVarsSomeLocal :: Fold SomeLocal Name
_freeVarsSomeLocal = foldVL $ \f (LSome l) -> traverseOf_ _freeVarsLocal f l

data Dir = Syn | Chk
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Dir

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

-- We record each binder, along with its let-bound RHS (if any)
-- and its original binding location and  context (to be able to detect capture)
-- Invariant: lookup x c == Just (Just l,_,_) ==> localName l == x
newtype Cxt = Cxt (M.Map Name (Maybe SomeLocal, ID, Cxt))
  -- We want right-biased mappend, as we will use this with 'Accum'
  -- and want later 'add's to overwrite earlier (more-global) context entries
  deriving (Semigroup, Monoid) via Dual (M.Map Name (Maybe SomeLocal, ID, Cxt))
-- TODO/REVIEW: is it worth trying to use a dependent map here?
{-
  deriving Show
deriving instance Show (SomeLocal)
-}

lookup :: Name -> Cxt -> Maybe (Maybe SomeLocal, ID, Cxt)
lookup n (Cxt cxt) = M.lookup n cxt

-- We only care about LLetType if we are looking up tyvars,
-- as we assume that the input is well-typed, and the only things
-- that tyvars can refer to are lettype, or foralls
lookupTy :: TyVarName -> Cxt -> Maybe (Maybe (Local 'ATyVar), ID, Cxt)
lookupTy n c = case lookup (unLocalName n) c of
  Just (Just (LSome l@LLetType{}),i, c') -> Just (Just l, i, c')
  _ -> Nothing

-- This notices all redexes
-- Note that if a term is not a redex, but stuck on some sub-term,
-- then it is either
-- - a let (of some flavor)
-- - stuck on its left-most child
-- - stuck on the type annotation on its left-most child
-- - stuck on expression under the type annotation in its left-most child
viewRedex :: TypeDefMap ->
  DefMap ->
  Dir ->
  Expr -> Reader Cxt (Maybe Redex)
viewRedex tydefs globals dir = \case
  Var _ (GlobalVarRef x) | Just (DefAST y) <- x `M.lookup` globals -> purer $ InlineGlobal x y
  Var _ (LocalVarRef v) -> do
    getNonCapturedLocal v <&> \x -> do
      case x of
        Just (LSome (LLet _ e)) -> pure $ InlineLet v e
        Just (LSome (LLetrec _ e t)) -> pure $ InlineLetrec v e t
        _ -> Nothing
  Let _ v e1 e2
  -- TODO: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    | unLocalName v `S.notMember` freeVars e2 -> purer $ ElideLet (LSome $ LLet v e1) e2
    | unLocalName v `S.member` freeVars e1 -> purer $ RenameSelfLet v e1 e2
    | otherwise -> pure Nothing
  LetType _ v t e
    | unLocalName v `S.notMember` freeVars e -> purer $ ElideLet (LSome $ LLetType v t) e
    | v `S.member` freeVarsTy t -> purer $ RenameSelfLetType v t e
    | otherwise -> pure Nothing
  Letrec _ v e1 t e2
    | unLocalName v `S.notMember` freeVars e2 -> purer $ ElideLet (LSome $ LLetrec v e1 t) e2
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
    -- This is good for evalfull, but bad for interactive
    -- maybe we want to offer both, or maybe it will evaporate when do push-down-let
    if getBoundHereDn e `S.disjoint` fvcxt
      then case viewCaseRedex tydefs e of
        Nothing -> pure Nothing
        Just r -> purer r
      else pure $ pure $ RenameBindingsCase m s brs fvcxt
  Ann _ t ty | Chk <- dir, concreteTy ty -> purer $ Upsilon t ty
  _ -> pure Nothing

viewRedexType :: Type -> Reader Cxt (Maybe RedexType)
viewRedexType = \case
  TVar _ v -> getNonCapturedLocal v <&> \case
        Just (LSome (LLetType _ t)) -> pure $ InlineLetInType v t
        _ -> Nothing
  -- TODO: We may be able to do better if we grab the free vars out of the "catamorphism"
  TLet _ v s t
    | notElemOf (getting _freeVarsTy % _2) v t -> purer $ ElideLetInType (LLetType v s) t
    | elemOf (getting _freeVarsTy % _2) v s -> purer $ RenameSelfLetInType v s t
    | otherwise -> pure Nothing
  fa@(TForall m v s t) -> do
    c <- ask
    fvcxt <- fvCxtTy $ freeVarsTy fa
    pure $
      if v `S.member` fvcxt
        then -- If anything we may substitute would cause capture, we should rename this binder
          pure $ RenameForall m v s t fvcxt
        else Nothing
  _ -> pure Nothing

-- Get the let-bound definition of this variable, if some such exists
-- and is substitutible in the current context
getNonCapturedLocal :: LocalName k -> Reader Cxt (Maybe SomeLocal)
getNonCapturedLocal v = do
  def <- asks (lookup $ unLocalName v)
  curCxt <- ask
  pure $ do
    (def', _origID, origCxt) <- def
    def'' <- def'
    let uncaptured x = ((==) `on` fmap snd3 . lookup x) origCxt curCxt
    if allOf _freeVarsSomeLocal uncaptured def''
      then Just def''
      else Nothing

-- What are the FVs of the RHS of these bindings?
fvCxt :: S.Set Name -> Reader Cxt (S.Set Name)
fvCxt vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _freeVarsSomeLocal) . flip lookup cxt) vs

fvCxtTy :: S.Set TyVarName -> Reader Cxt (S.Set TyVarName)
fvCxtTy vs = do
  cxt <- ask
  pure $ foldMap (setOf (_Just % _1 % _Just % _LLetType % _2 % getting _freeVarsTy % _2) . flip lookupTy cxt) vs

-- TODO: where should run go?
-- TODO: deal with metadata. https://github.com/hackworthltd/primer/issues/6
runRedex :: (MonadFresh ID m, MonadFresh NameCounter m) => Redex -> m Expr
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

runRedexTy :: (MonadLog (WithSeverity l) m, MonadFresh ID m, MonadFresh NameCounter m, ConvertLogMessage Text l) => RedexType -> m Type
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
      logError $ "runRedexTy.RenameForall: initial name choice was not fresh enough: chose " <> show b <> " for " <>
           show @_ @Text (m,a,k,s,avoid)
      untilJustM $ snd <$> rename
