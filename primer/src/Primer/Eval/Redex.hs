{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

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
  EvalLog (..),
  MonadEval,
  -- Exported for testing
  getNonCapturedLocal,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Data (Data)
import Data.List.Extra (zip3)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Set.Optics (setOf)
import Data.Tuple.Extra (snd3)
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
  Bind,
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
  getID,
 )
import Primer.Core.DSL (ann, letType, let_, letrec, lvar, tlet, tvar)
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
import Primer.Eval.Detail (
  ApplyPrimFunDetail (ApplyPrimFunDetail),
  BetaReductionDetail (BetaReductionDetail),
  BindRenameDetail (BindRenameDetail),
  CaseReductionDetail (CaseReductionDetail),
  EvalDetail (
    BETAReduction,
    BetaReduction,
    BindRename,
    CaseReduction,
    GlobalVarInline,
    LetRemoval,
    LocalTypeVarInline,
    LocalVarInline,
    RemoveAnn,
    TBindRename,
    TLetRemoval
  ),
  GlobalVarInlineDetail (GlobalVarInlineDetail),
  LetRemovalDetail (LetRemovalDetail),
  LocalVarInlineDetail (LocalVarInlineDetail),
  RemoveAnnDetail (RemoveAnnDetail),
  findFreeOccurrencesExpr,
  findFreeOccurrencesType,
 )
import Primer.Eval.Detail qualified
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
 )
import Primer.Zipper (
  LetBinding,
  LetBinding' (LetBind, LetTyBind, LetrecBind),
  bindersBelowTy,
  focus,
  getBoundHereDn,
  letBindingName,
 )
import Primer.Zipper.Type (
  LetTypeBinding,
  LetTypeBinding' (LetTypeBind),
 )

data EvalLog
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
  deriving stock (Show, Eq, Data, Generic)
  deriving anyclass (NFData)

instance ConvertLogMessage EvalLog EvalLog where
  convert = identity

data Redex
  = -- f  ~>  e : T  where we have  f : T ; f = e  in (global) scope
    InlineGlobal
      { gvar :: GVarName
      -- ^ What variable are we inlining (currently unused)
      , orig :: Expr
      -- ^ The original expression (just a wrapped @gvar@) (used for details)
      , def :: ASTDef
      -- ^ What is its definition
      }
  | -- x  ~>  e   where we are inside the scope of a  let x = e in ...
    InlineLet
      { var :: LVarName
      -- ^ What variable are we inlining
      , expr :: Expr
      -- ^ What is its definition
      , letID :: ID
      -- ^ Where was the binding (used for details)
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- x  ~>  letrec x:T=t in t:T   where we are inside the scope of a  letrec x : T = t in ...
    InlineLetrec
      { var :: LVarName
      -- ^ What variable are we inlining
      , expr :: Expr
      -- ^ What is its definition
      , ty :: Type
      -- ^ What type was it defined at
      , letID :: ID
      -- ^ Where was the binding (used for details)
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- let(rec/type) x = e in t  ~>  t  if x does not appear in t
    ElideLet
      { letBinding :: LetBinding
      -- ^ Original binding
      , body :: Expr
      -- ^ Body, in which the bound var does not occur
      , orig :: Expr
      -- ^ the original let (used for details)
      }
  | -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
    Beta
      { var :: LVarName
      -- ^ The lambda bound variable
      , body :: Expr
      -- ^ The body of the lambda
      , srcTy :: Type
      -- ^ The left-hand-side of the annotation (which is headed by an arrow)
      , tgtTy :: Type
      -- ^ The right-hand-side of the annotation
      , app :: Expr
      -- ^ The expression the lambda is applied to
      , orig :: Expr
      -- ^ The original beta redex (used for details)
      , lamID :: ID
      -- ^ Where was @var@ bound (used for details)
      }
  | -- (Λa.t : ∀b.T) S  ~>  (lettype a = S in t) : (lettype b = S in T)
    BETA
      { tyvar :: TyVarName
      -- ^ The Λ bound variable
      , body :: Expr
      -- ^ The body of the Λ
      , forallVar :: TyVarName
      -- ^ The annotation on the Λ must be a ∀, which binds this variable
      , forallKind :: Kind
      -- ^ The kind of the ∀ bound variable (used for details)
      , tgtTy :: Type
      -- ^ The body of the ∀ in the annotation
      , argTy :: Type
      -- ^ The type to which the Λ is applied
      , orig :: Expr
      -- ^ The original BETA redex (used for details)
      , lamID :: ID
      -- ^ Where was @var@ bound (used for details)
      }
  | -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- [However, for technical reasons (a hole type can act as a type-changing cast)
    -- we need to annotate the let bindings with *both* the type arising from the constructor,
    -- and that arising from the annotation, since these may differ in the presence of holes.
    -- In particular,
    --  case C @A a : T B of C x -> e
    -- should reduce to
    --  let x=a:S[A]:S[B] in e
    -- when we have
    --  data T p = C S[p]
    -- If the two annotations happen to be the same, then we only need one copy.
    -- ]
    -- also the non-annotated case, as we consider constructors to be synthesisable
    -- case C as of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
    -- (This is the natural rule if we consider non-annotated constructors to
    -- be shorthand for a annotated-lambda wrapping, and combine a few
    -- reduction steps. E.g.
    --     cons ==  (Λa λx λxs. Cons @a x xs) : ∀a. a -> List a -> List a
    -- )
    -- TODO (saturated constructors) update the above comment! NB: only annotated scrutinees are welltyped
    CaseRedex
      { con :: ValConName
      -- ^ The head of the scrutinee
      , args :: [Expr]
      -- ^ The arguments of the scrutinee
      , argTys :: forall m. MonadFresh NameCounter m => [m (Type' ())]
      -- ^ The type of each scrutinee's argument
      -- (from inspecting the type annotation on the scrutinee)
      , binders :: [Bind]
      -- ^ The binders of the matching branch
      , rhs :: Expr
      -- ^ The rhs of the matching branch
      , orig :: Expr
      -- ^ The original redex (used for details)
      , scrutID :: ID
      -- ^ The ID of the whole scrutinee (used for details)
      , conID :: ID
      -- ^ The ID of the whole constructor (head of the scrutinee) (used for details)
      }
  | -- [ t : T ]  ~>  t  writing [_] for the embedding of syn into chk
    -- This only fires for concrete (non-holey, no free vars) T, as otherwise the
    -- annotation can act as a type-changing cast:
    --   Nat ∋ [ True : ? ] checks, but Nat ∋ True fails
    Upsilon
      { expr :: Expr
      -- ^ The bare expression
      , ann :: Type
      -- ^ The annotation on @expr@ which we are removing
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | -- λy.t  ~>  λz.let y = z in t (and similar for other binding forms, but not
    -- let - that would create an infinite loop)
    -- This only fires when trying to do a substitution x|->y, and we need to go
    -- under a binder and avoid variable capture.
    -- We only record what names to avoid, and do the renaming in runRedex
    RenameBindingsLam
      { var :: LVarName
      -- ^ The (original) variable this lambda binds
      , meta :: ExprMeta
      -- ^ The metadata attached to the lambda
      , body :: Expr
      -- ^ The body of the lambda
      , avoid :: S.Set Name
      -- ^ What names to avoid when renaming
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | RenameBindingsLAM
      { tyvar :: TyVarName
      -- ^ The (original) variable this Λ binds
      , meta :: ExprMeta
      -- ^ The metadata attached to the Λ
      , body :: Expr
      -- ^ The body of the Λ
      , avoid :: S.Set Name
      -- ^ What names to avoid when renaming
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | RenameBindingsCase
      { meta :: ExprMeta
      -- ^ The metadata attached to the @case@
      , scrutinee :: Expr
      -- ^ The scrutinised expression
      , branches :: [CaseBranch]
      -- ^ The branches of the @case@
      , avoid :: S.Set Name
      -- ^ What names to avoid when renaming
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | -- let x = f x in g x x  ~>  let y = f x in let x = y in g x x
    -- Note that we cannot substitute the let in the initial term, since
    -- we only substitute one occurence at a time, and the 'let' would capture the 'x'
    -- in the expansion if we did a substitution.
    RenameSelfLet
      { var :: LVarName
      -- ^ The bound variable
      , rhs :: Expr
      -- ^ The local definition of @var@
      , body :: Expr
      -- ^ The body of the @let@, in which @var@ can occur free
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | -- As RenameSelfLet, but for LetType. (Note that it is unnecessary for letrec.)
    RenameSelfLetType
      { tyvar :: TyVarName
      -- ^ The bound variable
      , trhs :: Type
      -- ^ The local definition of @var@
      , body :: Expr
      -- ^ The body of the @let_type@, in which @var@ can occur free
      , orig :: Expr
      -- ^ The original redex (used for details)
      }
  | ApplyPrimFun
      { result :: forall m. MonadFresh ID m => m Expr
      -- ^ The result of the applied primitive function
      , primFun :: GVarName
      -- ^ The applied primitive function (used for details)
      , args :: [Expr]
      -- ^ The original arguments to @primFun@ (used for details)
      , orig :: Expr
      -- ^ The original redex (used for details)
      }

data RedexType
  = InlineLetInType
      { var :: TyVarName
      -- ^ What variable are we inlining (used for finding normal-order redex)
      , ty :: Type
      -- ^ What is its definition (used for reduction)
      , letID :: ID
      -- ^ Where was the binding (used for details)
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- let a = s in t  ~>  t  if a does not appear in t
    ElideLetInType
      { letBinding :: LetTypeBinding
      -- ^ original binding (name is used for finding normal-order redex; used for details)
      , body :: Type
      -- ^ Body, in which the bound var does not occur (used for reduction)
      , orig :: Type
      -- ^ the original let (used for details)
      }
  | -- let a = s a in t a a  ~>  let b = s a in let a = b in t a a
    -- Note that we cannot substitute the let in the initial term, since
    -- we only substitute one occurence at a time, and the 'let' would capture the 'a'
    -- in the expansion if we did a substitution.
    RenameSelfLetInType
      { letBinding :: LetTypeBinding
      -- ^ binding (name is used for finding normal-order redex; used for reduction)
      , body :: Type
      -- ^ body, in which th e bound var may occur (used for reduction)
      , orig :: Type
      -- ^  the original let (used for details)
      }
  | -- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
    RenameForall
      { meta :: TypeMeta
      -- ^ metadata on forall (used for reduction)
      , origBinder :: TyVarName
      -- ^ original name, which we want to freshen (used for reduction, and finding normal-order redex)
      , kind :: Kind
      -- ^ kind of bound var (used for reduction)
      , body :: Type
      -- ^ body of forall (used for reduction)
      , avoid :: S.Set TyVarName
      -- ^ must freshen to avoid this set (used for reduction)
      , orig :: Type
      -- ^ the original forall (used for details)
      }

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
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Dir

viewCaseRedex ::
  forall l m.
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  TypeDefMap ->
  Expr ->
  MaybeT m Redex
viewCaseRedex tydefs = \case
  -- Note that constructors are checkable, but scrutinees are synthesisable,
  -- thus we only have terms such as @case (C x y : T a) of ...@. Thus we
  -- know the type of the scrutinee syntactically.
  --
  -- The patterns in the case branch have a Maybe TypeCache attached, but we
  -- should not assume that this has been filled in correctly, so we record
  -- the type of the scrutinee, and reconstruct the types of the pattern
  -- variables. This is especially important, as we do not (yet?) take care of
  -- metadata correctly in this evaluator (for instance, substituting when we
  -- do a BETA reduction)!
  orig@(Case mCase scrut@(Ann _ (Con mCon c args) ty) brs) -> do
    -- Style note: unfortunately do notation does not work well with polytyped binds on ghc 9.2.4
    -- Thus we write this with an explicit bind instead.
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/18324
    -- and https://gitlab.haskell.org/ghc/ghc/-/issues/20020
    -- Implementation note: it is important to instantiate with the type-from-the-annotation first,
    -- since we could have 'case Cons : ? of {}' and we would like to silently say "not a redex,
    -- because hole type", rather than logging that the Cons is not saturated.
    instantiateCon (forgetTypeMetadata ty) c >>= \argTys -> do
      (patterns, br) <- extractBranch c brs
      renameBindings mCase scrut brs patterns orig
        <|> pure (formCaseRedex c argTys args patterns br (orig, scrut, getID mCon))
  _ -> mzero
  where
    pushMaybe :: Maybe (forall m'. c m' => [m' a]) -> forall m'. c m' => Maybe [m' a]
    pushMaybe Nothing = Nothing
    pushMaybe (Just xs) = Just xs

    extractCon expr = case decomposeAppCon expr of
      Just (c, m, params, as) -> pure (c, getID m, params, as)
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
       built from `A` and `B` according to the definition of the type `T`.
       Note that the binding of `a` may capture a reference in `t`
       or (assuming type and term variables can shadow) in `T`.
       We must catch this case and rename the case binders as a first step.
       Note that the free vars in `t : T` are a subset of the free vars in the
       arguments of the constructor (s, t) plus the arguments to its type
       annotations (A, B).
       We shall be conservative and rename all binders in every branch apart
       from these free vars, i.e. from any free var in the scrutinee
       `C s t : T A B`.
       (We could get away with only renaming within the matching branch, only
       avoiding those FVs that actually occur, and in a "telescope" fashion:
       the first binder needs to avoid the FVs of all except the first
       argument, the second needs to avoid all but the first two args, ...,
       the last doesn't need any renaming.)
    -}
    renameBindings meta scrutinee branches patterns orig =
      let avoid = freeVars scrutinee
          binders = S.fromList $ map (unLocalName . bindName) patterns
       in hoistMaybe $
            if S.disjoint avoid binders
              then Nothing
              else Just $ RenameBindingsCase{meta, scrutinee, branches, avoid, orig}
    formCaseRedex ::
      ValConName ->
      (forall m'. MonadFresh NameCounter m' => [m' (Type' ())]) ->
      [Expr] ->
      [Bind] ->
      Expr ->
      (Expr, Expr, ID) ->
      Redex
    formCaseRedex con argTys args binders rhs (orig, scrut, conID) =
      CaseRedex{con, args, argTys, binders, rhs, orig, scrutID = getID scrut, conID}

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
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  ReaderT Cxt (MaybeT m) Redex
viewRedex tydefs globals dir = \case
  orig@(Var _ (GlobalVarRef gvar))
    | Just (DefAST def) <- gvar `M.lookup` globals ->
        pure $
          InlineGlobal{gvar, def, orig}
  Var m (LocalVarRef var) -> do
    let varID = getID m
    runMaybeT (getNonCapturedLocal var) >>= \x -> do
      case x of
        Just (letID, LetBind _ expr) -> pure $ InlineLet{var, expr, letID, varID}
        Just (letID, LetrecBind _ expr ty) -> pure $ InlineLetrec{var, expr, ty, letID, varID}
        _ -> mzero
  orig@(Let _ var rhs body)
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | unLocalName var `S.notMember` freeVars body ->
        pure $
          ElideLet
            { letBinding = LetBind var rhs
            , body
            , orig
            }
    | unLocalName var `S.member` freeVars rhs -> pure $ RenameSelfLet{var, rhs, body, orig}
    | otherwise -> mzero
  orig@(LetType _ var trhs body)
    | unLocalName var `S.notMember` freeVars body ->
        pure $
          ElideLet
            { letBinding = LetTyBind $ LetTypeBind var trhs
            , body
            , orig
            }
    | var `S.member` freeVarsTy trhs -> pure $ RenameSelfLetType{tyvar = var, trhs, body, orig}
    | otherwise -> mzero
  orig@(Letrec _ v e1 t body)
    | unLocalName v `S.notMember` freeVars body ->
        pure $
          ElideLet
            { letBinding = LetrecBind v e1 t
            , body
            , orig
            }
    | otherwise -> mzero
  l@(Lam meta var body) -> do
    fvcxt <- fvCxt $ freeVars l
    if unLocalName var `S.member` fvcxt
      then pure $ RenameBindingsLam{var, meta, body, avoid = fvcxt, orig = l}
      else mzero
  l@(LAM meta v body) -> do
    fvcxt <- fvCxt $ freeVars l
    if unLocalName v `S.member` fvcxt
      then pure $ RenameBindingsLAM{tyvar = v, meta, body, avoid = fvcxt, orig = l}
      else mzero
  orig@(App _ (Ann _ (Lam m var body) (TFun _ srcTy tgtTy)) app) ->
    pure $
      Beta
        { var
        , body
        , srcTy
        , tgtTy
        , app
        , orig
        , lamID = getID m
        }
  e@App{} ->
    lift $
      hoistMaybe $
        tryPrimFun (M.mapMaybe defPrim globals) e >>= \(primFun, args, result) ->
          pure ApplyPrimFun{result, primFun, args, orig = e}
  -- (Λa.t : ∀b.T) S  ~> (letType a = S in t) : (letType b = S in T)
  orig@(APP _ (Ann _ (LAM m a body) (TForall _ forallVar forallKind tgtTy)) argTy) ->
    pure $
      BETA
        { tyvar = a
        , body
        , forallVar
        , forallKind
        , tgtTy
        , argTy
        , orig
        , lamID = getID m
        }
  APP{} -> mzero
  e@(Case meta scrutinee branches) -> do
    fvcxt <- fvCxt $ freeVars e
    -- TODO: we arbitrarily decide that renaming takes priority over reducing the case
    -- This is good for evalfull, but bad for interactive use.
    -- Maybe we want to offer both. See
    -- https://github.com/hackworthltd/primer/issues/734
    if getBoundHereDn e `S.disjoint` fvcxt
      then lift $ viewCaseRedex tydefs e
      else pure $ RenameBindingsCase{meta, scrutinee, branches, avoid = fvcxt, orig = e}
  orig@(Ann _ expr ty) | Chk <- dir, concreteTy ty -> pure $ Upsilon{expr, ann = ty, orig}
  _ -> mzero

viewRedexType :: Type -> Reader Cxt (Maybe RedexType)
viewRedexType = \case
  TVar m var ->
    runMaybeT (getNonCapturedLocal var) <&> \case
      Just (letID, LetTyBind (LetTypeBind _ ty)) -> pure $ InlineLetInType{var, ty, letID, varID = getID m}
      _ -> Nothing
  orig@(TLet _ v s body)
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | notElemOf (getting _freeVarsTy % _2) v body ->
        purer $
          ElideLetInType
            { letBinding = LetTypeBind v s
            , body
            , orig
            }
    | elemOf (getting _freeVarsTy % _2) v s ->
        purer $
          RenameSelfLetInType
            { letBinding = LetTypeBind v s
            , body
            , orig
            }
    | otherwise -> pure Nothing
  orig@(TForall meta origBinder kind body) -> do
    fvcxt <- fvCxtTy $ freeVarsTy orig
    pure $
      if origBinder `S.member` fvcxt
        then
          pure $
            -- If anything we may substitute would cause capture, we should rename this binder
            RenameForall
              { meta
              , origBinder
              , kind
              , body
              , avoid = fvcxt
              , orig
              }
        else Nothing
  _ -> pure Nothing

-- Get the let-bound definition of this variable, if some such exists
-- and is substitutible in the current context. (We also return the
-- id of the binding site.)
getNonCapturedLocal :: MonadReader Cxt m => LocalName k -> MaybeT m (ID, LetBinding)
getNonCapturedLocal v = do
  def <- asks (lookup $ unLocalName v)
  curCxt <- ask
  hoistMaybe $ do
    (def', origID, origCxt) <- def
    def'' <- def'
    let uncaptured x = ((==) `on` fmap snd3 . lookup x) origCxt curCxt
    if allOf _freeVarsLetBinding uncaptured def''
      then Just (origID, def'')
      else Nothing

-- What are the FVs of the RHS of these bindings?
fvCxt :: MonadReader Cxt m => S.Set Name -> m (S.Set Name)
fvCxt vs = do
  cxt <- ask
  pure $ foldMap' (setOf (_Just % _1 % _Just % _freeVarsLetBinding) . flip lookup cxt) vs

fvCxtTy :: S.Set TyVarName -> Reader Cxt (S.Set TyVarName)
fvCxtTy vs = do
  cxt <- ask
  pure $ foldMap' (setOf (_Just % _1 % _Just % _LetTypeBind % _2 % getting _freeVarsTy % _2) . flip lookupTy cxt) vs

-- TODO: deal with metadata. https://github.com/hackworthltd/primer/issues/6
runRedex :: MonadEval l m => Redex -> m (Expr, EvalDetail)
runRedex = \case
  InlineGlobal{def, orig} -> do
    after <- ann (regenerateExprIDs $ astDefExpr def) (regenerateTypeIDs $ astDefType def)
    let details = GlobalVarInlineDetail{def, var = orig, after}
    pure (after, GlobalVarInline details)
  InlineLet{var, expr, letID, varID} -> do
    expr' <- regenerateExprIDs expr
    let details =
          LocalVarInlineDetail
            { letID
            , varID
            , valueID = getID expr
            , bindingName = var
            , replacementID = getID expr'
            , isTypeVar = False
            }
    pure (expr', LocalVarInline details)
  InlineLetrec{var, expr, ty, letID, varID} -> do
    expr' <- letrec var (regenerateExprIDs expr) (regenerateTypeIDs ty) $ ann (regenerateExprIDs expr) (regenerateTypeIDs ty)
    let details =
          LocalVarInlineDetail
            { letID
            , varID
            , valueID = getID expr
            , bindingName = var
            , replacementID = getID expr'
            , isTypeVar = False
            }
    pure (expr', LocalVarInline details)
  -- let(rec/type) x = e in t  ~>  t  if e does not appear in t
  ElideLet{body, letBinding, orig} -> do
    let details =
          LetRemovalDetail
            { before = orig
            , after = body
            , bindingName = letBindingName letBinding
            , letID = getID orig
            , bodyID = getID body
            }

    pure (body, LetRemoval details)

  -- (λx.t : S -> T) s  ~>  let x = s:S in t : T
  Beta{var, body, srcTy, tgtTy, app, orig, lamID} -> do
    expr' <- let_ var (pure app `ann` pure srcTy) (pure body) `ann` pure tgtTy
    let details =
          BetaReductionDetail
            { before = orig
            , after = expr'
            , bindingName = var
            , lambdaID = lamID
            , letID = getID expr'
            , argID = getID app
            , bodyID = getID body
            , types = (srcTy, tgtTy)
            }
    pure (expr', BetaReduction details)
  -- (Λa.t : ∀b.T) S  ~>  (lettype a = S in t) : (lettype b = S in T)
  BETA{tyvar, body, forallVar, forallKind, tgtTy, argTy, orig, lamID} -> do
    expr' <- letType tyvar (pure argTy) (pure body) `ann` tlet forallVar (regenerateTypeIDs argTy) (pure tgtTy)
    let details =
          BetaReductionDetail
            { before = orig
            , after = expr'
            , bindingName = tyvar
            , lambdaID = lamID
            , letID = getID expr'
            , argID = getID argTy
            , bodyID = getID body
            , types = (forallKind, tgtTy)
            }
    pure (expr', BETAReduction details)
  -- case C as : T of ... ; C xs -> e ; ...   ~>  let xs=as:As in e for constructor C of type T, where args have types As
  -- Note that when forming the CaseRedex we checked that the variables @xs@ were fresh for @as@ and @As@,
  -- so this will not capture any variables.
  CaseRedex
    { con
    , args
    , argTys
    , binders
    , rhs
    , orig
    , scrutID
    , conID
    } -> do
      let binderNames = map bindName binders
      argTys' <- sequence argTys
      unless (length args == length argTys' && length args == length binders) $
        logWarning $
          CaseRedexWrongArgNum con args argTys' binderNames
      -- TODO: we are putting trivial metadata in here...
      -- See https://github.com/hackworthltd/primer/issues/6
      let ann' x t = x `ann` generateTypeIDs t
      let mkAnn (tyC, tyA') = case tyA' of
            Nothing -> (False, (`ann'` tyC))
            Just tyA
              | alphaEqTy tyC tyA -> (False, (`ann'` tyC))
              | otherwise -> (True, \x -> x `ann'` tyC `ann'` tyA)
      (letIDs, expr') <-
        foldrM
          ( \(x, a, ty) (is, t) -> do
              t' <- let_ x (pure a `ann'` ty) (pure t)
              pure (getID t' : is, t')
          )
          ([], rhs)
          -- TODO (saturated constructors)/REVIEW should we use a lettype, rather than doing the substitution in the type/annotation? This is not really related to satcon, but I happened to notice it!
          (zip3 binderNames args argTys')
      let details =
            CaseReductionDetail
              { before = orig
              , after = expr'
              , targetID = scrutID
              , targetCtorID = conID
              , ctorName = con
              , targetArgIDs = getID <$> args
              , branchBindingIDs = getID <$> binders
              , branchRhsID = getID rhs
              , letIDs
              }
      pure (expr', CaseReduction details)
  -- [ t : T ]  ~>  t  writing [_] for the embedding of syn into chk
  Upsilon{expr, ann = ty, orig} -> do
    let details =
          RemoveAnnDetail
            { before = orig
            , after = expr
            , typeID = getID ty
            }
    pure (expr, RemoveAnn details)
  -- λy.t  ~>  λz.let y = z in t (and similar for other binding forms, except let)
  RenameBindingsLam{var, meta, body, avoid, orig} -> do
    y <- freshLocalName' (avoid <> freeVars body)
    l <- let_ var (lvar y) (pure body)
    let expr' = Lam meta y l
    let details =
          BindRenameDetail
            { before = orig
            , after = expr'
            , bindingNamesOld = [unLocalName var]
            , bindingNamesNew = [unLocalName y]
            , bindersOld = [getID orig]
            , bindersNew = [getID expr']
            , bindingOccurrences = []
            , renamingLets = [getID l]
            , bodyID = getID body
            }
    pure (expr', BindRename details)
  RenameBindingsLAM{tyvar, meta, body, avoid, orig} -> do
    y <- freshLocalName' (avoid <> freeVars body)
    l <- letType tyvar (tvar y) (pure body)
    let expr' = LAM meta y l
    let details =
          BindRenameDetail
            { before = orig
            , after = expr'
            , bindingNamesOld = [unLocalName tyvar]
            , bindingNamesNew = [unLocalName y]
            , bindersOld = [getID orig]
            , bindersNew = [getID expr']
            , bindingOccurrences = []
            , renamingLets = [getID l]
            , bodyID = getID body
            }
    pure (expr', BindRename details)
  RenameBindingsCase{meta, scrutinee, branches, avoid, orig}
    | (brs0, CaseBranch ctor binds rhs : brs1) <- break (\(CaseBranch _ bs _) -> any ((`S.member` avoid) . unLocalName . bindName) bs) branches ->
        let bns = map bindName binds
            avoid' = avoid <> freeVars rhs <> S.fromList (map unLocalName bns)
         in do
              rn <- traverse (\b -> if unLocalName b `S.member` avoid then Right . (b,) <$> freshLocalName' avoid' else pure $ Left b) bns
              let f b@(Bind i _) = \case Left _ -> b; Right (_, w) -> Bind i w
              let binds' = zipWith f binds rn
              (renamingLets, rhs') <-
                foldrM
                  ( \(v, w) (ls, r) -> do
                      r' <- let_ v (lvar w) (pure r)
                      pure (getID r' : ls, r')
                  )
                  ([], rhs)
                  $ rights rn
              let expr' = Case meta scrutinee $ brs0 ++ CaseBranch ctor binds' rhs' : brs1
              let details =
                    BindRenameDetail
                      { before = orig
                      , after = expr'
                      , bindingNamesOld = map (unLocalName . bindName) binds
                      , bindingNamesNew = map (unLocalName . bindName) binds'
                      , bindersOld = map getID binds
                      , bindersNew = map getID binds'
                      , bindingOccurrences = []
                      , renamingLets
                      , bodyID = getID rhs
                      }
              pure (expr', BindRename details)
    -- We should replace this with a proper exception. See:
    -- https://github.com/hackworthltd/primer/issues/148
    | otherwise -> error "Internal Error: RenameBindingsCase found no applicable branches"
  -- let x = f x in g x x  ~>  let y = f x in let x = y in g x x
  RenameSelfLet{var, rhs, body, orig} -> do
    y <- freshLocalName' (freeVars rhs <> freeVars body)
    rl <- let_ var (lvar y) $ pure body
    expr' <- let_ y (pure rhs) $ pure rl
    let details =
          BindRenameDetail
            { before = orig
            , after = expr'
            , bindingNamesOld = [unLocalName var]
            , bindingNamesNew = [unLocalName y]
            , bindersOld = [getID orig]
            , bindersNew = [getID expr']
            , bindingOccurrences = findFreeOccurrencesExpr var rhs
            , renamingLets = [getID rl]
            , bodyID = getID body
            }
    pure (expr', BindRename details)
  -- As RenameSelfLet, but for LetType
  RenameSelfLetType{tyvar, trhs, body, orig} -> do
    b <- freshLocalName' (S.map unLocalName (freeVarsTy trhs) <> freeVars body)
    rl <- letType tyvar (tvar b) $ pure body
    expr' <- letType b (pure trhs) $ pure rl
    let details =
          BindRenameDetail
            { before = orig
            , after = expr'
            , bindingNamesOld = [unLocalName tyvar]
            , bindingNamesNew = [unLocalName b]
            , bindersOld = [getID orig]
            , bindersNew = [getID expr']
            , bindingOccurrences = findFreeOccurrencesType tyvar trhs
            , renamingLets = [getID rl]
            , bodyID = getID body
            }
    pure (expr', BindRename details)
  ApplyPrimFun{result, primFun, orig, args} -> do
    expr' <- result
    let details =
          ApplyPrimFunDetail
            { before = orig
            , after = expr'
            , name = primFun
            , argIDs = map getID args
            }
    pure (expr', Primer.Eval.Detail.ApplyPrimFun details)

runRedexTy :: MonadEval l m => RedexType -> m (Type, EvalDetail)
runRedexTy (InlineLetInType{ty, letID, varID, var}) = do
  ty' <- regenerateTypeIDs ty
  let details =
        LocalVarInlineDetail
          { letID
          , varID
          , valueID = getID ty
          , bindingName = var
          , replacementID = getID ty'
          , isTypeVar = True
          }
  pure (ty', LocalTypeVarInline details)
-- let a = s in t  ~>  t  if a does not appear in t
runRedexTy (ElideLetInType{body, orig, letBinding = (LetTypeBind v _)}) = do
  let details =
        LetRemovalDetail
          { before = orig
          , after = body
          , bindingName = unLocalName v
          , letID = getID orig
          , bodyID = getID body
          }
  pure (body, TLetRemoval details)
-- let a = s a in t a a  ~>  let b = s a in let a = b in t a a
runRedexTy (RenameSelfLetInType{letBinding = LetTypeBind a s, body, orig}) = do
  b <- freshLocalName (freeVarsTy s <> freeVarsTy body)
  insertedLet <- tlet a (tvar b) $ pure body
  result <- tlet b (pure s) $ pure insertedLet
  let details =
        BindRenameDetail
          { before = orig
          , after = result
          , bindingNamesOld = [unLocalName a]
          , bindingNamesNew = [unLocalName b]
          , bindersOld = [getID orig]
          , bindersNew = [getID result]
          , bindingOccurrences = findFreeOccurrencesType a s
          , renamingLets = [getID insertedLet]
          , bodyID = getID body
          }
  pure (result, TBindRename details)
-- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
runRedexTy (RenameForall{meta, origBinder, kind, body, avoid, orig}) = do
  newBinder <- freshLocalName (avoid <> freeVarsTy body <> bindersBelowTy (focus body))
  insertedLet <- tlet origBinder (tvar newBinder) (pure body)
  let result = TForall meta newBinder kind insertedLet
  let details =
        BindRenameDetail
          { before = orig
          , after = result
          , bindingNamesOld = [unLocalName origBinder]
          , bindingNamesNew = [unLocalName newBinder]
          , bindersOld = [getID orig]
          , bindersNew = [getID result]
          , bindingOccurrences = []
          , renamingLets = [getID insertedLet]
          , bodyID = getID body
          }
  pure (result, TBindRename details)

type MonadEval l m =
  ( MonadFresh ID m
  , MonadFresh NameCounter m
  , MonadLog (WithSeverity l) m
  , ConvertLogMessage EvalLog l
  )
