{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Primer.Eval.Redex (
  Redex (..),
  viewRedex,
  ViewRedexOptions (..),
  RunRedexOptions (..),
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
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Bitraversable (bitraverse)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (children, descendM)
import Data.List (zip3)
import Data.List.NonEmpty qualified as NonEmpty
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
  folded,
  getting,
  ifiltered,
  isnd,
  notElemOf,
  summing,
  to,
  traverseOf,
  (%),
  (<%),
  _Just,
  _1,
  _2,
 )
import Primer.Core (
  Bind,
  Bind' (Bind),
  CaseBranch,
  CaseBranch' (CaseBranch),
  CaseFallback,
  CaseFallback' (CaseExhaustive, CaseFallback),
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
    PrimCon,
    Var
  ),
  ExprMeta,
  GVarName,
  ID,
  LVarName,
  LocalName (unLocalName),
  Pattern (PatCon, PatPrim),
  PrimCon,
  TmVarRef (..),
  TyConName,
  TyVarName,
  Type,
  Type' (
    TCon,
    TForall,
    TFun,
    TLet,
    TVar
  ),
  TypeMeta,
  ValConName,
  bindName,
  caseBranchName,
  getID,
  typesInExpr,
 )
import Primer.Core.DSL (ann, letType, let_, letrec, lvar, tlet, tvar)
import Primer.Core.Transform (decomposeTAppCon)
import Primer.Core.Type (Kind')
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
  CaseReductionTrivialDetail (CaseReductionTrivialDetail),
  EvalDetail (
    BETAReduction,
    BetaReduction,
    BindRename,
    CaseReduction,
    CaseReductionTrivial,
    GlobalVarInline,
    LetRemoval,
    LocalTypeVarInline,
    LocalVarInline,
    PushLetDown,
    PushLetDownTy,
    RemoveAnn,
    TBindRename,
    TLetRemoval
  ),
  GlobalVarInlineDetail (GlobalVarInlineDetail),
  LetRemovalDetail (LetRemovalDetail),
  LocalVarInlineDetail (LocalVarInlineDetail),
  PushLetDetail (PushLetDetail),
  RemoveAnnDetail (RemoveAnnDetail),
 )
import Primer.Eval.Detail qualified
import Primer.Eval.Prim (tryPrimFun)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Log (ConvertLogMessage (convert), logWarning)
import Primer.Name (Name, NameCounter)
import Primer.Primitives (primConName)
import Primer.TypeDef (
  TypeDefMap,
  ValCon (valConArgs),
  astTypeDefParameters,
 )
import Primer.Typecheck.Utils (
  lookupConstructor,
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
  getBoundHereDnTy,
  letTypeBindingName,
 )

data ViewRedexOptions = ViewRedexOptions
  { pushMulti :: Bool
  }

data RunRedexOptions = RunRedexOptions
  {
  }

data EvalLog
  = -- | Found something that may have been a case redex,
    -- but the scrutinee's head is an out-of-scope constructor.
    -- This should not happen if the expression is type correct.
    CaseRedexUnknownCtor ValConName
  | -- | Found something that may have been a case redex,
    -- but there is no branch matching the constructor at the head of the scrutinee.
    -- This should not happen if the expression is type correct.
    CaseRedexMissingBranch Pattern
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
    CaseRedexCtorMismatch TyConName (Either ValConName PrimCon)
  | -- | Found something that may have been a case redex,
    -- but the number of arguments in the scrutinee differs from the number of bindings in the corresponding branch.
    -- (Or the number of arguments expected from the scrutinee's type differs from either of these.)
    -- This should not happen if the expression is type correct.
    CaseRedexWrongArgNum Pattern [Expr] [Type' ()] [LVarName]
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
  | -- let x = e in x  ~>  e
    InlineLet -- TODO/REVIEW: don't know if I want to change this def for push-down-lets, as only inline an immediate usage
      { var :: LVarName
      -- ^ What variable are we inlining
      , expr :: Expr
      -- ^ What is its definition
      , letID :: ID
      -- ^ Where was the binding (used for details)
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- letrec x = t : T in x  ~>  letrec x = t : T in t : T
    InlineLetrec -- TODO/REVIEW: don't know if I want to change this def for push-down-lets, as only inline an immediate usage
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
  | -- let x = e in f s  ~>  (let x = e in f) (let x = e in s)  etc
    -- [for any non-leaf @f s@ which neither binds @x@ (else we should elide)
    --  nor any free variable of @e@ (to avoid capture)]
    -- [We actually do this rule for a whole sequence of let bindings at once]
    -- [If we push into an annotation, we drop term variables:  let x = e in (t : T)  ~> (let x = e in t) : T]
    PushLet
      { bindings :: NonEmpty (ID, LetBinding)
      -- ^ The bindings we push
      , expr :: Expr
      -- ^ The expression we are pushing into, i.e. the original body of the above bindings
      , orig :: Expr
      -- ^ the original expression (used for details)
      }
  | -- let(rec/type) x = e in t  ~>  t  if x does not appear in t
    -- [We actually elide from a whole sequence of let bindings at once]
    ElideLet
      { letBindingsKeep :: [(ExprMeta, LetBinding)]
      -- ^ Subset of bindings to keep
      , letBindingsDrop :: NonEmpty (ID, LetBinding)
      -- ^ Subset of bindings to drop
      , body :: Expr
      -- ^ Body, in which the elided variables do not occur
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
      , forallKind :: Kind' ()
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
  | -- case e of _ -> t  ~>  t
    CaseRedexTrivial
      { rhs :: Expr
      -- ^ The rhs of the wildcard branch
      , orig :: Expr
      -- ^ The original redex (used for details)
      , scrutID :: ID
      -- ^ The ID of the whole scrutinee (used for details)
      }
  | -- case C as : T A of ... ; C xs -> e ; ...   ~>  let xs=as:(lettype p=A in S) in e for data T p = C S
    -- Since constructors are checkable and scrutinees must be synthesisable,
    -- there must be an annotation if the term is well-typed
    -- (i.e. we do not need to consider @case C as of ...@).
    CaseRedex
      { con :: Pattern
      -- ^ The head of the scrutinee
      , args :: [Expr]
      -- ^ The arguments of the scrutinee
      , argTys :: [Type' ()]
      -- ^ The type of each scrutinee's argument, directly from the constructor's definition
      -- (thus is not well formed in the current scope)
      , params :: [(TyVarName, Type' ())]
      -- ^ The parameters of the constructor's datatype, and their
      -- instantiations from inspecting the type annotation on the scrutinee.
      , binders :: Maybe [Bind]
      -- ^ The binders of the matching branch. 'Nothing' denotes the matching branch was the fallback branch.
      , rhs :: Expr
      -- ^ The rhs of the matching branch
      , orig :: Expr
      -- ^ The original redex (used for details)
      , scrutID :: ID
      -- ^ The ID of the whole scrutinee (used for details)
      , conID :: ID
      -- ^ The ID of the constructor node (head of the scrutinee) (used for details)
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
      , fallbackBranch :: CaseFallback
      , avoid :: S.Set Name
      -- ^ What names to avoid when renaming
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
  = -- let a = t in a  ~>  t
    InlineLetInType
      { var :: TyVarName
      -- ^ What variable are we inlining (used for finding normal-order redex)
      , ty :: Type
      -- ^ What is its definition (used for reduction)
      , letID :: ID
      -- ^ Where was the binding (used for details)
      -- TODO/REVIEW: it seems a bit silly recording both these IDs, since they'll be right next to each other...
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- let a = s in t1 t2  ~>  (let a = s in t1) (let a = s in t2)  etc
    -- (see notes on the analogous rule for Redex)
    PushLetType
      { bindings :: NonEmpty (ID, LetTypeBinding)
      -- ^ what bindings we are pushing (IDs used for details, bindings used for reduction)
      , intoTy :: Type
      -- ^ the type they are being pushed into (used for reduction)
      , origTy :: Type
      -- ^ what was the original ("let-outside") (used for details)
      }
  | -- let a = s in t  ~>  t  if a does not appear in t
    -- [We actually elide from a whole sequence of let bindings at once]
    ElideLetInType
      { letBindingsKeep :: [(TypeMeta, LetTypeBinding)]
      -- ^ Subset of bindings to keep
      , letBindingsDrop :: NonEmpty (ID, LetTypeBinding)
      -- ^ Subset of bindings to drop
      , body :: Type
      -- ^ Body, in which the elided variables do not occur
      , orig :: Type
      -- ^ the original let (used for details)
      }
  | -- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
    RenameForall
      { meta :: TypeMeta
      -- ^ metadata on forall (used for reduction)
      , origBinder :: TyVarName
      -- ^ original name, which we want to freshen (used for reduction, and finding normal-order redex)
      , kind :: Kind' ()
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
    `summing` ( _LetrecBind
                  <% ( to (\(a, b, c) -> (a, (b, c)))
                        % isnd
                        % (_1 % _freeVars' `summing` _2 % _freeVarsTy')
                        & ifiltered ((/=) . unLocalName)
                     )
              )
    `summing` (_LetTyBind % _freeVarsLetTypeBinding % to unLocalName)

_freeVarsLetTypeBinding :: Fold LetTypeBinding TyVarName
_freeVarsLetTypeBinding = _LetTypeBind % _2 % getting _freeVarsTy % _2

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
  orig@(Case _ scrut [] (CaseFallback rhs)) -> do
    -- If we have @case e of _ -> t@, then this reduces to @t@ without inspecting @e@
    -- i.e. a @case@ which does not actually discriminate is lazy
    pure $ CaseRedexTrivial rhs orig (getID scrut)
  -- Note that constructors are checkable, but scrutinees are synthesisable,
  -- thus we only have terms such as @case (C @a' x y : T a) of ...@. Thus we
  -- know the type of the scrutinee syntactically.
  --
  -- The patterns in the case branch have a Maybe TypeCache attached, but we
  -- should not assume that this has been filled in correctly, so we record
  -- the type of the scrutinee, and reconstruct the types of the pattern
  -- variables. This is especially important, as we do not (yet?) take care of
  -- metadata correctly in this evaluator (for instance, substituting when we
  -- do a BETA reduction)!
  orig@(Case mCase scrut@(Ann _ (Con mCon c args) annotation) brs fb) -> do
    (abstractArgTys, params) <- case lookupConstructor tydefs c of
      Nothing -> do
        logWarning $ CaseRedexUnknownCtor c
        mzero
      Just (vc, _, td) -> do
        pure (valConArgs vc, fst <$> astTypeDefParameters td)
    case fmap forgetTypeMetadata . snd <$> decomposeTAppCon annotation of
      -- If there is an annotation which is not an applied type
      -- constructor, then we do not consider it a redex. For example,
      -- 'case Cons : ? of {}' is a valid expression (scrutinising a
      -- hole-typed expression expects no branches), but we must not
      -- treat it as a redex, since there is no 'Cons' branch.
      Nothing -> mzero
      Just tyargsFromAnn -> do
        tyargs <- do
          unless (length params == length tyargsFromAnn) $
            logWarning $
              CaseRedexNotSaturated $
                forgetTypeMetadata annotation
          pure $ zip params tyargsFromAnn
        (patterns, br) <- extractBranch (PatCon c) brs fb
        renameBindings mCase scrut brs fb patterns orig
          <|> pure (formCaseRedex (PatCon c) abstractArgTys tyargs args patterns br (orig, scrut, getID mCon))
  orig@(Case _ scrut@(Ann _ (PrimCon mCon c) (TCon _ ty)) brs fb) ->
    if primConName c == ty
      then do
        (bindings, br) <- extractBranch (PatPrim c) brs fb
        pure $ formCaseRedex (PatPrim c) [] [] [] bindings br (orig, scrut, getID mCon)
      else do
        logWarning $ CaseRedexCtorMismatch ty $ Right c
        mzero
  -- literals (primitive constructors) are actually synthesisable, so may come
  -- without annotations
  orig@(Case _ scrut@(PrimCon mCon c) brs fb) -> do
    (bindings, br) <- extractBranch (PatPrim c) brs fb
    pure $ formCaseRedex (PatPrim c) [] [] [] bindings br (orig, scrut, getID mCon)
  _ -> mzero
  where
    extractBranch c brs fb =
      case (find ((c ==) . caseBranchName) brs, fb) of
        (Nothing, CaseExhaustive) -> do
          logWarning $ CaseRedexMissingBranch c
          mzero
        (Nothing, CaseFallback e) -> pure (Nothing, e)
        (Just (CaseBranch _ xs e), _) -> pure (Just xs, e)

    {- Note [Case reduction and variable capture]
       There is a subtlety here around variable capture.
       Consider
         case C s t : R A B of C a b -> e
       We would like to reduce this to
         let a = s : (lettype p1=A,p2=B in S); let b = t : (lettype p1=A,p2=B in T) in e
       where we have annotated `s` and `t` with their types, which will be
       built from `A` and `B` according to the definition of the type `data R p1 p2 = ... | C S T`.
       Note that the binding of `a` may capture a reference in `t`
       or (assuming type and term variables can shadow) in `A` or `B`.
       (NB: the free variables in `S` and `T` are a subset of `p1,p2`.)
       We must catch this case and rename the case binders as a first step.
       Note that the free vars in
       `t : (lettype p1=A,p2=B in T)` are a subset of the free vars in the
       arguments of the constructor (s, t) plus the arguments to its type
       annotation (A, B).
       We shall be conservative and rename all binders in every branch apart
       from these free vars, i.e. from any free var in the scrutinee
       `C s t : R A B`.
       (We could get away with only renaming within the matching branch, only
       avoiding those FVs that actually occur, and in a "telescope" fashion:
       the first binder needs to avoid the FVs of all except the first
       argument, the second needs to avoid all but the first two args, ...,
       the last doesn't need any renaming.)
    -}
    renameBindings meta scrutinee branches fallbackBranch patterns orig =
      let avoid = freeVars scrutinee
          binders = maybe mempty (S.fromList . map (unLocalName . bindName)) patterns
       in hoistMaybe $
            if S.disjoint avoid binders
              then Nothing
              else Just $ RenameBindingsCase{meta, scrutinee, branches, fallbackBranch, avoid, orig}
    formCaseRedex ::
      Pattern ->
      [Type' ()] ->
      [(TyVarName, Type' ())] ->
      [Expr] ->
      Maybe [Bind] ->
      Expr ->
      (Expr, Expr, ID) ->
      Redex
    formCaseRedex con argTys params args binders rhs (orig, scrut, conID) =
      CaseRedex{con, args, argTys, params, binders, rhs, orig, scrutID = getID scrut, conID}

-- We record each binder, along with its let-bound RHS (if any)
-- and its original binding location and  context (to be able to detect capture)
-- Invariant: lookup x c == Just (Just l,_,_) ==> letBindingName l == x
newtype Cxt = Cxt (M.Map Name (Maybe LetBinding, ID, Cxt))
  -- We want right-biased mappend, as we will use this with 'Accum'
  -- and want later 'add's to overwrite earlier (more-global) context entries
  deriving (Semigroup, Monoid) via Dual (M.Map Name (Maybe LetBinding, ID, Cxt))

lookup :: Name -> Cxt -> Maybe (Maybe LetBinding, ID, Cxt)
lookup n (Cxt cxt) = M.lookup n cxt

-- This notices all redexes
-- Note that if a term is not a redex, but stuck on some sub-term,
-- then it is either
-- - a let (of some flavor)
-- - stuck on its left-most child
-- - stuck on the type annotation on its left-most child
-- - stuck on expression under the type annotation in its left-most child
--
--
-- TODO/REVIEW: we use this to choose order, by going into type annotations first.
-- However, consider
--    (Λa.Λb. t : ∀a.∀b. T) @x @y
-- which reduces to
--    (let a = x in Λb. t : let a=x in ∀b. T) @y
-- one step in type and one in term would give
--    (Λb. let a = x in t : ∀b. let a=x in T) @y
-- which would then reduce the top application:
--    (let b = y in let a = x in t : let b = y in let a=x in T) @y
-- rather than forcing us to push the substitution through the annotation first.
-- This way would be more efficient (with grouped lets): we only have to push one large
-- substitution through T (and t) once, rather than two small ones.
--
-- IDEAS: either have an annotation "why stuck", or just say "do let-push in each (ty&tm) child first, before anything else)
viewRedex ::
  (MonadLog (WithSeverity l) m, ConvertLogMessage EvalLog l) =>
  ViewRedexOptions ->
  TypeDefMap ->
  DefMap ->
  Dir ->
  Expr ->
  ReaderT Cxt (MaybeT m) Redex
viewRedex opts tydefs globals dir = \case
  orig@(Var _ (GlobalVarRef gvar))
    | Just (DefAST def) <- gvar `M.lookup` globals ->
        pure $
          InlineGlobal{gvar, def, orig}
  Let mLet var rhs (Var mVar (LocalVarRef var'))
    | var == var'
    -> pure $
          InlineLet
            { var
            , expr = rhs
            , letID = getID mLet
            , varID = getID mVar
            }
  Letrec mLet var rhs ty (Var mVar (LocalVarRef var'))
    | var == var'
    -> pure $
          InlineLetrec
            { var
            , expr = rhs
            , ty
            , letID = getID mLet
            , varID = getID mVar
            }
  orig@(viewLets -> Just (letBinding1,expr',letBindings,body))
    | not opts.pushMulti
    , not $ isLeaf expr'
    , null letBindings
    , S.disjoint (getBoundHereDn expr') (setOf (_2 % (_freeVarsLetBinding `summing` to letBindingName)) letBinding1)
    -> pure $
        PushLet
          { bindings = pure $ first getID letBinding1
          , expr = expr'
          , orig
          }
    | opts.pushMulti
    , not $ isLeaf body
    , S.disjoint (getBoundHereDn body) (setOf (folded % _2 % (_freeVarsLetBinding `summing` to letBindingName)) $ letBinding1 : letBindings)
    -> pure $
        PushLet
          { bindings = first getID <$> letBinding1 :| letBindings
          , expr = body
          , orig
          }
    -- NB: we will recompute the freeVars set a lot (especially when doing EvalFull iterations)
    -- This could be optimised in the future. See
    -- https://github.com/hackworthltd/primer/issues/733
    | not opts.pushMulti
    , letBindingName (snd letBinding1) `S.notMember` freeVars expr'
    , isLeaf expr'
    -> pure $
        ElideLet
          { letBindingsDrop = pure $ first getID letBinding1
          , letBindingsKeep = mempty
          , body = expr'
          , orig
          }
    | opts.pushMulti
    , (letBindingsKeep, nonEmpty -> Just letBindingsDrop) <- partitionLets (letBinding1 : letBindings) body
    , isLeaf body
    -> pure $
        ElideLet
          { letBindingsDrop = first getID <$> letBindingsDrop
          , letBindingsKeep
          , body
          , orig
          }
  l@(Lam meta var body) -> do
    avoid <- cxtToAvoid
    if unLocalName var `S.member` avoid
      then pure $ RenameBindingsLam{var, meta, body, avoid, orig = l}
      else mzero
  l@(LAM meta v body) -> do
    avoid <- cxtToAvoid
    if unLocalName v `S.member` avoid
      then pure $ RenameBindingsLAM{tyvar = v, meta, body, avoid, orig = l}
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
  e@(Case meta scrutinee branches fallbackBranch) -> do
    avoid <- cxtToAvoid
    -- TODO: we arbitrarily decide that renaming takes priority over reducing the case
    -- This is good for evalfull, but bad for interactive use.
    -- Maybe we want to offer both. See
    -- https://github.com/hackworthltd/primer/issues/734
    if getBoundHereDn e `S.disjoint` avoid
      then lift $ viewCaseRedex tydefs e
      else pure $ RenameBindingsCase{meta, scrutinee, branches, fallbackBranch, avoid, orig = e}
  orig@(Ann _ expr ty) | Chk <- dir, concreteTy ty -> pure $ Upsilon{expr, ann = ty, orig}
  _ -> mzero
 where
   isLeaf = null . children

-- Decompose @let a = s in let b0 = t0 in ... let bn = tn in e@
-- into @(LetBind a s, let b0=t0 in ... e, [LetBind b0 t0, ..., LetBind bn tn], e)@
-- I.e. a combination of two views: first let & all lets.
-- Note that this decomposes lets of all flavors.
viewLets :: Expr -> Maybe ((ExprMeta, LetBinding), Expr, [(ExprMeta, LetBinding)], Expr)
viewLets e = do
    (l1,e') <- viewLet e
    let (ls,e'') = viewLets' e'
    pure (l1,e',ls,e'')
 where
  -- | Decompose multiple @let@s (of varying flavors) around a
  -- non-let expression.
  viewLets' :: Expr -> ([(ExprMeta, LetBinding)], Expr)
  viewLets' e = case viewLet e of
    Nothing -> ([],e)
    Just (l,e') -> first (l:) $ viewLets' e'
  -- | Decompose one @let@ (of any flavor)
  viewLet :: Expr -> Maybe ((ExprMeta, LetBinding), Expr)
  viewLet = \case
     Let m v e b -> Just ((m,LetBind v e),b)
     Letrec m v t ty b -> Just ((m,LetrecBind v t ty),b)
     LetType m a ty b -> Just ((m, LetTyBind (LetTypeBind a ty)),b)
     _ -> Nothing

-- TODO: rename?
unviewLets :: [(ExprMeta, LetBinding)] -> Expr -> Expr
unviewLets ls e = foldr
        (\(m,l) e' -> case l of
                LetBind v t -> Let m v t e'
                LetrecBind v t ty -> Letrec m v t ty e'
                LetTyBind (LetTypeBind v ty) -> LetType m v ty e'
            )
            e ls

viewLetsTy :: Type -> Maybe ((TypeMeta, LetTypeBinding), Type, [(TypeMeta, LetTypeBinding)], Type)
viewLetsTy ty = do
    (l1,ty') <- viewOne ty
    let (ls, ty'') = viewLets' ty'
    pure (l1, ty', ls, ty'')
  where
    viewOne = \case
      TLet m a t b -> Just ((m,LetTypeBind a t),b)
      _ -> Nothing
    viewLets' t = case viewOne t of
      Nothing -> ([],t)
      Just (l,t') -> first (l:) $ viewLets' t'

-- TODO: rename?
unviewLetsTy :: [(TypeMeta, LetTypeBinding)] -> Type -> Type
unviewLetsTy ls t = foldr
        (\(m,LetTypeBind v ty) t' -> TLet m v ty t' )
        t ls


viewRedexType :: ViewRedexOptions -> Type -> Reader Cxt (Maybe RedexType)
viewRedexType opts = \case
  TLet mLet v s (TVar mVar var)
    | v == var ->
        purer $
          InlineLetInType
            { var
            , ty = s
            , letID = getID mLet
            , varID = getID mVar
            }
  orig@(viewLetsTy -> Just (letBinding1,ty',letBindings,body))
    | not opts.pushMulti
    , not $ isLeaf ty'
    , null letBindings
    , S.disjoint (getBoundHereDnTy ty') (setOf (_2 % (_freeVarsLetTypeBinding `summing` to letTypeBindingName')) letBinding1)
    -> purer $
        PushLetType
          { bindings = pure $ first getID letBinding1
          , intoTy = ty'
          , origTy = orig
          }
    | opts.pushMulti
    , not $ isLeaf body
    , S.disjoint (getBoundHereDnTy body) (setOf (folded % _2 % (_freeVarsLetTypeBinding `summing` to letTypeBindingName')) $ letBinding1 : letBindings)
    -> purer $
        PushLetType
          { bindings = first getID <$> letBinding1 :| letBindings
          , intoTy = body
          , origTy = orig
          }
    | not opts.pushMulti
    , letTypeBindingName' (snd letBinding1) `S.notMember` freeVarsTy ty'
    , isLeaf ty'
    -> purer $
        ElideLetInType
          { letBindingsDrop = pure $ first getID letBinding1
          , letBindingsKeep = mempty
          , body = ty'
          , orig
          }
    | opts.pushMulti
    , (letBindingsKeep, nonEmpty -> Just letBindingsDrop) <- partitionLetsTy (letBinding1 : letBindings) body
    , isLeaf body
    -> purer $
        ElideLetInType
          { letBindingsDrop = first getID <$> letBindingsDrop
          , letBindingsKeep
          , body
          , orig
          }
  orig@(TForall meta origBinder kind body) -> do
    avoid <- cxtToAvoidTy
    pure $
      if origBinder `S.member` avoid
        then
          pure $
            -- If anything we may substitute would cause capture, we should rename this binder
            RenameForall
              { meta
              , origBinder
              , kind
              , body
              , avoid
              , orig
              }
        else Nothing
  _ -> pure Nothing
 where
   isLeaf = null . children
   letTypeBindingName' (LetTypeBind n _) = n

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

-- We may want to push some let bindings (some subset of the Cxt) under a
-- binder; what variable names must the binder avoid for this to be valid?
-- TODO: unfortunately, our 'Cxt' makes it hard to know precisely what bindings
-- we might push, so for now we are pessimistic. This will be improved shortly.
cxtToAvoid :: MonadReader Cxt m => m (S.Set Name)
cxtToAvoid = do
  Cxt cxt <- ask
  pure $ foldMap' (setOf (_1 % _Just % (to letBindingName `summing` _freeVarsLetBinding))) cxt

cxtToAvoidTy :: MonadReader Cxt m => m (S.Set TyVarName)
cxtToAvoidTy = do
  Cxt cxt <- ask
  pure $ foldMap' (setOf (_1 % _Just % _LetTyBind % _LetTypeBind % (_1 `summing` _2 % getting _freeVarsTy % _2))) cxt

-- TODO: deal with metadata. https://github.com/hackworthltd/primer/issues/6
runRedex :: forall l m. MonadEval l m => RunRedexOptions -> Redex -> m (Expr, EvalDetail)
runRedex opts = \case
  InlineGlobal{def, orig} -> do
    after <- ann (regenerateExprIDs $ astDefExpr def) (regenerateTypeIDs $ astDefType def)
    let details = GlobalVarInlineDetail{def, var = orig, after}
    pure (after, GlobalVarInline details)
  InlineLet{var, expr, letID, varID} -> do
    let details =
          LocalVarInlineDetail
            { letID
            , varID
            , valueID = getID expr
            , bindingName = var
            , replacementID = getID expr
            , isTypeVar = False
            }
    pure (expr, LocalVarInline details)
  InlineLetrec{var, expr, ty, letID, varID} -> do
    expr' <- letrec var (pure expr) (pure ty) $ ann (regenerateExprIDs expr) (regenerateTypeIDs ty)
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
  PushLet{bindings, expr, orig} -> do
    let binds = snd <$> bindings
    expr' <- descendM (addLets binds) =<< traverseOf typesInExpr (addTLets binds) expr
    let details =
          PushLetDetail
            { before = orig
            , after = expr'
            , letIDs = toList $ fst <$> bindings
            , letBindingNames = toList $ letBindingName <$> binds
            , intoID = getID expr
            }
    pure (expr', PushLetDown details)
  -- let(rec/type) x = e in t  ~>  t  if e does not appear in t
  ElideLet{body, letBindingsKeep, letBindingsDrop, orig} -> do
    let expr = unviewLets letBindingsKeep body
        details =
          LetRemovalDetail
            { before = orig
            , after = body
            , bindingNames = letBindingName . snd <$> letBindingsDrop
            , letIDs = fst <$> letBindingsDrop
            , bodyID = getID body
            }
    pure (expr, LetRemoval details)
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
  -- case e of _ -> t   ~>  t
  CaseRedexTrivial
    { rhs
    , orig
    , scrutID
    } ->
      let details =
            CaseReductionTrivialDetail
              { before = orig
              , after = rhs
              , targetID = scrutID
              , branchRhsID = getID rhs
              }
       in pure (rhs, CaseReductionTrivial details)
  -- case C as : T A of ... ; C xs -> e ; ...   ~>  let xs=as:(lettype p=A in S) in e for data T p = C S
  -- Note that when forming the CaseRedex we checked that the variables @xs@ were fresh for @as@ and @As@,
  -- so this will not capture any variables.
  CaseRedex
    { con
    , args
    , argTys
    , params
    , binders
    , rhs
    , orig
    , scrutID
    , conID
    } -> do
      let binderNames = maybe mempty (map bindName) binders
      unless (isNothing binders || (length args == length argTys && length args == length binderNames)) $
        logWarning $
          CaseRedexWrongArgNum con args argTys binderNames
      let freshLocalNameLike n avoid =
            if S.member n avoid
              then freshLocalName avoid
              else pure n
      -- ann' [(a,S),(b,T)] A = let a=S, b=T in A
      -- except we worry about capture (of 'a' in 'T') by actually doing
      --   let a'=S, b'=T, a=a', b=b' in A[a,b]
      --   for fresh a' and b'
      let subAnn ps' ty = do
            let avoid = (freeVarsTy ty S.\\ S.fromList (fst <$> ps')) <> foldMap' (freeVarsTy . snd) ps'
            ps <- for ps' $ \(a, t) -> (a,,) <$> freshLocalNameLike a avoid <*> generateTypeIDs t
            let renamed =
                  foldr
                    ( \(a, a', _) ->
                        if a == a'
                          then identity
                          else tlet a $ tvar a'
                    )
                    (generateTypeIDs ty)
                    ps
            foldr (\(_, a', tA) -> tlet a' $ pure tA) renamed ps
      let subAnns ps' ty = do
            let fvs = freeVarsTy ty
            let ps = filter (flip elem fvs . fst) ps'
            subAnn ps ty
      let ann' x t = x `ann` pure t
      (letIDs, expr') <-
        foldrM
          ( \(x, arg, argTy) (is, tm) -> do
              aA <- subAnns params argTy
              tm' <- let_ x (pure arg `ann'` aA) (pure tm)
              pure (getID tm' : is, tm')
          )
          ([], rhs)
          $ zip3 binderNames args argTys
      let details =
            CaseReductionDetail
              { before = orig
              , after = expr'
              , targetID = scrutID
              , targetCtorID = conID
              , ctorName = con
              , targetArgIDs = getID <$> args
              , branchBindingIDs = maybe mempty (fmap getID) binders
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
  RenameBindingsCase{meta, scrutinee, branches, fallbackBranch, avoid, orig}
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
              let expr' = Case meta scrutinee (brs0 ++ CaseBranch ctor binds' rhs' : brs1) fallbackBranch
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

addLets :: MonadFresh ID m => NonEmpty LetBinding -> Expr -> m Expr
addLets ls expr = foldrM addLet expr $ toList ls
  where
    addLet :: MonadFresh ID m => LetBinding -> Expr -> m Expr
    addLet (LetBind v e) b = let_ v (regenerateExprIDs e) (pure b)
    addLet (LetrecBind v t ty) b = letrec v (regenerateExprIDs t) (regenerateTypeIDs ty) (pure b)
    addLet (LetTyBind (LetTypeBind v ty)) b = letType v (regenerateTypeIDs ty) (pure b)

addTLets :: MonadFresh ID m => NonEmpty LetBinding -> Type -> m Type
addTLets ls t = foldrM addTLet t $ toList ls
  where
    addTLet :: MonadFresh ID m => LetBinding -> Type -> m Type
    -- drop let bindings of term variables
    addTLet (LetTyBind (LetTypeBind v ty)) b = tlet v (regenerateTypeIDs ty) (pure b)
    addTLet _ b = pure b

filterLets :: NonEmpty LetBinding -> Expr -> [LetBinding]
filterLets ls e = filterLets' (toList ls) (freeVars e)

filterLetsTy :: NonEmpty LetBinding -> Type -> [LetBinding]
filterLetsTy ls t = filterLets' (toList ls) (S.map unLocalName $ freeVarsTy t)

filterLets' :: [LetBinding] -> Set Name -> [LetBinding]
filterLets' ls fvs =
  fst $
    foldr
      ( \l (ls', fvs') ->
          let ln = letBindingName l
           in if ln `S.member` fvs'
                then (l : ls', S.delete ln fvs' `S.union` setOf _freeVarsLetBinding l)
                else (ls', fvs')
      )
      ([], fvs)
      ls

-- | Partitions lets into used and unused in a given term
-- note that some of the unused may be used in later unused, but not in later used or in the term
partitionLets :: [(a, LetBinding)] -> Expr -> ([(a, LetBinding)],[(a, LetBinding)])
partitionLets ls e = fst $ foldr
     ( \l ((used,unused), fvs') ->
          let ln = letBindingName $ snd l
           in if ln `S.member` fvs'
                then ((l : used, unused), S.delete ln fvs' `S.union` setOf (_2 % _freeVarsLetBinding) l)
                else ((used, l:unused), fvs')
      )
     (([],[]),freeVars e)
     ls

-- | Partitions lets into used and unused in a given term
-- note that some of the unused may be used in later unused, but not in later used or in the term
partitionLetsTy :: [(a, LetTypeBinding)] -> Type -> ([(a, LetTypeBinding)],[(a, LetTypeBinding)])
partitionLetsTy ls t = fst $ foldr
     ( \l@(_,LetTypeBind n l') ((used,unused), fvs') ->
           if n `S.member` fvs'
                then ((l : used, unused), S.delete n fvs' `S.union` freeVarsTy l')
                else ((used, l:unused), fvs')
      )
     (([],[]),freeVarsTy t)
     ls

runRedexTy :: MonadEval l m => RunRedexOptions -> RedexType -> m (Type, EvalDetail)
runRedexTy _opts (InlineLetInType{ty, letID, varID, var}) = do
  let details =
        LocalVarInlineDetail
          { letID
          , varID
          , valueID = getID ty
          , bindingName = var
          , replacementID = getID ty
          , isTypeVar = True
          }
  pure (ty, LocalTypeVarInline details)
runRedexTy opts (PushLetType{bindings, intoTy, origTy}) = do
  ty' <- descendM (addTLets $ LetTyBind . snd <$> bindings) intoTy
  let details =
        PushLetDetail
          { before = origTy
          , after = ty'
          , letIDs = toList $ fst <$> bindings
          , letBindingNames = toList $ letTypeBindingName . snd <$> bindings
          , intoID = getID intoTy
          }
  pure (ty', PushLetDownTy details)
-- let a = s in t  ~>  t  if a does not appear in t
runRedexTy _opts (ElideLetInType{body, orig, letBindingsKeep, letBindingsDrop}) = do
  let ty = unviewLetsTy letBindingsKeep body
      details =
        LetRemovalDetail
          { before = orig
          , after = body
            , bindingNames = letTypeBindingName . snd <$> letBindingsDrop
            , letIDs = fst <$> letBindingsDrop
          , bodyID = getID body
          }
  pure (ty, TLetRemoval details)
-- ∀a:k.t  ~>  ∀b:k. let a = b in t  for fresh b, avoiding the given set
runRedexTy _opts (RenameForall{meta, origBinder, kind, body, avoid, orig}) = do
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
