{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Monad.Trans.Maybe (MaybeT)
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
  _1,
  _2,
  _Just,
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

pushMulti = False

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
  | -- letrec x = t : T in x  ~>  letrec x = t : T in t : T
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
      , varID :: ID
      -- ^ Where was the occurrence (used for details)
      }
  | -- let a = s in t1 t2  ~>  (let a = s in t1) (let a = s in t2)  etc
    -- (see notes on the analogous rule for Redex)
    PushLetType
      { bindings :: NonEmpty LetTypeBinding
      -- ^ what bindings we are pushing (used for reduction)
      , intoTy :: Type
      -- ^ the type they are being pushed into (used for reduction)
      , origTy :: Type
      -- ^ what was the original ("let-outside") (used for details)
      , bindingIDs :: NonEmpty ID
      -- ^ the IDs of the bindings (used for details)
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
    `summing` (_LetTyBind % _freeVarsLetTypeBinding)

_freeVarsLetTypeBinding :: Fold LetTypeBinding Name
_freeVarsLetTypeBinding = _LetTypeBind % _2 % _freeVarsTy'

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
  orig@(viewLets -> Just (bindings, expr))
    | pushMulti || null (NonEmpty.tail bindings)
    , S.disjoint
        (getBoundHereDn expr)
        (foldMap' (S.singleton . letBindingName . snd) bindings <> setOf (folded % _2 % _freeVarsLetBinding) bindings) ->
        pure $ PushLet{bindings, expr, orig}
  orig@(Let _ var rhs body)
    | Var _ (LocalVarRef var') <- body
    , var == var' ->
        pure $
          InlineLet
            { var
            , expr = rhs
            , letID = getID orig
            , varID = getID body
            }
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
    | otherwise -> mzero
  orig@(LetType _ var trhs body)
    | unLocalName var `S.notMember` freeVars body ->
        pure $
          ElideLet
            { letBinding = LetTyBind $ LetTypeBind var trhs
            , body
            , orig
            }
    | otherwise -> mzero
  orig@(Letrec _ v e1 t body)
    | Var _ (LocalVarRef w) <- body
    , v == w ->
        pure $
          InlineLetrec
            { var = v
            , expr = e1
            , ty = t
            , letID = getID orig
            , varID = getID body
            }
    | unLocalName v `S.notMember` freeVars body ->
        pure $
          ElideLet
            { letBinding = LetrecBind v e1 t
            , body
            , orig
            }
    | otherwise -> mzero
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

-- | Decompose multiple @let@s (of varying flavors) around a
-- non-leaf expression.
viewLets :: Expr -> Maybe (NonEmpty (ID, LetBinding), Expr)
viewLets ex = case viewLets' ex of
  ([], _) -> Nothing
  (l : ls, b)
    | isLeaf b -> Nothing
    | otherwise -> Just (l :| ls, b)
  where
    viewLets' = \case
      Let m v e b -> first ((getID m, LetBind v e) :) $ viewLets' b
      Letrec m v t ty b -> first ((getID m, LetrecBind v t ty) :) $ viewLets' b
      LetType m a ty b -> first ((getID m, LetTyBind (LetTypeBind a ty)) :) $ viewLets' b
      e -> ([], e)
    isLeaf = null . children

viewLetsTy :: Type -> Maybe (NonEmpty (LetTypeBinding, ID), Type)
viewLetsTy ty = case viewLets' ty of
  ([], _) -> Nothing
  (l : ls, b)
    | isLeaf b -> Nothing
    | otherwise -> Just (l :| ls, b)
  where
    viewLets' = \case
      TLet m a t b -> first ((LetTypeBind a t, getID m) :) $ viewLets' b
      e -> ([], e)
    isLeaf = null . children

viewRedexType :: Type -> Reader Cxt (Maybe RedexType)
viewRedexType = \case
  origTy
    | Just (bindingsWithID, intoTy) <- viewLetsTy origTy
    , pushMulti || null (NonEmpty.tail bindingsWithID)
    , (bindings, bindingIDs) <- NonEmpty.unzip bindingsWithID
    , S.disjoint
        (S.map unLocalName $ getBoundHereDnTy intoTy)
        (foldMap' (S.singleton . letTypeBindingName) bindings <> setOf (folded % _freeVarsLetTypeBinding) bindings) ->
        purer $ PushLetType{bindings, intoTy, origTy, bindingIDs}
  orig@(TLet _ v s body)
    | TVar _ var <- body
    , v == var ->
        purer $
          InlineLetInType
            { var
            , ty = s
            , letID = getID orig
            , varID = getID body
            }
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
    | otherwise -> pure Nothing
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
runRedex :: forall l m. MonadEval l m => Redex -> m (Expr, EvalDetail)
runRedex = \case
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
    expr' <- descendM (addLets binds) =<< traverseOf typesInExpr (addLetTypes binds) expr
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
  where
    addLet :: LetBinding -> Expr -> m Expr
    addLet (LetBind v e) b = let_ v (regenerateExprIDs e) (pure b)
    addLet (LetrecBind v t ty) b = letrec v (regenerateExprIDs t) (regenerateTypeIDs ty) (pure b)
    addLet (LetTyBind (LetTypeBind v ty)) b = letType v (regenerateTypeIDs ty) (pure b)
    addLets :: NonEmpty LetBinding -> Expr -> m Expr
    addLets ls e = foldrM addLet e ls
    addLetType :: LetBinding -> Type -> m Type
    addLetType (LetTyBind (LetTypeBind v ty)) b = tlet v (regenerateTypeIDs ty) (pure b)
    -- drop let bindings of term variables
    addLetType _ b = pure b
    addLetTypes :: NonEmpty LetBinding -> Type -> m Type
    addLetTypes ls t = foldrM addLetType t ls

runRedexTy :: MonadEval l m => RedexType -> m (Type, EvalDetail)
runRedexTy (InlineLetInType{ty, letID, varID, var}) = do
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
runRedexTy (PushLetType{bindings, intoTy, origTy, bindingIDs}) = do
  let addTLet :: MonadFresh ID m => LetTypeBinding -> Type -> m Type
      addTLet (LetTypeBind v ty) b = tlet v (regenerateTypeIDs ty) (pure b)
      addTLets ls t = foldrM addTLet t ls
  ty' <- descendM (addTLets bindings) intoTy
  let details =
        PushLetDetail
          { before = origTy
          , -- \^ the expression before reduction
            after = ty'
          , letIDs = toList bindingIDs
          , -- \^ the ID of each let
            letBindingNames = map letTypeBindingName $ toList bindings
          , intoID = getID intoTy
          }
  pure (ty', PushLetDownTy details)
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
