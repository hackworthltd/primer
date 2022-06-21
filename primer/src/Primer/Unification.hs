module Primer.Unification (InternalUnifyError (..), unify) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as M
import qualified Data.Set as S
import Optics (anyOf, getting, over, set)
import Primer.Core (
  ID,
  TyVarName,
  Type' (TApp, TCon, TEmptyHole, TForall, TFun, THole, TVar),
  trivialMeta,
  _typeMeta,
 )
import Primer.Core.Utils (_freeVarsTy)
import Primer.Name (NameCounter)
import Primer.Subst (substTy)
import Primer.Typecheck (
  Cxt (smartHoles),
  SmartHoles (NoSmartHoles),
  Type,
  TypeError,
  checkKind,
  consistentKinds,
  lookupLocalTy,
 )

-- | This should never be thrown - it indicates a bug in either this module, or in how it is called
data InternalUnifyError
  = InternalUnifyVarNotInCxt Cxt TyVarName
  deriving (Show)

-- | Attempts to find a substitution for the given variables that makes the types consistent (i.e. equal-up-to-holes).
-- We represent unification variables as TVars which happen to have names in the given set.
-- We unify without caring about kinds, but afterwards check that the solution is well-kinded.
-- We ensure that (if a unifier is found) the returned substitution is idempotent, in the sense that
-- it only needs to be applied once to remove all solved unification variables; i.e. in the RHS there
-- never appears a solved unification variable.
-- We ensure we are stable under swapping the two input types
--  (for testing purposes: it is easy to do here, but quite awkward to figure out what the symmetry property should be if we don't
--   - the problem starts when unifying two unifvars: could get either way around, but then the knockon effects are complex
--   - eg @(unif1 -> unif3) \`unify\` (unif2 -> List unif2)@ says @unif1:=unif2; unif3:=List unif2@
--   - but symmetric version might say @unif2:=unif1; unif3:=List unif1@
--   - We are careful to always give the first result.
--   - However, this choice is not stable under renaming unification variables: an equivalent
--     unification problem with different names may choose the "other" solution.
--  )
unify ::
  (MonadFresh ID m, MonadFresh NameCounter m, MonadError InternalUnifyError m) =>
  -- | We only care about local type vars and typedefs, for kind-checking our unifier
  Cxt ->
  -- | Which type variables should be considered as unification variables? This should be a subset of the @Cxt@.
  -- All @Cxt@ vars are considered in scope for a solution of any unification variable.
  S.Set TyVarName ->
  Type ->
  Type ->
  m (Maybe (M.Map TyVarName Type))
unify cxt unificationVars s t = do
  result <-
    runExceptT . flip execStateT mempty . flip runReaderT initEnv . unU $ unify' s t
  case result of
    Left _err -> pure Nothing
    Right sb -> do
      -- We need Type' (Meta a) to call checkKind, but we know the meta will
      -- not be inspected in any way we care about, since we only care whether
      -- checkKind succeeded, and not the result. Thus we just add some dummy
      -- ones.
      -- TODO: this is a bit of a code smell!
      let addPointlessMeta = set _typeMeta $ trivialMeta 0
      let f v vt = case lookupLocalTy v cxt of
            Right k -> All . isRight <$> runExceptT @TypeError (runReaderT (checkKind k $ addPointlessMeta vt) (cxt{smartHoles = NoSmartHoles}))
            -- this catchall should never happen: sb should only contain
            -- solutions for unification variables, which should be a subset
            -- of the context!
            _ -> Ap . throwError $ InternalUnifyVarNotInCxt cxt v
      goodKinds <- getAll <$> getAp (M.foldMapWithKey f sb)
      pure $ if goodKinds then Just sb else Nothing
  where
    initEnv = Env{unifVars = unificationVars, boundVarsL = mempty, boundVarsR = mempty}

type UnifVars = S.Set TyVarName

data UnifError
  = NotUnify Type Type
  | OccursBoundCheckFail TyVarName Type

-- NB: we need to keep the input types on the same side always, to get boundVars info to line up
-- or rather, ensure we record the swap if we do a swap
data Env = Env
  { unifVars :: UnifVars
  -- ^ The variables originally declared to be uvs.
  -- These may be shadowed by bound variables (this set is not updated in that case).
  , boundVarsL :: M.Map TyVarName Int
  -- ^ Variables bound in the LHS of the unification problem.
  -- i.e. @forall@s that we have gone under, with their de Bruijn level
  , boundVarsR :: M.Map TyVarName Int
  -- ^ as 'boundVarsL', but for the RHS
  }

type Subst = M.Map TyVarName Type

newtype U m a = U {unU :: ReaderT Env (StateT Subst (ExceptT UnifError m)) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadState Subst
    , MonadError UnifError
    )

deriving instance MonadFresh NameCounter m => MonadFresh NameCounter (U m)

-- | @v@ is a unification variable if it both
-- - was declared to be, and
-- - has not been shadowed (by going under a equally-named forall)
isUnifVarL, isUnifVarR :: MonadFresh NameCounter m => TyVarName -> U m Bool
isUnifVarL n = asks (\env -> S.member n (unifVars env) && not (M.member n $ boundVarsL env))
isUnifVarR n = asks (\env -> S.member n (unifVars env) && not (M.member n $ boundVarsR env))

isSameVar :: MonadFresh NameCounter m => TyVarName -> TyVarName -> U m Bool
isSameVar n m = do
  nIdx <- asks (M.lookup n . boundVarsL)
  mIdx <- asks (M.lookup m . boundVarsR)
  pure $ case (nIdx, mIdx) of
    (Just i, Just j) -> i == j -- both locally bound (possibly with different names in the left/right type)
    (Nothing, Nothing) -> n == m -- both from the global context
    _ -> False -- locally-bound (forall) vars never unify with a variable from the context

swapEnv :: Env -> Env
swapEnv e = e{boundVarsL = boundVarsR e, boundVarsR = boundVarsL e}

-- Note: bound variables shadow unification variables.
-- This is handled in isUnifVarL and isUnifVarR
bind :: TyVarName -> TyVarName -> Env -> Env
bind n m e =
  e
    { boundVarsL = M.insert n (M.size $ boundVarsL e) $ boundVarsL e
    , boundVarsR = M.insert m (M.size $ boundVarsR e) $ boundVarsR e
    }

lookupSubst :: MonadFresh NameCounter m => TyVarName -> U m (Maybe Type)
lookupSubst = gets . M.lookup

-- We assume (both empty and non-empty) holes can unify with anything!
unify' :: MonadFresh NameCounter m => Type -> Type -> U m ()
unify' (TEmptyHole _) _ = pure ()
unify' _ (TEmptyHole _) = pure ()
unify' (THole _ _) _ = pure ()
unify' _ (THole _ _) = pure ()
unify' vx@(TVar _ x) vy@(TVar _ y) = do
  ux <- isUnifVarL x
  uy <- isUnifVarR y
  eq <- isSameVar x y
  case (ux, uy, eq) of
    (_, _, True) -> pure ()
    (True, True, _) -> if x < y then unifyVar x vy else local swapEnv $ unifyVar y vx -- ensure unify S T == unify T S
    (True, _, _) -> unifyVar x vy
    (False, True, _) -> local swapEnv $ unifyVar y vx
    (False, False, False) -> throwError $ NotUnify vx vy
unify' vx@(TVar _ x) t =
  isUnifVarL x >>= \case
    True -> unifyVar x t
    False -> throwError $ NotUnify vx t
unify' s vy@(TVar _ _) = local swapEnv $ unify' vy s
unify' (TCon _ n) (TCon _ m) | n == m = pure ()
unify' (TFun _ s1 t1) (TFun _ s2 t2) = unify' s1 s2 >> unify' t1 t2
-- Doing first-order unification, as applications are only constructor-like
-- (we don't have any bona fide functions at the type level)
unify' (TApp _ s1 t1) (TApp _ s2 t2) = unify' s1 s2 >> unify' t1 t2
unify' (TForall _ n1 k1 t1) (TForall _ n2 k2 t2) | consistentKinds k1 k2 = local (bind n1 n2) $ unify' t1 t2
unify' s t = throwError $ NotUnify s t

-- We delay substitution till unifyVar case, so the monadic (>>) can be trivial
-- but we want the substitution to be "grounded"/"idempotent": free of solved unif vars on rhs
--  so: before record, need to subst in soln; after record need to subst new sol'n in every rhs
unifyVar :: MonadFresh NameCounter m => TyVarName -> Type -> U m ()
unifyVar v t =
  lookupSubst v >>= \case
    Just v' -> unify' v' t
    Nothing -> do
      t' <- subst t
      bound <- asks (M.keysSet . boundVarsR)
      let f (_, n) = n == v || S.member n bound
      -- occurs check + check t' does not mention bound variables which wouldn't be in scope for the unifier
      -- (It is not necessary to check boundVarsL, since such a reference could only occur via the expansion
      -- of another uv (a uv on the left will get solved by a subterm of the rhs, modulo expanding
      -- previously-solved uvs), and thus we would have noticed the problem in a previous iteration.
      if anyOf (getting _freeVarsTy) f t'
        then throwError $ OccursBoundCheckFail v t'
        else solve v t'

-- We both insert the solution, and substitute it in the RHS of known solutions
solve :: MonadFresh NameCounter m => TyVarName -> Type -> U m ()
solve n t = do
  sb <- get
  sb' <- traverse (substTy n t) sb
  let sb'' = M.insert n t sb'
  put sb''

-- applies the substitution to the (unification variables in the) type
subst :: MonadFresh NameCounter m => Type -> U m Type
subst t = do
  sb <- get
  let f (m, n) = fromMaybe (TVar m n) $ M.lookup n sb
  pure $ over _freeVarsTy f t
