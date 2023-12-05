{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.EvalFullInterp (
  interp,
  Timeout (MicroSec),
  interp',
  InterpError (..),
  Dir (..),
  mkEnv,
  mkGlobalEnv,
) where

import Foreword

import Control.Exception (throw)
import Data.Map.Lazy qualified as Map
import Data.Set qualified as Set
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  CaseFallback' (CaseFallback),
  Expr' (..),
  GVarName,
  LVarName,
  LocalName,
  Pattern (PatCon, PatPrim),
  PrimCon,
  TmVarRef (GlobalVarRef, LocalVarRef),
  TyConName,
  TyVarName,
  Type' (..),
  ValConName,
  bindName,
  caseBranchName,
  mapFallback,
  unLocalName,
 )
import Primer.Core.DSL.Meta (create')
import Primer.Core.Transform (decomposeTAppCon, unfoldApp)
import Primer.Core.Utils (
  concreteTy,
  forgetMetadata,
  forgetTypeMetadata,
  freeVars,
  freeVarsTy,
  freshen,
 )
import Primer.Def (ASTDef (ASTDef), Def (DefAST, DefPrim), DefMap)
import Primer.Eval.Redex (
  Dir (Chk, Syn),
 )
import Primer.Name (Name)
import Primer.Primitives (primConName, primFunDef)
import Primer.Primitives.PrimDef (PrimDef)
import Primer.TypeDef (
  ASTTypeDef (ASTTypeDef),
  TypeDef (TypeDefAST),
  TypeDefMap,
  ValCon (valConArgs, valConName),
 )
import System.Timeout (timeout)

-- Invariant: vars is the set of free vars in the values of the map
-- Thus the binders we need to rename before going under them.
-- Invariant: the values in the env will be in normal form.
data EnvTm = EnvTm
  { vars :: Set Name
  , env :: Map.Map (Either GVarName LVarName) (Expr' () () ())
  , prims :: Map GVarName PrimDef
  }
data EnvTy = EnvTy
  { vars :: Set Name
  , env :: Map.Map TyVarName (Type' () ())
  }

-- | Convert an environment into the form needed for 'interp'
mkEnv ::
  [(Either GVarName LVarName, Expr' a b c)] ->
  Map GVarName PrimDef ->
  [(TyVarName, Type' a b)] ->
  (EnvTm, EnvTy)
mkEnv tms prims tys =
  extendTmsEnv
    (second forgetMetadata <$> tms)
    ( EnvTm mempty mempty prims
    , extendTysEnv' (second forgetTypeMetadata <$> tys) $ EnvTy mempty mempty
    )

-- | Convert an environment into the form needed for 'interp'
mkGlobalEnv :: DefMap -> (EnvTm, EnvTy)
mkGlobalEnv defs =
  mkEnv
    ( mapMaybe
        ( \(f, d) -> case d of
            DefAST (ASTDef tm ty) -> Just (Left f, Ann () (forgetMetadata tm) (forgetTypeMetadata ty))
            _ -> Nothing
        )
        $ Map.assocs defs
    )
    ( Map.mapMaybe
        ( \case
            DefPrim p -> Just p
            _ -> Nothing
        )
        defs
    )
    mempty

data InterpError
  = Timeout
  | NoBranch (Either ValConName PrimCon) [Pattern]
  | UnknownTyCon TyConName
  | UnknownValCon TyConName ValConName
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

newtype Timeout = MicroSec Int

-- | Wrap the interpreter in a IO-based timeout, and catch 'InterpError' exceptions
interp ::
  Timeout ->
  TypeDefMap ->
  (EnvTm, EnvTy) ->
  Dir ->
  Expr' () () () ->
  IO (Either InterpError (Expr' () () ()))
interp (MicroSec t) tydefs env dir e = do
  e' <- timeout t (try $ evaluate $ force $ interp' tydefs env dir e)
  pure $ case e' of
    Nothing -> Left Timeout
    Just e'' -> e''

{- HLINT ignore interp' "Avoid restricted function" -}
-- (we are intentionally using 'throw' here

-- | A somewhat-efficient interpreter for primer terms.
--
-- We fully evaluate the term, including under lambdas and
-- inside holes, by convincing Haskell's runtime system to do the
-- evaluation for us, in a call-by-need fashion. We return an AST
-- of the evaluated term, which will be type-correct (assuming the
-- input was): see 'Tests.EvalFullInterp.tasty_type_preservation';
-- and will agree with iterating the small-step interpreter: see
-- 'Tests.EvalFullInterp.tasty_two_interp_agree'.
--
-- Warnings:
-- - Trying to evaluate a divergent term will (unsurprisingly) not terminate,
--   and may consume a lot of memory.
-- - We 'throw' IO-based exceptions of type 'InterpError' for obvious issues
--   in case expressions (though this will not happen with well-typed
--   terms). This function will not throw 'Timeout' (that will only be
--   thrown by the 'interp' wrapper).
-- - To avoid these pitfalls, consider using 'interp', which limits runtime
--   with 'timeout', and catches exceptions.
--
-- The interpreter is designed to be fairly simple, and to stay in the
-- AST domain, but it does rely on subtle laziness properties (see below).
-- We attempt to make it reasonably fast, given the constraint of simplicity,
-- and maximally terminating.  We evaluate terms in some environment of local
-- bindings, which enables us to share computation in @let x=<expensive>
-- in f x x@ in a call-by-need fashion.  This is accomplished by the
-- environment being a mapping of variable names to normal forms which
-- are computed lazily.  The laziness is also used to avoid looping on
-- diverging subterms which do not need to be fully-evaluated, for example
-- in @head (letrec l=Cons True l in l)@, which should swiftly reduce
-- to @True@. Another somewhat-common source of diverging-but-productive
-- subterms are local recursive helper functions (since we evaluate under
-- lambdas and inside let bindings).
--
-- We have made the design choice to use imprecise exceptions to be able to
-- write simple code which can report obvious problems without compromising
-- laziness. Another choice would be to not report errors (and letting these
-- subterms be stuck). A non-option would be to use ExceptT to report errors,
-- since that would destroy our critical laziness properties: the monadic
-- bind would force the top Left/Right constructor, and that in turn would
-- force the full evaluation of the relevant subterms. This design choice
-- unfortunately precludes returning a partial result if we detect we have
-- been evaluating for too long (it is possible to add a recursion depth
-- limit and bail out if we hit it (though then evaluating recursive lets
-- is less efficient), but we cannot easily then both report we timed out
-- and also rebuild the whole term. Finding a smarter design that would
-- allow this may be interesting future work.
--
-- The laziness property we rely on is that where possible we return the
-- root node of an AST without forcing the evaluation of subterms. This
-- makes it possible to pattern match on recursive calls to 'interp'' without
-- incurring unbounded work. Note that this means we cannot make our 'Expr'
-- type spine-strict without refactoring this interpreter!
interp' ::
  TypeDefMap ->
  (EnvTm, EnvTy) ->
  Dir ->
  Expr' () () () ->
  Expr' () () ()
interp' tydefs env@(envTm, envTy) dir = \case
  Hole m e -> Hole m $ interp' tydefs env Syn e
  e@EmptyHole{} -> e
  Ann _ e t -> ann dir (interp' tydefs env Chk e) (interpTy envTy t)
  -- NB: for primitives, we attempt to reduce them
  -- - with unevaluated arguments
  -- - then with the first argument evaluated
  -- - then with the first two arguments evaluated
  -- - etc
  -- and we do not assume the result of a primitive will be in normal form.
  -- This means that we support lazy primitives.
  -- Note that this left-to-right order of argument evaluation is the
  -- same as EvalFullStep
  e@App{}
    -- The @upsilon Chk@ is a bit of a lie -- we are not really in a checkable
    -- position. We simply wish to remove annotations around a primitive function's name
    | (upsilon Chk . interp' tydefs env Syn -> Var _ (GlobalVarRef name), args) <- unfoldApp e
    , Just r <- getFirst $ foldMap' (First . tryPrimFun' name envTm.prims) (evalPrefixes tydefs env args) ->
        interp' tydefs env dir r
  App _ f s -> case interp' tydefs env Syn f of
    Ann _ (Lam _ v t) (TFun _ src tgt) ->
      ann dir (interp' tydefs (extendTmsEnv [(Right v, Ann () (interp' tydefs env Chk s) src)] env) Chk t) tgt
    f' -> App () f' $ interp' tydefs env Chk s
  APP _ f s -> case interp' tydefs env Syn f of
    Ann _ (LAM _ a t) (TForall _ b _ ty) ->
      let s' = interpTy envTy s
       in ann
            dir
            (interp' tydefs (extendTyEnv a s' env) Chk t)
            (interpTy (extendTyEnv' b s' envTy) ty)
    f' -> APP () f' (interpTy envTy s)
  Con m c ts -> Con m c $ map (interp' tydefs env Chk) ts
  Lam _ v t -> let v' = freshLike v env in Lam () v' $ interp' tydefs (renameTmEnv v v' env) Chk t
  -- NB: we must evaluate under lambdas (given the rest of our interpretation strategy).
  -- Consider  @(λx.(λy.x) : A -> B -> A) s t@ we will
  --   interp' @λy.x@ in context where @x:->t@, and this is the only time we have @x@ in the context!
  LAM _ v t -> let v' = freshLike v env in LAM () v' $ interp' tydefs (extendTyEnv v (TVar () v') env) Chk t
  Var _ (LocalVarRef v) -> upsilon dir $ envTm.env ! Right v -- recall the environment is in normal form
  Var _ (GlobalVarRef v) -> upsilon dir $ envTm.env ! Left v -- recall the environment is in normal form
  Let _ v e b -> interp' tydefs (extendTmEnv (Right v) (interp' tydefs env Syn e) env) dir b
  LetType _ v t b -> interp' tydefs (extendTyEnv v (interpTy envTy t) env) dir b
  -- this interpretation af letrec can easily cause deadlocked or infinite-size programs
  -- and can't be detected but has advantage of being lazy and having sharing
  Letrec _ v e t b ->
    let e' = interp' tydefs env' Chk e
        env' =
          extendTmEnvWithFVs
            (Right v)
            (Ann () e' $ interpTy envTy t)
            (Set.delete (unLocalName v) $ freeVars (Ann () e t))
            env
     in interp' tydefs env' dir b
  -- In step interpreter, case which does not discriminate is lazy. Same here for consistency.
  Case _ _ [] (CaseFallback e) -> interp' tydefs env Chk e
  Case _ e brs fb ->
    -- recall @case C as : T A of ... ; C xs -> e ; ...@ steps to
    --  @let xs=as:(lettype p=A in S) in e@ for @data T p = C S@
    case interp' tydefs env Syn e of
      Ann _ (Con _ c as) (decomposeTAppCon -> Just (tycon, tyargs))
        | Just (CaseBranch _ xs t) <- find ((PatCon c ==) . caseBranchName) brs ->
            let envTy' = extendTysEnv' (tyParamEnvExt tycon tyargs) envTy
             in interp'
                  tydefs
                  ( extendTmsEnv
                      ( zip (Right . bindName <$> xs)
                          $ zipWith (\a argTy -> Ann () a $ interpTy envTy' argTy) as
                          $ ctorArgTys tycon c
                      )
                      env
                  )
                  Chk
                  t
        | CaseFallback t <- fb -> interp' tydefs env Chk t
        | otherwise -> throw $ NoBranch (Left c) $ caseBranchName <$> brs
      -- Note that primitives never have Expr arguments
      -- i.e. are always atomic
      Ann _ (PrimCon _ c) (TCon _ ((== primConName c) -> True))
        | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp' tydefs env Chk t
        | CaseFallback t <- fb -> interp' tydefs env Chk t
        | otherwise -> throw $ NoBranch (Right c) $ caseBranchName <$> brs
      -- literals (primitive constructors) are actually synthesisable, so may come
      -- without annotations
      PrimCon _ c
        | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp' tydefs env Chk t
        | CaseFallback t <- fb -> interp' tydefs env Chk t
        | otherwise -> throw $ NoBranch (Right c) $ caseBranchName <$> brs
      e' ->
        let f = \case
              CaseBranch pat binds rhs ->
                let (env', binds') =
                      mapAccumL
                        (\env'' (bindName -> b) -> let b' = freshLike b env'' in (renameTmEnv b b' env'', Bind () b'))
                        env
                        binds
                 in CaseBranch pat binds' $ interp' tydefs env' Chk rhs
         in Case () e' (f <$> brs) (mapFallback (interp' tydefs env Chk) fb)
  e@PrimCon{} -> e
  where
    tyParamEnvExt tcon args = case Map.lookup tcon tydefs of
      Just (TypeDefAST (ASTTypeDef ps _ _)) -> zipWith (\(p, _) a -> (p, a)) ps args
      _ -> throw $ UnknownTyCon tcon
    ctorArgTys tcon vcon = case Map.lookup tcon tydefs of
      Just (TypeDefAST (ASTTypeDef _ as _)) ->
        case find ((== vcon) . valConName) as of
          Just vc -> valConArgs vc
          Nothing -> throw $ UnknownValCon tcon vcon
      _ -> throw $ UnknownTyCon tcon
    ann d e t = upsilon d $ Ann () e t
    -- Strip redundant annotations (recursively): we don't need a
    -- top-level annotation if we are in a checkable context (at least if
    -- the type is "concrete", else it could act as type-changing cast).
    upsilon d = \case
      Ann _ e t ->
        -- We force t before emitting anything, so this is not very lazy.
        -- However we know evaluation of types must terminate!
        case (d, concreteTy t) of
          (Chk, True) -> upsilon d e
          _ -> Ann () e t
      e -> e
    renameTmEnv v v' = extendTmEnv (Right v) (Var () $ LocalVarRef v')

-- Generate a fresh name (for a given context) similar to an existing name.
freshLike :: LocalName k -> (EnvTm, EnvTy) -> LocalName k
freshLike v (envTm, envTy) =
  let avoid =
        envTm.vars
          <> envTy.vars
          <> Set.delete
            (unLocalName v)
            ( Set.fromList (fmap unLocalName $ rights $ Map.keys envTm.env)
                <> Set.map unLocalName (Map.keysSet envTy.env)
            )
   in freshen avoid v

freshLikeTy :: LocalName k -> EnvTy -> LocalName k
freshLikeTy v env = freshLike v (EnvTm mempty mempty mempty, env)

interpTy :: EnvTy -> Type' () () -> Type' () ()
interpTy env = \case
  t@TEmptyHole{} -> t
  THole _ t -> THole () $ interpTy env t
  t@TCon{} -> t
  TFun _ s t -> TFun () (interpTy env s) (interpTy env t)
  TVar _ v -> env.env !! v
  TApp _ s t -> TApp () (interpTy env s) (interpTy env t)
  TForall _ v k t ->
    let v' = freshLikeTy v env
     in TForall () v' k (interpTy (extendTyEnv' v (TVar () v') env) t)
  TLet _ v s t -> interpTy (extendTyEnv' v s env) t

extendTmEnv ::
  Either GVarName LVarName ->
  Expr' () () () ->
  (EnvTm, EnvTy) ->
  (EnvTm, EnvTy)
extendTmEnv k v = extendTmsEnv [(k, v)]

extendTmEnvWithFVs ::
  Either GVarName LVarName ->
  Expr' () () () ->
  Set Name ->
  (EnvTm, EnvTy) ->
  (EnvTm, EnvTy)
extendTmEnvWithFVs k v fvs = extendTmsEnvWithFVs [(k, v, fvs)]

extendTmsEnv ::
  [(Either GVarName LVarName, Expr' () () ())] ->
  (EnvTm, EnvTy) ->
  (EnvTm, EnvTy)
extendTmsEnv tms = extendTmsEnvWithFVs $ (\(v, t) -> (v, t, freeVars t)) <$> tms

extendTmsEnvWithFVs ::
  [(Either GVarName LVarName, Expr' () () (), Set Name)] ->
  (EnvTm, EnvTy) ->
  (EnvTm, EnvTy)
extendTmsEnvWithFVs tms (envTm, envTy) =
  ( EnvTm
      { vars = envTm.vars <> Set.unions ((\(_, _, fvs) -> fvs) <$> tms)
      , env = Map.fromList ((\(x, y, _) -> (x, y)) <$> tms) <> envTm.env
      , prims = envTm.prims
      }
  , envTy
  )

extendTyEnv' ::
  TyVarName ->
  Type' () () ->
  EnvTy ->
  EnvTy
extendTyEnv' k v = extendTysEnv' [(k, v)]

extendTysEnv' ::
  [(TyVarName, Type' () ())] ->
  EnvTy ->
  EnvTy
extendTysEnv' tys EnvTy{vars, env} =
  EnvTy
    { vars = vars <> Set.unions (map (Set.map unLocalName . freeVarsTy . snd) tys)
    , env = Map.fromList tys <> env
    }

extendTyEnv ::
  TyVarName ->
  Type' () () ->
  (EnvTm, EnvTy) ->
  (EnvTm, EnvTy)
extendTyEnv k v (envTm, envTy) = (envTm, extendTyEnv' k v envTy)

(!) :: Map.Map (Either GVarName LVarName) (Expr' () () ()) -> Either GVarName LVarName -> Expr' () () ()
m ! k = case Map.lookup k m of
  Just v -> v
  Nothing -> case k of
    Left v -> Var () $ GlobalVarRef v
    Right v -> Var () $ LocalVarRef v

(!!) :: Map.Map TyVarName (Type' () ()) -> TyVarName -> Type' () ()
m !! k = case Map.lookup k m of
  Just v -> v
  Nothing -> TVar () k

-- This is an ugly hack: we don't care about the IDs, but the underlying
-- primitives always return an ID-ful expression, or rather a monadic
-- computation to create an ID-ful expression, in an arbitrary
-- @MonadFresh ID@ monad. We will simply generate nonsense IDs and forget
-- them.
tryPrimFun' :: GVarName -> Map GVarName PrimDef -> [Expr' () () ()] -> Maybe (Expr' () () ())
tryPrimFun' p ps as = do
  d <- Map.lookup p ps
  case primFunDef d as of
    Left _ -> Nothing
    Right res -> Just $ forgetMetadata $ create' res

evalPrefixes ::
  TypeDefMap ->
  (EnvTm, EnvTy) ->
  [Expr' () () ()] ->
  [[Expr' () () ()]]
evalPrefixes tydefs env = map (uncurry (++)) . mapPrefixes (interp' tydefs env Chk)

mapPrefixes :: (a -> b) -> [a] -> [([b], [a])]
mapPrefixes f = \case
  [] -> [([], [])]
  a : as -> ([], a : as) : map (first (f a :)) (mapPrefixes f as)
