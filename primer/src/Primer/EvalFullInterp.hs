{-# LANGUAGE ViewPatterns #-}
module Primer.EvalFullInterp (
    interp
    {-
  Dir (..),
  EvalFullError (..),
  --TerminationBound,
  evalFull,
  --evalFullStepCount,
  --EvalLog (..),
  -}
) where

-- TODO: document this!
-- don't bother with types?
-- only work with things that reduce to hereditarily canonical inhabitants of ADTs/primitives
-- AIMS
-- - fairly simple
-- - maximally terminating
-- - fairly fast
-- - agrees with iterating steps, on the supported subset.
-- QUESTIONS
-- - will simple tree walker be fast enough?
-- - can I reuse Redex module?
-- - Should I do NBE?
-- - Can I interp into haskell (poor-mans NBE / compile to closures)??

{-
Idea:
Call-by-need
do eval-with-an-environment (closed in the sense of "no free vars", open in the sense of "haven't just substituted"), so can share computation
simple tree walker to WHNF, going recursively root-down:
  interp (Con C ts) = Con C (map interp ts)
  interp (Lam x t) = Lam x t
  interp <redex> = <reduct>
  interp (f t) = interp (interp f) t

-}

import Foreword

import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Data (Data)
import Data.Map.Lazy qualified as Map
import Numeric.Natural (Natural)
import Primer.Core (
  Expr, Expr'(..), CaseFallback' (CaseFallback), caseBranchName, CaseBranch' (CaseBranch), Pattern (PatCon, PatPrim), bindName, GVarName, LVarName, Type' (..)
  , TyVarName, TmVarRef (LocalVarRef, GlobalVarRef), mapFallback,
 )
import Primer.Def (
  DefMap,
 )
import Primer.Eval.NormalOrder (NormalOrderOptions, RedexWithContext (RExpr, RType), findRedex)
import Primer.Eval.Redex (
  Dir (Chk, Syn),
  EvalLog (..),
  MonadEval,
  RunRedexOptions,
  ViewRedexOptions,
  runRedex,
  runRedexTy,
 )
import Primer.TypeDef (
  TypeDefMap, TypeDef (TypeDefAST), ASTTypeDef (ASTTypeDef), ValCon (valConArgs, valConName),
 )
import Primer.Zipper (
  replace,
  unfocusExpr,
  unfocusType,
 )
import Protolude.Error (error)
import Primer.Core.Transform (decomposeTAppCon)
import Primer.Core.Type (Type'(TEmptyHole, THole))
import Primer.Core.Utils (concreteTy)

-- A naive tree-walker / compile to closure (TODO: is this correct terminology?)
-- We reuse Haskell's runtime to do call-by-need
-- NB environment is map varName:->normal-form-computed-lazily
-- TODO: test @head $ repeat [True]@ work
--       also @(λx. case x of C y -> y + y) ((λx.C x) expensive-int-computation))@ works and only does the expensive computation once (how to test this??)
-- We don't compute under lambdas, but will compute after a beta (same with foralls)
--  this means that one can only trust the answer if it has no lambdas/foralls in!

-- we keep type annotations around ??
-- TODO: worry about name capture!
interp :: TypeDefMap
        -> (Map.Map (Either GVarName LVarName) (Expr' () () ()), Map.Map TyVarName (Type' () ()))
        -> Dir
        -> Expr' () () () -> Expr' () () ()
interp tydefs env@(envTm,envTy) dir = \case
  Hole m e -> Hole m $ interp tydefs env Syn e -- (TODO: maybe we should not eval inside holes? maybe should error out?)
  e@EmptyHole{} -> e
  Ann _ e t -> ann dir (interp tydefs env Chk e) (interpTy envTy t)
  App _ f s -> case interp tydefs env Syn f of
     Ann _ (Lam _ v t) (TFun _ src tgt) ->
       ann dir (interp tydefs (extendTmsEnv [(Right v,Ann () (interp tydefs env Chk s) src)] env) Chk t) tgt
     f' -> App () f' (interp tydefs env Chk s)
  APP _ f s -> case interp tydefs env Syn f of
     Ann _ (LAM _ a t) (TForall _ b _ ty) ->
       let s' = interpTy envTy s
       in ann dir (interp tydefs (extendTyEnv a s' env) Chk t)
                 (interpTy (extendTyEnv' b s' envTy) ty)
     f' -> APP () f' (interpTy envTy s)
  Con m c ts -> Con m c $ map (interp tydefs env Chk) ts
  Lam _ v t -> Lam () v $ interp tydefs (extendTmsIdEnv [v] env) Chk t
  -- TODO: we did not used to go under lambdas, but now do. Why did we not use to?
  --   (must do now as for @(λx.(λy.x) : A -> B -> A) s t@ we will
  --   interp @λy.x@ in context where @x:->t@, and this is the only time we have @x@ in the context!!
  -- TODO: this may have shadowing problems! e.g. λx.((λy.λx.y)x)
  --   NBE avoids this by freshening when reifying
  --   can we avoid by just refusing to go under such shadow-y lambdas?
  --   (i.e. for a closed term, do we believe this will never happen?)
  LAM _ v t -> LAM () v $ interp tydefs (extendTyEnv v (TVar () v) env) Chk t
  Var _ (LocalVarRef v) -> upsilon dir $ envTm ! Right v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  Var _ (GlobalVarRef v) -> upsilon dir $ envTm ! Left v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  -- TODO: deal with primitives!
  Let _ v e b -> interp tydefs (extendTmEnv (Right v) (interp tydefs env Syn e) env) dir b
  LetType _ v t b -> interp tydefs (extendTyEnv v (interpTy envTy t) env) dir b
  Letrec _ v e t b  -> let e' = interp tydefs env' Chk e
                           env' = extendTmEnv (Right v) (Ann () e' $ interpTy envTy t) env
                       in interp tydefs env' dir b
  -- In step interpreter, case which does not discriminate is lazy. Same here for consistency
  Case _ _ [] (CaseFallback e) -> interp tydefs env Chk e
  Case _ e brs fb -> -- this relies on @e@ computing to normal form lazily
-- case C as : T A of ... ; C xs -> e ; ...   ~>  let xs=as:(lettype p=A in S) in e for data T p = C S
   case interp tydefs env Syn e of
     Ann _ (Con _ c as) (decomposeTAppCon -> Just (tycon, tyargs))
       | Just (CaseBranch _ xs t) <- find ((PatCon c ==) . caseBranchName) brs ->
         let envTy' = extendTysEnv' (tyParamEnvExt tycon tyargs) envTy
         in interp tydefs (extendTmsEnv (zip (Right . bindName <$> xs)
                              $ zipWith (\a argTy -> Ann () a $ interpTy envTy' argTy) as $ ctorArgTys tycon c)
                              env) Chk
                         t
       | CaseFallback t <- fb -> interp tydefs env Chk t
       | otherwise -> error "no such branch"
     Ann _ (PrimCon _ c) ty
       | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp tydefs env Chk t
       | CaseFallback t <- fb -> interp tydefs env Chk t
       | otherwise -> error "no such branch"
     e' -> let f = \case
                 CaseBranch pat binds rhs -> CaseBranch pat binds $ interp tydefs (extendTmsIdEnv (bindName <$> binds) env) Chk rhs
           in Case () e' (f <$> brs) (mapFallback (interp tydefs env Chk) fb)
  e@PrimCon{} -> e
 where
   -- todo DRY with Redex/viewCaseRedex (and DRY stuff above with other redex stuff??)
   tyParamEnvExt tcon args = let (TypeDefAST (ASTTypeDef ps _ _)) = tydefs ! tcon
                             in zipWith (\(p,_) a -> (p,a)) ps args
   ctorArgTys tcon vcon = let (TypeDefAST (ASTTypeDef _ as _)) = tydefs ! tcon
                              Just vc = find ((== vcon) . valConName) as
                          in valConArgs vc
   ann d e t = upsilon d $ Ann () e t
   upsilon d = \case
     Ann _ e t ->
               -- We force t before emitting anything, so this is not very lazy.
               -- However we know evaluation of types must terminate!
               case (d, concreteTy t) of
                    (Chk, True) -> e
                    _ -> Ann () e t
     e -> e
   extendTmsIdEnv vs  = extendTmsEnv ((\v -> (Right v, Var () $ LocalVarRef v)) <$> vs)
-- todo: doc what sense this is "normal" -- not under binders...
interpTy :: Map.Map TyVarName (Type' () ()) -> Type' () () -> Type' () ()
interpTy env = \case
  t@TEmptyHole{} -> t
  THole _ t -> THole () $ interpTy env t
  t@TCon{} -> t
  TFun _ s t -> TFun () (interpTy env s) (interpTy env t)
  TVar _ v -> env ! v
  TApp _ s t -> TApp () (interpTy env s) (interpTy env t)
  TForall _ v k t -> TForall () v k (interpTy (extendTyEnv' v (TVar () v) env) t)
  TLet _ v s t -> interpTy (extendTyEnv' v s env) t

-- CONFUSED: how do I do to WHNF so can terminate when do `fst (3, letrec x = x in x)`
-- BUT SHARE NORMAL FORM when duplicate?
-- tentative ANSWER: do to normal form lazily/streamingly/productively

extendTmEnv :: Either GVarName LVarName
            -> Expr' a b c
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName v)
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName v)
extendTmEnv k v = extendTmsEnv [(k,v)]

extendTmsEnv :: [(Either GVarName LVarName ,Expr' a b c)]
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName v)
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName v)
extendTmsEnv tms (envTm, envTy) = (Map.fromList tms <> envTm, envTy)

extendTyEnv' :: TyVarName
            -> Type' b c
            -> Map.Map TyVarName (Type' b c)
            -> Map.Map TyVarName (Type' b c)
extendTyEnv' k v = extendTysEnv' [(k,v)]

extendTysEnv' :: [(TyVarName, Type' b c)]
            -> Map.Map TyVarName (Type' b c)
            -> Map.Map TyVarName (Type' b c)
extendTysEnv' tms env = Map.fromList tms <> env

extendTyEnv :: TyVarName
            -> Type' b c
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName (Type' b c))
            -> (Map.Map (Either GVarName LVarName) (Expr' a b c), Map.Map TyVarName (Type' b c))
extendTyEnv k v (envTm, envTy) = (envTm, extendTyEnv' k v envTy)

(!) :: (Ord k, Show k, HasCallStack) => Map k v -> k -> v
m ! k = case Map.lookup k m of
  Just v -> v
  Nothing -> error $ "the key " <> show k <> " was not in the map"
