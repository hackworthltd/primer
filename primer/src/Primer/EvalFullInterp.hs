{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.EvalFullInterp (
    InterpError,
    interp
    , BetaRecursionDepth(..)
    {-
  Dir (..),
  EvalFullError (..),
  --TerminationBound,
  evalFull,
  --evalFullStepCount,
  --EvalLog (..),
  -}
  , mkEnv
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
  , TyVarName, TmVarRef (LocalVarRef, GlobalVarRef), mapFallback, Type, unLocalName, LocalName, unsafeMkLocalName, Bind' (Bind), traverseFallback,
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
import Primer.Core.Utils (_freeVarsTy, concreteTy, freeVarsTy, freeVars, forgetMetadata, forgetTypeMetadata)
import Data.Set.Optics (setOf)
import Optics (_2,(%), to)
import qualified Data.Set as Set
import Primer.Name (Name, unName)
import Primer.Primitives (primConName)

-- A naive tree-walker / compile to closure (TODO: is this correct terminology?)
-- We reuse Haskell's runtime to do call-by-need
-- NB environment is map varName:->normal-form-computed-lazily
-- TODO: test @head $ repeat [True]@ work
--       also @(λx. case x of C y -> y + y) ((λx.C x) expensive-int-computation))@ works and only does the expensive computation once (how to test this??)
-- We don't compute under lambdas, but will compute after a beta (same with foralls)
--  this means that one can only trust the answer if it has no lambdas/foralls in!


-- Invariant: vars is the set of free vars in the values of the map
-- Thus the binders we need to rename before going under them
data EnvTm = EnvTm
  { vars :: Set Name
  , env :: Map.Map (Either GVarName LVarName) (Expr' () () ())
  }
data EnvTy = EnvTy
  { vars :: Set Name
  , env :: Map.Map TyVarName (Type' () ())
  }

mkEnv :: [(Either GVarName LVarName, Expr' a b c)] -> [(TyVarName,Type' a b)] -> (EnvTm, EnvTy)
mkEnv tms tys = extendTmsEnv (second forgetMetadata <$> tms) (EnvTm mempty mempty, extendTysEnv' (second forgetTypeMetadata <$> tys) $ EnvTy mempty mempty)

data BetaRecursionDepth
  = BRDNone
  | BRDLim !Int

betaRecursionDepthPred :: BetaRecursionDepth -> BetaRecursionDepth
betaRecursionDepthPred = \case
  BRDNone -> BRDNone
  BRDLim n -> BRDLim (pred n)

data InterpError = TimedOut
  deriving (Eq, Show)

-- we keep type annotations around ??
-- TODO: worry about name capture!
interp :: BetaRecursionDepth -> TypeDefMap
        -> (EnvTm, EnvTy)
        -> Dir
        -> Expr' () () () -> Either InterpError (Expr' () () ())
interp (BRDLim ((<0) -> True)) tydefs env@(envTm,envTy) dir = \_ -> Left TimedOut -- TODO: proper error?; TODO: can I simply return the Expr here, and have a "this is how far I got"?
interp brd tydefs env@(envTm,envTy) dir = \case
  Hole m e -> Hole m <$> interp brd tydefs env Syn e -- (TODO: maybe we should not eval inside holes? maybe should error out?)
  e@EmptyHole{} -> pure e
  Ann _ e t -> ann dir <$> interp brd tydefs env Chk e <*> pure (interpTy envTy t)
  App _ f s -> interp (betaRecursionDepthPred brd) tydefs env Syn f >>= \case
     Ann _ (Lam _ v t) (TFun _ src tgt) -> do
         s' <-interp (betaRecursionDepthPred brd) tydefs env Chk s
         ann dir <$> interp (betaRecursionDepthPred brd) tydefs (extendTmsEnv [(Right v,Ann () s' src)] env) Chk t <*> pure tgt
     f' -> App () f' <$> interp brd tydefs env Chk s
  APP _ f s -> interp (betaRecursionDepthPred brd) tydefs env Syn f >>= \case
     Ann _ (LAM _ a t) (TForall _ b _ ty) ->
       let s' = interpTy envTy s
       in ann dir <$> interp (betaRecursionDepthPred brd) tydefs (extendTyEnv a s' env) Chk t
                  <*> pure (interpTy (extendTyEnv' b s' envTy) ty)
     f' -> pure $ APP () f' (interpTy envTy s)
  Con m c ts -> Con m c <$> traverse (interp brd tydefs env Chk) ts
  Lam _ v t -> let v' = freshLike v env in Lam () v' <$> interp brd tydefs (renameTmEnv v v' env) Chk t
  -- TODO: we did not used to go under lambdas, but now do. Why did we not use to?
  --   (must do now as for @(λx.(λy.x) : A -> B -> A) s t@ we will
  --   interp @λy.x@ in context where @x:->t@, and this is the only time we have @x@ in the context!!
  -- TODO: this may have shadowing problems! e.g. λx.((λy.λx.y)x)
  --   NBE avoids this by freshening when reifying
  --   can we avoid by just refusing to go under such shadow-y lambdas?
  --   (i.e. for a closed term, do we believe this will never happen?)
  LAM _ v t -> let v' = freshLike v env in LAM () v' <$> interp brd tydefs (extendTyEnv v (TVar () v') env) Chk t
  Var _ (LocalVarRef v) -> pure $ upsilon dir $ envTm.env ! Right v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  Var _ (GlobalVarRef v) -> pure $ upsilon dir $ envTm.env ! Left v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  -- TODO: deal with primitives!
  Let _ v e b -> do
      e' <- interp brd tydefs env Syn e
      interp brd tydefs (extendTmEnv (Right v) e' env) dir b
  LetType _ v t b -> interp brd tydefs (extendTyEnv v (interpTy envTy t) env) dir b
  -- TODO: benchmark two implementations of letrec
  -- perhaps switch dependant on whether have recursion limit?
  {-
  -- lift recursive lets to recusive lets in the metalanguage (haskell)
  -- this can easily cause deadlocked or infinite-size programs
  -- and can't be detected
  -- but has advantage of being lazy and having sharing
  Letrec _ v e t b  -> let e' = interp brd tydefs env' Chk e
                           env' = extendTmEnvWithFVs (Right v) (Ann () e' $ interpTy envTy t)
                                                     (Set.delete (unLocalName v) $ freeVars (Ann () e t))
                                                     env
                       in interp brd tydefs env' dir b
                       -}
  -- iteratively unroll the let. this causes reduntant work, but at least we will notice infinite loops
  Letrec _ v e t b -> let e' n = do env'' <- env' $ betaRecursionDepthPred n
                                    interp n tydefs env'' Chk e
                          t' = interpTy envTy t
                          env' n = e' n <&> \e'n -> extendTmEnvWithFVs (Right v) (Ann () (e'n) t')
                                                     (Set.delete (unLocalName v) $ freeVars (Ann () e t))
                                                     env
                      in do env'' <- env' brd
                            interp brd tydefs env'' dir b
  -- In step interpreter, case which does not discriminate is lazy. Same here for consistency
  Case _ _ [] (CaseFallback e) -> interp brd tydefs env Chk e
  Case _ e brs fb -> -- this relies on @e@ computing to normal form lazily
-- case C as : T A of ... ; C xs -> e ; ...   ~>  let xs=as:(lettype p=A in S) in e for data T p = C S
   interp (betaRecursionDepthPred brd) tydefs env Syn e >>= \case
     Ann _ (Con _ c as) (decomposeTAppCon -> Just (tycon, tyargs))
       | Just (CaseBranch _ xs t) <- find ((PatCon c ==) . caseBranchName) brs ->
         let envTy' = extendTysEnv' (tyParamEnvExt tycon tyargs) envTy
         in interp (betaRecursionDepthPred brd) tydefs (extendTmsEnv (zip (Right . bindName <$> xs)
                              $ zipWith (\a argTy -> Ann () a $ interpTy envTy' argTy) as $ ctorArgTys tycon c)
                              env) Chk
                         t
       | CaseFallback t <- fb -> interp (betaRecursionDepthPred brd) tydefs env Chk t
       | otherwise -> error $ "no such branch: " <> show c
     Ann _ (PrimCon _ c) (TCon _ ((== primConName c) -> True))
       | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp (betaRecursionDepthPred brd) tydefs env Chk t
       | CaseFallback t <- fb -> interp (betaRecursionDepthPred brd) tydefs env Chk t
       | otherwise -> error $ "no such branch: " <> show c
     e' -> let f = \case
                 CaseBranch pat binds rhs ->
                   let (env',binds') = mapAccumL (\env'' (bindName -> b) -> let b' = freshLike b env'' in (renameTmEnv b b' env'', Bind () b'))
                                         env binds
                   in
                     CaseBranch pat binds' <$> interp brd tydefs env' Chk rhs
           in Case () e' <$> traverse f brs
                   <*> traverseFallback (interp brd tydefs env Chk) fb
  e@PrimCon{} -> pure e
 where
   -- todo DRY with Redex/viewCaseRedex (and DRY stuff above with other redex stuff??)
   tyParamEnvExt tcon args = let (TypeDefAST (ASTTypeDef ps _ _)) = tydefs Map.! tcon
                             in zipWith (\(p,_) a -> (p,a)) ps args
   ctorArgTys tcon vcon = let (TypeDefAST (ASTTypeDef _ as _)) = tydefs Map.! tcon
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
   renameTmEnv v v' = extendTmEnv (Right v) (Var () $ LocalVarRef v')
   --renameTmEnv v v' = renameTmsEnv [(v,v')]
   renameTmsEnv vvs = extendTmsEnv $ map (\(v, v') -> (Right v, Var () $ LocalVarRef v')) vvs

-- TODO: common up with something ??
freshLike :: LocalName k -> (EnvTm, EnvTy) -> LocalName k
freshLike v (envTm, envTy) =
  let avoid = envTm.vars <> envTy.vars <>
            Set.delete (unLocalName v) (Set.fromList (fmap unLocalName $ rights $ Map.keys envTm.env)
            <> Set.map unLocalName (Map.keysSet envTy.env))
  in unsafeHead $ filter (flip Set.notMember avoid . unLocalName) $
       v : [unsafeMkLocalName $ unName (unLocalName v) <> show i | i<-[0..]]

freshLikeTy :: LocalName k -> EnvTy -> LocalName k
freshLikeTy v env = freshLike v (EnvTm mempty mempty, env)

-- todo: doc what sense this is "normal" -- not under binders...
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

-- CONFUSED: how do I do to WHNF so can terminate when do `fst (3, letrec x = x in x)`
-- BUT SHARE NORMAL FORM when duplicate?
-- tentative ANSWER: do to normal form lazily/streamingly/productively

extendTmEnv :: Either GVarName LVarName
            -> Expr' () () ()
            -> (EnvTm, EnvTy)
            -> (EnvTm, EnvTy)
extendTmEnv k v = extendTmsEnv [(k,v)]

extendTmEnvWithFVs :: Either GVarName LVarName
            -> Expr' () () ()
            -> Set Name
            -> (EnvTm, EnvTy)
            -> (EnvTm, EnvTy)
extendTmEnvWithFVs k v fvs = extendTmsEnvWithFVs [(k,v,fvs)]

extendTmsEnv :: [(Either GVarName LVarName ,Expr' () () ())]
            -> (EnvTm, EnvTy)
            -> (EnvTm, EnvTy)
extendTmsEnv tms = extendTmsEnvWithFVs $ (\(v,t) -> (v,t,freeVars t)) <$> tms

extendTmsEnvWithFVs :: [(Either GVarName LVarName ,Expr' () () (), Set Name)]
            -> (EnvTm, EnvTy)
            -> (EnvTm, EnvTy)
extendTmsEnvWithFVs tms (envTm, envTy) = (EnvTm {vars = envTm.vars <> Set.unions ((\(_,_,fvs) -> fvs) <$> tms)
                 , env = Map.fromList ((\(x,y,_) -> (x,y)) <$> tms) <> envTm.env}
    , envTy)

extendTyEnv' :: TyVarName
            -> Type' () ()
            -> EnvTy
            -> EnvTy
extendTyEnv' k v = extendTysEnv' [(k,v)]

extendTysEnv' :: [(TyVarName, Type' () ())]
            -> EnvTy
            -> EnvTy
extendTysEnv' tys EnvTy{vars,env} = EnvTy
   {vars = vars <> Set.unions (map (Set.map unLocalName . freeVarsTy . snd) tys)
   , env = Map.fromList tys <> env}

extendTyEnv :: TyVarName
            -> Type' () ()
            -> (EnvTm, EnvTy)
            -> (EnvTm, EnvTy)
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
