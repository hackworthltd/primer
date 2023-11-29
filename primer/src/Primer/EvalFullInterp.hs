{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.EvalFullInterp (
    interp
    , interp'
    , InterpError(..)
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
  , TyVarName, TmVarRef (LocalVarRef, GlobalVarRef), mapFallback, Type, unLocalName, LocalName, unsafeMkLocalName, Bind' (Bind),
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
import Primer.Core.Transform (decomposeTAppCon, unfoldApp)
import Primer.Core.Type (Type'(TEmptyHole, THole))
import Primer.Core.Utils (_freeVarsTy, concreteTy, freeVarsTy, freeVars, forgetMetadata, forgetTypeMetadata, generateIDs)
import Data.Set.Optics (setOf)
import Optics (_2,(%), to)
import qualified Data.Set as Set
import Primer.Name (Name, unName)
import Primer.Primitives (primConName, primFunDef)
import Control.Exception (throw)
import Primer.Eval.Prim (tryPrimFun)
import Primer.Primitives.PrimDef (PrimDef)
import Primer.Core.DSL.Meta (create')

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
  , prims :: Map GVarName PrimDef
  }
data EnvTy = EnvTy
  { vars :: Set Name
  , env :: Map.Map TyVarName (Type' () ())
  }

mkEnv :: [(Either GVarName LVarName, Expr' a b c)] -> Map GVarName PrimDef -> [(TyVarName,Type' a b)] -> (EnvTm, EnvTy)
mkEnv tms prims tys = extendTmsEnv (second forgetMetadata <$> tms) (EnvTm mempty mempty prims, extendTysEnv' (second forgetTypeMetadata <$> tys) $ EnvTy mempty mempty)

data InterpError = NoBranch
 deriving stock (Eq, Show)
 deriving anyclass Exception

interp :: TypeDefMap
        -> (EnvTm, EnvTy)
        -> Dir
        -> Expr' () () () -> IO (Either InterpError (Expr' () () ()))
interp tydefs env dir e =  try $ evaluate $ force $ interp' tydefs env dir e

-- we keep type annotations around ??
-- TODO: worry about name capture!
interp' :: TypeDefMap
        -> (EnvTm, EnvTy)
        -> Dir
        -> Expr' () () () -> Expr' () () ()
-- NB: this may throw imprecise exceptions of type InterpError
-- We use this mechanism so that we can have a "recursion depth" limit
-- but also lazily consume a result of a diverging-but-productive
-- recursive call. These are common in Primer programs,
-- since as well as writing @head $ cycle [0,1]@, we evaluate inside let bindings
-- and under lambdas, meaning any local recursive helper function will probably be interpreted as such a diverging-but-productive call.
--
-- Unfortunately this precludes returning a partial result if we hit a timeout.
--interp' (BRDLim ((<0) -> True)) tydefs env@(envTm,envTy) dir = \_ -> throw RecursionDepthExceeded -- TODO: proper error?; TODO: can I simply return the Expr here, and have a "this is how far I got"?
interp' tydefs env@(envTm,envTy) dir = \case
  Hole m e -> Hole m $ interp' tydefs env Syn e -- (TODO: maybe we should not eval inside holes? maybe should error out?)
  e@EmptyHole{} -> e
  Ann _ e t -> ann dir (interp' tydefs env Chk e) (interpTy envTy t)
  -- NB: for primitives, we attempt to reduce them
  -- - with unevaluated arguments
  -- - then with the first argument evaluated
  -- - then with the first two
  -- - etc
  -- and we do not assume the result of a primitive will be in normal form
  e@App{}
    -- The @upsilon Chk@ is a bit of a lie -- we are not really in a checkable
    -- position. We simply wish to remove annotations around a primitive function's name
    | (upsilon Chk -> Var _ (GlobalVarRef name), args) <- unfoldApp e
    , Just r <- getFirst $ foldMap' (First . tryPrimFun'' name envTm.prims) (evalPrefixes tydefs env args)
         -> interp' tydefs env dir r
  App _ f s -> case interp' tydefs env Syn f of
     Ann _ (Lam _ v t) (TFun _ src tgt) ->
       ann dir (interp' tydefs (extendTmsEnv [(Right v,Ann () (interp' tydefs env Chk s) src)] env) Chk t) tgt
     f' -> App () f' $ interp' tydefs env Chk s
  APP _ f s -> case interp' tydefs env Syn f of
     Ann _ (LAM _ a t) (TForall _ b _ ty) ->
       let s' = interpTy envTy s
       in ann dir (interp' tydefs (extendTyEnv a s' env) Chk t)
                 (interpTy (extendTyEnv' b s' envTy) ty)
     f' -> APP () f' (interpTy envTy s)
  Con m c ts -> Con m c $ map (interp' tydefs env Chk) ts
  Lam _ v t -> let v' = freshLike v env in Lam () v' $ interp' tydefs (renameTmEnv v v' env) Chk t
  -- TODO: we did not used to go under lambdas, but now do. Why did we not use to?
  --   (must do now as for @(λx.(λy.x) : A -> B -> A) s t@ we will
  --   interp' @λy.x@ in context where @x:->t@, and this is the only time we have @x@ in the context!!
  -- TODO: this may have shadowing problems! e.g. λx.((λy.λx.y)x)
  --   NBE avoids this by freshening when reifying
  --   can we avoid by just refusing to go under such shadow-y lambdas?
  --   (i.e. for a closed term, do we believe this will never happen?)
  LAM _ v t -> let v' = freshLike v env in LAM () v' $ interp' tydefs (extendTyEnv v (TVar () v') env) Chk t
  Var _ (LocalVarRef v) -> upsilon dir $ envTm.env ! Right v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  Var _ (GlobalVarRef v) -> upsilon dir $ envTm.env ! Left v -- THIS KINDA NEEDS ENVIRONMENT TO BE TO NF
  -- TODO: deal with primitives!
  Let _ v e b -> interp' tydefs (extendTmEnv (Right v) (interp' tydefs env Syn e) env) dir b
  LetType _ v t b -> interp' tydefs (extendTyEnv v (interpTy envTy t) env) dir b
  -- TODO: benchmark two implementations of letrec
  -- perhaps switch dependant on whether have recursion limit?
  -- lift recursive lets to recusive lets in the metalanguage (haskell)
  --
  -- this can easily cause deadlocked or infinite-size programs
  -- and can't be detected
  -- but has advantage of being lazy and having sharing
  Letrec _ v e t b
     -> let e' = interp' tydefs env' Chk e
            env' = extendTmEnvWithFVs (Right v) (Ann () e' $ interpTy envTy t)
                                                     (Set.delete (unLocalName v) $ freeVars (Ann () e t))
                                                     env
                       in interp' tydefs env' dir b
  -- In step interp'reter, case which does not discriminate is lazy. Same here for consistency
  Case _ _ [] (CaseFallback e) -> interp' tydefs env Chk e
  Case _ e brs fb -> -- this relies on @e@ computing to normal form lazily
-- case C as : T A of ... ; C xs -> e ; ...   ~>  let xs=as:(lettype p=A in S) in e for data T p = C S
   case interp' tydefs env Syn e of
     Ann _ (Con _ c as) (decomposeTAppCon -> Just (tycon, tyargs))
       | Just (CaseBranch _ xs t) <- find ((PatCon c ==) . caseBranchName) brs ->
         let envTy' = extendTysEnv' (tyParamEnvExt tycon tyargs) envTy
         in interp' tydefs (extendTmsEnv (zip (Right . bindName <$> xs)
                              $ zipWith (\a argTy -> Ann () a $ interpTy envTy' argTy) as $ ctorArgTys tycon c)
                              env) Chk
                         t
       | CaseFallback t <- fb -> interp' tydefs env Chk t
       | otherwise -> error $ "no such branch: " <> show c
     Ann _ (PrimCon _ c) (TCon _ ((== primConName c) -> True))
       | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp' tydefs env Chk t
       | CaseFallback t <- fb -> interp' tydefs env Chk t
       | otherwise -> error $ "no such branch: " <> show c
     -- literals (primitive constructors) are actually synthesisable, so may come
     -- without annotations
     PrimCon _ c
       | Just (CaseBranch _ [] t) <- find ((PatPrim c ==) . caseBranchName) brs -> interp' tydefs env Chk t
       | CaseFallback t <- fb -> interp' tydefs env Chk t
       | otherwise -> error $ "no such branch: " <> show c
     e' -> let f = \case
                 CaseBranch pat binds rhs ->
                   let (env',binds') = mapAccumL (\env'' (bindName -> b) -> let b' = freshLike b env'' in (renameTmEnv b b' env'', Bind () b'))
                                         env binds
                   in
                     CaseBranch pat binds' $ interp' tydefs env' Chk rhs
           in Case () e' (f <$> brs) (mapFallback (interp' tydefs env Chk) fb)
  e@PrimCon{} -> e
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
                    (Chk, True) -> upsilon d e
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
freshLikeTy v env = freshLike v (EnvTm mempty mempty mempty, env)

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
                 , env = Map.fromList ((\(x,y,_) -> (x,y)) <$> tms) <> envTm.env
                 , prims = envTm.prims
                 }
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

-- This is an ugly hack: we don't care about the IDs, but the underlying
-- primitives always return an ID-ful expression, and the wrapper tryPrimFun
-- takes an id-ful expression. We will simply generate nonsense IDs and
-- forget the returned ones.
tryPrimFun' :: Map GVarName PrimDef -> Expr' () () ()
    -> Maybe (Expr' () () ())
tryPrimFun' prims e =
    tryPrimFun prims (create' $ generateIDs e) >>= \(_,_,res) ->
      pure (forgetMetadata $ create' res)

-- This is an ugly hack: we don't care about the IDs, but the underlying
-- primitives always return an ID-ful expression, or rather a monadic
-- computation to create an ID-ful expression, in an arbitrary
-- @MonadFresh ID@ monad. We will simply generate nonsense IDs and forget
-- them.
tryPrimFun'' :: GVarName -> Map GVarName PrimDef -> [Expr' () () ()] -> Maybe (Expr' () () ())
tryPrimFun'' p ps as = do
    d <- Map.lookup p ps
    case primFunDef d as of
      Left _ -> Nothing
      Right res -> Just $ forgetMetadata $ create' res

evalPrefixes :: TypeDefMap
        -> (EnvTm, EnvTy)
  -> [Expr' () () ()] -> [[Expr' () () ()]]
evalPrefixes tydefs env = map (uncurry (++)) . mapPrefixes (interp' tydefs env Chk)

mapPrefixes :: (a -> b) -> [a] -> [([b],[a])]
mapPrefixes f = \case
 [] -> [([],[])]
 a:as -> ([],a:as) : map (first (f a :)) (mapPrefixes f as)
