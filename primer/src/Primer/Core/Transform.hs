module Primer.Core.Transform (
  renameVar,
  renameLocalVar,
  renameTyVar,
  renameTyVarExpr,
  unfoldApp,
  foldApp,
  unfoldAPP,
  foldAPP,
  decomposeAppCon,
  mkAppCon,
  unfoldTApp,
  decomposeTAppCon,
  foldTApp,
  mkTAppCon,
  unfoldFun,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty qualified as NE
import Optics (Field2 (_2), getting, noneOf, notElemOf, to, traverseOf, (%))
import Primer.Core (
  CaseBranch' (..),
  Expr,
  Expr' (..),
  ID,
  LVarName,
  LocalName (unLocalName),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (..),
  ValConName,
  bindName,
  typesInExpr,
 )
import Primer.Core.DSL (meta)
import Primer.Core.Meta (TyConName)
import Primer.Core.Utils (_freeVars, _freeVarsTy)

-- AST transformations.
-- This module contains global transformations on expressions and types, in
-- contrast to the focused, local transformations provided by the zipper.

-- | Attempt to replace all free ocurrences of @x@ in @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameVar :: (Data a, Data b) => TmVarRef -> TmVarRef -> Expr' a b -> Maybe (Expr' a b)
renameVar x y expr = case expr of
  Lam _ v _
    | sameVarRef v x -> whenNotFreeIn y expr
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  LAM _ v _
    -- NB: local term and type variables are in the same namespace
    | sameVarRef v x -> whenNotFreeIn y expr
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  Let m v e1 e2
    -- the binding only scopes over e2
    | sameVarRef v x -> Let m v <$> renameVar x y e1 <*> whenNotFreeIn y e2
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  LetType _ v _ _
    -- the binding only scopes over _e, but due to assuming well-scoped-ness,
    -- we don't need to rename inside ty. However, we need to check y is
    -- not free in both the type and term, as type and term variables live
    -- in the same namespace, so can capture each other.
    | sameVarRef v x -> whenNotFreeIn y expr
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  Letrec _ v _ _ _
    -- the binding scopes over both expressions, and we need not rename inside types
    -- however, we need to check y is not free in the type (since type and term
    -- variables live in the same namespace).
    | sameVarRef v x -> whenNotFreeIn y expr
    | sameVarRef v y -> Nothing
    | otherwise -> substAllChildren
  Case m scrut branches -> Case m <$> renameVar x y scrut <*> mapM renameBranch branches
    where
      renameBranch b@(CaseBranch con termargs rhs)
        | any (`sameVarRef` y) $ bindingNames b = Nothing
        | any (`sameVarRef` x) $ bindingNames b = guard (notFreeIn y rhs) >> pure b
        | otherwise = CaseBranch con termargs <$> renameVar x y rhs
      bindingNames (CaseBranch _ bs _) = map bindName bs
  Var m v
    | v == x -> pure $ Var m y
    | v == y -> Nothing
    | otherwise -> pure expr
  Hole{} -> substAllChildren
  EmptyHole{} -> substAllChildren
  Ann{} -> substAllChildren
  App{} -> substAllChildren
  APP{} -> substAllChildren
  Con{} -> substAllChildren
  PrimCon{} -> substAllChildren
  -- We assume the term is well-scoped, so do not have any references to the
  -- term vars x,y inside any type child (e.g. annotation), so no need to
  -- consider renaming inside them. However, but we do need to worry about
  -- references to the type var y (term and type variables are in the same
  -- namespace) -- we do not want to capture such a y.
  where
    substAllChildren = do
      guard $ noneOf (typesInExpr % getting _freeVarsTy % _2) (`sameVarRef` y) expr
      descendM (renameVar x y) expr

whenNotFreeIn :: TmVarRef -> Expr' a b -> Maybe (Expr' a b)
whenNotFreeIn x e = do
  guard $ notFreeIn x e
  pure e

notFreeIn :: TmVarRef -> Expr' a b -> Bool
notFreeIn x = noneOf (_freeVars % to (bimap snd snd)) (either (`sameVarRef` x) (`sameVarRef` x))

whenNotFreeInTy :: TyVarName -> Type' b -> Maybe (Type' b)
whenNotFreeInTy x ty = do
  guard $ notFreeInTy x ty
  pure ty

notFreeInTy :: TyVarName -> Type' b -> Bool
notFreeInTy = notElemOf (getting _freeVarsTy % _2)

sameVarRef :: LocalName k -> TmVarRef -> Bool
sameVarRef v (LocalVarRef v') = sameVar v v'
sameVarRef _ (GlobalVarRef _) = False

sameVar :: LocalName k -> LocalName l -> Bool
sameVar v v' = unLocalName v == unLocalName v'

-- | As 'renameVar', but specialised to local variables
renameLocalVar :: (Data a, Data b) => LVarName -> LVarName -> Expr' a b -> Maybe (Expr' a b)
renameLocalVar x y = renameVar (LocalVarRef x) (LocalVarRef y)

-- | Attempt to replace all free ocurrences of @x@ in @t@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVar :: Data a => TyVarName -> TyVarName -> Type' a -> Maybe (Type' a)
-- We cannot use substTy to implement renaming, as that restricts to b~(), so as to not
-- duplicate metadata. But for renaming, we know that will not happen.
renameTyVar x y ty = case ty of
  TForall _ v _ _
    | v == x -> whenNotFreeInTy y ty
    | v == y -> Nothing
    | otherwise -> substAllChildren
  TVar m v
    | v == x -> pure $ TVar m y
    | v == y -> Nothing
    | otherwise -> substAllChildren
  TLet m v t b
    | v == x -> TLet m v <$> renameTyVar x y t <*> whenNotFreeInTy y b
    | v == y -> Nothing
    | otherwise -> substAllChildren
  TEmptyHole{} -> substAllChildren
  THole{} -> substAllChildren
  TCon{} -> substAllChildren
  TFun{} -> substAllChildren
  TApp{} -> substAllChildren
  where
    substAllChildren = descendM (renameTyVar x y) ty

-- | Attempt to replace all free ocurrences of @x@ in some type inside @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVarExpr :: forall a b. (Data a, Data b) => TyVarName -> TyVarName -> Expr' a b -> Maybe (Expr' a b)
renameTyVarExpr x y expr = case expr of
  Lam _ v _
    | sameVar v x -> pure expr
    | sameVar v y -> Nothing
    | otherwise -> substAllChildren
  LAM _ v _
    | v == x -> pure expr
    | v == y -> Nothing
    | otherwise -> substAllChildren
  Let m v e1 e2
    -- the binding only scopes over e2
    | sameVar v x -> Let m v <$> renameTyVarExpr x y e1 <*> pure e2
    | sameVar v y -> Nothing
    | otherwise -> substAllChildren
  LetType m v ty e
    -- the binding only scopes over e
    | sameVar v x -> LetType m v <$> renameTyVar x y ty <*> pure e
    | sameVar v y -> Nothing
    | otherwise -> substAllChildren
  Letrec m v e1 ty e2
    -- the binding only scopes over e1 and e2
    | sameVar v x -> Letrec m v e1 <$> renameTyVar x y ty <*> pure e2
    | sameVar v y -> Nothing
    | otherwise -> substAllChildren
  Case m scrut branches -> Case m <$> renameTyVarExpr x y scrut <*> mapM renameBranch branches
    where
      renameBranch b@(CaseBranch con termargs rhs)
        | any (sameVar x) $ bindingNames b = pure b
        | any (sameVar y) $ bindingNames b = Nothing
        | otherwise = CaseBranch con termargs <$> renameTyVarExpr x y rhs
      bindingNames (CaseBranch _ bs _) = map bindName bs
  Var{} -> substAllChildren
  Hole{} -> substAllChildren
  EmptyHole{} -> substAllChildren
  Ann{} -> substAllChildren
  App{} -> substAllChildren
  APP{} -> substAllChildren
  Con{} -> substAllChildren
  PrimCon{} -> substAllChildren
  where
    substAllChildren = descendM (renameTyVarExpr x y) =<< descendTypeM (renameTyVar x y) expr
    -- NB: cannot use descendBiM here: I want only immediate type
    -- children, but descendBiM does "top-most" type children: i.e. (λx.x:t)
    -- will target t even though it is two layers deep!
    descendTypeM = traverseOf typesInExpr

-- | Unfold a nested term application into the application head and a list of arguments.
unfoldApp :: Expr' a b -> (Expr' a b, [Expr' a b])
unfoldApp = second reverse . go
  where
    go (App _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Fold an application head and a list of arguments into a single expression.
foldApp :: (Foldable t, MonadFresh ID m) => Expr -> t Expr -> m Expr
foldApp = foldlM $ \a b -> do
  m <- meta
  pure $ App m a b

-- | Unfold a nested term-level type application into the application head and a list of arguments.
unfoldAPP :: Expr' a b -> (Expr' a b, [Type' b])
unfoldAPP = second reverse . go
  where
    go (APP _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Fold an application head and a list of type arguments into a single expression.
foldAPP :: (Foldable t, MonadFresh ID m) => Expr -> t Type -> m Expr
foldAPP = foldlM $ \a b -> do
  m <- meta
  pure $ APP m a b

-- | Decompose @C @A @B x y z@ to @(C,[A,B],[x,y,z])@
decomposeAppCon :: Expr' a b -> Maybe (ValConName, a, [Type' b], [Expr' a b])
decomposeAppCon =
  unfoldApp <&> first unfoldAPP <&> \case
    -- This is suspicious (we reorder types and terms), but
    -- (a) for well-typed terms, either tms0 or tys will be empty (since constructors only have top-level foralls)
    -- (b) the situation that constructors can be on the left of an app or aPP node is temporary
    --     and shortly decomposeAppCon will become a trivial match on the 'Con' constructor.
    ((Con m c tys0 tms0, tys), tms) -> Just (c, m, tys0 ++ tys, tms0 ++ tms)
    _ -> Nothing

-- | Apply a constructor to a spine of types and a spine of terms
mkAppCon :: (Foldable t1, Foldable t2, MonadFresh ID m) => ValConName -> t1 Type -> t2 Expr -> m Expr
mkAppCon c tys tms = do
  c' <- (`Con` c) <$> meta
  pure $ c' (toList tys) (toList tms)

-- | Unfold a nested type-level application into the application head and a list of arguments.
unfoldTApp :: Type' a -> (Type' a, [Type' a])
unfoldTApp = second reverse . go
  where
    go (TApp _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Fold an type-level application head and a list of arguments into a single expression.
foldTApp' :: (Monad m, Foldable t) => m a -> Type' a -> t (Type' a) -> m (Type' a)
foldTApp' m = foldlM $ \a b -> (\m' -> TApp m' a b) <$> m

-- | Fold an type-level application head and a list of arguments into a single expression.
foldTApp :: (MonadFresh ID m, Foldable t) => Type -> t Type -> m Type
foldTApp = foldTApp' meta

-- | @mkTAppCon C [X,Y,Z] = C X Y Z@
mkTAppCon :: TyConName -> [Type' ()] -> Type' ()
mkTAppCon c = runIdentity . foldTApp' (pure ()) (TCon () c)

-- | Decompose @C X Y Z@ to @(C,[X,Y,Z])@
decomposeTAppCon :: Type' a -> Maybe (TyConName, [Type' a])
decomposeTAppCon =
  unfoldTApp <&> \case
    (TCon _ con, args) -> Just (con, args)
    _ -> Nothing

-- | Split a function type into an array of argument types and the result type.
-- Takes two arguments: the lhs and rhs of the topmost function node.
unfoldFun :: Type' a -> Type' a -> (NonEmpty (Type' a), Type' a)
unfoldFun a (TFun _ b c) =
  let (argTypes, resultType) = unfoldFun b c
   in (NE.cons a argTypes, resultType)
unfoldFun a t = (pure a, t)
