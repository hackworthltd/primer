module Primer.Core.Transform (
  renameVar,
  renameLocalVar,
  renameTyVar,
  renameTyVarExpr,
  unfoldApp,
  unfoldAPP,
  unfoldTApp,
  decomposeTAppCon,
  foldTApp,
  mkTAppCon,
  unfoldFun,
) where

import Foreword

import Data.Data (Data)
import Data.List.NonEmpty qualified as NE
import Optics (Field2 (_2), getting, noneOf, notElemOf, to, (%))
import Primer.Core (
  Expr' (..),
  LVarName,
  LocalName (LocalName, unLocalName),
  LocalNameKind (ATmVar),
  SomeBind',
  TmVarRef (..),
  TyVarName,
  Type' (..),
  bindName,
  scopedChildren,
  scopedChildrenTy,
 )
import Primer.Core.Meta (TyConName)
import Primer.Core.Utils (_freeVars, _freeVarsTy)

-- AST transformations.
-- This module contains global transformations on expressions and types, in
-- contrast to the focused, local transformations provided by the zipper.

-- | Attempt to replace all free ocurrences of @x@ in @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameVar :: (Data a, Data b, Data c) => TmVarRef -> TmVarRef -> Expr' a b c -> Maybe (Expr' a b c)
renameVar x y expr = case expr of
  Var m v
    | v == x -> pure $ Var m y
    | v == y -> Nothing
    | otherwise -> pure expr
  _ ->
    scopedChildren
      -- NB: local term and type variables are in the same namespace,
      -- so a binding of either sort can shadow @x@ or capture @y@.
      ( \bs e ->
          renameUnder
            (any (`sameVarRef` x) $ bindingNames bs)
            (any (`sameVarRef` y) $ bindingNames bs)
            (whenNotFreeIn y e)
            (renameVar x y e)
      )
      -- We assume the term is well-scoped, so does not have any references to the
      -- term vars x,y inside any type (e.g. annotation), so there is no need to
      -- consider renaming inside them. However, we do need to worry about
      -- references to the type var y (term and type variables are in the same
      -- namespace): we do not want to capture such a y.
      (\_ t -> t <$ guard (noneOf (getting _freeVarsTy % _2) (`sameVarRef` y) t))
      expr

whenNotFreeIn :: TmVarRef -> Expr' a b c -> Maybe (Expr' a b c)
whenNotFreeIn x e = do
  guard $ notFreeIn x e
  pure e

notFreeIn :: TmVarRef -> Expr' a b c -> Bool
notFreeIn x = noneOf (_freeVars % to (bimap snd snd)) (either (`sameVarRef` x) (`sameVarRef` x))

whenNotFreeInTy :: TyVarName -> Type' b c -> Maybe (Type' b c)
whenNotFreeInTy x ty = do
  guard $ notFreeInTy x ty
  pure ty

notFreeInTy :: TyVarName -> Type' b c -> Bool
notFreeInTy = notElemOf (getting _freeVarsTy % _2)

sameVarRef :: LocalName k -> TmVarRef -> Bool
sameVarRef v (LocalVarRef v') = sameVar v v'
sameVarRef _ (GlobalVarRef _) = False

sameVar :: LocalName k -> LocalName l -> Bool
sameVar v v' = unLocalName v == unLocalName v'

-- | As 'renameVar', but specialised to local variables
renameLocalVar :: (Data a, Data b, Data c) => LVarName -> LVarName -> Expr' a b c -> Maybe (Expr' a b c)
renameLocalVar x y = renameVar (LocalVarRef x) (LocalVarRef y)

-- | Attempt to replace all free ocurrences of @x@ in @t@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVar :: (Data a, Data b) => TyVarName -> TyVarName -> Type' a b -> Maybe (Type' a b)
-- We cannot use substTy to implement renaming, as that restricts to b~(), so as to not
-- duplicate metadata. But for renaming, we know that will not happen.
renameTyVar x y ty = case ty of
  TVar m v
    | v == x -> pure $ TVar m y
    | v == y -> Nothing
    | otherwise -> pure ty
  _ ->
    scopedChildrenTy
      (\bs t -> renameUnder (x `elem` (bindName <$> bs)) (y `elem` (bindName <$> bs)) (whenNotFreeInTy y t) (renameTyVar x y t))
      ty

-- | Attempt to replace all free ocurrences of @x@ in some type inside @e@ with @y@
-- Returns 'Nothing' if replacement could result in variable capture.
-- See the tests for explanation and examples.
renameTyVarExpr :: forall a b c. (Data a, Data b, Data c) => TyVarName -> TyVarName -> Expr' a b c -> Maybe (Expr' a b c)
renameTyVarExpr x y expr = case expr of
  -- Since term and type variables are in the same namespace, we need
  -- to worry about references to the term var y since we do not want
  -- to capture such a y.
  Var _ v
    | sameVarRef y v -> Nothing
    | otherwise -> pure expr
  _ ->
    scopedChildren
      (\bs e -> renameUnder (shadows bs) (captures bs) (whenNotFreeIn (LocalVarRef $ LocalName $ unLocalName y) e) (renameTyVarExpr x y e))
      (\bs t -> renameUnder (shadows bs) (captures bs) (whenNotFreeInTy y t) (renameTyVar x y t))
      expr
  where
    shadows = any (sameVar x) . bindingNames
    captures = any (sameVar y) . bindingNames

-- | How to rename within a child of a node, given whether the bindings introduced by that node which scope over it
-- shadow the variable being renamed, and whether they would capture the new name.
-- If the variable is shadowed, there is nothing to rename within the child, but we must still check that the new name
-- does not occur free in it, since it would then be captured by the binding we are renaming.
-- Otherwise, if a binding has the new name, renaming within the child would result in capture.
renameUnder :: Bool -> Bool -> Maybe a -> Maybe a -> Maybe a
renameUnder shadowed capturing whenShadowed renamed
  | shadowed = whenShadowed
  | capturing = Nothing
  | otherwise = renamed

-- | The names of some bindings, of either term or type variables.
bindingNames :: [SomeBind' a b] -> [LocalName 'ATmVar]
bindingNames = map $ LocalName . either (unLocalName . bindName) (unLocalName . bindName)

-- | Unfold a nested term application into the application head and a list of arguments.
unfoldApp :: Expr' a b c -> (Expr' a b c, [Expr' a b c])
unfoldApp = second reverse . go
  where
    go (App _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Unfold a nested term-level type application into the application head and a list of arguments.
unfoldAPP :: Expr' a b c -> (Expr' a b c, [Type' b c])
unfoldAPP = second reverse . go
  where
    go (APP _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Unfold a nested type-level application into the application head and a list of arguments.
unfoldTApp :: Type' a b -> (Type' a b, [Type' a b])
unfoldTApp = second reverse . go
  where
    go (TApp _ f x) = let (g, args) = go f in (g, x : args)
    go e = (e, [])

-- | Fold an type-level application head and a list of arguments into a single expression.
foldTApp :: (Monad m, Foldable t) => m a -> Type' a b -> t (Type' a b) -> m (Type' a b)
foldTApp m = foldlM $ \a b -> (\m' -> TApp m' a b) <$> m

-- | @mkTAppCon C [X,Y,Z] = C X Y Z@
mkTAppCon :: TyConName -> [Type' () ()] -> Type' () ()
mkTAppCon c = runIdentity . foldTApp (pure ()) (TCon () c)

-- | Decompose @C X Y Z@ to @(C,[X,Y,Z])@
decomposeTAppCon :: Type' a b -> Maybe (TyConName, [Type' a b])
decomposeTAppCon =
  unfoldTApp <&> \case
    (TCon _ con, args) -> Just (con, args)
    _ -> Nothing

-- | Split a function type into an array of argument types and the result type.
-- Takes two arguments: the lhs and rhs of the topmost function node.
unfoldFun :: Type' a b -> Type' a b -> (NonEmpty (Type' a b), Type' a b)
unfoldFun a (TFun _ b c) =
  let (argTypes, resultType) = unfoldFun b c
   in (NE.cons a argTypes, resultType)
unfoldFun a t = (pure a, t)
