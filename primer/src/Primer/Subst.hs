module Primer.Subst (
  substTy,
  substTyIter,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as Set
import Primer.Core.Fresh (freshLocalName)
import Primer.Core.Meta (TyVarName)
import Primer.Core.Type (Type' (..))
import Primer.Core.Type.Utils (freeVarsTy)
import Primer.Name (NameCounter)

-- | Simple and inefficient capture-avoiding substitution.
-- @substTy n a t@  is @t[a/n]@
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTy :: MonadFresh NameCounter m => TyVarName -> Type' () -> Type' () -> m (Type' ())
substTy n a = go
  where
    avoid = Set.singleton n <> freeVarsTy a
    -- We must avoid this binder @m@ capturing a free variable in @a@
    -- (e.g. @substTy a (T b) (∀b.b a)@ should give @∀c.c (T b)@, and not @∀b.b (T b)@)
    -- The generated names will not enter the user's program, so we don't need to worry about shadowing, only variable capture
    subUnderBinder m t
      | m == n = pure (m, t)
      | m `elem` avoid = do
          m' <- freshLocalName (avoid <> freeVarsTy t)
          t' <- substTy m (TVar () m') t
          (m',) <$> go t'
      | otherwise = (m,) <$> go t
    go = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go t
      t@TCon{} -> pure t
      TFun _ s t -> TFun () <$> go s <*> go t
      t@(TVar _ m)
        | n == m -> pure a
        | otherwise -> pure t
      TApp _ s t -> TApp () <$> go s <*> go t
      TForall _ m k s -> do
        (m', s') <- subUnderBinder m s
        pure $ TForall () m' k s'
      TLet _ m s b -> do
        s' <- go s
        (m', b') <- subUnderBinder m b
        pure $ TLet () m' s' b'

-- | Iterated substitution: @substTyIter [(a,A),(b,B)] ty@ gives @(ty[B/b])[A/a]@.
-- Thus if @B@ refers to a variable @a@, this reference will also be
-- substituted.
substTyIter :: MonadFresh NameCounter m => [(TyVarName, Type' ())] -> Type' () -> m (Type' ())
substTyIter sb t = foldrM (uncurry substTy) t sb
