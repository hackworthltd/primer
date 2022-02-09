module Primer.Subst (
  substTy,
  substTys,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Primer.Core (Type' (..))
import Primer.Core.Utils (freeVarsTy)
import Primer.Name (Name, NameCounter, freshName)

-- | Simple and inefficient capture-avoiding substitution.
-- @substTy n a t@  is t[a/n]
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTy :: MonadFresh NameCounter m => Name -> Type' () -> Type' () -> m (Type' ())
substTy n a = go
  where
    avoid = freeVarsTy a
    go = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go t
      t@TCon{} -> pure t
      TFun _ s t -> TFun () <$> go s <*> go t
      t@(TVar _ m)
        | n == m -> pure a
        | otherwise -> pure t
      TApp _ s t -> TApp () <$> go s <*> go t
      t@(TForall _ m k s)
        | m == n -> pure t
        -- these names will not enter the user's program, so we don't need to worry about shadowing, only variable capture
        | m `elem` avoid -> freshName (avoid <> freeVarsTy s) >>= \m' -> substTy m (TVar () m') s >>= fmap (TForall () m' k) . go
        | otherwise -> TForall () m k <$> go s

substTys :: MonadFresh NameCounter m => [(Name, Type' ())] -> Type' () -> m (Type' ())
substTys sb t = foldrM (uncurry substTy) t sb
