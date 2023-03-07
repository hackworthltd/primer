module Primer.Subst (
  substTy,
  substTys,
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
        -- We must avoid this @∀m@ capturing a free variable in @a@
        -- (e.g. @substTy a (T b) (∀b.b a)@ should give @∀c.c (T b)@, and not @∀b.b (T b)@)
        -- these names will not enter the user's program, so we don't need to worry about shadowing, only variable capture
        | m `elem` avoid -> freshLocalName (avoid <> freeVarsTy s) >>= \m' -> substTy m (TVar () m') s >>= fmap (TForall () m' k) . go
        | otherwise -> TForall () m k <$> go s
      TLet _ m s b
        | m == n -> TLet () m <$> go s <*> pure b
        -- We must avoid this let-bound @m@ capturing a free variable in @a@,
        -- similarly to the TForall case
        | m `elem` avoid -> freshLocalName (avoid <> freeVarsTy b) >>= \m' -> substTy m (TVar () m') b >>= ap (TLet () m' <$> go s) . go
        | otherwise -> TLet () m <$> go s <*> go b

substTys :: MonadFresh NameCounter m => [(TyVarName, Type' ())] -> Type' () -> m (Type' ())
substTys sb t = foldrM (uncurry substTy) t sb
