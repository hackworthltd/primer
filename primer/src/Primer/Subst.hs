module Primer.Subst (
  substTy,
  substTyTele,
  substTySimul,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import qualified Data.Map as M
import Primer.Core.Fresh (freshLocalName)
import Primer.Core.Meta (TyVarName)
import Primer.Core.Type (Type' (..))
import Primer.Core.Type.Utils (freeVarsTy)
import Primer.Name (NameCounter)

-- | Simple and inefficient capture-avoiding simultaneous substitution.
-- @substTySimul [(a,A),(b,B)] t@ is @t[A,B/a,b]@, where any references to @a,b@
-- in their replacements @A,B@ are not substituted.
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTySimul :: MonadFresh NameCounter m => Map TyVarName (Type' ()) -> Type' () -> m (Type' ())
substTySimul sub | M.null sub = pure
                 | otherwise = go
  where
    -- When going under a binder, we must rename it if it may capture a variable
    -- from @sub@'s rhs
    avoid = foldMap' freeVarsTy sub
    -- We must avoid this binder @m@ capturing a free variable in (some rhs of) @sub@
    -- (e.g. @substTy [a :-> T b] (∀b.b a)@ should give @∀c.c (T b)@, and not @∀b.b (T b)@)
    -- The generated names will not enter the user's program, so we don't need to worry about shadowing, only variable capture
    subUnderBinder m t = do
      let sub' = M.delete m sub
      (m', sub'') <- if m `elem` avoid
        then do
          -- If we are renaming, we
          -- - must also avoid capturing any existing free variable
          -- - choose to also avoid the names of any variables we are
          --   substituting away (for clarity and ease of implementation)
          m' <- freshLocalName (avoid <> freeVarsTy t <> M.keysSet sub)
          pure (m',M.insert m (TVar () m') sub')
        else pure (m,sub')
      (m',) <$> substTySimul sub'' t
    go = \case
      t@TEmptyHole{} -> pure t
      THole m t -> THole m <$> go t
      t@TCon{} -> pure t
      TFun _ s t -> TFun () <$> go s <*> go t
      t@(TVar _ m)
        | Just a <- M.lookup m sub -> pure a
        | otherwise -> pure t
      TApp _ s t -> TApp () <$> go s <*> go t
      TForall _ m k s -> do
        (m',s') <- subUnderBinder m s
        pure $ TForall () m' k s'
      TLet _ m s b -> do
        s' <- go s
        (m',b') <- subUnderBinder m b
        pure $ TLet () m' s' b'

-- | Simple and inefficient capture-avoiding substitution.
-- @substTy n a t@  is @t[a/n]@
-- We restrict to '()', i.e. no metadata as we don't want to duplicate IDs etc
substTy :: MonadFresh NameCounter m => TyVarName -> Type' () -> Type' () -> m (Type' ())
substTy n a = substTySimul $ M.singleton n a

-- | Substitute a telescope: @substTys [(a,A),(b,B)] ty@ gives the iterated
-- substitution @(ty[B/b])[A/a]@. Thus if @B@ refers to a variable @a@, this
-- reference will also be substituted.
substTyTele :: MonadFresh NameCounter m => [(TyVarName, Type' ())] -> Type' () -> m (Type' ())
substTyTele sb t = foldrM (uncurry substTy) t sb
