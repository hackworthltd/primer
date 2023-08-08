-- | These functions allow you to create Core metadata easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Meta (
  meta,
  kmeta,
  meta',
  create,
  create',
  S,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Primer.Core.Meta (
  ID,
  Meta (..),
 )
import Primer.Core.Type (KindMeta)

newtype S a = S {unS :: State ID a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFresh ID S where
  fresh = S $ do
    i <- get
    put (i + 1)
    pure i

-- | Evaluate a DSL expression with a starting ID of 0, producing an
-- @a@ and the next available fresh 'ID'.
create :: S a -> (a, ID)
create = flip runState 0 . unS

-- | As 'create', but drop the 'ID'.
create' :: S a -> a
create' = fst . create

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

kmeta :: MonadFresh ID m => m KindMeta
kmeta = meta' ()

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing
