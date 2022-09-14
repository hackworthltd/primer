module Control.Monad.Fresh (MonadFresh (..)) where

import Foreword
import Control.Monad.Log (PureLoggingT, DiscardLoggingT)

-- | This class gives access to a method @fresh@ which generates a new, unique
--  value of type i.
--  We use it for generating new IDs, hole and variables names.
class Monad m => MonadFresh i m where
  fresh :: m i

deriving instance MonadFresh i m => MonadFresh i (Ap m)

instance MonadFresh i m => MonadFresh i (ExceptT e m) where
  fresh = lift fresh

instance MonadFresh i m => MonadFresh i (StateT s m) where
  fresh = lift fresh

instance MonadFresh i m => MonadFresh i (ReaderT e m) where
  fresh = lift fresh

instance MonadFresh i m => MonadFresh i (PureLoggingT l m) where
  fresh = lift fresh

instance MonadFresh i m => MonadFresh i (DiscardLoggingT l m) where
  fresh = lift fresh
