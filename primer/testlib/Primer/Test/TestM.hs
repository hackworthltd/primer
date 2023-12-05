-- A test monad for generating names and IDs and typechecking
module Primer.Test.TestM (
  TestT,
  TestM,
  evalTestT,
  evalTestM,
  isolateTestM,
) where

import Foreword

import Control.Monad.Fresh
import Primer.Core (ID (..))
import Primer.Name (NameCounter)

-- This monad is responsible for generating fresh IDs and names in tests.
-- If we need other abilities, this will be the base monad.
newtype TestT m a = TestT {unTestT :: StateT Int m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

type TestM a = TestT Identity a

-- | Run an action and ignore any effect on the fresh name/id state
isolateTestM :: Monad m => TestT m a -> TestT m a
isolateTestM m = TestT $ do
  st <- get
  x <- unTestT m
  put st
  pure x

evalTestT :: Monad m => ID -> TestT m a -> m a
evalTestT (ID id_) = flip evalStateT id_ . unTestT

evalTestM :: ID -> TestM a -> a
evalTestM id_ = runIdentity . evalTestT id_

instance Monad m => MonadFresh ID (TestT m) where
  fresh = TestT $ do
    i <- get
    put $ i + 1
    pure $ ID i

instance Monad m => MonadFresh NameCounter (TestT m) where
  fresh = TestT $ do
    i <- get
    put $ i + 1
    -- A bit of a hack: make the names generated a,a1,a2,... as the testsuite
    -- expects. This testsuite fragility should be refactored at some point,
    -- probably when we overhaul name generation in the (hopefully) near
    -- future.
    pure $ toEnum $ 26 * i
