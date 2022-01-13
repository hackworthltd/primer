-- A test monad for generating names and IDs and typechecking
module TestM (TestM, evalTestM, isolateTestM) where

import Foreword

import Control.Monad.Fresh
import Primer.Core (ID (..))
import Primer.Name (NameCounter)

-- This monad is responsible for generating fresh IDs and names in tests.
-- If we need other abilities, this will be the base monad.
newtype TestM a = TestM {unTestM :: State Int a}
  deriving newtype (Functor, Applicative, Monad, MonadState Int)

-- | Run an action and ignore any effect on the fresh name/id state
isolateTestM :: TestM a -> TestM a
isolateTestM m = do
  st <- get
  x <- m
  put st
  pure x

evalTestM :: ID -> TestM a -> a
evalTestM (ID id_) = fst . flip runState id_ . unTestM

instance MonadFresh ID TestM where
  fresh = do
    i <- get
    put $ i + 1
    pure $ ID i

instance MonadFresh NameCounter TestM where
  fresh = do
    i <- get
    put $ i + 1
    -- A bit of a hack: make the names generated a,a1,a2,... as the testsuite
    -- expects. This testsuite fragility should be refactored at some point,
    -- probably when we overhaul name generation in the (hopefully) near
    -- future.
    pure $ toEnum $ 26 * i
