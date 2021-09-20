{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines an alternative MonadError class which automatically lifts
-- 'smaller' errors into 'larger' ones. A larger error is a type with a
-- constructor wrapping the smaller error.
--
-- We use this in Primer.Action, which has a type ActionError, one of whose
-- constructors is 'TypeError :: TypeError -> ActionError'. Thus any computation
-- that raises a TypeError can be lifted to a computation that raises an
-- ActionError by wrapping the error in the @TypeError@ constructor.
--
-- This class handles that automatically by making use of the AsType class from
-- generic-optics. So all you need to do is add the constructor to your type and
-- replace your `MonadError E m` constraint with `MonadNestedError E e m`.
--
-- This technique is stolen shamelessly from Csongor Kiss:
-- https://github.com/kcsongor/generic-lens/pull/54#issuecomment-396991177
--
-- Here's a simple example. We have two error types: SpecificError and
-- GeneralError. We want to automatically lift SpecificErrors into GeneralErrors
-- by wrapping them in the GE2 constructor.
--
-- > import GHC.Generics
-- > data SpecificError = SpecificError deriving (Generic, Show)
-- > data GeneralError = GE1 | GE2 SpecificError deriving (Generic, Show)
--
-- > :t throwError' SpecificError
-- throwError' SpecificError
--   :: MonadNestedError SpecificError larger m => m a
--
-- Because GeneralError has a constructor which wraps SpecificError, we can just
-- cast this to a normal @MonadError GeneralError m@ constraint:
--
-- > :t throwError' SpecificError :: MonadError GeneralError m => m a
-- throwError' SpecificError :: MonadError GeneralError m => m a
--   :: MonadError GeneralError m => m a
--
-- Note that this only works if the larger type has a matching constructor:
--
-- > :t throwError' SpecificError :: MonadError Bool m => m a
--
-- <interactive>:1:1: error:
--     • The type Bool does not contain a constructor whose field is of type SpecificError
--     • In the expression:
--           throwError' SpecificError :: MonadError Bool m => m a
module Control.Monad.NestedError where

import Control.Monad.Except
import Data.Generics.Sum.Typed

-- | This class is like MonadError but is parameterised by two error
-- types: the type we are throwing and the larger type we have a MonadError
-- instance for. We automatically lift the smaller type to the larger one via
-- AsType.
class (Monad m, MonadError larger m) => MonadNestedError smaller larger m | m -> larger where
  throwError' :: smaller -> m a
  catchError' :: m a -> (smaller -> m a) -> m a

-- | We can trivially lift the type to itself
instance (Monad m, MonadError e m) => MonadNestedError e e m where
  throwError' = throwError
  catchError' = catchError

-- | Or we can lift the smaller type to the larger type, if there is an AsType
-- instance for it.
instance
  {-# OVERLAPPABLE #-}
  (Monad m, MonadError larger m, AsType smaller larger) =>
  MonadNestedError smaller larger m
  where
  throwError' = throwError . injectTyped
  catchError' ma f = catchError ma $ \e -> maybe ma f (projectTyped e)
