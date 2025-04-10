{-# LANGUAGE ExplicitNamespaces #-}

-- |
-- Module: Foreword
-- Description: Primer's custom prelude
-- Copyright: © 2023 Hackworth Ltd
-- License: AGPL-3.0-or-later
-- Maintainer: src@hackworthltd.com
-- Stability: experimental
--
-- The Primer project's custom prelude.
module Foreword (
  -- * Re-exports
  module Protolude,
  module Unsafe,
  module Catch,
  module Foldable,
  module Foldable1,
  module TypeEquality,

  -- * Helper functions
  insertAt,
  deleteAt,
  adjustAt,
  adjustAtA,
  findAndAdjust,
  findAndAdjustA,
  modifyError,
  mwhen,
  munless,
  hoistAccum,
  hoistMaybe,
  (?:),
  curry4,
  unsafeMaximum,
  spanMaybe,
  realToFixed,
) where

-- In general, we should defer to "Protolude"'s exports and avoid name
-- clashes, but if there's a name that we really want to use and it's
-- unlikely we'll need the "Protolude" version much, we hide the
-- "Protolude" export. When we do hide a "Protolude" export, we try to
-- hide related functions & types, as well, under the assumption that
-- if you ever do need it, you can just import the original module
-- qualified. Examples are "Control.Monad.STM", 'GHC.Generics.from',
-- and 'GHC.Generics.to'.
import Protolude hiding (
  Handler,
  Meta,
  OnDecodeError,
  OnError,
  STM,
  Type,
  TypeError,
  TypeRep,
  UnicodeException,
  atomically,
  bracket,
  bracketOnError,
  bracket_,
  cast,
  catch,
  catchJust,
  catchSTM,
  catches,
  check,
  eqT,
  finally,
  -- hide 'foldMap' as it is lazy in the accumulator
  foldMap,
  -- hide 'foldl' as it is lazy in the accumulator
  foldl,
  from,
  gcast,
  handle,
  handleJust,
  head,
  ignore,
  lenientDecode,
  mask,
  mask_,
  maximum,
  maximumBy,
  minimum,
  minimumBy,
  moduleName,
  onException,
  orElse,
  replace,
  retry,
  strictDecode,
  throwSTM,
  to,
  try,
  tryJust,
  typeOf,
  typeRep,
  uninterruptibleMask,
  uninterruptibleMask_,
  (%),
 )

-- We should remove all uses of 'Protolude.Unsafe.unsafeHead'. See:
-- https://github.com/hackworthltd/primer/issues/147

import Protolude qualified as P
import Protolude.Unsafe as Unsafe (unsafeHead)

import Data.Foldable as Foldable (foldMap')
import Data.Foldable1 as Foldable1
import Data.Type.Equality as TypeEquality (type (~))

-- We want @exceptions@ rather than @base@'s equivalents.
import Control.Monad.Catch as Catch
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

import Control.Monad.Trans.Accum (AccumT (AccumT))
import Data.Fixed (Fixed, HasResolution)

-- | Insert an element at some index, returning 'Nothing' if it is out
-- of bounds.
insertAt :: Int -> a -> [a] -> Maybe [a]
insertAt n y xs =
  if length a == n
    then Just $ a ++ [y] ++ b
    else Nothing
  where
    (a, b) = splitAt n xs

-- | Delete an element at some index, returning 'Nothing' if it is out
-- of bounds.
deleteAt :: Int -> [b] -> Maybe ([b], b)
deleteAt n xs = case splitAt n xs of
  (a, b : bs) -> Just (a ++ bs, b)
  _ -> Nothing

-- | Apply a function to the element at some index, returning
-- 'Nothing' if it is out of bounds.
adjustAt :: Int -> (a -> a) -> [a] -> Maybe [a]
adjustAt n f = runIdentity . adjustAtA n (pure . f)

-- | Like 'adjustAt', but in an 'Applicative'.
adjustAtA :: Applicative f => Int -> (a -> f a) -> [a] -> f (Maybe [a])
adjustAtA n f xs = case splitAt n xs of
  (a, b : bs) -> f b <&> \b' -> Just $ a ++ [b'] ++ bs
  _ -> pure Nothing

-- | Adjust the first element of the list which satisfies the
-- predicate. Returns 'Nothing' if there is no such element.
findAndAdjust :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
findAndAdjust p f = \case
  [] -> Nothing
  x : xs -> if p x then Just $ f x : xs else (x :) <$> findAndAdjust p f xs

-- | Like 'findAndAdjust', but in an 'Applicative'.
findAndAdjustA :: Applicative m => (a -> Bool) -> (a -> m a) -> [a] -> m (Maybe [a])
findAndAdjustA p f = \case
  [] -> pure Nothing
  x : xs -> if p x then Just . (: xs) <$> f x else (x :) <<$>> findAndAdjustA p f xs

-- | Change the type of an error.
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f = runExceptT >=> either (throwError . f) pure

-- | @munless b x@ is @x@ if @b@ is 'False', otherwise it is 'mempty'.
-- It's like 'Control.Monad.unless', but for 'Monoid's rather than
-- 'Applicative's.
munless :: Monoid a => Bool -> a -> a
munless b x = if b then mempty else x

-- | @mwhen b x@ is @x@ if @b@ is 'True', otherwise it is 'mempty'.
-- It's like 'Control.Monad.when', but for 'Monoid's rather than
-- 'Applicative's.
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty

-- This is upstream as of https://github.com/Gabriella439/Haskell-MMorph-Library/pull/66
-- but has not yet been released.
-- (i.e. there is now a @MFunctor (AccumT a)@ instance,
--  whose class method @hoist@ is equivalent to this)
hoistAccum :: (forall x. m x -> n x) -> AccumT a m b -> AccumT a n b
hoistAccum f (AccumT acc) = AccumT $ f . acc

-- This will be exported from "Control.Monad.Trans.Maybe" in
-- @transformers@ 0.6.0.0 and later
hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

-- | An optional version of '(:)'.
(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x : xs
Nothing ?: xs = xs

infixr 5 ?:

curry4 :: ((a, b, c, d) -> r) -> a -> b -> c -> d -> r
curry4 f a b c d = f (a, b, c, d)

{- HLINT ignore unsafeMaximum "Avoid restricted function" -}

-- | This will throw a runtime error when called with an empty list.
--
-- (@unsafeMaximum =@ 'P.maximum', renamed to make its partiality obvious.)
unsafeMaximum :: Ord a => [a] -> a
unsafeMaximum = P.maximum

-- | Similar to 'Data.List.span', but takes a 'Maybe' predicate.
spanMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
spanMaybe f = go
  where
    go [] = ([], [])
    go xxs@(x : xs) = case f x of
      Just b -> first (b :) $ go xs
      Nothing -> ([], xxs)

{- HLINT ignore realToFixed "Avoid restricted function" -}

-- | A restricted `realToFrac`.
-- Sources such as the Haskell Wiki recommend avoiding `realToFrac` for converting between _floating-point_ types.
-- Converting to `Fixed` is harmless, aside from the obvious potential loss of precision.
-- Indeed, `Fixed`'s own docs recommend it as the standard way to convert.
realToFixed :: (Real a, HasResolution b) => a -> Fixed b
realToFixed = realToFrac
