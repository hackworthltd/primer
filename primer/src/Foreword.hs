module Foreword (
  module Protolude,
  module Unsafe,
  module Catch,
  insertAt,
  adjustAt,
  findAndAdjust,
  findAndAdjustA,
  modifyError,
  mwhen,
  munless,
  hoistAccum,
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
  from,
  gcast,
  handle,
  handleJust,
  ignore,
  lenientDecode,
  mask,
  mask_,
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

-- We should remove all uses of `unsafeHead`. See:
-- https://github.com/hackworthltd/primer/issues/147
import Protolude.Unsafe as Unsafe (unsafeHead)

-- We want @exceptions@ rather than @base@'s equivalents.
import Control.Monad.Catch as Catch

import Control.Monad.Trans.Accum (AccumT (AccumT))

-- | Insert an element at some index, returning `Nothing` if it is out of bounds.
insertAt :: Int -> a -> [a] -> Maybe [a]
insertAt n y xs =
  if length a == n
    then Just $ a ++ [y] ++ b
    else Nothing
  where
    (a, b) = splitAt n xs

-- | Apply a function to the element at some index, returning `Nothing` if it is out of bounds.
adjustAt :: Int -> (a -> a) -> [a] -> Maybe [a]
adjustAt n f xs = case splitAt n xs of
  (a, b : bs) -> Just $ a ++ [f b] ++ bs
  _ -> Nothing

-- | Adjust the first element of the list which satisfies the predicate.
-- Returns `Nothing` if there is no such element.
findAndAdjust :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
findAndAdjust p f = \case
  [] -> Nothing
  x : xs -> if p x then Just $ f x : xs else (x :) <$> findAndAdjust p f xs

-- | Like `findAndAdjust`, but in an `Applicative`.
findAndAdjustA :: Applicative m => (a -> Bool) -> (a -> m a) -> [a] -> m (Maybe [a])
findAndAdjustA p f = \case
  [] -> pure Nothing
  x : xs -> if p x then Just . (: xs) <$> f x else (x :) <<$>> findAndAdjustA p f xs

-- | Change the type of an error.
modifyError :: MonadError e' m => (e -> e') -> ExceptT e m a -> m a
modifyError f = runExceptT >=> either (throwError . f) pure

-- | @munless b x@ is `x` if `b` is 'False', otherwise it is 'mempty'.
-- It's like 'Control.Monad.unless' but for Monoids rather than Applicatives.
munless :: Monoid a => Bool -> a -> a
munless b x = if b then mempty else x

-- | @mwhen b x@ is `x` if `b` is 'True', otherwise it is 'mempty'.
-- It's like 'Control.Monad.when' but for Monoids rather than Applicatives.
mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty

-- This is upstream as of https://github.com/Gabriella439/Haskell-MMorph-Library/pull/66
-- but has not yet been released.
-- (i.e. there is now a @MFunctor (AccumT a)@ instance,
--  whose class method @hoist@ is equivalent to this)
hoistAccum :: (forall x. m x -> n x) -> AccumT a m b -> AccumT a n b
hoistAccum f (AccumT acc) = AccumT $ f . acc
