module Foreword (
  module Protolude,
  module Unsafe,
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
  Meta,
  OnDecodeError,
  OnError,
  STM,
  Type,
  TypeError,
  TypeRep,
  UnicodeException,
  atomically,
  cast,
  catchSTM,
  check,
  eqT,
  from,
  gcast,
  ignore,
  lenientDecode,
  orElse,
  replace,
  retry,
  strictDecode,
  throwSTM,
  to,
  typeOf,
  typeRep,
  (%),
 )

-- We should remove all uses of `unsafeHead`. See:
-- https://github.com/hackworthltd/primer/issues/147
import Protolude.Unsafe as Unsafe (unsafeHead)
