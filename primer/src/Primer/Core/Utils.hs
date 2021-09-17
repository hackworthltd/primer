module Primer.Core.Utils (
  generateTypeIDs,
  forgetTypeIDs,
  generateIDs,
  forgetIDs,
  noHoles,
) where

import Control.Monad ((<=<))
import Control.Monad.Fresh (MonadFresh)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (universe)
import Optics (set, traverseOf)
import Primer.Core (
  Expr,
  Expr',
  ID,
  Kind (KHole),
  Type,
  Type' (TEmptyHole, TForall, THole),
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL (meta)

-- | Adds 'ID's and trivial metadata
generateTypeIDs :: MonadFresh ID m => Type' () -> m Type
generateTypeIDs = traverseOf _typeMeta $ const meta

-- | Replace all 'ID's in a Type with unit.
-- Technically this replaces all annotations, regardless of what they are.
forgetTypeIDs :: Type' a -> Type' ()
forgetTypeIDs = set _typeMeta ()

-- | Like 'generateTypeIDs', but for expressions
generateIDs :: MonadFresh ID m => Expr' () () -> m Expr
generateIDs = traverseOf _exprTypeMeta (const meta) <=< traverseOf _exprMeta (const meta)

-- | Like 'forgetTypeIDs', but for expressions
forgetIDs :: Expr' a b -> Expr' () ()
forgetIDs = set _exprTypeMeta () . set _exprMeta ()

-- | Test whether an type contains any holes
-- (empty or non-empty, or inside a kind)
noHoles :: Data a => Type' a -> Bool
noHoles t = flip all (universe t) $ \case
  THole {} -> False
  TEmptyHole {} -> False
  TForall _ _ k _ -> flip all (universe k) $ \case
    KHole -> False
    _ -> True
  _ -> True
