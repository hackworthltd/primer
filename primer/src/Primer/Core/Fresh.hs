module Primer.Core.Fresh (
  freshLocalName,
  freshLocalName',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as S

import Primer.Core.Meta (LocalName (..))
import Primer.Name (Name, NameCounter, freshName)

-- | Helper, wrapping 'freshName'
freshLocalName :: MonadFresh NameCounter m => S.Set (LocalName k) -> m (LocalName k)
freshLocalName = freshLocalName' . S.map unLocalName

-- | Helper, wrapping 'freshName'
freshLocalName' :: MonadFresh NameCounter m => S.Set Name -> m (LocalName k)
freshLocalName' = fmap LocalName . freshName
