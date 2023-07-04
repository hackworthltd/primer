{-# LANGUAGE RecordWildCards #-}

module Primer.Gen.API (
  genName,
) where

import Foreword

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Primer.API (Name (..))
import Primer.Gen.Core.Raw qualified as Raw
import Primer.Name (unsafeMkName)

genName :: MonadGen m => m Name
genName = do
  baseName <- unsafeMkName <$> G.text (R.linear 1 20) G.unicode
  qualifiedModule <- G.maybe Raw.genModuleName
  pure Name{..}
