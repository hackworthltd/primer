{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Primer.Database.Rel8.OrphanInstances (
  -- * Orphan instances
  -- $orphanInstances
) where

import Primer.App (App)
import Rel8 (
  DBType,
  JSONEncoded (..),
 )

-- $orphanInstances
--
-- In order to keep the Primer core library free of a "Rel8"
-- dependency, we need to define a few orphan instances.

deriving via (JSONEncoded App) instance (DBType App)
