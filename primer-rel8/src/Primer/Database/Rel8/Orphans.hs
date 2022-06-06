{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module exists so that we don't need a dependency on "Rel8" in
-- Primer core.

module Primer.Database.Rel8.Orphans () where

import Primer.App (App)
import Rel8 (
  DBType,
  JSONBEncoded (..),
 )

deriving via JSONBEncoded App instance DBType App
