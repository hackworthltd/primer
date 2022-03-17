{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Primer.OpenAPI (
  -- * Orphan instances
  -- $orphanInstances
) where

import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Primer.API (Def, Prog, Tree)
import Primer.App (InitialApp)
import Primer.Core (ID (..), TyConName)
import Primer.Database (Session, SessionName)
import Primer.Name (Name)

-- $orphanInstances
--
-- We define some OpenApi orphan instances in primer-service, to avoid
-- pulling in the openapi3 dependency into primer core. This is necessary to
-- build primer with ghcjs, because openapi3 transitively depends on network,
-- which ghcjs currently cannot build.

instance ToSchema SessionName
instance ToSchema Session
instance ToSchema InitialApp

-- We need to GND the ID instance to match its To/FromJSON instances
deriving newtype instance ToSchema ID

-- We can't GND derive for Name as it is an opaque type
-- But the JSON instance is done by GND, so we must match here...
-- This instance works because the parameter has a phantom role!
deriving via Text instance (ToSchema Name)

-- For TyConName, we must derive ToSchema via Name,
-- as that is how the To/FromJSON instances are derived
deriving via Name instance (ToSchema TyConName)
instance ToSchema Tree
instance ToSchema Def
instance ToSchema Prog
