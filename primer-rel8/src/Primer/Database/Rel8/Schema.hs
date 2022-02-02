{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Primer.Database.Rel8.Schema (
  SessionRow (..),
  sessionRowSchema,
) where

import Foreword

import Data.UUID (UUID)
import Primer.App (App)
import Primer.Database (
  Version,
 )
import Primer.Database.Rel8.OrphanInstances ()
import Rel8 (
  Column,
  Name,
  Rel8able,
  Result,
  namesFromLabels,
 )
import qualified Rel8 (
  TableSchema (..),
 )

-- | A Primer session, as represented in the database.
data SessionRow f = SessionRow
  { uuid :: Column f UUID
  -- ^ The session's UUID.
  , gitversion :: Column f Version
  -- ^ Primer's git version. We would prefer that this were a git
  -- rev, but for technical reasons, it may also be a last-modified
  -- date.
  , app :: Column f App
  -- ^ The session's 'App'.
  , name :: Column f Text
  -- ^ The session's name.
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (SessionRow f)

sessionRowSchema :: Rel8.TableSchema (SessionRow Name)
sessionRowSchema =
  Rel8.TableSchema
    { Rel8.name = "sessions"
    , Rel8.schema = Nothing
    , Rel8.columns = namesFromLabels @(SessionRow Name)
    }
