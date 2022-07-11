{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Primer.Database.Rel8.Schema (
  SessionRow (..),
  sessionRowSchema,
) where

import Foreword

import Data.String (String)
import Data.UUID (UUID)
import Primer.App (App)
import Primer.Database (
  Version,
 )
import Primer.Database.Rel8.Orphans ()
import Rel8 (
  Column,
  Name,
  Rel8able,
  Result,
  namesFromLabels,
 )
import Rel8 qualified (
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

-- Our database schema name. We need to provide this to Rel8.
dbSchema :: Maybe String
dbSchema = Just "primer"

sessionRowSchema :: Rel8.TableSchema (SessionRow Name)
sessionRowSchema =
  Rel8.TableSchema
    { Rel8.name = "sessions"
    , Rel8.schema = dbSchema
    , Rel8.columns = namesFromLabels @(SessionRow Name)
    }
