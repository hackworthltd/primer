{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Primer.Database.Rel8.Schema (
  SessionRow (..),
  sessionRowSchema,
) where

import Foreword

import Data.ByteString.Lazy as BL
import Data.String (String)
import Data.UUID (UUID)
import Primer.Database (
  Version,
 )
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
  , app :: Column f BL.ByteString
  -- ^ The session's 'App'. Note that the 'App' is serialized to
  -- JSON before being stored as a bytestring in the database.
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
