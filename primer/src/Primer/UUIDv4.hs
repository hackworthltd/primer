module Primer.UUIDv4 (nextRandom) where

import Foreword

import Data.UUID.Types (UUID)
import System.Random (randomIO)

-- | Generate a pseudo-random 'UUID'.
--
-- Note: this effect does __not__ generate cryptographically secure
-- UUIDs. If you need cryptographically secure 'UUID's, use
-- the effect of the same name from the @uuid@ package.
nextRandom :: IO UUID
nextRandom = randomIO
