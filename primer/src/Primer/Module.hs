module Primer.Module (Module (..), mkTypeDefMap) where

import qualified Data.Map as M
import Foreword
import Primer.Core (Def, GlobalName, GlobalNameKind (ADefName, ATyCon), TypeDef, typeDefName)
import Primer.JSON

data Module = Module
  { moduleTypes :: Map (GlobalName 'ATyCon) TypeDef
  , moduleDefs :: Map (GlobalName 'ADefName) Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module

-- | Create a mapping of name to typedef for fast lookup.
-- Ensures that @typeDefName (mkTypeDefMap ! n) == n@
mkTypeDefMap :: [TypeDef] -> Map (GlobalName 'ATyCon) TypeDef
mkTypeDefMap defs = M.fromList $ map (\d -> (typeDefName d, d)) defs
