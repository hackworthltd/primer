module Primer.Module (Module (..)) where

import Foreword
import Primer.Core (Def, ID, TypeDef)
import Primer.JSON

data Module = Module
  { moduleTypes :: [TypeDef]
  , moduleDefs :: Map ID Def -- The current program: a set of definitions indexed by ID
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module
