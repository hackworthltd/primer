module Primer.Module (Module (..)) where

import Foreword
import Primer.Core (Def, TypeDef)
import Primer.JSON
import Primer.Name (Name)

data Module = Module
  { moduleTypes :: [TypeDef]
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module
