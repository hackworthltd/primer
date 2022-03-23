module Primer.Module (Module (..)) where

import Foreword
import Primer.Core (Def, GVarName, TypeDef)
import Primer.JSON

data Module = Module
  { moduleTypes :: [TypeDef]
  , moduleDefs :: Map GVarName Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module
