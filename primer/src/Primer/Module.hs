module Primer.Module (Module (..)) where

import Foreword
import Primer.Core (Def, GlobalName, GlobalNameKind (ADefName, ATyCon), TypeDef)
import Primer.JSON

data Module = Module
  { moduleTypes :: Map (GlobalName 'ATyCon) TypeDef
  , moduleDefs :: Map (GlobalName 'ADefName) Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module
