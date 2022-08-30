module Primer.Prelude.Utils (modName) where

import Primer.Core (ModuleName, mkSimpleModuleName)

modName :: ModuleName
modName = mkSimpleModuleName "Prelude"
