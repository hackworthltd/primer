module Primer.Prelude (prelude) where

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Foreword
import Primer.Core (ID)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Prelude.Logic (andDef, notDef, orDef)
import Primer.Prelude.Utils (modName)

prelude :: (MonadFresh ID m) => m Module
prelude = do
  defs <- traverse sequence [("not", notDef), ("and", andDef), ("or", orDef)]
  pure Module{moduleName = modName, moduleTypes = Map.empty, moduleDefs = Map.fromList defs}
