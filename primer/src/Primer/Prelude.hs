module Primer.Prelude (prelude) where

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Foreword
import Primer.Core (ID)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Prelude.Integer (absDef, gcdDef, gcdHelperDef, maxDef, minDef, negateDef)
import Primer.Prelude.Logic (andDef, impliesDef, notDef, orDef, xorDef)
import Primer.Prelude.Utils (modName)

prelude :: (MonadFresh ID m) => m Module
prelude = do
  defs <-
    traverse
      sequence
      [ ("not", notDef)
      , ("and", andDef)
      , ("or", orDef)
      , ("xor", xorDef)
      , ("implies", impliesDef)
      , ("min", minDef)
      , ("max", maxDef)
      , ("negate", negateDef)
      , ("abs", absDef)
      , ("gcd", gcdDef)
      , ("gcdHelper", gcdHelperDef)
      ]
  pure Module{moduleName = modName, moduleTypes = Map.empty, moduleDefs = Map.fromList defs}
