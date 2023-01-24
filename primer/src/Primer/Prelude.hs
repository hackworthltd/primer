module Primer.Prelude (prelude) where

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Foreword
import Primer.Core (ID)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Prelude.Integer (absDef, evenDef, gcdDef, gcdHelperDef, lcmDef, maxDef, minDef, negateDef, oddDef)
import Primer.Prelude.Logic (andDef, impliesDef, notDef, orDef, xorDef)
import Primer.Prelude.Polymorphism (
  constDef,
  foldrDef,
  idDef,
  mapDef,
 )
import Primer.Prelude.Utils (modName)

prelude :: (MonadFresh ID m) => m Module
prelude = do
  defs <-
    traverse
      sequence
      ( let logic =
              [ ("not", notDef)
              , ("and", andDef)
              , ("or", orDef)
              , ("xor", xorDef)
              , ("implies", impliesDef)
              ]
            integer =
              [ ("min", minDef)
              , ("max", maxDef)
              , ("negate", negateDef)
              , ("abs", absDef)
              , ("gcd", gcdDef)
              , ("gcdHelper", gcdHelperDef)
              , ("lcm", lcmDef)
              , ("even", evenDef)
              , ("odd", oddDef)
              ]
            polymorphism =
              [ ("id", idDef)
              , ("const", constDef)
              , ("map", mapDef)
              , ("foldr", foldrDef)
              ]
         in logic ++ integer ++ polymorphism
      )
  pure Module{moduleName = modName, moduleTypes = Map.empty, moduleDefs = Map.fromList defs}
