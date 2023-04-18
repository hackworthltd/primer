module Primer.Test.App (
  comprehensive,
) where

import Foreword

import Data.Map.Strict qualified as Map
import Primer.App (
  App,
  Prog (..),
  defaultProg,
  mkApp,
 )
import Primer.Core (
  baseName,
  mkSimpleModuleName,
 )
import Primer.Core.DSL (create)
import Primer.Examples qualified as Examples
import Primer.Module (
  Module (
    Module,
    moduleDefs,
    moduleName,
    moduleTypes
  ),
  builtinModule,
  primitiveModule,
 )

-- | An initial test 'App' instance that contains all default type
-- definitions (including primitive types), all primitive functions,
-- and a top-level definition with extensive coverage of Primer's
-- core language.
comprehensive :: App
comprehensive =
  let modName = mkSimpleModuleName "TestModule"
      ((builtinMod, (defName, def)), id_) = create $ (,) <$> builtinModule <*> Examples.comprehensive modName
      testProg =
        defaultProg
          { progImports = [builtinMod, primitiveModule]
          , progModules =
              [ Module
                  { moduleName = mkSimpleModuleName "TestModule"
                  , moduleTypes = mempty
                  , moduleDefs = Map.singleton (baseName defName) def
                  }
              ]
          }
   in mkApp id_ (toEnum 0) testProg
