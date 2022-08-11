-- |
-- This module contains tests that the generators in "Gen.App" do in fact
-- generate well-typed programs
module Tests.Gen.App where

import Foreword
import Gen.App (genProg)
import Gen.Core.Typed (
  propertyWT,
 )
import Hedgehog (
  annotateShow,
  failure,
 )
import Hedgehog.Internal.Property (forAllT)
import Primer.App (checkProgWellFormed)
import Primer.Builtins (builtinModule)
import Primer.Primitives (primitiveModule)
import Primer.Typecheck (SmartHoles (NoSmartHoles), TypeError)
import TestUtils (Property, withDiscards, withTests)

tasty_genProg_well_formed :: Property
tasty_genProg_well_formed = withTests 1000 $
  withDiscards 2000 $
    propertyWT [] $ do
      p <- forAllT $ genProg NoSmartHoles [builtinModule, primitiveModule]
      c <- runExceptT @TypeError $ checkProgWellFormed p
      case c of
        Left err -> annotateShow err >> failure
        Right _ -> pure ()
