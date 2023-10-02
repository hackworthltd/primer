-- |
-- This module contains tests that the generators in "Gen.App" do in fact
-- generate well-typed programs
module Tests.Gen.App where

import Foreword
import Hedgehog (
  annotateShow,
  failure,
 )
import Hedgehog.Internal.Property (forAllT)
import Primer.App (checkProgWellFormed)
import Primer.Gen.App (genProg)
import Primer.Gen.Core.Typed (
  propertyWT,
 )
import Primer.Module (builtinModule, primitiveModule)
import Primer.Typecheck (SmartHoles (NoSmartHoles), TypeError)
import Tasty (Property, withDiscards, withTests)

tasty_genProg_well_formed :: Property
tasty_genProg_well_formed = withTests 500
  $ withDiscards 2000
  $ propertyWT []
  $ do
    builtinModule' <- builtinModule
    primitiveModule' <- primitiveModule
    p <- forAllT $ genProg NoSmartHoles [builtinModule', primitiveModule']
    c <- runExceptT @TypeError $ checkProgWellFormed p
    case c of
      Left err -> annotateShow err >> failure
      Right _ -> pure ()
