{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.Prelude.TypeCheck where

import Foreword

import Primer.Core.DSL (create')
import Primer.Prelude (prelude)

import Primer.Builtins (builtinModule)
import Primer.Module (Module)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles),
  checkEverything,
 )
import Test.Tasty.HUnit (Assertion, assertFailure)
import Tests.Typecheck (TypecheckTestM, runTypecheckTestM)

checkPreludeRequest :: CheckEverythingRequest
checkPreludeRequest = CheckEverything{trusted = [], toCheck = [create' prelude, builtinModule]}

checkPrelude :: TypecheckTestM [Module]
checkPrelude = checkEverything NoSmartHoles checkPreludeRequest

unit_check_prelude :: Assertion
unit_check_prelude = case runTypecheckTestM NoSmartHoles checkPrelude of
  Left err -> assertFailure $ show err
  Right _ -> pure ()
