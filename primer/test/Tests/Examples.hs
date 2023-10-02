module Tests.Examples where

import Foreword hiding (not)

import Primer.Core.DSL (create')

import Data.Map qualified as Map
import Primer.Core (GlobalName (baseName), ModuleName (ModuleName))
import Primer.Examples (
  comprehensive,
  comprehensiveWellTyped,
  evenOddModule,
  mapModule,
  not,
 )
import Primer.Module (Module (Module), builtinModule)
import Primer.Typecheck (
  CheckEverythingRequest (CheckEverything, toCheck, trusted),
  SmartHoles (NoSmartHoles),
  checkEverything,
 )
import Test.Tasty.HUnit (Assertion, assertFailure)
import Tests.Typecheck (runTypecheckTestM)

checkExamplesRequest :: CheckEverythingRequest
checkExamplesRequest =
  CheckEverything
    { trusted = [create' builtinModule]
    , toCheck =
        [ fst $ mapModule $ ModuleName $ pure "MapModule"
        , fst $ evenOddModule $ ModuleName $ pure "EvenOddModule"
        , otherExamples
        ]
    }
  where
    others = ModuleName $ pure "OtherExamples"
    otherExamples =
      Module others mempty
        $ Map.fromList
          [ first baseName $ create' $ not others
          , first baseName $ create' $ comprehensiveWellTyped others
          ]

unit_check_examples :: Assertion
unit_check_examples = case runTypecheckTestM
  NoSmartHoles
  (checkEverything NoSmartHoles checkExamplesRequest) of
  Left err -> assertFailure $ show err
  Right _ -> pure ()

unit_comprehensive_ill_typed :: Assertion
unit_comprehensive_ill_typed = case runTypecheckTestM
  NoSmartHoles
  ( checkEverything NoSmartHoles
      $ CheckEverything
        { trusted = [create' builtinModule]
        , toCheck = [Module modName mempty $ Map.fromList [first baseName $ create' $ comprehensive modName]]
        }
  ) of
  Left _ -> pure ()
  Right _ -> assertFailure "expected comprehensive to be ill-typed"
  where
    modName = ModuleName $ pure "Comprehensive"
