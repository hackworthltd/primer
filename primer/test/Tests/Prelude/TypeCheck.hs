{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.Prelude.TypeCheck where

import Foreword

import Primer.Core (ASTDef (astDefExpr, astDefType), Def (DefAST))
import Primer.Core.DSL (ann, create')
import Primer.Prelude (prelude)
import Primer.Prelude.Logic (andDef, impliesDef, notDef, orDef, xorDef)
import Protolude.Error (error)

import Primer.Builtins (builtinModule)
import Primer.Typecheck (SmartHoles (NoSmartHoles), buildTypingContextFromModules)
import Test.Tasty.HUnit (Assertion)
import Tests.Typecheck (TypecheckTestM, expectTypedIn)

checkDef :: TypecheckTestM Def -> Assertion
checkDef def = expectTypedIn (buildTypingContextFromModules [create' prelude, builtinModule] NoSmartHoles) $ do
  def >>= \case
    DefAST d ->
      pure (astDefExpr d) `ann` pure (astDefType d)
    _ -> error "this can't happen"

unit_not :: Assertion
unit_not = checkDef notDef

unit_and :: Assertion
unit_and = checkDef andDef

unit_or :: Assertion
unit_or = checkDef orDef

unit_xor :: Assertion
unit_xor = checkDef xorDef

unit_implies :: Assertion
unit_implies = checkDef impliesDef
