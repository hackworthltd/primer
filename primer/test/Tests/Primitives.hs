-- | Tests specific to primitives.
-- Note that there are other primitives-related tests in 'Primer.Typecheck', 'Primer.Eval' etc.
module Tests.Primitives where

import Foreword

import Data.Map qualified as M
import Primer.Builtins (cFalse, cTrue, tBool)
import Primer.Core (
  Kind' (KFun, KType),
  Pattern (PatPrim),
  PrimCon (PrimDouble),
  Type' (TCon),
 )
import Primer.Core.DSL (branchPrim, caseFB_, char, con0, double, tcon)
import Primer.Primitives (tChar, tDouble)
import Primer.TypeDef (
  ASTTypeDef (
    ASTTypeDef,
    astTypeDefConstructors,
    astTypeDefNameHints,
    astTypeDefParameters
  ),
  TypeDef (TypeDefAST),
 )
import Primer.Typecheck (
  KindError (UnknownTypeConstructor),
  SmartHoles (NoSmartHoles),
  TypeError (KindError, PrimitiveTypeNotInScope, WrongCaseBranches),
  buildTypingContext,
  buildTypingContextFromModules,
  check,
  checkKind,
  checkValidContext,
  synth,
 )
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Tests.Typecheck (runTypecheckTestMIn, runTypecheckTestMWithPrims)

-- If we use a prim con, then we need the corresponding prim type
-- in scope
unit_prim_con_scope :: Assertion
unit_prim_con_scope = do
  -- Char is indeed not in scope
  test (checkKind (KType ()) =<< tcon tChar) @?= Left (KindError $ UnknownTypeConstructor tChar)
  test (synth =<< char 'a') @?= Left (PrimitiveTypeNotInScope tChar)
  where
    cxt = buildTypingContextFromModules mempty NoSmartHoles
    test = runTypecheckTestMIn cxt

-- If we use a prim con, then we need the corresponding prim type
-- in scope, and not some other type of that name
unit_prim_con_scope_ast :: Assertion
unit_prim_con_scope_ast = do
  -- Our type def is accepted
  test (checkValidContext =<< ask) @?= Right ()
  -- Char is in scope (though the wrong kind to accept 'PrimChar's!)
  assertBool "Char is not in scope?" $
    isRight $
      test $
        checkKind (KFun () (KType ()) (KType ())) =<< tcon tChar
  test (synth =<< char 'a') @?= Left (PrimitiveTypeNotInScope tChar)
  where
    charASTDef =
      TypeDefAST $
        ASTTypeDef
          { astTypeDefParameters = [("a", KType ())]
          , astTypeDefConstructors = mempty
          , astTypeDefNameHints = mempty
          }

    cxt = buildTypingContext (M.singleton tChar charASTDef) mempty NoSmartHoles
    test = runTypecheckTestMIn cxt

-- We do not allow pattern matching on 'Double's,
-- since equality on floating-point numbers is rarely what one wants.
unit_prim_double_no_case_branches :: Assertion
unit_prim_double_no_case_branches = do
  assertBool "Case with only a fallback branch should be accepted" $
    isRight $
      test $
        check (TCon () tBool) =<< caseFB_ (double 1.5) [] (con0 cFalse)
  test (check (TCon () tBool) =<< caseFB_ (double 1.5) [branchPrim (PrimDouble 1.5) $ con0 cTrue] (con0 cFalse))
    @?= Left (WrongCaseBranches tDouble [PatPrim $ PrimDouble 1.5] True)
  where
    test = runTypecheckTestMWithPrims NoSmartHoles
