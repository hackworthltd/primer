-- | Tests specific to primitives.
-- Note that there are other primitives-related tests in 'Primer.Typecheck', 'Primer.Eval' etc.
module Tests.Primitives where

import Foreword

import Data.Map qualified as M
import Primer.Core (
  Kind (KFun, KType),
 )
import Primer.Core.DSL (char, tcon)
import Primer.Primitives (tChar)
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
  SmartHoles (NoSmartHoles),
  TypeError (PrimitiveTypeNotInScope, UnknownTypeConstructor),
  buildTypingContext,
  buildTypingContextFromModules,
  checkKind,
  checkValidContext,
  synth,
 )
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Tests.Typecheck (runTypecheckTestMIn)

-- If we use a prim con, then we need the corresponding prim type
-- in scope
unit_prim_con_scope :: Assertion
unit_prim_con_scope = do
  -- Char is indeed not in scope
  test (checkKind KType =<< tcon tChar) @?= Left (UnknownTypeConstructor tChar)
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
        checkKind (KType `KFun` KType) =<< tcon tChar
  test (synth =<< char 'a') @?= Left (PrimitiveTypeNotInScope tChar)
  where
    charASTDef =
      TypeDefAST $
        ASTTypeDef
          { astTypeDefParameters = [("a", KType)]
          , astTypeDefConstructors = mempty
          , astTypeDefNameHints = mempty
          }

    cxt = buildTypingContext (M.singleton tChar charASTDef) mempty NoSmartHoles
    test = runTypecheckTestMIn cxt
