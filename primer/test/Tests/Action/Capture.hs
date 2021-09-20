-- | Tests for actions handling variable capture.
module Tests.Action.Capture where

import Foreword

import Primer.Action (
  Action (..),
  ActionError (NameCapture, NeedEmptyHole),
  Movement (..),
 )
import Primer.Core (
  Kind (KType),
 )
import Primer.Core.DSL
import Primer.Typecheck (SmartHoles (NoSmartHoles))
import Test.Tasty.HUnit (Assertion)
import Tests.Action (actionTest, actionTestExpectFail)

unit_ConstructLam_no_capture :: Assertion
unit_ConstructLam_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ var "x") tEmptyHole)
    [Move Child1, Move Child1, ConstructLam (Just "x")]

unit_ConstructLAM_no_capture :: Assertion
unit_ConstructLAM_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ var "x") tEmptyHole)
    [Move Child1, Move Child1, ConstructLAM (Just "x")]

unit_ConstructLet_no_capture :: Assertion
unit_ConstructLet_no_capture =
  actionTestExpectFail
    isNeedEmptyHole
    NoSmartHoles
    (ann (lam "x" $ var "x") tEmptyHole)
    [Move Child1, Move Child1, ConstructLet (Just "x")]

unit_ConstructLetrec_no_capture :: Assertion
unit_ConstructLetrec_no_capture =
  actionTestExpectFail
    isNeedEmptyHole
    NoSmartHoles
    (ann (lam "x" $ var "x") tEmptyHole)
    [Move Child1, Move Child1, ConstructLetrec (Just "x")]

-- ensure trivial renaming is fine even though the "new"(="old") name is not
-- fresh
unit_RenameLam_noop :: Assertion
unit_RenameLam_noop =
  actionTest
    NoSmartHoles
    (ann (lam "x" $ var "x") tEmptyHole)
    [Move Child1, RenameLam "x"]
    (ann (lam "x" $ var "x") tEmptyHole)

unit_RenameLam_no_capture :: Assertion
unit_RenameLam_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ lam "y" $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLam "x"]

unit_RenameLAM_noop :: Assertion
unit_RenameLAM_noop =
  actionTest
    NoSmartHoles
    (ann (lAM "x" $ ann emptyHole $ tvar "x") tEmptyHole)
    [Move Child1, RenameLAM "x"]
    (ann (lAM "x" $ ann emptyHole $ tvar "x") tEmptyHole)

unit_RenameLAM_no_capture :: Assertion
unit_RenameLAM_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lAM "x" $ lAM "y" $ ann emptyHole $ tvar "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLAM "x"]

unit_RenameLet_noop :: Assertion
unit_RenameLet_noop =
  actionTest
    NoSmartHoles
    (ann (lam "x" $ let_ "y" emptyHole $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLet "y"]
    (ann (lam "x" $ let_ "y" emptyHole $ var "x") tEmptyHole)

unit_RenameLet_no_capture_1 :: Assertion
unit_RenameLet_no_capture_1 =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ let_ "y" emptyHole $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLet "x"]

-- We forbid this case, even though lets do not scope over the bound expression
-- (only letrecs do)
unit_RenameLet_no_capture_2 :: Assertion
unit_RenameLet_no_capture_2 =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ let_ "y" (var "x") emptyHole) tEmptyHole)
    [Move Child1, Move Child1, RenameLet "x"]

unit_RenameLetrec_noop :: Assertion
unit_RenameLetrec_noop =
  actionTest
    NoSmartHoles
    (ann (lam "x" $ letrec "y" emptyHole tEmptyHole $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLet "y"]
    (ann (lam "x" $ letrec "y" emptyHole tEmptyHole $ var "x") tEmptyHole)

unit_RenameLetrec_no_capture_1 :: Assertion
unit_RenameLetrec_no_capture_1 =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ letrec "y" emptyHole tEmptyHole $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLet "x"]

unit_RenameLetrec_no_capture_2 :: Assertion
unit_RenameLetrec_no_capture_2 =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lam "x" $ letrec "y" emptyHole tEmptyHole $ var "x") tEmptyHole)
    [Move Child1, Move Child1, RenameLet "x"]

unit_ConstructTForall_no_capture :: Assertion
unit_ConstructTForall_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann emptyHole $ tforall "x" KType $ tvar "x")
    [EnterType, Move Child1, ConstructTForall (Just "x")]

unit_RenameForall_noop :: Assertion
unit_RenameForall_noop =
  actionTest
    NoSmartHoles
    (ann emptyHole $ tforall "x" KType $ tforall "y" KType $ tvar "x")
    [EnterType, Move Child1, RenameForall "y"]
    (ann emptyHole $ tforall "x" KType $ tforall "y" KType $ tvar "x")

unit_RenameForall_no_capture :: Assertion
unit_RenameForall_no_capture =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann emptyHole $ tforall "x" KType $ tforall "y" KType $ tvar "x")
    [EnterType, Move Child1, RenameForall "x"]

unit_ty_tm_same_namespace :: Assertion
unit_ty_tm_same_namespace =
  actionTestExpectFail
    isNameCapture
    NoSmartHoles
    (ann (lAM "a" $ con "Nil" `aPP` tvar "a") tEmptyHole)
    [Move Child1, Move Child1, ConstructLam (Just "a")]

-- * Helpers

isNameCapture :: ActionError -> Bool
isNameCapture = \case
  NameCapture -> True
  _ -> False

isNeedEmptyHole :: ActionError -> Bool
isNeedEmptyHole = \case
  NeedEmptyHole{} -> True
  _ -> False
