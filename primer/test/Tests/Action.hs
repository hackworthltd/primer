-- | Tests for action application.
module Tests.Action where

import Foreword

import Data.Generics.Uniplate.Data (universe)
import Gen.Core.Raw (
  evalExprGen,
  genExpr,
 )
import Hedgehog hiding (
  Action,
  Var,
 )
import Optics (over, view)
import Primer.Action (
  Action (..),
  ActionError (RefineError),
  Movement (..),
  applyActionsToExpr,
 )
import Primer.App (defaultTypeDefs)
import Primer.Core (
  Expr,
  Expr' (..),
  ExprMeta,
  ID (..),
  Kind (KType),
  TypeMeta,
  Value,
  VarRef (LocalVarRef),
  getID,
  _exprMeta,
  _exprTypeMeta,
  _metadata,
 )
import Primer.Core.DSL
import Primer.Typecheck (SmartHoles (NoSmartHoles, SmartHoles))
import Primer.Zipper (
  down,
  farthest,
  focus,
  target,
  unfocusExpr,
  unfocusType,
 )
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import TestM (evalTestM)

-- | The largest used ID in an expression
maxID :: Expr -> ID
maxID = maximum . map getID . universe

hprop_ConstructVar_succeeds_on_hole_when_in_scope :: Property
hprop_ConstructVar_succeeds_on_hole_when_in_scope = property $ do
  -- Generate \x -> ?
  let expr = fst $ create $ ann (lam "x" emptyHole) (tfun tEmptyHole tEmptyHole)
  annotateShow expr
  expr' <-
    either (\err -> footnoteShow err >> failure) pure $
      runTestActions
        NoSmartHoles
        (maxID expr)
        expr
        [SetCursor (getID expr), Move Child1, Move Child1, ConstructVar $ LocalVarRef "x"]

  -- Extract the same point in the resulting AST
  -- We should now find the variable "x"
  let result = (target . farthest down . focus) expr'
  case result of
    Var _ (LocalVarRef x) -> x === "x"
    _ -> annotateShow result >> failure

-- TODO: when we can generate well typed expressions, turn this back into a
-- property
unit_SetCursor_succeeds_when_ID_exists :: Assertion
unit_SetCursor_succeeds_when_ID_exists =
  actionTest
    NoSmartHoles
    (ann (lam "x" (lvar "x")) (tfun tEmptyHole tEmptyHole))
    [SetCursor 1]
    (ann (lam "x" (lvar "x")) (tfun tEmptyHole tEmptyHole))

hprop_SetCursor_fails_when_ID_doesn't_exist :: Property
hprop_SetCursor_fails_when_ID_doesn't_exist = property $ do
  -- TODO: generate a random list of actions to run
  let actions = [SetCursor (-1)]
  e <- forAll $ evalExprGen 0 genExpr
  either (const success) (\r -> annotateShow r >> failure) $ runTestActions NoSmartHoles (maxID e) e actions

unit_1 :: Assertion
unit_1 =
  actionTest
    NoSmartHoles
    (ann (lam "x" (lam "y" (app emptyHole (lvar "y")))) (tfun tEmptyHole tEmptyHole))
    [Move Child1, Move Child1, Move Child1, Move Child1, ConstructVar $ LocalVarRef "x"]
    (ann (lam "x" (lam "y" (app (lvar "x") (lvar "y")))) (tfun tEmptyHole tEmptyHole))

-- | Constructing a variable succeeds in an empty hole
unit_2 :: Assertion
unit_2 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (lam "x" (app emptyHole emptyHole)))
        (tfun (tfun tEmptyHole tEmptyHole) (tfun tEmptyHole tEmptyHole))
    )
    [ Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , ConstructVar $ LocalVarRef "f"
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "x"
    ]
    ( ann
        ( lam
            "f"
            ( lam
                "x"
                (app (lvar "f") (lvar "x"))
            )
        )
        (tfun (tfun tEmptyHole tEmptyHole) (tfun tEmptyHole tEmptyHole))
    )

-- | Constructing an arrow succeeds in a type hole
unit_3 :: Assertion
unit_3 =
  actionTest
    NoSmartHoles
    (ann emptyHole tEmptyHole)
    [EnterType, ConstructArrowL]
    (ann emptyHole (tfun tEmptyHole tEmptyHole))

unit_4 :: Assertion
unit_4 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (lam "x" (app (lvar "f") (lvar "x"))))
        (tfun (tfun tEmptyHole tEmptyHole) (tfun tEmptyHole tEmptyHole))
    )
    [Move Child1, Move Child1, Move Child1, Move Child2, ConstructAnn]
    ( ann
        ( lam
            "f"
            ( lam
                "x"
                (app (lvar "f") (ann (lvar "x") tEmptyHole))
            )
        )
        (tfun (tfun tEmptyHole tEmptyHole) (tfun tEmptyHole tEmptyHole))
    )

unit_5 :: Assertion
unit_5 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (app (lvar "f") emptyHole))
        (tfun (tfun tEmptyHole tEmptyHole) tEmptyHole)
    )
    [Move Child1, Move Child1, Move Child2, ConstructLam (Just "x")]
    ( ann
        (lam "f" (app (lvar "f") (lam "x" emptyHole)))
        (tfun (tfun tEmptyHole tEmptyHole) tEmptyHole)
    )

-- We can construct a lambda without supplying a name, and one will be
-- automatically generated.
unit_6 :: Assertion
unit_6 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (app (lvar "f") emptyHole))
        (tfun (tfun tEmptyHole tEmptyHole) tEmptyHole)
    )
    [Move Child1, Move Child1, Move Child2, ConstructLam Nothing]
    ( ann
        (lam "f" (app (lvar "f") (lam "a11" emptyHole)))
        (tfun (tfun tEmptyHole tEmptyHole) tEmptyHole)
    )

unit_7 :: Assertion
unit_7 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (lam "g" (app (lvar "f") (hole (lvar "g")))))
        ( tfun
            (tfun (tcon "Nat") (tcon "Nat"))
            ( tfun
                (tfun (tcon "Nat") (tcon "Nat"))
                (tcon "Nat")
            )
        )
    )
    [Move Child1, Move Child1, Move Child1, Move Child2, Move Child1, ConstructApp]
    ( ann
        (lam "f" (lam "g" (app (lvar "f") (hole (app (lvar "g") emptyHole)))))
        ( tfun
            (tfun (tcon "Nat") (tcon "Nat"))
            ( tfun
                (tfun (tcon "Nat") (tcon "Nat"))
                (tcon "Nat")
            )
        )
    )

unit_8 :: Assertion
unit_8 =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructAnn
    , EnterType
    , ConstructArrowL
    , Move Child1
    , ConstructTCon "Bool"
    , Move Parent
    , Move Child2
    , ConstructTCon "Bool"
    , ExitType
    , Move Child1
    , ConstructLam (Just "x")
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move Parent
    , ConstructApp
    , Move Child2
    , ConstructCon "True"
    ]
    (app (ann (lam "x" (lvar "x")) (tfun (tcon "Bool") (tcon "Bool"))) (con "True"))

unit_9 :: Assertion
unit_9 =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructLet (Just "x")
    , Move Child1
    , ConstructCon "True"
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "x"
    ]
    (let_ "x" (con "True") (lvar "x"))

unit_construct_arrow_left :: Assertion
unit_construct_arrow_left =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon "Bool"))
    [EnterType, ConstructArrowL, Move Child2, ConstructTCon "Nat"]
    (ann emptyHole (tfun (tcon "Bool") (tcon "Nat")))

unit_construct_arrow_right :: Assertion
unit_construct_arrow_right =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon "Bool"))
    [EnterType, ConstructArrowR, Move Child1, ConstructTCon "Nat"]
    (ann emptyHole (tfun (tcon "Nat") (tcon "Bool")))

unit_construct_letrec :: Assertion
unit_construct_letrec =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructLetrec (Just "x")
    , EnterType
    , ConstructTCon "Bool"
    , ExitType
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "x"
    ]
    (letrec "x" (lvar "x") (tcon "Bool") (lvar "x"))

unit_rename_let :: Assertion
unit_rename_let =
  actionTest
    NoSmartHoles
    (let_ "x" (con "True") (lvar "x"))
    [RenameLet "y"]
    (let_ "y" (con "True") (lvar "y"))

unit_rename_letrec :: Assertion
unit_rename_letrec =
  actionTest
    NoSmartHoles
    (letrec "x" (lvar "x") (tcon "Bool") (lvar "x"))
    [RenameLet "y"]
    (letrec "y" (lvar "y") (tcon "Bool") (lvar "y"))

unit_rename_lam :: Assertion
unit_rename_lam =
  actionTest
    NoSmartHoles
    (ann (lam "x" (app (lvar "x") (con "False"))) tEmptyHole)
    [Move Child1, RenameLam "y"]
    (ann (lam "y" (app (lvar "y") (con "False"))) tEmptyHole)

unit_rename_lam_2 :: Assertion
unit_rename_lam_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann (lam "y" (lam "x" (app (lvar "x") (lvar "y")))) tEmptyHole)
    [Move Child1, Move Child1, RenameLam "y"]

unit_rename_LAM :: Assertion
unit_rename_LAM =
  actionTest
    NoSmartHoles
    (ann (lAM "a" (aPP (con "Nil") (tvar "a"))) (tforall "b" KType $ tapp (tcon "List") (tvar "b")))
    [Move Child1, RenameLAM "b"]
    (ann (lAM "b" (aPP (con "Nil") (tvar "b"))) (tforall "b" KType $ tapp (tcon "List") (tvar "b")))

unit_rename_LAM_2 :: Assertion
unit_rename_LAM_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann (lAM "b" (lAM "a" (aPP (con "Nil") (tvar "b")))) tEmptyHole)
    [Move Child1, Move Child1, RenameLAM "b"]

unit_convert_let_to_letrec :: Assertion
unit_convert_let_to_letrec =
  actionTest
    NoSmartHoles
    (let_ "x" (con "True") (lvar "x"))
    [ConvertLetToLetrec]
    (letrec "x" (con "True") tEmptyHole (lvar "x"))

unit_delete_type :: Assertion
unit_delete_type =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon "Nat"))
    [EnterType, Delete]
    (ann emptyHole tEmptyHole)

unit_setcursor_type :: Assertion
unit_setcursor_type =
  -- Note: we guess that the ID of the tcon will be 2
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon "Nat"))
    [SetCursor 2, Delete]
    (ann emptyHole tEmptyHole)

unit_bad_constructor :: Assertion
unit_bad_constructor =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    emptyHole
    [ConstructCon "NotARealConstructor"]

unit_bad_type_constructor :: Assertion
unit_bad_type_constructor =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann emptyHole tEmptyHole)
    [EnterType, ConstructTCon "NotARealTypeConstructor"]

unit_bad_app :: Assertion
unit_bad_app =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (con "True")
    [ConstructApp]

unit_insert_expr_in_type :: Assertion
unit_insert_expr_in_type =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann emptyHole tEmptyHole)
    [EnterType, ConstructCon "True"]

unit_bad_lambda :: Assertion
unit_bad_lambda =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    emptyHole
    [ConstructLam (Just "x")]

unit_enter_emptyHole :: Assertion
unit_enter_emptyHole =
  actionTest
    NoSmartHoles
    emptyHole
    [EnterHole, ConstructCon "True"]
    (hole $ con "True")

unit_enter_nonEmptyHole :: Assertion
unit_enter_nonEmptyHole =
  actionTest
    NoSmartHoles
    (hole emptyHole)
    [Move Child1, ConstructCon "True"]
    (hole $ con "True")

unit_bad_enter_hole :: Assertion
unit_bad_enter_hole =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (hole emptyHole)
    [EnterHole]

-- Test creation of cases
unit_case_create :: Assertion
unit_case_create =
  actionTest
    NoSmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , EnterHole
    , ConstructVar $ LocalVarRef "x"
    , ConstructAnn
    , Move Child1
    , ConstructCase
    , Move (Branch "True")
    , ConstructCon "Zero"
    ]
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )

-- Test tidying up after the creation of cases
unit_case_tidy :: Assertion
unit_case_tidy =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [Move Child1, Move Child1, FinishHole, RemoveAnn]
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )

-- Test movement into RHS of branches
unit_case_move_branch_1 :: Assertion
unit_case_move_branch_1 =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , Move (Branch "Zero")
    , ConstructCon "Zero"
    , Move Parent
    , Move (Branch "Succ")
    , ConstructVar $ LocalVarRef "n"
    ]
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "Zero" [] (con "Zero"), branch "Succ" [("n", Nothing)] (lvar "n")]
                )
                tEmptyHole
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

-- Test movement into RHS of branches (case not wrapped in a hole)
unit_case_move_branch_2 :: Assertion
unit_case_move_branch_2 =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , Move (Branch "Zero")
    , ConstructCon "Zero"
    , Move Parent
    , Move (Branch "Succ")
    , ConstructVar $ LocalVarRef "n"
    ]
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch "Zero" [] (con "Zero"), branch "Succ" [("n", Nothing)] (lvar "n")]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

-- Test movement into scrutinee
unit_case_move_scrutinee_1 :: Assertion
unit_case_move_scrutinee_1 =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , SetMetadata "meta"
    ]
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (setMeta "meta" $ lvar "x")
                    [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

-- Test movement into scrutinee (case not wrapped in a hole)
unit_case_move_scrutinee_2 :: Assertion
unit_case_move_scrutinee_2 =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [Move Child1, Move Child1, Move Child1, SetMetadata "meta"]
    ( ann
        ( lam "x" $
            case_
              (setMeta "meta" $ lvar "x")
              [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

unit_bad_case_1 :: Assertion
unit_bad_case_1 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    ( ann
        (lam "x" $ hole $ lvar "x")
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [ConstructCase]

unit_bad_case_2 :: Assertion
unit_bad_case_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    emptyHole
    [ConstructCase]

unit_bad_case_3 :: Assertion
unit_bad_case_3 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch "True" [] emptyHole, branch "False" [] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [Move Child1, Move Child1, Move Child1, Move Child1, Move Child2]

-- You can also do a case on a hole of known type, if you don't want to write
-- the scrutinee first. However this does not help with the annoying dance to
-- get the types to match up. We side step that issue here by having the
-- scrutinee and the case result be the same type.
unit_case_on_hole :: Assertion
unit_case_on_hole =
  actionTest
    NoSmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , ConstructAnn
    , EnterType
    , ConstructTCon "Nat"
    , ExitType
    , ConstructCase
    ]
    ( ann
        ( lam "x" $
            case_
              (ann emptyHole $ tcon "Nat")
              [branch "Zero" [] emptyHole, branch "Succ" [("a13", Nothing)] emptyHole] -- NB: fragile names here
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

-- Changing the scrutinee is ok, as long as the type does not change
unit_case_fill_hole_scrut :: Assertion
unit_case_fill_hole_scrut =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            case_
              (ann emptyHole $ tcon "Nat")
              [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , RemoveAnn
    ]
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch "Zero" [] emptyHole, branch "Succ" [("n", Nothing)] emptyHole]
        )
        (tfun (tcon "Nat") (tcon "Nat"))
    )

-- Test creation of cases, with smart actions
unit_case_create_smart_on_term :: Assertion
unit_case_create_smart_on_term =
  actionTest
    SmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , ConstructCase
    , Move (Branch "True")
    , ConstructCon "Zero"
    ]
    ( ann
        ( lam
            "x"
            ( case_
                (lvar "x")
                [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
            )
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )

unit_case_create_smart_on_hole :: Assertion
unit_case_create_smart_on_hole =
  actionTest
    SmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon "Bool") (tcon "Nat"))
    )
    [ Move Child1
    , Move Child1
    , ConstructCase
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move (Branch "True")
    , ConstructCon "Zero"
    ]
    ( ann
        ( lam
            "x"
            ( case_
                (lvar "x")
                [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
            )
        )
        (tfun (tcon "Bool") (tcon "Nat"))
    )

unit_case_change_smart_scrutinee_type :: Assertion
unit_case_change_smart_scrutinee_type =
  actionTest
    SmartHoles
    ( ann
        ( case_
            (con "True")
            [branch "True" [] (con "Zero"), branch "False" [] emptyHole]
        )
        (tcon "Nat")
    )
    [ Move Child1
    , Move Child1
    , Delete
    , ConstructCon "Zero"
    ]
    ( ann
        ( case_
            (con "Zero")
            [branch "Zero" [] emptyHole, branch "Succ" [("a11", Nothing)] emptyHole] -- fragile names here
        )
        (tcon "Nat")
    )

unit_constructAPP :: Assertion
unit_constructAPP =
  actionTest
    NoSmartHoles
    (con "Nil")
    [ConstructAPP, EnterType, ConstructTCon "Bool"]
    (con "Nil" `aPP` tcon "Bool")

unit_constructLAM :: Assertion
unit_constructLAM =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [Move Child1, ConstructLAM (Just "a"), ConstructCon "True"]
    (lAM "a" (con "True") `ann` tEmptyHole)

unit_construct_TForall :: Assertion
unit_construct_TForall =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTForall (Just "a")]
    (ann emptyHole $ tforall "a" KType tEmptyHole)

unit_rename_TForall :: Assertion
unit_rename_TForall =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tforall "a" KType (tapp (tcon "List") (tvar "a")))
    [EnterType, RenameForall "b"]
    (emptyHole `ann` tforall "b" KType (tapp (tcon "List") (tvar "b")))

unit_rename_TForall_2 :: Assertion
unit_rename_TForall_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (emptyHole `ann` tforall "b" KType (tforall "a" KType $ tapp (tcon "List") (tvar "b")))
    [EnterType, Move Child1, RenameLAM "b"]

unit_construct_TForall_TVar :: Assertion
unit_construct_TForall_TVar =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTForall (Just "a"), Move Child1, ConstructTVar "a"]
    (ann emptyHole $ tforall "a" KType $ tvar "a")

unit_poly_1 :: Assertion
unit_poly_1 =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructLet (Just "id")
    , Move Child1
    , ConstructAnn
    , EnterType
    , ConstructTForall (Just "a")
    , Move Child1
    , ConstructTVar "a"
    , ConstructArrowL
    , Move Child2
    , ConstructTVar "a"
    , Move Parent
    , Move Parent
    , ExitType
    , Move Child1
    , ConstructLAM (Just "a")
    , ConstructLam (Just "x")
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move Parent
    , Move Parent
    , Move Parent
    , Move Child2
    , ConstructApp
    , Move Child1
    , ConstructAPP
    , Move Child1
    , ConstructVar $ LocalVarRef "id"
    , Move Parent
    , EnterType
    , ConstructTForall (Just "b")
    , Move Child1
    , ConstructTVar "b"
    , ConstructArrowL
    , Move Child2
    , ConstructTVar "b"
    , Move Parent
    , Move Parent
    , ExitType
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "id"
    ]
    ( let_ "id" (ann (lAM "a" $ lam "x" $ lvar "x") (tforall "a" KType $ tfun (tvar "a") (tvar "a"))) $
        app (aPP (lvar "id") (tforall "b" KType $ tfun (tvar "b") (tvar "b"))) (lvar "id")
    )

unit_constructTApp :: Assertion
unit_constructTApp =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTApp, Move Child1, ConstructTCon "List", Move Parent, Move Child2, ConstructTCon "Bool"]
    (emptyHole `ann` (tcon "List" `tapp` tcon "Bool"))

unit_construct_lam :: Assertion
unit_construct_lam =
  actionTest
    SmartHoles
    (con "True")
    [ConstructLam (Just "x")]
    (ann (lam "x" (con "True")) tEmptyHole)

unit_construct_LAM :: Assertion
unit_construct_LAM =
  actionTest
    SmartHoles
    (con "True")
    [ConstructLAM (Just "a")]
    (ann (lAM "a" (con "True")) tEmptyHole)

unit_smart_type_1 :: Assertion
unit_smart_type_1 =
  actionTest
    SmartHoles
    (emptyHole `ann` tcon "Nat")
    [EnterType, ConstructTApp, Move Child1]
    (emptyHole `ann` (thole (tcon "Nat") `tapp` tEmptyHole))

unit_smart_type_2 :: Assertion
unit_smart_type_2 =
  actionTest
    SmartHoles
    (emptyHole `ann` thole (tcon "List"))
    [EnterType, ConstructTApp]
    (emptyHole `ann` (tcon "List" `tapp` tEmptyHole))

unit_refine_1 :: Assertion
unit_refine_1 =
  actionTestExpectFail
    (\case RefineError _ -> True; _ -> False)
    NoSmartHoles
    emptyHole
    [ConstructRefinedCon "Nil"]

unit_refine_2 :: Assertion
unit_refine_2 =
  actionTest
    NoSmartHoles
    (emptyHole `ann` (tcon "List" `tapp` tcon "Nat"))
    [Move Child1, ConstructRefinedCon "Nil"]
    ((con "Nil" `aPP` tcon "Nat") `ann` (tcon "List" `tapp` tcon "Nat"))

unit_refine_3 :: Assertion
unit_refine_3 =
  actionTest
    NoSmartHoles
    (emptyHole `ann` (tcon "List" `tapp` tEmptyHole))
    [Move Child1, ConstructRefinedCon "Nil"]
    ((con "Nil" `aPP` tEmptyHole) `ann` (tcon "List" `tapp` tEmptyHole))

unit_refine_4 :: Assertion
unit_refine_4 =
  actionTest
    NoSmartHoles
    (let_ "nil" (con "Nil") $ emptyHole `ann` (tcon "List" `tapp` tcon "Nat"))
    [Move Child2, Move Child1, InsertRefinedVar $ LocalVarRef "nil"]
    (let_ "nil" (con "Nil") $ (lvar "nil" `aPP` tcon "Nat") `ann` (tcon "List" `tapp` tcon "Nat"))

unit_refine_5 :: Assertion
unit_refine_5 =
  actionTest
    NoSmartHoles
    (let_ "nil" (con "Nil") $ emptyHole `ann` (tcon "List" `tapp` tEmptyHole))
    [Move Child2, Move Child1, InsertRefinedVar $ LocalVarRef "nil"]
    (let_ "nil" (con "Nil") $ (lvar "nil" `aPP` tEmptyHole) `ann` (tcon "List" `tapp` tEmptyHole))

-- * Helpers

-- | Apply the actions to the input expression and test that the result matches
-- the expected output, up to renaming of IDs and changing cached types.
actionTest :: SmartHoles -> S Expr -> [Action] -> S Expr -> Assertion
actionTest sh inputExpr actions expectedOutput = do
  let (expr, i) = create inputExpr
  result <- either (assertFailure . show) pure $ runTestActions sh i expr actions
  let (expected, _) = create expectedOutput
  -- Compare result to input, ignoring any difference in metadata
  -- NB: we don't compare up-to-alpha, as names should be determined by the
  -- actions on-the-nose
  clearMeta result @?= clearMeta expected
  where
    -- Clear the backend-created metadata (IDs and cached types) in the given expression
    clearMeta :: Expr' ExprMeta TypeMeta -> Expr' (Maybe Value) (Maybe Value)
    clearMeta = over _exprMeta (view _metadata) . over _exprTypeMeta (view _metadata)

-- | Attempt to apply the actions to the input expression and test that they
-- in fact cause an error to be raised.
actionTestExpectFail :: (ActionError -> Bool) -> SmartHoles -> S Expr -> [Action] -> Assertion
actionTestExpectFail f sh expr actions =
  case runTestActions sh i e actions of
    Right _ -> assertFailure "action succeeded"
    Left err | not (f err) -> assertFailure $ "error does not satisfy predicate: " <> show err
    _ -> pure ()
  where
    (e, i) = create expr

-- | Run the actions against the given AST, setting the ID counter to (1+) the
-- given value. Fails if the actions fail.
runTestActions :: SmartHoles -> ID -> Expr -> [Action] -> Either ActionError Expr
runTestActions sh i expr actions =
  either unfocusExpr (unfocusExpr . unfocusType)
    <$> evalTestM (i + 1) (applyActionsToExpr sh defaultTypeDefs expr actions)
