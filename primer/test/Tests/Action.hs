-- | Tests for action application.
module Tests.Action where

import Foreword
import Prelude (error)

import Hedgehog hiding (
  Action,
  Property,
  Var,
  property,
 )
import Optics (maximumOf)
import Primer.Action (
  Action (..),
  ActionError (CaseBindsClash, NameCapture),
  BranchMove (..),
  Movement (..),
  applyActionsToExpr,
 )
import Primer.Builtins
import Primer.Builtins.DSL (listOf)
import Primer.Core (
  Expr,
  Expr' (..),
  ID (..),
  Kind' (KType),
  Meta (Meta),
  Pattern (PatCon, PatPrim),
  PrimCon (PrimChar),
  TmVarRef (LocalVarRef),
  Type' (TEmptyHole, TForall),
  getID,
 )
import Primer.Core.DSL
import Primer.Core.Utils (exprIDs)
import Primer.Gen.Core.Raw (
  evalExprGen,
  genExpr,
 )
import Primer.Module (builtinModule, primitiveModule)
import Primer.Primitives (tChar, tInt)
import Primer.Test.TestM (evalTestM)
import Primer.Test.Util (
  clearMeta,
  constructSaturatedCon,
  constructTCon,
  toQualText,
 )
import Primer.Typecheck (SmartHoles (NoSmartHoles, SmartHoles))
import Primer.Zipper (
  down,
  farthest,
  focus,
  target,
  unfocusExpr,
  unfocusLoc,
 )
import Tasty (Property, property)
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))

-- Note: the 'fromMaybe' will only ever see a 'Just' (i.e. could be replaced
-- with 'fromJust'), since 'exprIDs' will always target at least one ID (namely,
-- the ID at the root)
maxID :: Expr -> ID
maxID e = fromMaybe (getID e) $ maximumOf exprIDs e

unit_maxID :: Assertion
unit_maxID =
  let m :: ID -> Meta (Maybe a)
      m i = Meta i Nothing Nothing
      m' i = Meta i () Nothing
      h = EmptyHole . m
      expr a b c d e f g = App (m a) (h b) (APP (m c) (h d) $ TForall (m e) "a" (KType (m' f)) (TEmptyHole $ m g))
   in for_ (permutations [0 .. 6]) $ \case
        [a, b, c, d, e, f, g] -> maxID (expr a b c d e f g) @?= 6
        _ -> error "impossible"

tasty_ConstructVar_succeeds_on_hole_when_in_scope :: Property
tasty_ConstructVar_succeeds_on_hole_when_in_scope = property $ do
  -- Generate \x -> ?
  let expr = create' $ ann (lam "x" emptyHole) (tfun tEmptyHole tEmptyHole)
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

tasty_SetCursor_fails_when_ID_doesn't_exist :: Property
tasty_SetCursor_fails_when_ID_doesn't_exist = property $ do
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
        (lam "f" (app (lvar "f") (lam "a42" emptyHole)))
        (tfun (tfun tEmptyHole tEmptyHole) tEmptyHole)
    )

unit_7 :: Assertion
unit_7 =
  actionTest
    NoSmartHoles
    ( ann
        (lam "f" (lam "g" (app (lvar "f") (hole (lvar "g")))))
        ( tfun
            (tfun (tcon tNat) (tcon tNat))
            ( tfun
                (tfun (tcon tNat) (tcon tNat))
                (tcon tNat)
            )
        )
    )
    [Move Child1, Move Child1, Move Child1, Move Child2, Move Child1, ConstructApp]
    ( ann
        (lam "f" (lam "g" (app (lvar "f") (hole (app (lvar "g") emptyHole)))))
        ( tfun
            (tfun (tcon tNat) (tcon tNat))
            ( tfun
                (tfun (tcon tNat) (tcon tNat))
                (tcon tNat)
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
    , constructTCon tBool
    , Move Parent
    , Move Child2
    , constructTCon tBool
    , ExitType
    , Move Child1
    , ConstructLam (Just "x")
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move Parent
    , ConstructApp
    , Move Child2
    , constructSaturatedCon cTrue
    ]
    (app (ann (lam "x" (lvar "x")) (tfun (tcon tBool) (tcon tBool))) (con0 cTrue))

unit_9 :: Assertion
unit_9 =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructLet (Just "x")
    , Move Child1
    , ConstructAnn
    , Move Child1
    , constructSaturatedCon cTrue
    , Move Parent
    , EnterType
    , constructTCon tBool
    , ExitType
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "x"
    ]
    (let_ "x" (con0 cTrue `ann` tcon tBool) (lvar "x"))

unit_construct_arrow_left :: Assertion
unit_construct_arrow_left =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon tBool))
    [EnterType, ConstructArrowL, Move Child2, constructTCon tNat]
    (ann emptyHole (tfun (tcon tBool) (tcon tNat)))

unit_construct_arrow_right :: Assertion
unit_construct_arrow_right =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon tBool))
    [EnterType, ConstructArrowR, Move Child1, constructTCon tNat]
    (ann emptyHole (tfun (tcon tNat) (tcon tBool)))

unit_construct_letrec :: Assertion
unit_construct_letrec =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructLetrec (Just "x")
    , EnterType
    , constructTCon tBool
    , ExitType
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , Move Child2
    , ConstructVar $ LocalVarRef "x"
    ]
    (letrec "x" (lvar "x") (tcon tBool) (lvar "x"))

unit_rename_let :: Assertion
unit_rename_let =
  actionTest
    NoSmartHoles
    (let_ "x" (con0 cTrue `ann` tEmptyHole) (lvar "x"))
    [RenameLet "y"]
    (let_ "y" (con0 cTrue `ann` tEmptyHole) (lvar "y"))

unit_rename_letrec :: Assertion
unit_rename_letrec =
  actionTest
    NoSmartHoles
    (letrec "x" (lvar "x") (tcon tBool) (lvar "x"))
    [RenameLet "y"]
    (letrec "y" (lvar "y") (tcon tBool) (lvar "y"))

-- If the let is shadowing an outer binder, we can rename to un-shadow it
unit_rename_let_shadowed :: Assertion
unit_rename_let_shadowed =
  actionTest
    NoSmartHoles
    (let_ "x" emptyHole $ let_ "x" (lvar "x") (lvar "x"))
    [Move Child2, RenameLet "y"]
    (let_ "x" emptyHole $ let_ "y" (lvar "x") (lvar "y"))

-- Renaming a let can cause shadowing, but this is ok
unit_rename_let_shadows :: Assertion
unit_rename_let_shadows =
  actionTest
    NoSmartHoles
    (ann (lam "x" $ let_ "y" (lvar "x") emptyHole) tEmptyHole)
    [Move Child1, Move Child1, RenameLet "x"]
    (ann (lam "x" $ let_ "x" (lvar "x") emptyHole) tEmptyHole)

unit_rename_lam :: Assertion
unit_rename_lam =
  actionTest
    NoSmartHoles
    (ann (lam "x" (app (lvar "x") (con0 cFalse))) tEmptyHole)
    [Move Child1, RenameLam "y"]
    (ann (lam "y" (app (lvar "y") (con0 cFalse))) tEmptyHole)

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
    (ann (lAM "a" (emptyHole `aPP` tvar "a")) (tforall "b" ktype $ listOf (tvar "b")))
    [Move Child1, RenameLAM "b"]
    (ann (lAM "b" (emptyHole `aPP` tvar "b")) (tforall "b" ktype $ listOf (tvar "b")))

unit_rename_LAM_2 :: Assertion
unit_rename_LAM_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann (lAM "b" (lAM "a" (emptyHole `aPP` tvar "b"))) tEmptyHole)
    [Move Child1, Move Child1, RenameLAM "b"]

unit_rename_LAM_3 :: Assertion
unit_rename_LAM_3 =
  actionTestExpectFail
    ( \case
        NameCapture -> True
        _ -> False
    )
    NoSmartHoles
    (lam "x" (lAM "y" $ lvar "x") `ann` tEmptyHole)
    [Move Child1, Move Child1, RenameLAM "x"]

unit_convert_let_to_letrec :: Assertion
unit_convert_let_to_letrec =
  actionTest
    NoSmartHoles
    (let_ "x" (con0 cTrue) (lvar "x"))
    [ConvertLetToLetrec]
    (letrec "x" (con0 cTrue) tEmptyHole (lvar "x"))

unit_delete_type :: Assertion
unit_delete_type =
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon tNat))
    [EnterType, Delete]
    (ann emptyHole tEmptyHole)

unit_setcursor_type :: Assertion
unit_setcursor_type =
  -- Note: we guess that the ID of the tcon will be 2
  actionTest
    NoSmartHoles
    (ann emptyHole (tcon tNat))
    [SetCursor 2, Delete]
    (ann emptyHole tEmptyHole)

unit_bad_constructor :: Assertion
unit_bad_constructor =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    emptyHole
    [ConstructSaturatedCon (["M"], "NotARealConstructor")]

unit_bad_type_constructor :: Assertion
unit_bad_type_constructor =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann emptyHole tEmptyHole)
    [EnterType, ConstructTCon (["M"], "NotARealTypeConstructor")]

unit_bad_app :: Assertion
unit_bad_app =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (con0 cTrue)
    [ConstructApp]

unit_insert_expr_in_type :: Assertion
unit_insert_expr_in_type =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (ann emptyHole tEmptyHole)
    [EnterType, constructSaturatedCon cTrue]

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
    [EnterHole, constructSaturatedCon cTrue]
    (hole $ con0 cTrue)

unit_enter_nonEmptyHole :: Assertion
unit_enter_nonEmptyHole =
  actionTest
    NoSmartHoles
    (hole emptyHole)
    [Move Child1, constructSaturatedCon cTrue]
    (hole $ con0 cTrue)

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
        (tfun (tcon tBool) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , EnterHole
    , ConstructVar $ LocalVarRef "x"
    , ConstructCase
    , Move (Branch $ Pattern $ PatCon cTrue)
    , constructSaturatedCon cZero
    ]
    ( ann
        ( lam "x" $
            hole $
              case_
                (lvar "x")
                [branch cTrue [] (con0 cZero), branch cFalse [] emptyHole]
        )
        (tfun (tcon tBool) (tcon tNat))
    )

-- Test tidying up after the creation of cases
unit_case_tidy :: Assertion
unit_case_tidy =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            hole $
              case_
                (lvar "x")
                [branch cTrue [] (con0 cZero), branch cFalse [] emptyHole]
        )
        (tfun (tcon tBool) (tcon tNat))
    )
    [Move Child1, Move Child1, FinishHole]
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch cTrue [] (con0 cZero), branch cFalse [] emptyHole]
        )
        (tfun (tcon tBool) (tcon tNat))
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
                    [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon tNat) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , Move Child1
    , Move Child1
    , Move (Branch $ Pattern $ PatCon cZero)
    , constructSaturatedCon cZero
    , Move Parent
    , Move (Branch $ Pattern $ PatCon cSucc)
    , ConstructVar $ LocalVarRef "n"
    ]
    ( ann
        ( lam "x" $
            hole $
              ann
                ( case_
                    (lvar "x")
                    [branch cZero [] (con0 cZero), branch cSucc [("n", Nothing)] (lvar "n")]
                )
                tEmptyHole
        )
        (tfun (tcon tNat) (tcon tNat))
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
              [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
        )
        (tfun (tcon tNat) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , Move (Branch $ Pattern $ PatCon cZero)
    , constructSaturatedCon cZero
    , Move Parent
    , Move (Branch $ Pattern $ PatCon cSucc)
    , ConstructVar $ LocalVarRef "n"
    ]
    ( ann
        ( lam "x" $
            case_
              (lvar "x")
              [branch cZero [] (con0 cZero), branch cSucc [("n", Nothing)] (lvar "n")]
        )
        (tfun (tcon tNat) (tcon tNat))
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
                    [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon tNat) (tcon tNat))
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
                    [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon tNat) (tcon tNat))
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
              [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
        )
        (tfun (tcon tNat) (tcon tNat))
    )
    [Move Child1, Move Child1, Move Child1, SetMetadata "meta"]
    ( ann
        ( lam "x" $
            case_
              (setMeta "meta" $ lvar "x")
              [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
        )
        (tfun (tcon tNat) (tcon tNat))
    )

unit_bad_case_1 :: Assertion
unit_bad_case_1 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    ( ann
        (lam "x" $ hole $ lvar "x")
        (tfun (tcon tBool) (tcon tNat))
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
                    [branch cTrue [] emptyHole, branch cFalse [] emptyHole]
                )
                tEmptyHole
        )
        (tfun (tcon tBool) (tcon tNat))
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
        (tfun (tcon tNat) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , ConstructAnn
    , EnterType
    , constructTCon tNat
    , ExitType
    , ConstructCase
    ]
    ( ann
        ( lam "x" $
            case_
              (ann emptyHole $ tcon tNat)
              [branch cZero [] emptyHole, branch cSucc [("a44", Nothing)] emptyHole] -- NB: fragile names here
        )
        (tfun (tcon tNat) (tcon tNat))
    )

-- Changing the scrutinee is ok, as long as the type does not change
unit_case_fill_hole_scrut :: Assertion
unit_case_fill_hole_scrut =
  actionTest
    NoSmartHoles
    ( ann
        ( lam "x" $
            case_
              (ann emptyHole $ tcon tNat)
              [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
        )
        (tfun (tcon tNat) (tcon tNat))
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
              [branch cZero [] emptyHole, branch cSucc [("n", Nothing)] emptyHole]
        )
        (tfun (tcon tNat) (tcon tNat))
    )

-- Test creation of cases, with smart actions
unit_case_create_smart_on_term :: Assertion
unit_case_create_smart_on_term =
  actionTest
    SmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon tBool) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , ConstructCase
    , Move (Branch $ Pattern $ PatCon cTrue)
    , constructSaturatedCon cZero
    ]
    ( ann
        ( lam
            "x"
            ( case_
                (lvar "x")
                [branch cTrue [] (con0 cZero), branch cFalse [] emptyHole]
            )
        )
        (tfun (tcon tBool) (tcon tNat))
    )

unit_case_create_smart_on_hole :: Assertion
unit_case_create_smart_on_hole =
  actionTest
    SmartHoles
    ( ann
        (lam "x" emptyHole)
        (tfun (tcon tBool) (tcon tNat))
    )
    [ Move Child1
    , Move Child1
    , ConstructCase
    , Move Child1
    , ConstructVar $ LocalVarRef "x"
    , Move Parent
    , AddCaseBranch $ toQualText cTrue
    , Move (Branch $ Pattern $ PatCon cTrue)
    , constructSaturatedCon cZero
    ]
    ( ann
        ( lam
            "x"
            ( caseFB_
                (lvar "x")
                [branch cTrue [] (con0 cZero)]
                emptyHole
            )
        )
        (tfun (tcon tBool) (tcon tNat))
    )

unit_case_change_smart_scrutinee_type :: Assertion
unit_case_change_smart_scrutinee_type =
  actionTest
    SmartHoles
    ( ann
        ( case_
            (con0 cTrue `ann` tcon tBool)
            [branch cTrue [] (con0 cZero), branch cFalse [] emptyHole]
        )
        (tcon tNat)
    )
    [ Move Child1
    , Move Child1
    , Delete
    , ConstructAnn
    , EnterType
    , constructTCon tNat
    ]
    ( ann
        ( caseFB_
            (emptyHole `ann` tcon tNat)
            [] -- since an intermediate stage is scrutinee type is hole, we remove all branches
            emptyHole -- then when we set type is Nat, we only create a fallback branch
        )
        (tcon tNat)
    )

unit_rename_case_binding :: Assertion
unit_rename_case_binding =
  actionTest
    NoSmartHoles
    ( case_
        (emptyHole `ann` (tcon tList `tapp` tcon tBool))
        [ branch cNil [] emptyHole
        , branch cCons [("a", Nothing), ("b", Nothing)] emptyHole
        ]
        `ann` tcon tNat
    )
    [SetCursor 8, RenameCaseBinding "c"]
    ( case_
        (emptyHole `ann` (tcon tList `tapp` tcon tBool))
        [ branch cNil [] emptyHole
        , branch cCons [("c", Nothing), ("b", Nothing)] emptyHole
        ]
        `ann` tcon tNat
    )

unit_same_rename_case_binding :: Assertion
unit_same_rename_case_binding =
  actionTest
    NoSmartHoles
    ( case_
        (emptyHole `ann` (tcon tList `tapp` tcon tBool))
        [ branch cNil [] emptyHole
        , branch cCons [("a", Nothing), ("b", Nothing)] emptyHole
        ]
        `ann` tcon tNat
    )
    [SetCursor 8, RenameCaseBinding "a"]
    ( case_
        (emptyHole `ann` (tcon tList `tapp` tcon tBool))
        [ branch cNil [] emptyHole
        , branch cCons [("a", Nothing), ("b", Nothing)] emptyHole
        ]
        `ann` tcon tNat
    )

unit_rename_case_bind_clash :: Assertion
unit_rename_case_bind_clash =
  actionTestExpectFail
    (\case CaseBindsClash "b" ["b"] -> True; _ -> False)
    NoSmartHoles
    ( case_
        (emptyHole `ann` (tcon tList `tapp` tcon tBool))
        [ branch cNil [] emptyHole
        , branch cCons [("a", Nothing), ("b", Nothing)] emptyHole
        ]
        `ann` tcon tNat
    )
    [SetCursor 8, RenameCaseBinding "b"]

unit_case_branches :: Assertion
unit_case_branches =
  let e cse = ann cse (tcon tBool)
      n = "a39"
      e0 =
        e $
          caseFB_
            (emptyHole `ann` tcon tNat)
            []
            (con0 cTrue)
      e1 =
        e $
          caseFB_
            (emptyHole `ann` tcon tNat)
            [branch cSucc [(n, Nothing)] $ con0 cTrue]
            (con0 cTrue)
      e2 =
        e $
          case_
            (emptyHole `ann` tcon tNat)
            [ branch cZero [] $ con0 cFalse
            , branch cSucc [(n, Nothing)] $ con0 cTrue
            ]
      e3 =
        e $
          caseFB_
            (emptyHole `ann` tcon tNat)
            [branch cSucc [(n, Nothing)] $ con0 cTrue]
            (con0 cFalse)
      e4 =
        e $
          caseFB_
            (emptyHole `ann` tcon tNat)
            []
            (con0 cFalse)
      e' cse = ann cse (tcon tMaybe `tapp` tcon tNat)
      e5 =
        e' $
          case_
            (emptyHole `ann` tcon tNat)
            [ branch cZero [] $ con0 cNothing
            , branch cSucc [("n", Nothing)] $ con1 cJust $ lvar "n"
            ]
      e6 =
        e' $
          caseFB_
            (emptyHole `ann` tcon tNat)
            [branch cZero [] $ con0 cNothing]
            (con1 cJust emptyHole)
      e7 =
        e' $
          caseFB_
            (emptyHole `ann` tcon tNat)
            [branch cZero [] $ con0 cNothing]
            (con1 cJust $ con0 cZero)
   in do
        actionTest
          SmartHoles
          e0
          [ Move Child1
          , AddCaseBranch $ toQualText cSucc
          ]
          e1
        actionTest
          SmartHoles
          e1
          [ Move Child1
          , AddCaseBranch $ toQualText cZero
          , Move (Branch $ Pattern $ PatCon cZero)
          , Delete
          , constructSaturatedCon cFalse
          ]
          e2
        actionTest
          SmartHoles
          e2
          [ Move Child1
          , DeleteCaseBranch $ toQualText cZero
          ]
          e3
        actionTest
          SmartHoles
          e3
          [ Move Child1
          , DeleteCaseBranch $ toQualText cSucc
          ]
          e4
        actionTest
          SmartHoles
          e5
          [ Move Child1
          , DeleteCaseBranch $ toQualText cSucc
          ]
          e6
        actionTest
          SmartHoles
          e6
          [ Move Child1
          , Move (Branch Fallback)
          , Move (ConChild 0)
          , constructSaturatedCon cZero
          ]
          e7

unit_case_prim :: Assertion
unit_case_prim =
  let e cse = ann cse (tcon tBool)
      e0 =
        e $
          caseFB_
            (emptyHole `ann` tcon tChar)
            []
            (con0 cTrue)
      e1 =
        e $
          caseFB_
            (emptyHole `ann` tcon tChar)
            [branchPrim (PrimChar 'x') $ con0 cTrue]
            (con0 cTrue)
      e2 =
        e $
          caseFB_
            (emptyHole `ann` tcon tChar)
            [branchPrim (PrimChar 'x') $ con0 cFalse]
            (con0 cTrue)
      e3 =
        e $
          caseFB_
            (emptyHole `ann` tcon tChar)
            []
            (con0 cTrue)
   in do
        actionTest
          SmartHoles
          e0
          [ Move Child1
          , AddCaseBranchPrim $ PrimChar 'x'
          ]
          e1
        actionTest
          SmartHoles
          e1
          [ Move Child1
          , Move $ Branch $ Pattern $ PatPrim $ PrimChar 'x'
          , Delete
          , constructSaturatedCon cFalse
          ]
          e2
        actionTest
          SmartHoles
          e2
          [ Move Child1
          , DeleteCaseBranchPrim $ PrimChar 'x'
          ]
          e3

unit_constructAPP :: Assertion
unit_constructAPP =
  actionTest
    NoSmartHoles
    emptyHole
    [ConstructAPP, EnterType, constructTCon tBool]
    (emptyHole `aPP` tcon tBool)

unit_constructLAM :: Assertion
unit_constructLAM =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [Move Child1, ConstructLAM (Just "a"), constructSaturatedCon cTrue]
    (lAM "a" (con0 cTrue) `ann` tEmptyHole)

unit_construct_TForall :: Assertion
unit_construct_TForall =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTForall (Just "a")]
    (ann emptyHole $ tforall "a" ktype tEmptyHole)

unit_rename_TForall :: Assertion
unit_rename_TForall =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tforall "a" ktype (listOf (tvar "a")))
    [EnterType, RenameForall "b"]
    (emptyHole `ann` tforall "b" ktype (listOf (tvar "b")))

unit_rename_TForall_2 :: Assertion
unit_rename_TForall_2 =
  actionTestExpectFail
    (const True)
    NoSmartHoles
    (emptyHole `ann` tforall "b" ktype (tforall "a" ktype $ listOf (tvar "b")))
    [EnterType, Move Child1, RenameLAM "b"]

unit_construct_TForall_TVar :: Assertion
unit_construct_TForall_TVar =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTForall (Just "a"), Move Child1, ConstructTVar "a"]
    (ann emptyHole $ tforall "a" ktype $ tvar "a")

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
    ( let_ "id" (ann (lAM "a" $ lam "x" $ lvar "x") (tforall "a" ktype $ tfun (tvar "a") (tvar "a"))) $
        app (aPP (lvar "id") (tforall "b" ktype $ tfun (tvar "b") (tvar "b"))) (lvar "id")
    )

unit_constructTApp :: Assertion
unit_constructTApp =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [EnterType, ConstructTApp, Move Child1, constructTCon tList, Move Parent, Move Child2, constructTCon tBool]
    (emptyHole `ann` (tcon tList `tapp` tcon tBool))

unit_construct_lam :: Assertion
unit_construct_lam =
  actionTest
    SmartHoles
    (con0 cTrue)
    [ConstructLam (Just "x")]
    (ann (lam "x" (con0 cTrue)) tEmptyHole)

unit_construct_LAM :: Assertion
unit_construct_LAM =
  actionTest
    SmartHoles
    (con0 cTrue)
    [ConstructLAM (Just "a")]
    (ann (lAM "a" (con0 cTrue)) tEmptyHole)

unit_smart_type_1 :: Assertion
unit_smart_type_1 =
  actionTest
    SmartHoles
    (emptyHole `ann` tcon tNat)
    [EnterType, ConstructTApp, Move Child1]
    (emptyHole `ann` (thole (tcon tNat) `tapp` tEmptyHole))

unit_smart_type_2 :: Assertion
unit_smart_type_2 =
  actionTest
    SmartHoles
    (emptyHole `ann` thole (tcon tList))
    [EnterType, ConstructTApp]
    (emptyHole `ann` (tcon tList `tapp` tEmptyHole))

unit_refine_4 :: Assertion
unit_refine_4 =
  actionTest
    NoSmartHoles
    (let_ "nil" (lAM "a" (con cNil []) `ann` tforall "a" ktype (tcon tList `tapp` tvar "a")) $ emptyHole `ann` (tcon tList `tapp` tcon tNat))
    [Move Child2, Move Child1, InsertRefinedVar $ LocalVarRef "nil"]
    (let_ "nil" (lAM "a" (con cNil []) `ann` tforall "a" ktype (tcon tList `tapp` tvar "a")) $ (lvar "nil" `aPP` tcon tNat) `ann` (tcon tList `tapp` tcon tNat))

unit_refine_5 :: Assertion
unit_refine_5 =
  actionTest
    NoSmartHoles
    (let_ "nil" (lAM "a" (con cNil []) `ann` tforall "a" ktype (tcon tList `tapp` tvar "a")) $ emptyHole `ann` (tcon tList `tapp` tEmptyHole))
    [Move Child2, Move Child1, InsertRefinedVar $ LocalVarRef "nil"]
    (let_ "nil" (lAM "a" (con cNil []) `ann` tforall "a" ktype (tcon tList `tapp` tvar "a")) $ (lvar "nil" `aPP` tEmptyHole) `ann` (tcon tList `tapp` tEmptyHole))

-- If there is no valid refinement, insert saturated variable into a non-empty hole
unit_refine_mismatch_var :: Assertion
unit_refine_mismatch_var =
  actionTest
    NoSmartHoles
    ( let_
        "cons"
        ( emptyHole
            `ann` tforall
              "a"
              ktype
              ( tvar "a"
                  `tfun` ( (tcon tList `tapp` tvar "a")
                             `tfun` (tcon tList `tapp` tvar "a")
                         )
              )
        )
        $ emptyHole `ann` tcon tBool
    )
    [Move Child2, Move Child1, InsertRefinedVar $ LocalVarRef "cons"]
    ( let_
        "cons"
        ( emptyHole
            `ann` tforall
              "a"
              ktype
              ( tvar "a"
                  `tfun` ( (tcon tList `tapp` tvar "a")
                             `tfun` (tcon tList `tapp` tvar "a")
                         )
              )
        )
        $ hole (lvar "cons" `aPP` tEmptyHole `app` emptyHole `app` emptyHole) `ann` tcon tBool
    )

unit_primitive_1 :: Assertion
unit_primitive_1 =
  actionTest
    NoSmartHoles
    emptyHole
    [ ConstructAnn
    , EnterType
    , constructTCon tInt
    , ConstructArrowL
    , Move Child2
    , constructTCon tChar
    , Move Parent
    , ExitType
    , Move Child1
    , ConstructLam (Just "x")
    , ConstructPrim (PrimChar 'c')
    ]
    (lam "x" (char 'c') `ann` (tcon tInt `tfun` tcon tChar))

unit_move_ctor :: Assertion
unit_move_ctor =
  actionTest
    NoSmartHoles
    (emptyHole `ann` tEmptyHole)
    [ Move Child1
    , constructSaturatedCon cMakePair
    , Move $ ConChild 0
    , constructSaturatedCon cZero
    , Move Parent
    , Move $ ConChild 1
    , constructSaturatedCon cFalse
    , Move Parent
    ]
    (con cMakePair [con0 cZero, con0 cFalse] `ann` tEmptyHole)

-- Regression test
-- We have @Bool ∋ {? ? : Nat ?}@ and delete the @Nat@, leaving
-- @Bool ∋ {? ? : ? ?}@, with the focus on the empty type hole;
-- now smartholes notices and changes @{? e : ? ?}@ into @e@
-- (which is @?@ in the present situation). However, the action
-- code was not smart enough to cope with its focus being deleted
-- in this manner (in 71c86a469527fced60e363405ddbbd1ac0cacdec it
-- gained the ability to cope in many situations, but not when
-- focused inside an annotation which is elided).
unit_elide_ann_lost_id :: Assertion
unit_elide_ann_lost_id =
  actionTest
    SmartHoles
    (hole (emptyHole `ann` tcon tNat) `ann` tcon tBool)
    [ Move Child1
    , Move Child1
    , EnterType
    , -- this @Delete@ is the point where we used to error out because of a lost ID
      Delete
    , -- insert a constructor to show that the focus was left at the correct place
      constructSaturatedCon cTrue
    ]
    (con0 cTrue `ann` tcon tBool)

-- * Helpers

-- | Apply the actions to the input expression and test that the result matches
-- the expected output, up to renaming of IDs and changing cached types.
actionTest :: HasCallStack => SmartHoles -> S Expr -> [Action] -> S Expr -> Assertion
actionTest sh inputExpr actions expectedOutput = do
  let (expr, i) = create inputExpr
  result <- either (assertFailure . show) pure $ runTestActions sh i expr actions
  let expected = create' expectedOutput
  -- Compare result to input, ignoring any difference in metadata
  -- NB: we don't compare up-to-alpha, as names should be determined by the
  -- actions on-the-nose
  clearMeta result @?= clearMeta expected

-- | Attempt to apply the actions to the input expression and test that they
-- in fact cause an error to be raised.
actionTestExpectFail :: HasCallStack => (ActionError -> Bool) -> SmartHoles -> S Expr -> [Action] -> Assertion
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
  unfocusExpr . unfocusLoc
    <$> evalTestM
      (i + 1)
      ( do
          builtinModule' <- builtinModule
          primitiveModule' <- primitiveModule
          applyActionsToExpr sh [builtinModule', primitiveModule'] expr actions
      )
