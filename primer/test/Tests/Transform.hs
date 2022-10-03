module Tests.Transform where

import Foreword

import Primer.Builtins
import Primer.Core
import Primer.Core.DSL
import Primer.Core.Transform
import Test.Tasty.HUnit (Assertion, assertFailure, (@?=))
import TestUtils (clearMeta, clearTypeMeta, vcn)

-- When renaming we have to be careful of binding sites. If we're renaming x to
-- y and we encounter a binding site for a new variable v, then there are three
-- cases to consider:
-- 1. v == x : we don't want to rename anything inside the binder, because its
--    'x' refers to a different binding
-- 2. v == y : we have to abort the whole rename, because any occurrences of
--    'x' inside the binder will be accidentally rebound to this binder instead
--    of the original
-- 3. otherwise : we can rename the expression underneath the binder

-- Similarly, if we encounter a free variable v such that v == y, we must abort
-- because renaming will cause this variable to be rebound.

-- We have a three sorts of binders:
--   those that live in terms and bind term variables (lambdas, lets and case branches);
--   those that live in types and bind type variables (foralls);
--   and those that live in terms and bind type variables ("big lambdas").
-- We need to handle each of them in broadly the same way.

-- Lambdas

-- We can rename a variable underneath a lambda provided it doesn't bind a
-- conflicting name
unit_lam_1 :: Assertion
unit_lam_1 =
  afterRename "x" "y" (lam "z" (lvar "x")) (Just (lam "z" (lvar "y")))

-- We can't rename an expression if it has a lambda binding the new name already
unit_lam_2 :: Assertion
unit_lam_2 = afterRename "x" "y" (app (lam "y" (lvar "x")) (lvar "x")) Nothing

-- We can rename an expression with a lambda that binds the same variable name,
-- but we won't do any renaming underneath the lambda.
unit_lam_3 :: Assertion
unit_lam_3 =
  afterRename
    "x"
    "y"
    (app (lam "x" (lvar "x")) (lvar "x"))
    (Just (app (lam "x" (lvar "x")) (lvar "y")))

-- Lets

-- We can rename a variable underneath a let provided it doesn't bind a conflicting name
unit_let_1 :: Assertion
unit_let_1 = afterRename "x" "y" (let_ "z" (lvar "x") (lvar "x")) (Just (let_ "z" (lvar "y") (lvar "y")))

-- We can't rename an expression if it has a let binding the new name already
unit_let_2 :: Assertion
unit_let_2 = afterRename "x" "y" (let_ "y" (lvar "x") (lvar "x")) Nothing

-- We can rename an expression with a let that binds the same variable name,
-- and this will rename inside the bound expression but not the body.
unit_let_3 :: Assertion
unit_let_3 = afterRename "x" "y" (app (let_ "x" (lvar "x") (lvar "x")) (lvar "x")) (Just (app (let_ "x" (lvar "y") (lvar "x")) (lvar "y")))

-- We can rename an expression with a letrec that binds the same variable name,
-- but we won't do any renaming inside the bound value or the let body.
unit_letrec_1 :: Assertion
unit_letrec_1 = afterRename "x" "y" (app (letrec "x" (lvar "x") tEmptyHole (lvar "x")) (lvar "x")) (Just (app (letrec "x" (lvar "x") tEmptyHole (lvar "x")) (lvar "y")))

-- Cases

-- We can rename a case provided its branches don't bind a conflicting name
unit_case_1 :: Assertion
unit_case_1 =
  afterRename
    "x"
    "y"
    ( case_
        (lvar "x")
        [ branch' (["M"], "A") [("t", Nothing), ("u", Nothing)] (lvar "x")
        , branch' (["M"], "B") [("v", Nothing), ("w", Nothing)] (lvar "x")
        ]
    )
    ( Just
        ( case_
            (lvar "y")
            [ branch' (["M"], "A") [("t", Nothing), ("u", Nothing)] (lvar "y")
            , branch' (["M"], "B") [("v", Nothing), ("w", Nothing)] (lvar "y")
            ]
        )
    )

-- We can't rename a case if any of its branches bind the new name already
unit_case_2 :: Assertion
unit_case_2 =
  afterRename
    "x"
    "y"
    ( case_
        (lvar "x")
        [ branch' (["M"], "A") [("t", Nothing), ("u", Nothing)] (lvar "x")
        , branch' (["M"], "B") [("v", Nothing), ("y", Nothing)] (lvar "x")
        ]
    )
    Nothing

-- We can rename a case if its branch binds the original name, but we don't do
-- any renaming inside the RHS of that branch.
unit_case_3 :: Assertion
unit_case_3 =
  afterRename
    "x"
    "y"
    ( case_
        (lvar "x")
        [ branch' (["M"], "A") [("t", Nothing), ("u", Nothing)] (lvar "x")
        , branch' (["M"], "B") [("x", Nothing), ("w", Nothing)] (lvar "x")
        ]
    )
    ( Just
        ( case_
            (lvar "y")
            [ branch' (["M"], "A") [("t", Nothing), ("u", Nothing)] (lvar "y")
            , branch' (["M"], "B") [("x", Nothing), ("w", Nothing)] (lvar "x")
            ]
        )
    )

-- We can't rename if there's a free variable equal to the variable we're renaming to.
unit_var_1 :: Assertion
unit_var_1 = afterRename "x" "y" (app (lvar "f") (lvar "y")) Nothing

unit_var_2 :: Assertion
unit_var_2 = afterRename "x" "y" (app (lvar "f") (lvar "x")) (Just (app (lvar "f") (lvar "y")))

-- We can't rename if there's a free type variable equal to the variable we're renaming to.
unit_var_3 :: Assertion
unit_var_3 = afterRename "x" "y" (aPP (lvar "f") (tvar "y")) Nothing

-- We can't rename if there's a free type variable equal to the variable we're renaming to.
unit_var_4 :: Assertion
unit_var_4 = afterRename "bar" "foo" (letrec "bar" emptyHole (tvar "foo") emptyHole) Nothing

-- We can't rename if there's a free type variable equal to the variable we're renaming to.
unit_var_5 :: Assertion
unit_var_5 = afterRename "bar" "foo" (letType "bar" (tvar "foo") emptyHole) Nothing

-- We notice such free variables under binders
unit_var_6 :: Assertion
unit_var_6 = do
  let tst c = afterRename "foo" "bar" (c $ emptyHole `ann` tvar "bar") Nothing
  tst $ lam "foo"
  tst $ lAM "foo"
  tst $ let_ "foo" emptyHole
  tst $ letType "foo" tEmptyHole
  tst $ letrec "foo" emptyHole tEmptyHole
  tst $ \e -> case_ emptyHole [branch cJust [("foo", Nothing)] e]

-- We notice such free variables under binders in types
unit_var_7 :: Assertion
unit_var_7 = do
  let tst c = afterRenameTy "foo" "bar" (c $ tvar "bar") Nothing
  tst $ tforall "foo" KType
  tst $ tlet "foo" tEmptyHole

-- All other expressions are renamed as expected

unit_hole :: Assertion
unit_hole = afterRename "x" "y" (hole (lvar "x")) (Just (hole (lvar "y")))

unit_ann :: Assertion
unit_ann = afterRename "x" "y" (ann (lvar "x") tEmptyHole) (Just (ann (lvar "y") tEmptyHole))

unit_app :: Assertion
unit_app = afterRename "x" "y" (app (lvar "x") (lvar "x")) (Just (app (lvar "y") (lvar "y")))

unit_con :: Assertion
unit_con = afterRename "x" "y" (con cTrue) (Just (con cTrue))

unit_case :: Assertion
unit_case =
  afterRename
    "x"
    "y"
    ( case_
        (lvar "x")
        [ branch' (["M"], "A") [("y", Nothing), ("z", Nothing)] (lvar "y")
        , branch' (["M"], "B") [("u", Nothing), ("v", Nothing)] (lvar "u")
        ]
    )
    Nothing

-- Foralls

-- We can rename a variable underneath a forall provided it doesn't bind a
-- conflicting name
unit_forall_1 :: Assertion
unit_forall_1 =
  afterRenameTy "x" "y" (tforall "z" KType (tvar "x")) (Just (tforall "z" KType (tvar "y")))

-- We can't rename inside a type if it has a forall binding the new name already
unit_forall_2 :: Assertion
unit_forall_2 = afterRenameTy "x" "y" (tapp (tforall "y" KType (tvar "x")) (tvar "x")) Nothing

-- We can rename a type with a forall that binds the same variable name,
-- but we won't do any renaming underneath the forall.
unit_forall_3 :: Assertion
unit_forall_3 =
  afterRenameTy
    "x"
    "y"
    (tapp (tforall "x" KType (tvar "x")) (tvar "x"))
    (Just (tapp (tforall "x" KType (tvar "x")) (tvar "y")))

-- All other types are renamed as we expect
unit_tEmptyHole :: Assertion
unit_tEmptyHole = afterRenameTy "x" "y" tEmptyHole (Just tEmptyHole)

unit_tcon :: Assertion
unit_tcon = afterRenameTy "x" "y" (tcon tBool) (Just $ tcon tBool)

unit_tfun :: Assertion
unit_tfun = afterRenameTy "x" "y" (tfun (tvar "x") (tvar "x")) (Just $ tfun (tvar "y") (tvar "y"))

unit_tapp :: Assertion
unit_tapp = afterRenameTy "x" "y" (tapp (tvar "x") (tvar "x")) (Just $ tapp (tvar "y") (tvar "y"))

-- Renaming type vars in terms "cross-cutting"

-- We can rename a variable underneath an annotation
unit_cross_ann :: Assertion
unit_cross_ann =
  afterRenameCross "x" "y" (ann emptyHole $ tvar "x") (Just $ ann emptyHole $ tvar "y")

-- We can rename a variable underneath a type application
unit_cross_aPP :: Assertion
unit_cross_aPP = afterRenameCross "x" "y" (aPP emptyHole $ tvar "x") (Just $ aPP emptyHole $ tvar "y")

-- We properly detect potential capture arising from term and type
-- vars being in the same scope.
-- This tests renaming term variables underneath a equally-named
-- binding of a type variable.
unit_cross_capture_1 :: Assertion
unit_cross_capture_1 = do
  afterRename "x" "y" (lAM "y" $ lvar "x") Nothing
  afterRename "x" "y" (letType "y" tEmptyHole $ lvar "x") Nothing

-- We properly detect potential capture arising from term and type
-- vars being in the same scope.
-- This tests renaming type variables underneath a equally-named
-- binding of a term variable.
unit_cross_capture_2 :: Assertion
unit_cross_capture_2 = do
  afterRenameCross "x" "y" (lam "y" $ emptyHole `ann` tvar "x") Nothing
  afterRenameCross "x" "y" (let_ "y" emptyHole $ emptyHole `ann` tvar "x") Nothing
  afterRenameCross "x" "y" (letrec "y" emptyHole tEmptyHole $ emptyHole `ann` tvar "x") Nothing
  afterRenameCross "x" "y" (case_ emptyHole [branch' (["M"], "C") [("y", Nothing)] $ emptyHole `ann` tvar "x"]) Nothing

afterRename :: HasCallStack => LVarName -> LVarName -> S Expr -> Maybe (S Expr) -> Assertion
afterRename = afterRename' renameLocalVar clearMeta

afterRenameTy :: HasCallStack => TyVarName -> TyVarName -> S Type -> Maybe (S Type) -> Assertion
afterRenameTy = afterRename' renameTyVar clearTypeMeta

-- | A helper to test the renaming of type variables inside terms
afterRenameCross :: HasCallStack => TyVarName -> TyVarName -> S Expr -> Maybe (S Expr) -> Assertion
afterRenameCross = afterRename' renameTyVarExpr clearMeta

afterRename' ::
  (HasCallStack, Show a, Show b, Eq a, Eq b) =>
  (LocalName k -> LocalName k -> a -> Maybe a) ->
  (a -> b) ->
  LocalName k ->
  LocalName k ->
  S a ->
  Maybe (S a) ->
  Assertion
afterRename' rename normalise fromVar toVar input output = do
  let x = create' input
      result = rename fromVar toVar x
  case output of
    Nothing -> result @?= Nothing
    Just o -> do
      let expected = create' o
      case result of
        Nothing -> assertFailure "rename failed"
        Just r -> on (@?=) normalise r expected

-- * 'unfoldApp' tests

unit_unfoldApp_1 :: Assertion
unit_unfoldApp_1 =
  let expr :: Expr' () ()
      expr = App () (App () (App () (Con () $ vcn ["M"] "C") (Lam () "x" (v "x"))) (App () (v "w") (v "y"))) (v "z")
      v = Var () . LocalVarRef
   in unfoldApp expr @?= (Con () $ vcn ["M"] "C", [Lam () "x" (v "x"), App () (v "w") (v "y"), v "z"])

unit_unfoldApp_2 :: Assertion
unit_unfoldApp_2 =
  let expr :: Expr' () ()
      expr = Con () $ vcn ["M"] "C"
   in unfoldApp expr @?= (Con () $ vcn ["M"] "C", [])
