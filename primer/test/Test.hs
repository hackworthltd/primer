{-# LINE 6 "Test.hs" #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main, ingredients, tests) where

import Prelude
import qualified System.Environment as E
import qualified Test.Tasty as T
import qualified Test.Tasty.Discover as TD
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.Ingredients as T
import qualified Tests.API
import qualified Tests.Action
import qualified Tests.Action.Available
import qualified Tests.Action.Capture
import qualified Tests.Action.Prog
import qualified Tests.AlphaEquality
import qualified Tests.App
import qualified Tests.Database
import qualified Tests.Eval
import qualified Tests.EvalFull
import qualified Tests.Examples
import qualified Tests.FreeVars
import qualified Tests.Gen.App
import qualified Tests.Gen.Core.Typed
import qualified Tests.Module
import qualified Tests.NullDb
import qualified Tests.Prelude.Integer
import qualified Tests.Prelude.Logic
import qualified Tests.Prelude.Polymorphism
import qualified Tests.Prelude.TypeCheck
import qualified Tests.Pretty
import qualified Tests.Primitives
import qualified Tests.Prog
import qualified Tests.Questions
import qualified Tests.Refine
import qualified Tests.Serialization
import qualified Tests.Subst
import qualified Tests.Transform
import qualified Tests.Typecheck
import qualified Tests.Unification
import qualified Tests.Utils
import qualified Tests.Zipper
import qualified Tests.Zipper.BindersAbove

{- HLINT ignore "Use let" -}


class TestGroup a where testGroup :: String -> a -> IO T.TestTree
instance TestGroup T.TestTree        where testGroup _ a = pure a
instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a
instance TestGroup (IO T.TestTree)   where testGroup _ a = a
instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a

class TestCase a where testCase :: String -> a -> IO T.TestTree
instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n
instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n
instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n

tests :: IO T.TestTree
tests = do
  t0 <- TD.tasty (TD.description "viewTreeExpr injective" <> TD.name "Tests.API.tasty_viewTreeExpr_injective") Tests.API.tasty_viewTreeExpr_injective

  t1 <- TD.tasty (TD.description "viewTreeType injective" <> TD.name "Tests.API.tasty_viewTreeType_injective") Tests.API.tasty_viewTreeType_injective

  t2 <- testGroup "golden" Tests.API.test_golden

  t3 <- testCase "viewTreeExpr injective con" Tests.API.unit_viewTreeExpr_injective_con

  t4 <- testCase "viewTreeExpr injective lam" Tests.API.unit_viewTreeExpr_injective_lam

  t5 <- testCase "viewTreeExpr injective LAM" Tests.API.unit_viewTreeExpr_injective_LAM

  t6 <- testCase "viewTreeExpr injective var" Tests.API.unit_viewTreeExpr_injective_var

  t7 <- testCase "viewTreeExpr injective globalvar" Tests.API.unit_viewTreeExpr_injective_globalvar

  t8 <- testCase "viewTreeExpr injective locglobvar" Tests.API.unit_viewTreeExpr_injective_locglobvar

  t9 <- testCase "viewTreeExpr injective let" Tests.API.unit_viewTreeExpr_injective_let

  t10 <- testCase "viewTreeExpr injective lettype" Tests.API.unit_viewTreeExpr_injective_lettype

  t11 <- testCase "viewTreeExpr injective letrec" Tests.API.unit_viewTreeExpr_injective_letrec

  t12 <- testCase "viewTreeExpr injective case conName" Tests.API.unit_viewTreeExpr_injective_case_conName

  t13 <- testCase "viewTreeExpr injective case paramName" Tests.API.unit_viewTreeExpr_injective_case_paramName

  t14 <- testCase "viewTreeType injective con" Tests.API.unit_viewTreeType_injective_con

  t15 <- testCase "viewTreeType injective var" Tests.API.unit_viewTreeType_injective_var

  t16 <- testCase "viewTreeType injective forall param" Tests.API.unit_viewTreeType_injective_forall_param

  t17 <- testCase "viewTreeType injective forall kind" Tests.API.unit_viewTreeType_injective_forall_kind

  t18 <- testGroup "newSession roundtrip" Tests.API.test_newSession_roundtrip

  t19 <- testGroup "newSession duplicate names" Tests.API.test_newSession_duplicate_names

  t20 <- testGroup "newSession invalid names" Tests.API.test_newSession_invalid_names

  t21 <- testGroup "newSession modified names" Tests.API.test_newSession_modified_names

  t22 <- testGroup "addSession roundtrip" Tests.API.test_addSession_roundtrip

  t23 <- testGroup "listSessions" Tests.API.test_listSessions

  t24 <- testGroup "copySession" Tests.API.test_copySession

  t25 <- testGroup "copySession failure" Tests.API.test_copySession_failure

  t26 <- testGroup "deleteSession" Tests.API.test_deleteSession

  t27 <- testGroup "deleteSession failure" Tests.API.test_deleteSession_failure

  t28 <- testGroup "getSessionName failure" Tests.API.test_getSessionName_failure

  t29 <- testGroup "getVersion" Tests.API.test_getVersion

  t30 <- testGroup "renameSession" Tests.API.test_renameSession

  t31 <- testGroup "renameSession failure" Tests.API.test_renameSession_failure

  t32 <- testGroup "renameSession invalid name" Tests.API.test_renameSession_invalid_name

  t33 <- testGroup "renameSession too long" Tests.API.test_renameSession_too_long

  t34 <- TD.tasty (TD.description "ConstructVar succeeds on hole when in scope" <> TD.name "Tests.Action.tasty_ConstructVar_succeeds_on_hole_when_in_scope") Tests.Action.tasty_ConstructVar_succeeds_on_hole_when_in_scope

  t35 <- testCase "SetCursor succeeds when ID exists" Tests.Action.unit_SetCursor_succeeds_when_ID_exists

  t36 <- TD.tasty (TD.description "SetCursor fails when ID doesn't exist" <> TD.name "Tests.Action.tasty_SetCursor_fails_when_ID_doesn't_exist") Tests.Action.tasty_SetCursor_fails_when_ID_doesn't_exist

  t37 <- testCase "1" Tests.Action.unit_1

  t38 <- testCase "2" Tests.Action.unit_2

  t39 <- testCase "3" Tests.Action.unit_3

  t40 <- testCase "4" Tests.Action.unit_4

  t41 <- testCase "5" Tests.Action.unit_5

  t42 <- testCase "6" Tests.Action.unit_6

  t43 <- testCase "7" Tests.Action.unit_7

  t44 <- testCase "8" Tests.Action.unit_8

  t45 <- testCase "9" Tests.Action.unit_9

  t46 <- testCase "construct arrow left" Tests.Action.unit_construct_arrow_left

  t47 <- testCase "construct arrow right" Tests.Action.unit_construct_arrow_right

  t48 <- testCase "construct letrec" Tests.Action.unit_construct_letrec

  t49 <- testCase "rename let" Tests.Action.unit_rename_let

  t50 <- testCase "rename letrec" Tests.Action.unit_rename_letrec

  t51 <- testCase "rename let shadowed" Tests.Action.unit_rename_let_shadowed

  t52 <- testCase "rename let shadows" Tests.Action.unit_rename_let_shadows

  t53 <- testCase "rename lam" Tests.Action.unit_rename_lam

  t54 <- testCase "rename lam 2" Tests.Action.unit_rename_lam_2

  t55 <- testCase "rename LAM" Tests.Action.unit_rename_LAM

  t56 <- testCase "rename LAM 2" Tests.Action.unit_rename_LAM_2

  t57 <- testCase "convert let to letrec" Tests.Action.unit_convert_let_to_letrec

  t58 <- testCase "delete type" Tests.Action.unit_delete_type

  t59 <- testCase "setcursor type" Tests.Action.unit_setcursor_type

  t60 <- testCase "bad constructor" Tests.Action.unit_bad_constructor

  t61 <- testCase "bad type constructor" Tests.Action.unit_bad_type_constructor

  t62 <- testCase "bad app" Tests.Action.unit_bad_app

  t63 <- testCase "insert expr in type" Tests.Action.unit_insert_expr_in_type

  t64 <- testCase "bad lambda" Tests.Action.unit_bad_lambda

  t65 <- testCase "enter emptyHole" Tests.Action.unit_enter_emptyHole

  t66 <- testCase "enter nonEmptyHole" Tests.Action.unit_enter_nonEmptyHole

  t67 <- testCase "bad enter hole" Tests.Action.unit_bad_enter_hole

  t68 <- testCase "case create" Tests.Action.unit_case_create

  t69 <- testCase "case tidy" Tests.Action.unit_case_tidy

  t70 <- testCase "case move branch 1" Tests.Action.unit_case_move_branch_1

  t71 <- testCase "case move branch 2" Tests.Action.unit_case_move_branch_2

  t72 <- testCase "case move scrutinee 1" Tests.Action.unit_case_move_scrutinee_1

  t73 <- testCase "case move scrutinee 2" Tests.Action.unit_case_move_scrutinee_2

  t74 <- testCase "bad case 1" Tests.Action.unit_bad_case_1

  t75 <- testCase "bad case 2" Tests.Action.unit_bad_case_2

  t76 <- testCase "bad case 3" Tests.Action.unit_bad_case_3

  t77 <- testCase "case on hole" Tests.Action.unit_case_on_hole

  t78 <- testCase "case fill hole scrut" Tests.Action.unit_case_fill_hole_scrut

  t79 <- testCase "case create smart on term" Tests.Action.unit_case_create_smart_on_term

  t80 <- testCase "case create smart on hole" Tests.Action.unit_case_create_smart_on_hole

  t81 <- testCase "case change smart scrutinee type" Tests.Action.unit_case_change_smart_scrutinee_type

  t82 <- testCase "rename case binding" Tests.Action.unit_rename_case_binding

  t83 <- testCase "same rename case binding" Tests.Action.unit_same_rename_case_binding

  t84 <- testCase "rename case bind clash" Tests.Action.unit_rename_case_bind_clash

  t85 <- testCase "constructAPP" Tests.Action.unit_constructAPP

  t86 <- testCase "constructLAM" Tests.Action.unit_constructLAM

  t87 <- testCase "construct TForall" Tests.Action.unit_construct_TForall

  t88 <- testCase "rename TForall" Tests.Action.unit_rename_TForall

  t89 <- testCase "rename TForall 2" Tests.Action.unit_rename_TForall_2

  t90 <- testCase "construct TForall TVar" Tests.Action.unit_construct_TForall_TVar

  t91 <- testCase "poly 1" Tests.Action.unit_poly_1

  t92 <- testCase "constructTApp" Tests.Action.unit_constructTApp

  t93 <- testCase "construct lam" Tests.Action.unit_construct_lam

  t94 <- testCase "construct LAM" Tests.Action.unit_construct_LAM

  t95 <- testCase "smart type 1" Tests.Action.unit_smart_type_1

  t96 <- testCase "smart type 2" Tests.Action.unit_smart_type_2

  t97 <- testCase "refine 1" Tests.Action.unit_refine_1

  t98 <- testCase "refine 2" Tests.Action.unit_refine_2

  t99 <- testCase "refine 3" Tests.Action.unit_refine_3

  t100 <- testCase "refine 4" Tests.Action.unit_refine_4

  t101 <- testCase "refine 5" Tests.Action.unit_refine_5

  t102 <- testCase "primitive 1" Tests.Action.unit_primitive_1

  t103 <- testGroup "1" Tests.Action.Available.test_1

  t104 <- testCase "def in use" Tests.Action.Available.unit_def_in_use

  t105 <- TD.tasty (TD.description "available actions accepted" <> TD.name "Tests.Action.Available.tasty_available_actions_accepted") Tests.Action.Available.tasty_available_actions_accepted

  t106 <- testCase "ConstructLam no capture" Tests.Action.Capture.unit_ConstructLam_no_capture

  t107 <- testCase "ConstructLAM no capture" Tests.Action.Capture.unit_ConstructLAM_no_capture

  t108 <- testCase "ConstructLet no capture" Tests.Action.Capture.unit_ConstructLet_no_capture

  t109 <- testCase "ConstructLetrec no capture" Tests.Action.Capture.unit_ConstructLetrec_no_capture

  t110 <- testCase "RenameLam noop" Tests.Action.Capture.unit_RenameLam_noop

  t111 <- testCase "RenameLam no capture" Tests.Action.Capture.unit_RenameLam_no_capture

  t112 <- testCase "RenameLAM noop" Tests.Action.Capture.unit_RenameLAM_noop

  t113 <- testCase "RenameLAM no capture" Tests.Action.Capture.unit_RenameLAM_no_capture

  t114 <- testCase "RenameLet noop" Tests.Action.Capture.unit_RenameLet_noop

  t115 <- testCase "RenameLet no capture 1" Tests.Action.Capture.unit_RenameLet_no_capture_1

  t116 <- testCase "RenameLetrec noop" Tests.Action.Capture.unit_RenameLetrec_noop

  t117 <- testCase "RenameLetrec no capture 1" Tests.Action.Capture.unit_RenameLetrec_no_capture_1

  t118 <- testCase "RenameLetrec no capture 2" Tests.Action.Capture.unit_RenameLetrec_no_capture_2

  t119 <- testCase "convert let to letrec no capture" Tests.Action.Capture.unit_convert_let_to_letrec_no_capture

  t120 <- testCase "ConstructTForall no capture" Tests.Action.Capture.unit_ConstructTForall_no_capture

  t121 <- testCase "RenameForall noop" Tests.Action.Capture.unit_RenameForall_noop

  t122 <- testCase "RenameForall no capture" Tests.Action.Capture.unit_RenameForall_no_capture

  t123 <- testCase "ty tm same namespace" Tests.Action.Capture.unit_ty_tm_same_namespace

  t124 <- testCase "empty actions only change the log" Tests.Action.Prog.unit_empty_actions_only_change_the_log

  t125 <- testCase "move to def main" Tests.Action.Prog.unit_move_to_def_main

  t126 <- testCase "move to def and construct let" Tests.Action.Prog.unit_move_to_def_and_construct_let

  t127 <- testCase "rename def" Tests.Action.Prog.unit_rename_def

  t128 <- testCase "rename def to same name as existing def" Tests.Action.Prog.unit_rename_def_to_same_name_as_existing_def

  t129 <- testCase "rename def to same name as existing def prim" Tests.Action.Prog.unit_rename_def_to_same_name_as_existing_def_prim

  t130 <- testCase "rename def referenced" Tests.Action.Prog.unit_rename_def_referenced

  t131 <- testCase "rename def recursive" Tests.Action.Prog.unit_rename_def_recursive

  t132 <- testCase "delete def" Tests.Action.Prog.unit_delete_def

  t133 <- testCase "delete def unknown id" Tests.Action.Prog.unit_delete_def_unknown_id

  t134 <- testCase "delete def used id" Tests.Action.Prog.unit_delete_def_used_id

  t135 <- testCase "delete def used id cross module" Tests.Action.Prog.unit_delete_def_used_id_cross_module

  t136 <- testCase "delete def recursive" Tests.Action.Prog.unit_delete_def_recursive

  t137 <- testCase "move to unknown def" Tests.Action.Prog.unit_move_to_unknown_def

  t138 <- testCase "rename unknown def" Tests.Action.Prog.unit_rename_unknown_def

  t139 <- testCase "construct let without moving to def first" Tests.Action.Prog.unit_construct_let_without_moving_to_def_first

  t140 <- testCase "create def" Tests.Action.Prog.unit_create_def

  t141 <- testCase "create def clash prim" Tests.Action.Prog.unit_create_def_clash_prim

  t142 <- testCase "create def nonexistant module" Tests.Action.Prog.unit_create_def_nonexistant_module

  t143 <- testCase "create def imported module" Tests.Action.Prog.unit_create_def_imported_module

  t144 <- testCase "create typedef" Tests.Action.Prog.unit_create_typedef

  t145 <- testCase "create typedef bad 1" Tests.Action.Prog.unit_create_typedef_bad_1

  t146 <- testCase "create typedef bad 2" Tests.Action.Prog.unit_create_typedef_bad_2

  t147 <- testCase "create typedef bad 3" Tests.Action.Prog.unit_create_typedef_bad_3

  t148 <- testCase "create typedef bad 4" Tests.Action.Prog.unit_create_typedef_bad_4

  t149 <- testCase "create typedef bad 5" Tests.Action.Prog.unit_create_typedef_bad_5

  t150 <- testCase "create typedef bad 6" Tests.Action.Prog.unit_create_typedef_bad_6

  t151 <- testCase "create typedef bad 7" Tests.Action.Prog.unit_create_typedef_bad_7

  t152 <- testCase "create typedef bad prim" Tests.Action.Prog.unit_create_typedef_bad_prim

  t153 <- testCase "create typedef 8" Tests.Action.Prog.unit_create_typedef_8

  t154 <- testCase "create typedef 9" Tests.Action.Prog.unit_create_typedef_9

  t155 <- testCase "construct arrow in sig" Tests.Action.Prog.unit_construct_arrow_in_sig

  t156 <- testCase "sigaction creates holes" Tests.Action.Prog.unit_sigaction_creates_holes

  t157 <- testCase "copy paste duplicate" Tests.Action.Prog.unit_copy_paste_duplicate

  t158 <- testCase "copy paste type scoping" Tests.Action.Prog.unit_copy_paste_type_scoping

  t159 <- testCase "raise" Tests.Action.Prog.unit_raise

  t160 <- testCase "copy paste expr 1" Tests.Action.Prog.unit_copy_paste_expr_1

  t161 <- testCase "copy paste ann" Tests.Action.Prog.unit_copy_paste_ann

  t162 <- testCase "copy paste ann2sig" Tests.Action.Prog.unit_copy_paste_ann2sig

  t163 <- testCase "copy paste sig2ann" Tests.Action.Prog.unit_copy_paste_sig2ann

  t164 <- testCase "import vars" Tests.Action.Prog.unit_import_vars

  t165 <- testCase "import reference" Tests.Action.Prog.unit_import_reference

  t166 <- testCase "import twice 1" Tests.Action.Prog.unit_import_twice_1

  t167 <- testCase "import twice 2" Tests.Action.Prog.unit_import_twice_2

  t168 <- testCase "copy paste import" Tests.Action.Prog.unit_copy_paste_import

  t169 <- testCase "RenameType" Tests.Action.Prog.unit_RenameType

  t170 <- testCase "RenameType clash" Tests.Action.Prog.unit_RenameType_clash

  t171 <- testCase "RenameCon" Tests.Action.Prog.unit_RenameCon

  t172 <- testCase "RenameCon clash" Tests.Action.Prog.unit_RenameCon_clash

  t173 <- testCase "RenameTypeParam" Tests.Action.Prog.unit_RenameTypeParam

  t174 <- testCase "RenameTypeParam clash" Tests.Action.Prog.unit_RenameTypeParam_clash

  t175 <- testCase "AddCon" Tests.Action.Prog.unit_AddCon

  t176 <- testCase "SetConFieldType" Tests.Action.Prog.unit_SetConFieldType

  t177 <- testCase "SetConFieldType partial app" Tests.Action.Prog.unit_SetConFieldType_partial_app

  t178 <- testCase "SetConFieldType case" Tests.Action.Prog.unit_SetConFieldType_case

  t179 <- testCase "SetConFieldType shadow" Tests.Action.Prog.unit_SetConFieldType_shadow

  t180 <- testCase "AddConField" Tests.Action.Prog.unit_AddConField

  t181 <- testCase "AddConField partial app" Tests.Action.Prog.unit_AddConField_partial_app

  t182 <- testCase "AddConField partial app end" Tests.Action.Prog.unit_AddConField_partial_app_end

  t183 <- testCase "AddConField case ann" Tests.Action.Prog.unit_AddConField_case_ann

  t184 <- testCase "generate names import" Tests.Action.Prog.unit_generate_names_import

  t185 <- testCase "rename module" Tests.Action.Prog.unit_rename_module

  t186 <- testCase "rename module clash" Tests.Action.Prog.unit_rename_module_clash

  t187 <- testCase "rename module nonexistant" Tests.Action.Prog.unit_rename_module_nonexistant

  t188 <- testCase "rename module imported" Tests.Action.Prog.unit_rename_module_imported

  t189 <- testCase "cross module actions" Tests.Action.Prog.unit_cross_module_actions

  t190 <- testCase "sh lost id" Tests.Action.Prog.unit_sh_lost_id

  t191 <- testCase "sh lost id 2" Tests.Action.Prog.unit_sh_lost_id_2

  t192 <- testCase "good defaultEmptyProg" Tests.Action.Prog.unit_good_defaultEmptyProg

  t193 <- testCase "good defaultFullProg" Tests.Action.Prog.unit_good_defaultFullProg

  t194 <- testCase "defaultFullProg no clash" Tests.Action.Prog.unit_defaultFullProg_no_clash

  t195 <- testCase "1" Tests.AlphaEquality.unit_1

  t196 <- testCase "2" Tests.AlphaEquality.unit_2

  t197 <- testCase "3" Tests.AlphaEquality.unit_3

  t198 <- testCase "4" Tests.AlphaEquality.unit_4

  t199 <- testCase "5" Tests.AlphaEquality.unit_5

  t200 <- testCase "6" Tests.AlphaEquality.unit_6

  t201 <- testCase "7" Tests.AlphaEquality.unit_7

  t202 <- testCase "8" Tests.AlphaEquality.unit_8

  t203 <- testCase "9" Tests.AlphaEquality.unit_9

  t204 <- testCase "10" Tests.AlphaEquality.unit_10

  t205 <- testCase "11" Tests.AlphaEquality.unit_11

  t206 <- testCase "repeated names" Tests.AlphaEquality.unit_repeated_names

  t207 <- TD.tasty (TD.description "refl" <> TD.name "Tests.AlphaEquality.tasty_refl") Tests.AlphaEquality.tasty_refl

  t208 <- TD.tasty (TD.description "alpha" <> TD.name "Tests.AlphaEquality.tasty_alpha") Tests.AlphaEquality.tasty_alpha

  t209 <- testCase "checkAppWellFormed newApp" Tests.App.unit_checkAppWellFormed_newApp

  t210 <- testCase "checkAppWellFormed newEmptyApp" Tests.App.unit_checkAppWellFormed_newEmptyApp

  t211 <- testCase "checkAppWellFormed even3App" Tests.App.unit_checkAppWellFormed_even3App

  t212 <- testCase "checkAppWellFormed mapOddApp" Tests.App.unit_checkAppWellFormed_mapOddApp

  t213 <- testCase "checkAppWellFormed badEven3App" Tests.App.unit_checkAppWellFormed_badEven3App

  t214 <- testCase "checkAppWellFormed badEvenApp" Tests.App.unit_checkAppWellFormed_badEvenApp

  t215 <- testCase "checkAppWellFormed badMapApp" Tests.App.unit_checkAppWellFormed_badMapApp

  t216 <- testCase "mkAppSafe even3Prog" Tests.App.unit_mkAppSafe_even3Prog

  t217 <- testCase "mkAppSafe mapOddProg" Tests.App.unit_mkAppSafe_mapOddProg

  t218 <- testCase "mkAppSafe badEven3Prog" Tests.App.unit_mkAppSafe_badEven3Prog

  t219 <- testCase "mkAppSafe badEvenProg" Tests.App.unit_mkAppSafe_badEvenProg

  t220 <- testCase "mkAppSafe badMapProg" Tests.App.unit_mkAppSafe_badMapProg

  t221 <- testGroup "unmodified" Tests.Database.test_unmodified

  t222 <- testGroup "modified" Tests.Database.test_modified

  t223 <- testGroup "invalid" Tests.Database.test_invalid

  t224 <- testGroup "insert empty q" Tests.Database.test_insert_empty_q

  t225 <- testGroup "updateapp empty q" Tests.Database.test_updateapp_empty_q

  t226 <- testGroup "updatename empty q" Tests.Database.test_updatename_empty_q

  t227 <- testGroup "loadsession empty q" Tests.Database.test_loadsession_empty_q

  t228 <- testGroup "listsessions empty q" Tests.Database.test_listsessions_empty_q

  t229 <- testGroup "deletesession empty q" Tests.Database.test_deletesession_empty_q

  t230 <- testGroup "insert faildb" Tests.Database.test_insert_faildb

  t231 <- testGroup "updateapp faildb" Tests.Database.test_updateapp_faildb

  t232 <- testGroup "updatename faildb" Tests.Database.test_updatename_faildb

  t233 <- testGroup "loadsession faildb" Tests.Database.test_loadsession_faildb

  t234 <- testGroup "listsessions faildb" Tests.Database.test_listsessions_faildb

  t235 <- testGroup "deletesession faildb" Tests.Database.test_deletesession_faildb

  t236 <- testCase "tryReduce no redex" Tests.Eval.unit_tryReduce_no_redex

  t237 <- testCase "tryReduce beta" Tests.Eval.unit_tryReduce_beta

  t238 <- testCase "tryReduce beta annotation" Tests.Eval.unit_tryReduce_beta_annotation

  t239 <- testCase "tryReduce beta annotation hole" Tests.Eval.unit_tryReduce_beta_annotation_hole

  t240 <- testCase "tryReduce BETA" Tests.Eval.unit_tryReduce_BETA

  t241 <- testCase "tryReduce local term var" Tests.Eval.unit_tryReduce_local_term_var

  t242 <- testCase "tryReduce local type var" Tests.Eval.unit_tryReduce_local_type_var

  t243 <- testCase "tryReduce global var" Tests.Eval.unit_tryReduce_global_var

  t244 <- testCase "tryReduce let" Tests.Eval.unit_tryReduce_let

  t245 <- testCase "tryReduce let self capture" Tests.Eval.unit_tryReduce_let_self_capture

  t246 <- testCase "tryReduce lettype" Tests.Eval.unit_tryReduce_lettype

  t247 <- testCase "tryReduce lettype self capture" Tests.Eval.unit_tryReduce_lettype_self_capture

  t248 <- testCase "tryReduce tlet elide" Tests.Eval.unit_tryReduce_tlet_elide

  t249 <- testCase "tryReduce tlet self capture" Tests.Eval.unit_tryReduce_tlet_self_capture

  t250 <- testCase "tryReduce letrec" Tests.Eval.unit_tryReduce_letrec

  t251 <- testCase "tryReduce case 1" Tests.Eval.unit_tryReduce_case_1

  t252 <- testCase "tryReduce case 2" Tests.Eval.unit_tryReduce_case_2

  t253 <- testCase "tryReduce case 3" Tests.Eval.unit_tryReduce_case_3

  t254 <- testCase "tryReduce case name clash" Tests.Eval.unit_tryReduce_case_name_clash

  t255 <- testCase "tryReduce case scrutinee not redex" Tests.Eval.unit_tryReduce_case_scrutinee_not_redex

  t256 <- testCase "tryReduce prim" Tests.Eval.unit_tryReduce_prim

  t257 <- testCase "tryReduce prim fail unsaturated" Tests.Eval.unit_tryReduce_prim_fail_unsaturated

  t258 <- testCase "tryReduce prim fail unreduced args" Tests.Eval.unit_tryReduce_prim_fail_unreduced_args

  t259 <- testCase "step non redex" Tests.Eval.unit_step_non_redex

  t260 <- testCase "findNodeByID letrec" Tests.Eval.unit_findNodeByID_letrec

  t261 <- testCase "findNodeByID 1" Tests.Eval.unit_findNodeByID_1

  t262 <- testCase "findNodeByID 2" Tests.Eval.unit_findNodeByID_2

  t263 <- testCase "findNodeByID tlet" Tests.Eval.unit_findNodeByID_tlet

  t264 <- testCase "findNodeByID scoping 1" Tests.Eval.unit_findNodeByID_scoping_1

  t265 <- testCase "findNodeByID scoping 2" Tests.Eval.unit_findNodeByID_scoping_2

  t266 <- testCase "findNodeByID capture" Tests.Eval.unit_findNodeByID_capture

  t267 <- testCase "findNodeByID capture type" Tests.Eval.unit_findNodeByID_capture_type

  t268 <- testCase "redexes con" Tests.Eval.unit_redexes_con

  t269 <- testCase "redexes lam 1" Tests.Eval.unit_redexes_lam_1

  t270 <- testCase "redexes lam 2" Tests.Eval.unit_redexes_lam_2

  t271 <- testCase "redexes lam 3" Tests.Eval.unit_redexes_lam_3

  t272 <- testCase "redexes lam 4" Tests.Eval.unit_redexes_lam_4

  t273 <- testCase "redexes LAM 1" Tests.Eval.unit_redexes_LAM_1

  t274 <- testCase "redexes LAM 2" Tests.Eval.unit_redexes_LAM_2

  t275 <- testCase "redexes LAM 3" Tests.Eval.unit_redexes_LAM_3

  t276 <- testCase "redexes LAM 4" Tests.Eval.unit_redexes_LAM_4

  t277 <- testCase "redexes let 1" Tests.Eval.unit_redexes_let_1

  t278 <- testCase "redexes let 2" Tests.Eval.unit_redexes_let_2

  t279 <- testCase "redexes let 3" Tests.Eval.unit_redexes_let_3

  t280 <- testCase "redexes let capture" Tests.Eval.unit_redexes_let_capture

  t281 <- testCase "redexes lettype capture" Tests.Eval.unit_redexes_lettype_capture

  t282 <- testCase "redexes letrec 1" Tests.Eval.unit_redexes_letrec_1

  t283 <- testCase "redexes letrec 2" Tests.Eval.unit_redexes_letrec_2

  t284 <- testCase "redexes letrec 3" Tests.Eval.unit_redexes_letrec_3

  t285 <- testCase "redexes letrec app 1" Tests.Eval.unit_redexes_letrec_app_1

  t286 <- testCase "redexes letrec APP 1" Tests.Eval.unit_redexes_letrec_APP_1

  t287 <- testCase "redexes lettype 1" Tests.Eval.unit_redexes_lettype_1

  t288 <- testCase "redexes lettype 2" Tests.Eval.unit_redexes_lettype_2

  t289 <- testCase "redexes lettype 3" Tests.Eval.unit_redexes_lettype_3

  t290 <- testCase "redexes lettype 4" Tests.Eval.unit_redexes_lettype_4

  t291 <- testCase "redexes tlet 1" Tests.Eval.unit_redexes_tlet_1

  t292 <- testCase "redexes tlet 2" Tests.Eval.unit_redexes_tlet_2

  t293 <- testCase "redexes tlet 3" Tests.Eval.unit_redexes_tlet_3

  t294 <- testCase "redexes tlet 4" Tests.Eval.unit_redexes_tlet_4

  t295 <- testCase "redexes case 1" Tests.Eval.unit_redexes_case_1

  t296 <- testCase "redexes case 1 annotated" Tests.Eval.unit_redexes_case_1_annotated

  t297 <- testCase "redexes case 2" Tests.Eval.unit_redexes_case_2

  t298 <- testCase "redexes case 3" Tests.Eval.unit_redexes_case_3

  t299 <- testCase "redexes case 4" Tests.Eval.unit_redexes_case_4

  t300 <- testCase "redexes case 5" Tests.Eval.unit_redexes_case_5

  t301 <- testCase "redexes let upsilon" Tests.Eval.unit_redexes_let_upsilon

  t302 <- testCase "redexes prim 1" Tests.Eval.unit_redexes_prim_1

  t303 <- testCase "redexes prim 2" Tests.Eval.unit_redexes_prim_2

  t304 <- testCase "redexes prim 3" Tests.Eval.unit_redexes_prim_3

  t305 <- testCase "redexes prim ann" Tests.Eval.unit_redexes_prim_ann

  t306 <- testCase "eval modules" Tests.Eval.unit_eval_modules

  t307 <- testCase "eval modules scrutinize imported type" Tests.Eval.unit_eval_modules_scrutinize_imported_type

  t308 <- TD.tasty (TD.description "type preservation" <> TD.name "Tests.Eval.tasty_type_preservation") Tests.Eval.tasty_type_preservation

  t309 <- TD.tasty (TD.description "redex independent" <> TD.name "Tests.Eval.tasty_redex_independent") Tests.Eval.tasty_redex_independent

  t310 <- testCase "1" Tests.EvalFull.unit_1

  t311 <- testCase "2" Tests.EvalFull.unit_2

  t312 <- testCase "3" Tests.EvalFull.unit_3

  t313 <- testCase "4" Tests.EvalFull.unit_4

  t314 <- testCase "5" Tests.EvalFull.unit_5

  t315 <- testCase "6" Tests.EvalFull.unit_6

  t316 <- testCase "7" Tests.EvalFull.unit_7

  t317 <- testCase "8" Tests.EvalFull.unit_8

  t318 <- testCase "9" Tests.EvalFull.unit_9

  t319 <- testCase "10" Tests.EvalFull.unit_10

  t320 <- testCase "11" Tests.EvalFull.unit_11

  t321 <- testCase "12" Tests.EvalFull.unit_12

  t322 <- testCase "13" Tests.EvalFull.unit_13

  t323 <- testCase "14" Tests.EvalFull.unit_14

  t324 <- testCase "15" Tests.EvalFull.unit_15

  t325 <- testCase "hole ann case" Tests.EvalFull.unit_hole_ann_case

  t326 <- testCase "tlet" Tests.EvalFull.unit_tlet

  t327 <- testCase "tlet elide" Tests.EvalFull.unit_tlet_elide

  t328 <- testCase "tlet self capture" Tests.EvalFull.unit_tlet_self_capture

  t329 <- TD.tasty (TD.description "resume" <> TD.name "Tests.EvalFull.tasty_resume") Tests.EvalFull.tasty_resume

  t330 <- TD.tasty (TD.description "resume regression" <> TD.name "Tests.EvalFull.tasty_resume_regression") Tests.EvalFull.tasty_resume_regression

  t331 <- testCase "type preservation rename LAM regression" Tests.EvalFull.unit_type_preservation_rename_LAM_regression

  t332 <- testCase "type preservation case regression tm" Tests.EvalFull.unit_type_preservation_case_regression_tm

  t333 <- testCase "type preservation case regression ty" Tests.EvalFull.unit_type_preservation_case_regression_ty

  t334 <- testCase "type preservation case hole regression" Tests.EvalFull.unit_type_preservation_case_hole_regression

  t335 <- testCase "type preservation BETA regression" Tests.EvalFull.unit_type_preservation_BETA_regression

  t336 <- testCase "let self capture" Tests.EvalFull.unit_let_self_capture

  t337 <- testCase "regression self capture let let" Tests.EvalFull.unit_regression_self_capture_let_let

  t338 <- TD.tasty (TD.description "type preservation" <> TD.name "Tests.EvalFull.tasty_type_preservation") Tests.EvalFull.tasty_type_preservation

  t339 <- testCase "prim stuck" Tests.EvalFull.unit_prim_stuck

  t340 <- testCase "prim toUpper" Tests.EvalFull.unit_prim_toUpper

  t341 <- testCase "prim isSpace 1" Tests.EvalFull.unit_prim_isSpace_1

  t342 <- testCase "prim isSpace 2" Tests.EvalFull.unit_prim_isSpace_2

  t343 <- TD.tasty (TD.description "prim hex nat" <> TD.name "Tests.EvalFull.tasty_prim_hex_nat") Tests.EvalFull.tasty_prim_hex_nat

  t344 <- testCase "prim char eq 1" Tests.EvalFull.unit_prim_char_eq_1

  t345 <- testCase "prim char eq 2" Tests.EvalFull.unit_prim_char_eq_2

  t346 <- testCase "prim char partial" Tests.EvalFull.unit_prim_char_partial

  t347 <- testCase "prim int add" Tests.EvalFull.unit_prim_int_add

  t348 <- testCase "prim int add big" Tests.EvalFull.unit_prim_int_add_big

  t349 <- testCase "prim int sub" Tests.EvalFull.unit_prim_int_sub

  t350 <- testCase "prim int sub negative" Tests.EvalFull.unit_prim_int_sub_negative

  t351 <- testCase "prim int mul" Tests.EvalFull.unit_prim_int_mul

  t352 <- testCase "prim int quotient" Tests.EvalFull.unit_prim_int_quotient

  t353 <- testCase "prim int quotient negative" Tests.EvalFull.unit_prim_int_quotient_negative

  t354 <- testCase "prim int quotient zero" Tests.EvalFull.unit_prim_int_quotient_zero

  t355 <- testCase "prim int remainder" Tests.EvalFull.unit_prim_int_remainder

  t356 <- testCase "prim int remainder negative 1" Tests.EvalFull.unit_prim_int_remainder_negative_1

  t357 <- testCase "prim int remainder negative 2" Tests.EvalFull.unit_prim_int_remainder_negative_2

  t358 <- testCase "prim int remainder negative 3" Tests.EvalFull.unit_prim_int_remainder_negative_3

  t359 <- testCase "prim int remainder zero" Tests.EvalFull.unit_prim_int_remainder_zero

  t360 <- testCase "prim int quot" Tests.EvalFull.unit_prim_int_quot

  t361 <- testCase "prim int quot negative" Tests.EvalFull.unit_prim_int_quot_negative

  t362 <- testCase "prim int quot zero" Tests.EvalFull.unit_prim_int_quot_zero

  t363 <- testCase "prim int rem" Tests.EvalFull.unit_prim_int_rem

  t364 <- testCase "prim int rem negative 1" Tests.EvalFull.unit_prim_int_rem_negative_1

  t365 <- testCase "prim int rem negative 2" Tests.EvalFull.unit_prim_int_rem_negative_2

  t366 <- testCase "prim int rem negative 3" Tests.EvalFull.unit_prim_int_rem_negative_3

  t367 <- testCase "prim int rem zero" Tests.EvalFull.unit_prim_int_rem_zero

  t368 <- testCase "prim int eq 1" Tests.EvalFull.unit_prim_int_eq_1

  t369 <- testCase "prim int eq 2" Tests.EvalFull.unit_prim_int_eq_2

  t370 <- testCase "prim int neq 1" Tests.EvalFull.unit_prim_int_neq_1

  t371 <- testCase "prim int neq 2" Tests.EvalFull.unit_prim_int_neq_2

  t372 <- testCase "prim int less than 1" Tests.EvalFull.unit_prim_int_less_than_1

  t373 <- testCase "prim int less than 2" Tests.EvalFull.unit_prim_int_less_than_2

  t374 <- testCase "prim int less than or equal 1" Tests.EvalFull.unit_prim_int_less_than_or_equal_1

  t375 <- testCase "prim int less than or equal 2" Tests.EvalFull.unit_prim_int_less_than_or_equal_2

  t376 <- testCase "prim int less than or equal 3" Tests.EvalFull.unit_prim_int_less_than_or_equal_3

  t377 <- testCase "prim int greater than 1" Tests.EvalFull.unit_prim_int_greater_than_1

  t378 <- testCase "prim int greater than 2" Tests.EvalFull.unit_prim_int_greater_than_2

  t379 <- testCase "prim int greater than or equal 1" Tests.EvalFull.unit_prim_int_greater_than_or_equal_1

  t380 <- testCase "prim int greater than or equal 2" Tests.EvalFull.unit_prim_int_greater_than_or_equal_2

  t381 <- testCase "prim int greater than or equal 3" Tests.EvalFull.unit_prim_int_greater_than_or_equal_3

  t382 <- testCase "prim int toNat" Tests.EvalFull.unit_prim_int_toNat

  t383 <- testCase "prim int toNat negative" Tests.EvalFull.unit_prim_int_toNat_negative

  t384 <- testCase "prim int fromNat" Tests.EvalFull.unit_prim_int_fromNat

  t385 <- testCase "prim ann" Tests.EvalFull.unit_prim_ann

  t386 <- testCase "prim partial map" Tests.EvalFull.unit_prim_partial_map

  t387 <- testCase "eval full modules" Tests.EvalFull.unit_eval_full_modules

  t388 <- testCase "eval full modules scrutinize imported type" Tests.EvalFull.unit_eval_full_modules_scrutinize_imported_type

  t389 <- TD.tasty (TD.description "unique ids" <> TD.name "Tests.EvalFull.tasty_unique_ids") Tests.EvalFull.tasty_unique_ids

  t390 <- testCase "check examples" Tests.Examples.unit_check_examples

  t391 <- testCase "comprehensive ill typed" Tests.Examples.unit_comprehensive_ill_typed

  t392 <- testCase "1" Tests.FreeVars.unit_1

  t393 <- testCase "2" Tests.FreeVars.unit_2

  t394 <- TD.tasty (TD.description "genProg well formed" <> TD.name "Tests.Gen.App.tasty_genProg_well_formed") Tests.Gen.App.tasty_genProg_well_formed

  t395 <- TD.tasty (TD.description "genTy" <> TD.name "Tests.Gen.Core.Typed.tasty_genTy") Tests.Gen.Core.Typed.tasty_genTy

  t396 <- TD.tasty (TD.description "genCxtExtending typechecks" <> TD.name "Tests.Gen.Core.Typed.tasty_genCxtExtending_typechecks") Tests.Gen.Core.Typed.tasty_genCxtExtending_typechecks

  t397 <- TD.tasty (TD.description "inExtendedLocalGlobalCxt valid" <> TD.name "Tests.Gen.Core.Typed.tasty_inExtendedLocalGlobalCxt_valid") Tests.Gen.Core.Typed.tasty_inExtendedLocalGlobalCxt_valid

  t398 <- TD.tasty (TD.description "genCxtExtending is extension" <> TD.name "Tests.Gen.Core.Typed.tasty_genCxtExtending_is_extension") Tests.Gen.Core.Typed.tasty_genCxtExtending_is_extension

  t399 <- TD.tasty (TD.description "genSyns" <> TD.name "Tests.Gen.Core.Typed.tasty_genSyns") Tests.Gen.Core.Typed.tasty_genSyns

  t400 <- TD.tasty (TD.description "genChk" <> TD.name "Tests.Gen.Core.Typed.tasty_genChk") Tests.Gen.Core.Typed.tasty_genChk

  t401 <- testCase "nextModuleID exampleMapModule" Tests.Module.unit_nextModuleID_exampleMapModule

  t402 <- testCase "nextModuleID exampleEvenOddModule" Tests.Module.unit_nextModuleID_exampleEvenOddModule

  t403 <- testGroup "insertSession roundtrip" Tests.NullDb.test_insertSession_roundtrip

  t404 <- testGroup "insertSession failure" Tests.NullDb.test_insertSession_failure

  t405 <- testGroup "listSessions" Tests.NullDb.test_listSessions

  t406 <- testGroup "updateSessionApp roundtrip" Tests.NullDb.test_updateSessionApp_roundtrip

  t407 <- testGroup "updateSessionApp failure" Tests.NullDb.test_updateSessionApp_failure

  t408 <- testGroup "updateSessionName roundtrip" Tests.NullDb.test_updateSessionName_roundtrip

  t409 <- testGroup "updateSessionName failure" Tests.NullDb.test_updateSessionName_failure

  t410 <- testGroup "querySessionId" Tests.NullDb.test_querySessionId

  t411 <- testGroup "deleteSession" Tests.NullDb.test_deleteSession

  t412 <- TD.tasty (TD.description "min prop" <> TD.name "Tests.Prelude.Integer.tasty_min_prop") Tests.Prelude.Integer.tasty_min_prop

  t413 <- TD.tasty (TD.description "max prop" <> TD.name "Tests.Prelude.Integer.tasty_max_prop") Tests.Prelude.Integer.tasty_max_prop

  t414 <- TD.tasty (TD.description "negate prop" <> TD.name "Tests.Prelude.Integer.tasty_negate_prop") Tests.Prelude.Integer.tasty_negate_prop

  t415 <- TD.tasty (TD.description "abs prop" <> TD.name "Tests.Prelude.Integer.tasty_abs_prop") Tests.Prelude.Integer.tasty_abs_prop

  t416 <- TD.tasty (TD.description "gcd prop" <> TD.name "Tests.Prelude.Integer.tasty_gcd_prop") Tests.Prelude.Integer.tasty_gcd_prop

  t417 <- TD.tasty (TD.description "lcm prop" <> TD.name "Tests.Prelude.Integer.tasty_lcm_prop") Tests.Prelude.Integer.tasty_lcm_prop

  t418 <- TD.tasty (TD.description "even prop" <> TD.name "Tests.Prelude.Integer.tasty_even_prop") Tests.Prelude.Integer.tasty_even_prop

  t419 <- TD.tasty (TD.description "odd prop" <> TD.name "Tests.Prelude.Integer.tasty_odd_prop") Tests.Prelude.Integer.tasty_odd_prop

  t420 <- TD.tasty (TD.description "sum prop" <> TD.name "Tests.Prelude.Integer.tasty_sum_prop") Tests.Prelude.Integer.tasty_sum_prop

  t421 <- TD.tasty (TD.description "product prop" <> TD.name "Tests.Prelude.Integer.tasty_product_prop") Tests.Prelude.Integer.tasty_product_prop

  t422 <- TD.tasty (TD.description "not correct" <> TD.name "Tests.Prelude.Logic.tasty_not_correct") Tests.Prelude.Logic.tasty_not_correct

  t423 <- TD.tasty (TD.description "and correct" <> TD.name "Tests.Prelude.Logic.tasty_and_correct") Tests.Prelude.Logic.tasty_and_correct

  t424 <- TD.tasty (TD.description "or correct" <> TD.name "Tests.Prelude.Logic.tasty_or_correct") Tests.Prelude.Logic.tasty_or_correct

  t425 <- TD.tasty (TD.description "xor correct" <> TD.name "Tests.Prelude.Logic.tasty_xor_correct") Tests.Prelude.Logic.tasty_xor_correct

  t426 <- TD.tasty (TD.description "implies correct" <> TD.name "Tests.Prelude.Logic.tasty_implies_correct") Tests.Prelude.Logic.tasty_implies_correct

  t427 <- TD.tasty (TD.description "id prop" <> TD.name "Tests.Prelude.Polymorphism.tasty_id_prop") Tests.Prelude.Polymorphism.tasty_id_prop

  t428 <- TD.tasty (TD.description "const prop" <> TD.name "Tests.Prelude.Polymorphism.tasty_const_prop") Tests.Prelude.Polymorphism.tasty_const_prop

  t429 <- TD.tasty (TD.description "map prop" <> TD.name "Tests.Prelude.Polymorphism.tasty_map_prop") Tests.Prelude.Polymorphism.tasty_map_prop

  t430 <- TD.tasty (TD.description "foldr list char" <> TD.name "Tests.Prelude.Polymorphism.tasty_foldr_list_char") Tests.Prelude.Polymorphism.tasty_foldr_list_char

  t431 <- TD.tasty (TD.description "foldr bool" <> TD.name "Tests.Prelude.Polymorphism.tasty_foldr_bool") Tests.Prelude.Polymorphism.tasty_foldr_bool

  t432 <- TD.tasty (TD.description "foldr right assoc" <> TD.name "Tests.Prelude.Polymorphism.tasty_foldr_right_assoc") Tests.Prelude.Polymorphism.tasty_foldr_right_assoc

  t433 <- testCase "foldr short circuit" Tests.Prelude.Polymorphism.unit_foldr_short_circuit

  t434 <- testCase "check prelude" Tests.Prelude.TypeCheck.unit_check_prelude

  t435 <- testGroup "examples" Tests.Pretty.test_examples

  t436 <- testCase "prim con scope" Tests.Primitives.unit_prim_con_scope

  t437 <- testCase "prim con scope ast" Tests.Primitives.unit_prim_con_scope_ast

  t438 <- testCase "nextProgID exampleEven3Prog" Tests.Prog.unit_nextProgID_exampleEven3Prog

  t439 <- testCase "nextProgID exampleBadEven3Prog" Tests.Prog.unit_nextProgID_exampleBadEven3Prog

  t440 <- testCase "nextProgID exampleBadEvenProg" Tests.Prog.unit_nextProgID_exampleBadEvenProg

  t441 <- testCase "nextProgID exampleBadMapProg" Tests.Prog.unit_nextProgID_exampleBadMapProg

  t442 <- testGroup "laws" Tests.Questions.test_laws

  t443 <- TD.tasty (TD.description "shadow monoid types" <> TD.name "Tests.Questions.tasty_shadow_monoid_types") Tests.Questions.tasty_shadow_monoid_types

  t444 <- TD.tasty (TD.description "shadow monoid expr" <> TD.name "Tests.Questions.tasty_shadow_monoid_expr") Tests.Questions.tasty_shadow_monoid_expr

  t445 <- testCase "variablesInScope empty" Tests.Questions.unit_variablesInScope_empty

  t446 <- testCase "variablesInScope lambda" Tests.Questions.unit_variablesInScope_lambda

  t447 <- testCase "variablesInScope let" Tests.Questions.unit_variablesInScope_let

  t448 <- testCase "variablesInScope letrec" Tests.Questions.unit_variablesInScope_letrec

  t449 <- testCase "variablesInScope case" Tests.Questions.unit_variablesInScope_case

  t450 <- testCase "variablesInScope type" Tests.Questions.unit_variablesInScope_type

  t451 <- testCase "variablesInScope shadowed" Tests.Questions.unit_variablesInScope_shadowed

  t452 <- testCase "hasGeneratedNames 1" Tests.Questions.unit_hasGeneratedNames_1

  t453 <- testCase "hasGeneratedNames 2" Tests.Questions.unit_hasGeneratedNames_2

  t454 <- testCase "hasGeneratedNames 3" Tests.Questions.unit_hasGeneratedNames_3

  t455 <- testCase "var refl" Tests.Refine.unit_var_refl

  t456 <- testCase "con refl" Tests.Refine.unit_con_refl

  t457 <- testCase "distinct con" Tests.Refine.unit_distinct_con

  t458 <- testCase "instApp" Tests.Refine.unit_instApp

  t459 <- testCase "instUnconstrainedAPP" Tests.Refine.unit_instUnconstrainedAPP

  t460 <- testCase "instAPP" Tests.Refine.unit_instAPP

  t461 <- testCase "forall fail" Tests.Refine.unit_forall_fail

  t462 <- testCase "ill kinded fail" Tests.Refine.unit_ill_kinded_fail

  t463 <- testCase "ill kinded fail 2" Tests.Refine.unit_ill_kinded_fail_2

  t464 <- testCase "alpha" Tests.Refine.unit_alpha

  t465 <- TD.tasty (TD.description "refl" <> TD.name "Tests.Refine.tasty_refl") Tests.Refine.tasty_refl

  t466 <- TD.tasty (TD.description "tgt hole" <> TD.name "Tests.Refine.tasty_tgt_hole") Tests.Refine.tasty_tgt_hole

  t467 <- TD.tasty (TD.description "src hole" <> TD.name "Tests.Refine.tasty_src_hole") Tests.Refine.tasty_src_hole

  t468 <- TD.tasty (TD.description "con" <> TD.name "Tests.Refine.tasty_con") Tests.Refine.tasty_con

  t469 <- TD.tasty (TD.description "arr app" <> TD.name "Tests.Refine.tasty_arr_app") Tests.Refine.tasty_arr_app

  t470 <- TD.tasty (TD.description "matches" <> TD.name "Tests.Refine.tasty_matches") Tests.Refine.tasty_matches

  t471 <- TD.tasty (TD.description "refinement synths" <> TD.name "Tests.Refine.tasty_refinement_synths") Tests.Refine.tasty_refinement_synths

  t472 <- TD.tasty (TD.description "scoping" <> TD.name "Tests.Refine.tasty_scoping") Tests.Refine.tasty_scoping

  t473 <- testGroup "encode" Tests.Serialization.test_encode

  t474 <- testGroup "decode" Tests.Serialization.test_decode

  t475 <- testCase "1" Tests.Subst.unit_1

  t476 <- testCase "2" Tests.Subst.unit_2

  t477 <- testCase "3" Tests.Subst.unit_3

  t478 <- TD.tasty (TD.description "subst non free id" <> TD.name "Tests.Subst.tasty_subst_non_free_id") Tests.Subst.tasty_subst_non_free_id

  t479 <- TD.tasty (TD.description "subst remove free" <> TD.name "Tests.Subst.tasty_subst_remove_free") Tests.Subst.tasty_subst_remove_free

  t480 <- TD.tasty (TD.description "subst counter indep" <> TD.name "Tests.Subst.tasty_subst_counter_indep") Tests.Subst.tasty_subst_counter_indep

  t481 <- testCase "lam 1" Tests.Transform.unit_lam_1

  t482 <- testCase "lam 2" Tests.Transform.unit_lam_2

  t483 <- testCase "lam 3" Tests.Transform.unit_lam_3

  t484 <- testCase "let 1" Tests.Transform.unit_let_1

  t485 <- testCase "let 2" Tests.Transform.unit_let_2

  t486 <- testCase "let 3" Tests.Transform.unit_let_3

  t487 <- testCase "letrec 1" Tests.Transform.unit_letrec_1

  t488 <- testCase "case 1" Tests.Transform.unit_case_1

  t489 <- testCase "case 2" Tests.Transform.unit_case_2

  t490 <- testCase "case 3" Tests.Transform.unit_case_3

  t491 <- testCase "var 1" Tests.Transform.unit_var_1

  t492 <- testCase "var 2" Tests.Transform.unit_var_2

  t493 <- testCase "var 3" Tests.Transform.unit_var_3

  t494 <- testCase "var 4" Tests.Transform.unit_var_4

  t495 <- testCase "var 5" Tests.Transform.unit_var_5

  t496 <- testCase "var 6" Tests.Transform.unit_var_6

  t497 <- testCase "var 7" Tests.Transform.unit_var_7

  t498 <- testCase "hole" Tests.Transform.unit_hole

  t499 <- testCase "ann" Tests.Transform.unit_ann

  t500 <- testCase "app" Tests.Transform.unit_app

  t501 <- testCase "con" Tests.Transform.unit_con

  t502 <- testCase "case" Tests.Transform.unit_case

  t503 <- testCase "forall 1" Tests.Transform.unit_forall_1

  t504 <- testCase "forall 2" Tests.Transform.unit_forall_2

  t505 <- testCase "forall 3" Tests.Transform.unit_forall_3

  t506 <- testCase "tEmptyHole" Tests.Transform.unit_tEmptyHole

  t507 <- testCase "tcon" Tests.Transform.unit_tcon

  t508 <- testCase "tfun" Tests.Transform.unit_tfun

  t509 <- testCase "tapp" Tests.Transform.unit_tapp

  t510 <- testCase "cross ann" Tests.Transform.unit_cross_ann

  t511 <- testCase "cross aPP" Tests.Transform.unit_cross_aPP

  t512 <- testCase "cross capture 1" Tests.Transform.unit_cross_capture_1

  t513 <- testCase "cross capture 2" Tests.Transform.unit_cross_capture_2

  t514 <- testCase "unfoldApp 1" Tests.Transform.unit_unfoldApp_1

  t515 <- testCase "unfoldApp 2" Tests.Transform.unit_unfoldApp_2

  t516 <- testCase "identity" Tests.Typecheck.unit_identity

  t517 <- testCase "undefined variable" Tests.Typecheck.unit_undefined_variable

  t518 <- testCase "const" Tests.Typecheck.unit_const

  t519 <- testCase "true" Tests.Typecheck.unit_true

  t520 <- testCase "constructor doesn't exist" Tests.Typecheck.unit_constructor_doesn't_exist

  t521 <- testCase "inc" Tests.Typecheck.unit_inc

  t522 <- testCase "compose nat" Tests.Typecheck.unit_compose_nat

  t523 <- testCase "let" Tests.Typecheck.unit_let

  t524 <- testCase "recursive let" Tests.Typecheck.unit_recursive_let

  t525 <- testCase "letrec 1" Tests.Typecheck.unit_letrec_1

  t526 <- testCase "letrec 2" Tests.Typecheck.unit_letrec_2

  t527 <- testCase "nested let" Tests.Typecheck.unit_nested_let

  t528 <- testCase "let function" Tests.Typecheck.unit_let_function

  t529 <- testCase "let in arg" Tests.Typecheck.unit_let_in_arg

  t530 <- testCase "mkTAppCon" Tests.Typecheck.unit_mkTAppCon

  t531 <- TD.tasty (TD.description "decomposeTAppCon" <> TD.name "Tests.Typecheck.tasty_decomposeTAppCon") Tests.Typecheck.tasty_decomposeTAppCon

  t532 <- testCase "typeDefKind" Tests.Typecheck.unit_typeDefKind

  t533 <- testCase "valConType" Tests.Typecheck.unit_valConType

  t534 <- testCase "case isZero" Tests.Typecheck.unit_case_isZero

  t535 <- testCase "case badEmpty" Tests.Typecheck.unit_case_badEmpty

  t536 <- testCase "case badType" Tests.Typecheck.unit_case_badType

  t537 <- testCase "ann bad" Tests.Typecheck.unit_ann_bad

  t538 <- testCase "ann insert" Tests.Typecheck.unit_ann_insert

  t539 <- testCase "app not arrow" Tests.Typecheck.unit_app_not_arrow

  t540 <- testCase "chk lam not arrow" Tests.Typecheck.unit_chk_lam_not_arrow

  t541 <- testCase "check emb" Tests.Typecheck.unit_check_emb

  t542 <- testCase "case scrutinee" Tests.Typecheck.unit_case_scrutinee

  t543 <- testCase "case branches" Tests.Typecheck.unit_case_branches

  t544 <- testCase "remove hole" Tests.Typecheck.unit_remove_hole

  t545 <- testCase "remove hole not perfect" Tests.Typecheck.unit_remove_hole_not_perfect

  t546 <- testCase "smart remove clean case" Tests.Typecheck.unit_smart_remove_clean_case

  t547 <- testCase "poly" Tests.Typecheck.unit_poly

  t548 <- testCase "poly head Nat" Tests.Typecheck.unit_poly_head_Nat

  t549 <- testCase "higher kinded match forall" Tests.Typecheck.unit_higher_kinded_match_forall

  t550 <- testCase "type hole 1" Tests.Typecheck.unit_type_hole_1

  t551 <- testCase "type hole 2" Tests.Typecheck.unit_type_hole_2

  t552 <- testCase "type hole 3" Tests.Typecheck.unit_type_hole_3

  t553 <- testCase "type hole 4" Tests.Typecheck.unit_type_hole_4

  t554 <- testCase "type hole 5" Tests.Typecheck.unit_type_hole_5

  t555 <- testCase "type hole 6" Tests.Typecheck.unit_type_hole_6

  t556 <- testCase "smart type not arrow" Tests.Typecheck.unit_smart_type_not_arrow

  t557 <- testCase "smart type forall" Tests.Typecheck.unit_smart_type_forall

  t558 <- testCase "smart type not type" Tests.Typecheck.unit_smart_type_not_type

  t559 <- testCase "smart type fun" Tests.Typecheck.unit_smart_type_fun

  t560 <- testCase "smart type inside hole 1" Tests.Typecheck.unit_smart_type_inside_hole_1

  t561 <- testCase "smart type inside hole 2" Tests.Typecheck.unit_smart_type_inside_hole_2

  t562 <- testCase "smart type inside hole 3" Tests.Typecheck.unit_smart_type_inside_hole_3

  t563 <- testCase "smart type remove 1" Tests.Typecheck.unit_smart_type_remove_1

  t564 <- testCase "smart type remove 2" Tests.Typecheck.unit_smart_type_remove_2

  t565 <- testCase "smart type remove 3" Tests.Typecheck.unit_smart_type_remove_3

  t566 <- testCase "smart type remove 4" Tests.Typecheck.unit_smart_type_remove_4

  t567 <- testCase "smart type remove 5" Tests.Typecheck.unit_smart_type_remove_5

  t568 <- testCase "prim char" Tests.Typecheck.unit_prim_char

  t569 <- testCase "prim fun" Tests.Typecheck.unit_prim_fun

  t570 <- testCase "prim fun applied" Tests.Typecheck.unit_prim_fun_applied

  t571 <- TD.tasty (TD.description "synth well typed extcxt" <> TD.name "Tests.Typecheck.tasty_synth_well_typed_extcxt") Tests.Typecheck.tasty_synth_well_typed_extcxt

  t572 <- TD.tasty (TD.description "synth well typed defcxt" <> TD.name "Tests.Typecheck.tasty_synth_well_typed_defcxt") Tests.Typecheck.tasty_synth_well_typed_defcxt

  t573 <- testCase "smartholes idempotent created hole typecache" Tests.Typecheck.unit_smartholes_idempotent_created_hole_typecache

  t574 <- testCase "smartholes idempotent holey ann" Tests.Typecheck.unit_smartholes_idempotent_holey_ann

  t575 <- testCase "smartholes idempotent alpha typecache" Tests.Typecheck.unit_smartholes_idempotent_alpha_typecache

  t576 <- TD.tasty (TD.description "smartholes idempotent syn" <> TD.name "Tests.Typecheck.tasty_smartholes_idempotent_syn") Tests.Typecheck.tasty_smartholes_idempotent_syn

  t577 <- TD.tasty (TD.description "smartholes idempotent chk" <> TD.name "Tests.Typecheck.tasty_smartholes_idempotent_chk") Tests.Typecheck.tasty_smartholes_idempotent_chk

  t578 <- testCase "tcWholeProg notice type updates" Tests.Typecheck.unit_tcWholeProg_notice_type_updates

  t579 <- TD.tasty (TD.description "tcWholeProg idempotent" <> TD.name "Tests.Typecheck.tasty_tcWholeProg_idempotent") Tests.Typecheck.tasty_tcWholeProg_idempotent

  t580 <- testCase "good defaults" Tests.Typecheck.unit_good_defaults

  t581 <- testCase "good maybeT" Tests.Typecheck.unit_good_maybeT

  t582 <- testCase "Int refl" Tests.Unification.unit_Int_refl

  t583 <- testCase "diff module not refl" Tests.Unification.unit_diff_module_not_refl

  t584 <- testCase "a refl" Tests.Unification.unit_a_refl

  t585 <- testCase "var con" Tests.Unification.unit_var_con

  t586 <- testCase "unif var con" Tests.Unification.unit_unif_var_con

  t587 <- testCase "unif var refl" Tests.Unification.unit_unif_var_refl

  t588 <- testCase "ill kinded" Tests.Unification.unit_ill_kinded

  t589 <- testCase "List Nat" Tests.Unification.unit_List_Nat

  t590 <- testCase "List" Tests.Unification.unit_List

  t591 <- testCase "List Nat refl" Tests.Unification.unit_List_Nat_refl

  t592 <- testCase "higher kinded" Tests.Unification.unit_higher_kinded

  t593 <- testCase "ill kinded 0" Tests.Unification.unit_ill_kinded_0

  t594 <- testCase "uv not in context" Tests.Unification.unit_uv_not_in_context

  t595 <- testCase "ill kinded 1" Tests.Unification.unit_ill_kinded_1

  t596 <- testCase "ill kinded 2" Tests.Unification.unit_ill_kinded_2

  t597 <- testCase "ill kinded 3" Tests.Unification.unit_ill_kinded_3

  t598 <- testCase "ill kinded 4" Tests.Unification.unit_ill_kinded_4

  t599 <- testCase "unify uv uv 1" Tests.Unification.unit_unify_uv_uv_1

  t600 <- testCase "unify uv uv 2" Tests.Unification.unit_unify_uv_uv_2

  t601 <- testCase "unify occurs" Tests.Unification.unit_unify_occurs

  t602 <- testCase "unify forall" Tests.Unification.unit_unify_forall

  t603 <- testCase "unify hole trivial 1" Tests.Unification.unit_unify_hole_trivial_1

  t604 <- testCase "unify hole trivial 2" Tests.Unification.unit_unify_hole_trivial_2

  t605 <- testCase "unify shadow" Tests.Unification.unit_unify_shadow

  t606 <- TD.tasty (TD.description "extendedUVCxt typechecks" <> TD.name "Tests.Unification.tasty_extendedUVCxt_typechecks") Tests.Unification.tasty_extendedUVCxt_typechecks

  t607 <- TD.tasty (TD.description "refl" <> TD.name "Tests.Unification.tasty_refl") Tests.Unification.tasty_refl

  t608 <- TD.tasty (TD.description "eq" <> TD.name "Tests.Unification.tasty_eq") Tests.Unification.tasty_eq

  t609 <- TD.tasty (TD.description "only sub uvs" <> TD.name "Tests.Unification.tasty_only_sub_uvs") Tests.Unification.tasty_only_sub_uvs

  t610 <- TD.tasty (TD.description "sub unifies" <> TD.name "Tests.Unification.tasty_sub_unifies") Tests.Unification.tasty_sub_unifies

  t611 <- TD.tasty (TD.description "sub checks" <> TD.name "Tests.Unification.tasty_sub_checks") Tests.Unification.tasty_sub_checks

  t612 <- TD.tasty (TD.description "unified checks" <> TD.name "Tests.Unification.tasty_unified_checks") Tests.Unification.tasty_unified_checks

  t613 <- TD.tasty (TD.description "diff kinds never unify" <> TD.name "Tests.Unification.tasty_diff_kinds_never_unify") Tests.Unification.tasty_diff_kinds_never_unify

  t614 <- TD.tasty (TD.description "sym" <> TD.name "Tests.Unification.tasty_sym") Tests.Unification.tasty_sym

  t615 <- TD.tasty (TD.description "non cyclic" <> TD.name "Tests.Unification.tasty_non_cyclic") Tests.Unification.tasty_non_cyclic

  t616 <- TD.tasty (TD.description "uv succeeds" <> TD.name "Tests.Unification.tasty_uv_succeeds") Tests.Unification.tasty_uv_succeeds

  t617 <- testCase "nextID exampleMap" Tests.Utils.unit_nextID_exampleMap

  t618 <- testCase "nextID exampleEven" Tests.Utils.unit_nextID_exampleEven

  t619 <- testCase "nextID exampleOdd" Tests.Utils.unit_nextID_exampleOdd

  t620 <- testCase "nextID exampleComprehensive" Tests.Utils.unit_nextID_exampleComprehensive

  t621 <- TD.tasty (TD.description "focus unfocus roundtrip" <> TD.name "Tests.Zipper.tasty_focus_unfocus_roundtrip") Tests.Zipper.tasty_focus_unfocus_roundtrip

  t622 <- TD.tasty (TD.description "focusOn unfocus roundtrip" <> TD.name "Tests.Zipper.tasty_focusOn_unfocus_roundtrip") Tests.Zipper.tasty_focusOn_unfocus_roundtrip

  t623 <- TD.tasty (TD.description "focusOn succeeds on valid ids" <> TD.name "Tests.Zipper.tasty_focusOn_succeeds_on_valid_ids") Tests.Zipper.tasty_focusOn_succeeds_on_valid_ids

  t624 <- testCase "1" Tests.Zipper.BindersAbove.unit_1

  t625 <- testCase "2" Tests.Zipper.BindersAbove.unit_2

  t626 <- testCase "3" Tests.Zipper.BindersAbove.unit_3

  t627 <- testCase "4" Tests.Zipper.BindersAbove.unit_4

  t628 <- testCase "5" Tests.Zipper.BindersAbove.unit_5

  t629 <- testCase "6" Tests.Zipper.BindersAbove.unit_6

  t630 <- testCase "7" Tests.Zipper.BindersAbove.unit_7

  t631 <- testCase "8" Tests.Zipper.BindersAbove.unit_8

  t632 <- testCase "9" Tests.Zipper.BindersAbove.unit_9

  t633 <- testCase "10" Tests.Zipper.BindersAbove.unit_10

  pure $ T.testGroup "Test.hs" [t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30,t31,t32,t33,t34,t35,t36,t37,t38,t39,t40,t41,t42,t43,t44,t45,t46,t47,t48,t49,t50,t51,t52,t53,t54,t55,t56,t57,t58,t59,t60,t61,t62,t63,t64,t65,t66,t67,t68,t69,t70,t71,t72,t73,t74,t75,t76,t77,t78,t79,t80,t81,t82,t83,t84,t85,t86,t87,t88,t89,t90,t91,t92,t93,t94,t95,t96,t97,t98,t99,t100,t101,t102,t103,t104,t105,t106,t107,t108,t109,t110,t111,t112,t113,t114,t115,t116,t117,t118,t119,t120,t121,t122,t123,t124,t125,t126,t127,t128,t129,t130,t131,t132,t133,t134,t135,t136,t137,t138,t139,t140,t141,t142,t143,t144,t145,t146,t147,t148,t149,t150,t151,t152,t153,t154,t155,t156,t157,t158,t159,t160,t161,t162,t163,t164,t165,t166,t167,t168,t169,t170,t171,t172,t173,t174,t175,t176,t177,t178,t179,t180,t181,t182,t183,t184,t185,t186,t187,t188,t189,t190,t191,t192,t193,t194,t195,t196,t197,t198,t199,t200,t201,t202,t203,t204,t205,t206,t207,t208,t209,t210,t211,t212,t213,t214,t215,t216,t217,t218,t219,t220,t221,t222,t223,t224,t225,t226,t227,t228,t229,t230,t231,t232,t233,t234,t235,t236,t237,t238,t239,t240,t241,t242,t243,t244,t245,t246,t247,t248,t249,t250,t251,t252,t253,t254,t255,t256,t257,t258,t259,t260,t261,t262,t263,t264,t265,t266,t267,t268,t269,t270,t271,t272,t273,t274,t275,t276,t277,t278,t279,t280,t281,t282,t283,t284,t285,t286,t287,t288,t289,t290,t291,t292,t293,t294,t295,t296,t297,t298,t299,t300,t301,t302,t303,t304,t305,t306,t307,t308,t309,t310,t311,t312,t313,t314,t315,t316,t317,t318,t319,t320,t321,t322,t323,t324,t325,t326,t327,t328,t329,t330,t331,t332,t333,t334,t335,t336,t337,t338,t339,t340,t341,t342,t343,t344,t345,t346,t347,t348,t349,t350,t351,t352,t353,t354,t355,t356,t357,t358,t359,t360,t361,t362,t363,t364,t365,t366,t367,t368,t369,t370,t371,t372,t373,t374,t375,t376,t377,t378,t379,t380,t381,t382,t383,t384,t385,t386,t387,t388,t389,t390,t391,t392,t393,t394,t395,t396,t397,t398,t399,t400,t401,t402,t403,t404,t405,t406,t407,t408,t409,t410,t411,t412,t413,t414,t415,t416,t417,t418,t419,t420,t421,t422,t423,t424,t425,t426,t427,t428,t429,t430,t431,t432,t433,t434,t435,t436,t437,t438,t439,t440,t441,t442,t443,t444,t445,t446,t447,t448,t449,t450,t451,t452,t453,t454,t455,t456,t457,t458,t459,t460,t461,t462,t463,t464,t465,t466,t467,t468,t469,t470,t471,t472,t473,t474,t475,t476,t477,t478,t479,t480,t481,t482,t483,t484,t485,t486,t487,t488,t489,t490,t491,t492,t493,t494,t495,t496,t497,t498,t499,t500,t501,t502,t503,t504,t505,t506,t507,t508,t509,t510,t511,t512,t513,t514,t515,t516,t517,t518,t519,t520,t521,t522,t523,t524,t525,t526,t527,t528,t529,t530,t531,t532,t533,t534,t535,t536,t537,t538,t539,t540,t541,t542,t543,t544,t545,t546,t547,t548,t549,t550,t551,t552,t553,t554,t555,t556,t557,t558,t559,t560,t561,t562,t563,t564,t565,t566,t567,t568,t569,t570,t571,t572,t573,t574,t575,t576,t577,t578,t579,t580,t581,t582,t583,t584,t585,t586,t587,t588,t589,t590,t591,t592,t593,t594,t595,t596,t597,t598,t599,t600,t601,t602,t603,t604,t605,t606,t607,t608,t609,t610,t611,t612,t613,t614,t615,t616,t617,t618,t619,t620,t621,t622,t623,t624,t625,t626,t627,t628,t629,t630,t631,t632,t633]
ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients
main :: IO ()
main = do
  args <- E.getArgs
  E.withArgs ([] ++ args) $    tests >>= T.defaultMainWithIngredients ingredients
