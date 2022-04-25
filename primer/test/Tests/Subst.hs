module Tests.Subst where

import Foreword

import Optics (set)
import Primer.Builtins (tBool, tList)
import Primer.Core (
  Kind (KType),
  TyVarName,
  Type',
  _typeMeta,
 )
import Primer.Core.DSL
import Primer.Subst
import Test.Tasty.HUnit hiding (assert)
import TestM (evalTestM)

unit_1 :: Assertion
unit_1 =
  create' (tcon tBool)
    @=? substTy'
      "a"
      (create' $ tcon tBool)
      (create' $ tvar "a")

unit_2 :: Assertion
unit_2 =
  create' (tforall "a" KType $ tvar "a")
    @=? substTy'
      "a"
      (create' $ tcon tBool)
      (create' $ tforall "a" KType $ tvar "a")

unit_3 :: Assertion
unit_3 =
  create' (tforall "b" KType $ tcon tList `tapp` tcon tBool)
    @=? substTy'
      "a"
      (create' $ tcon tBool)
      (create' $ tforall "b" KType $ tcon tList `tapp` tvar "a")

create' :: S (Type' a) -> Type' ()
create' = set _typeMeta () . fst . create

substTy' :: TyVarName -> Type' () -> Type' () -> Type' ()
substTy' n s t = evalTestM 0 $ substTy n s t
