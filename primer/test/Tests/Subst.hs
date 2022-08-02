module Tests.Subst where

import Foreword

import Primer.Builtins (tBool, tList)
import Primer.Core (
  Kind (KType),
  TyVarName,
  Type',
 )
import Primer.Core.DSL
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Subst
import Test.Tasty.HUnit hiding (assert)
import TestM (evalTestM)

unit_1 :: Assertion
unit_1 =
  create_ (tcon tBool)
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tvar "a")

unit_2 :: Assertion
unit_2 =
  create_ (tforall "a" KType $ tvar "a")
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tforall "a" KType $ tvar "a")

unit_3 :: Assertion
unit_3 =
  create_ (tforall "b" KType $ tcon tList `tapp` tcon tBool)
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tforall "b" KType $ tcon tList `tapp` tvar "a")

create_ :: S (Type' a) -> Type' ()
create_ = forgetTypeMetadata . create'

substTy' :: TyVarName -> Type' () -> Type' () -> Type' ()
substTy' n s t = evalTestM 0 $ substTy n s t
