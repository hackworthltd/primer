module Tests.Subst where

import Optics (set)
import Primer.Core
  ( Kind (KType),
    Type',
    _typeMeta,
  )
import Primer.Core.DSL
import Primer.Name
import Primer.Subst
import Test.Tasty.HUnit hiding (assert)
import TestM (evalTestM)

unit_1 :: Assertion
unit_1 =
  create' (tcon "Bool")
    @=? substTy'
      "a"
      (create' $ tcon "Bool")
      (create' $ tvar "a")

unit_2 :: Assertion
unit_2 =
  create' (tforall "a" KType $ tvar "a")
    @=? substTy'
      "a"
      (create' $ tcon "Bool")
      (create' $ tforall "a" KType $ tvar "a")

unit_3 :: Assertion
unit_3 =
  create' (tforall "b" KType $ tcon "List" `tapp` tcon "Bool")
    @=? substTy'
      "a"
      (create' $ tcon "Bool")
      (create' $ tforall "b" KType $ tcon "List" `tapp` tvar "a")

create' :: S (Type' a) -> Type' ()
create' = set _typeMeta () . fst . create

substTy' :: Name -> Type' () -> Type' () -> Type' ()
substTy' n s t = evalTestM 0 $ substTy n s t
