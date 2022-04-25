module Tests.FreeVars where

import Foreword

import qualified Data.Set as Set
import Primer.Builtins
import Primer.Core (Kind (KType))
import Primer.Core.DSL
import Primer.Core.Utils
import Test.Tasty.HUnit

unit_1 :: Assertion
unit_1 = freeVars (fst $ create emptyHole) @=? Set.empty

unit_2 :: Assertion
unit_2 =
  Set.fromList ["f", "y", "b"]
    @=? freeVars (fst $ create t)
  where
    t =
      ann
        ( app
            ( lam "x" $
                case_
                  (lvar "x")
                  [ branch cZero [] $ con cTrue
                  , branch cSucc [("n", Nothing)] (app (lvar "f") (lvar "n"))
                  ]
            )
            (lvar "y")
        )
        (tforall "a" KType $ tcon' ["M"] "T" `tapp` tvar "a" `tapp` tvar "b")
