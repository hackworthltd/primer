-- | Tests for Primer.Zipper
module Tests.Zipper where

import Foreword

import Data.Set qualified as Set
import Hedgehog hiding (Property, property)
import Hedgehog.Gen qualified as Gen
import Optics ((^..))
import Primer.Core
import Primer.Core.DSL (aPP, ann, branch', case_, create', emptyHole, hole, ktype, lAM, lam, tEmptyHole, tapp, tforall, thole)
import Primer.Core.Utils (exprIDs)
import Primer.Gen.Core.Raw (
  evalExprGen,
  genExpr,
  runExprGen,
 )
import Primer.Zipper
import Tasty (Property, property)
import Test.Tasty.HUnit (Assertion, (@?=))

-- | @unfocus . focus == id@
tasty_focus_unfocus_roundtrip :: Property
tasty_focus_unfocus_roundtrip = property $ do
  (e, _) <- forAll $ runExprGen 0 genExpr
  (unfocusExpr . focus) e === e

-- | @unfocus . focusOn i . focus == id@ for any valid ID @i@
tasty_focusOn_unfocus_roundtrip :: Property
tasty_focusOn_unfocus_roundtrip = property $ do
  e <- forAll $ evalExprGen 0 genExpr
  i <- forAll $ Gen.element $ e ^.. exprIDs
  case focusOn i e of
    Just e' -> unfocus e' === e
    _ -> annotateShow i >> failure

-- | For any valid ID @i@ in an expression @e@, @focusOn i e@ should succeed
-- and return a zipper focusing on a node with the matching ID
tasty_focusOn_succeeds_on_valid_ids :: Property
tasty_focusOn_succeeds_on_valid_ids = property $ do
  e <- forAll $ evalExprGen 0 genExpr
  forM_ (e ^.. exprIDs) $ \i -> do
    case focusOn i e of
      Just (InExpr e') -> getID (target e') === i
      Just (InType t) -> getID (target t) === i
      Just (InBind (BindCase b)) -> getID (target b) === i
      Just (InKind k) -> getID (target k) === i
      _ -> annotateShow i >> failure

unit_binders_below_type :: Assertion
unit_binders_below_type =
  let t = create' $ tapp tEmptyHole $ tforall "a" ktype $ thole $ tforall "b" ktype tEmptyHole
   in bindersBelowTy (focus t) @?= Set.fromList ["a", "b"]

unit_binders_below :: Assertion
unit_binders_below =
  let e =
        create'
          $ ann
            ( lam "x"
                $ case_
                  (lAM "y" emptyHole)
                  [branch' (["M"], "C") [("z", Nothing), ("w", Nothing)] $ aPP (hole $ lam "v" emptyHole) (tapp tEmptyHole $ tforall "a" ktype $ tforall "b" ktype tEmptyHole)]
            )
            (thole $ tforall "c" ktype tEmptyHole)
   in bindersBelow (focus e) @?= Set.fromList ["x", "y", "z", "w", "v", "a", "b", "c"]
