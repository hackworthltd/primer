-- | Tests for Primer.Zipper
module Tests.Zipper where

import Foreword

import Hedgehog hiding (Property, property)
import Hedgehog.Gen qualified as Gen
import Optics ((^..))
import Primer.Core
import Primer.Core.Utils (exprIDs)
import Primer.Gen.Core.Raw (
  evalExprGen,
  genExpr,
  runExprGen,
 )
import Primer.Zipper
import Tasty (Property, property)

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
      Just (InKind k _) -> annotateShow (target k) >> footnote "impossible for focusOn to return a kind" >> failure
      _ -> annotateShow i >> failure
