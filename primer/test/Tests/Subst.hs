module Tests.Subst where

import Foreword

import Data.Set qualified as Set
import Hedgehog (MonadGen, discard, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Builtins (tBool, tList)
import Primer.Core (
  ID (ID),
  Kind' (KType),
  TyVarName,
  Type' (..),
 )
import Primer.Core.DSL
import Primer.Core.Utils (boundVarsTy, forgetTypeMetadata, freeVarsTy)
import Primer.Gen.Core.Raw (genTyVarName)
import Primer.Gen.Core.Typed (forAllT, genWTKind, genWTType, propertyWT)
import Primer.Subst
import Primer.Test.TestM (evalTestM)
import Tasty (Property, withDiscards)
import Test.Tasty.HUnit hiding (assert)
import Tests.AlphaEquality (Alpha (Alpha))
import Tests.Gen.Core.Typed (inExtendedLocalCxt)

unit_1 :: Assertion
unit_1 =
  create_ (tcon tBool)
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tvar "a")

unit_2 :: Assertion
unit_2 =
  create_ (tforall "a" (KType ()) $ tvar "a")
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tforall "a" (KType ()) $ tvar "a")

unit_3 :: Assertion
unit_3 =
  create_ (tforall "b" (KType ()) $ tcon tList `tapp` tcon tBool)
    @=? substTy'
      "a"
      (create_ $ tcon tBool)
      (create_ $ tforall "b" (KType ()) $ tcon tList `tapp` tvar "a")

-- Substituting a variable that does not occur free is the identity
tasty_subst_non_free_id :: Property
tasty_subst_non_free_id = propertyWT [] $ do
  t <- forAllT $ genWTType =<< genWTKind
  let free = freeVarsTy t
  let bound = boundVarsTy t
  a <-
    forAllT $
      frequency
        [ (1, Just genTyVarName)
        , (1, element bound)
        ]
  when (Set.member a free) discard
  -- We frequently try substituting @a@ with some variable occuring in @t@
  -- as (terms containing) those are more likely to flush out bugs in substitution
  s <-
    forAllT $
      frequency
        [ (1, Just $ genWTType =<< genWTKind)
        , (1, TVar () <<$>> element free)
        , (1, TVar () <<$>> element bound)
        ]
  -- Currently this equality is only up to alpha, since we go under @∀@s
  -- (and thus maybe rename them to avoid capture) before noticing that
  -- the target does not appear underneath them.
  Alpha (substTy' a s t) === Alpha t

-- Substituting a free variable results in a term where that variable is no longer free
tasty_subst_remove_free :: Property
tasty_subst_remove_free = withDiscards 300 $ propertyWT [] $ inExtendedLocalCxt $ do
  t <- forAllT $ genWTType =<< genWTKind
  let free = freeVarsTy t
  when (Set.null free) discard
  a <- forAllT $ Gen.element $ Set.toList free
  let free' = Set.delete a free
  -- We frequently try substituting @a@ with some variable occuring in @t@
  -- as (terms containing) those are more likely to flush out bugs in substitution
  s <-
    forAllT $
      frequency
        [ (1, Just $ genWTType =<< genWTKind)
        , (1, TVar () <<$>> element free')
        , (1, TVar () <<$>> element (boundVarsTy t))
        ]
  freeVarsTy (substTy' a s t) === free' <> freeVarsTy s

-- Substitution does not depend on the state of our fresh name supply
-- (up to alpha, of course)
tasty_subst_counter_indep :: Property
tasty_subst_counter_indep = withDiscards 300 $ propertyWT [] $ inExtendedLocalCxt $ do
  t <- forAllT $ genWTType =<< genWTKind
  let free = freeVarsTy t
  when (Set.null free) discard
  a <- forAllT $ Gen.element $ Set.toList free
  -- We frequently try substituting @a@ with some variable occuring in @t@
  -- as (terms containing) those are more likely to flush out bugs in substitution
  s <-
    forAllT $
      frequency
        [ (1, Just $ genWTType =<< genWTKind)
        , (1, TVar () <<$>> element free)
        , (1, TVar () <<$>> element (boundVarsTy t))
        ]
  let subst i = evalTestM i (substTy a s t)
  i <- forAllT $ ID <$> Gen.int (Range.linear 0 100)
  j <- forAllT $ ID <$> Gen.int (Range.linear 0 100)
  Alpha (subst i) === Alpha (subst j)

create_ :: S (Type' a b) -> Type' () ()
create_ = forgetTypeMetadata . create'

substTy' :: TyVarName -> Type' () () -> Type' () () -> Type' () ()
substTy' n s t = evalTestM 0 $ substTy n s t

-- Pick an element from this set, without throwing an error if it is empty
element :: MonadGen m => Set a -> Maybe (m a)
element s = if null s then Nothing else Just $ Gen.element $ Set.toList s

-- A utility to work with 'element'
frequency :: MonadGen m => [(Int, Maybe (m a))] -> m a
frequency = Gen.frequency . mapMaybe sequence
