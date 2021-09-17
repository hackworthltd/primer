{-# LANGUAGE TypeApplications #-}

-- |
-- This module contains tests that the generators in "Gen.Core.Typed" do in fact
-- generate well-typed terms
module Tests.Gen.Core.Typed where

import Control.Monad.Except (runExceptT)
import Control.Monad.Morph (lift)
import Control.Monad.Reader (ask, local)
import qualified Data.Map as M
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Gen.Core.Typed
  ( WT,
    genChk,
    genCxtExtendingGlobal,
    genCxtExtendingLocal,
    genSyns,
    genWTKind,
    genWTType,
    propertyWT,
  )
import Hedgehog
  ( Property,
    PropertyT,
    annotateShow,
    diff,
    failure,
    withDiscards,
    withTests,
    (===),
  )
import Hedgehog.Internal.Property (forAllT)
import Primer.Core
  ( Expr,
    Kind (KType),
    Meta,
    Type,
    Type',
    defaultTypeDefs,
  )
import Primer.Core.Utils
  ( forgetIDs,
    forgetTypeIDs,
    generateIDs,
    generateTypeIDs,
  )
import Primer.Typecheck
  ( Cxt (..),
    ExprT,
    SmartHoles (NoSmartHoles),
    TypeError,
    buildTypingContext,
    check,
    checkKind,
    checkValidContext,
    consistentTypes,
    initialCxt,
    synth,
    synthKind,
  )

inExtendedGlobalCxt :: PropertyT WT a -> PropertyT WT a
inExtendedGlobalCxt p = do
  cxt <- ask
  cxtE <- forAllT genCxtExtendingGlobal
  -- NB: we only extend typedefs and globals (see hprop_genCxtExtending_is_extension)
  annotateShow $ M.differenceWith (\l r -> if l == r then Nothing else Just l) (typeDefs cxtE) (typeDefs cxt)
  annotateShow $ M.differenceWith (\l r -> if l == r then Nothing else Just l) (globalCxt cxtE) (globalCxt cxt)
  local (const cxtE) p

inExtendedLocalCxt :: PropertyT WT a -> PropertyT WT a
inExtendedLocalCxt p = do
  cxt <- ask
  cxtE <- forAllT genCxtExtendingLocal
  -- NB: we only extend locals (see hprop_genCxtExtending_is_extension)
  annotateShow $ M.differenceWith (\l r -> if l == r then Nothing else Just l) (localCxt cxtE) (localCxt cxt)
  local (const cxtE) p

propertyWTInExtendedGlobalCxt :: Cxt -> PropertyT WT () -> Property
propertyWTInExtendedGlobalCxt cxt = propertyWT cxt . inExtendedGlobalCxt

propertyWTInExtendedLocalGlobalCxt :: Cxt -> PropertyT WT () -> Property
propertyWTInExtendedLocalGlobalCxt cxt = propertyWT cxt . inExtendedLocalCxt . inExtendedGlobalCxt

hprop_genTy :: Property
hprop_genTy = withTests 1000 $
  propertyWTInExtendedGlobalCxt (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
    k <- forAllT genWTKind
    ty <- forAllT $ genWTType k
    ty' <- checkKindTest k =<< generateTypeIDs ty
    ty === forgetTypeIDs ty' -- check no smart holes stuff happened

-- | Lift 'checkKind' into a property
checkKindTest :: HasCallStack => Kind -> Type -> PropertyT WT (Type' (Meta Kind))
checkKindTest k t = do
  x <- lift $ runExceptT @TypeError $ checkKind k t
  case x of
    Left e -> withFrozenCallStack $ annotateShow e >> failure
    Right s -> pure s

-- | Lift 'synthKind' into a property
synthKindTest :: HasCallStack => Type -> PropertyT WT (Kind, Type' (Meta Kind))
synthKindTest t = do
  x <- lift $ runExceptT @TypeError $ synthKind t
  case x of
    Left e -> withFrozenCallStack $ annotateShow e >> failure
    Right s -> pure s

-- Lift 'checkValidContext' into a property
checkValidContextTest :: HasCallStack => Cxt -> PropertyT WT ()
checkValidContextTest t = do
  x <- lift $ runExceptT @TypeError $ checkValidContext t
  case x of
    Left e -> withFrozenCallStack $ annotateShow e >> failure
    Right s -> pure s

-- This indirectly also tests genCxtExtendingLocal, genCxtExtendingGlobal and genTypeDefGroup
hprop_genCxtExtending_typechecks :: Property
hprop_genCxtExtending_typechecks = withTests 1000 $
  propertyWT (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
    cxt <- forAllT genCxtExtendingGlobal
    checkValidContextTest cxt
    cxt' <- forAllT $ local (const cxt) genCxtExtendingLocal
    checkValidContextTest cxt'

hprop_genCxtExtending_is_extension :: Property
hprop_genCxtExtending_is_extension =
  withTests 1000 $
    let cxt0 = initialCxt NoSmartHoles
     in propertyWT cxt0 $ do
          cxt1 <- forAllT genCxtExtendingGlobal
          diff cxt0 extendsGlobal cxt1
          cxt2 <- forAllT $ local (const cxt1) genCxtExtendingGlobal
          diff cxt1 extendsGlobal cxt2
          cxt3 <- forAllT $ local (const cxt2) genCxtExtendingLocal
          diff cxt2 extendsLocal cxt3
          cxt4 <- forAllT $ local (const cxt3) genCxtExtendingLocal
          diff cxt3 extendsLocal cxt4
  where
    extendsGlobal
      (Cxt {typeDefs = tds1, localCxt = lc1, globalCxt = gc1, smartHoles = sh1})
      (Cxt {typeDefs = tds2, localCxt = lc2, globalCxt = gc2, smartHoles = sh2}) =
        tds1 `M.isSubmapOf` tds2
          && lc1 == lc2 -- we don't extend the locals
          && lc1 == mempty -- and it doesn't make too much sense to do a global extension if already have locals in scope
          && gc1 `M.isSubmapOf` gc2
          && sh1 == sh2
    extendsLocal
      (Cxt {typeDefs = tds1, localCxt = lc1, globalCxt = gc1, smartHoles = sh1})
      (Cxt {typeDefs = tds2, localCxt = lc2, globalCxt = gc2, smartHoles = sh2}) =
        tds1 == tds2
          && lc1 `M.isSubmapOf` lc2 -- we only extend the locals
          && gc1 == gc2
          && sh1 == sh2

hprop_genSyns :: Property
hprop_genSyns = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
      tgtTy <- forAllT $ genWTType KType
      _ :: Type' (Meta Kind) <- checkKindTest KType =<< generateTypeIDs tgtTy
      (e, ty) <- forAllT $ genSyns tgtTy
      (ty', e') <- synthTest =<< generateIDs e
      annotateShow e'
      annotateShow ty'
      diff ty consistentTypes $ forgetTypeIDs tgtTy
      ty === ty'
      e === forgetIDs e' -- check no smart holes stuff happened

hprop_genChk :: Property
hprop_genChk = withTests 1000 $
  withDiscards 2000 $
    propertyWTInExtendedLocalGlobalCxt (buildTypingContext defaultTypeDefs mempty NoSmartHoles) $ do
      ty <- forAllT $ genWTType KType
      _ :: Type' (Meta Kind) <- checkKindTest KType =<< generateTypeIDs ty
      t <- forAllT $ genChk ty
      t' <- checkTest ty =<< generateIDs t
      t === forgetIDs t' -- check no smart holes stuff happened

-- Lift 'synth' into a property
synthTest :: HasCallStack => Expr -> PropertyT WT (Type' (), ExprT)
synthTest e = do
  x <- lift $ runExceptT @TypeError $ synth e
  case x of
    Left err -> withFrozenCallStack $ annotateShow err >> failure
    Right y -> pure y

-- Lift 'check' into a property
checkTest :: HasCallStack => Type' () -> Expr -> PropertyT WT ExprT
checkTest ty t = do
  x <- lift $ runExceptT @TypeError $ check ty t
  case x of
    Left e -> withFrozenCallStack $ annotateShow e >> failure
    Right y -> pure y
