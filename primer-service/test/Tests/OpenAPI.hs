{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.OpenAPI where

import Foreword

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.OpenApi (ToSchema, validatePrettyToJSON)
import Data.Text qualified as T
import Data.Time (
  UTCTime (..),
  fromGregorian,
  secondsToDiffTime,
 )
import Data.UUID (UUID, fromWords64)
import Hedgehog (
  Gen,
  MonadGen,
  annotate,
  failure,
  forAll,
 )
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R
import Primer.API (
  ApplyActionBody (..),
  Def (Def),
  Module (Module),
  NodeBody (BoxBody, NoBody, TextBody),
  NodeFlavor,
  NodeSelection (..),
  Prog (Prog),
  Selection (..),
  Tree,
  viewTreeExpr,
  viewTreeType,
 )
import Primer.Action.Available qualified as Available
import Primer.Core (GVarName, ID (ID), Level, NodeType (..))
import Primer.Database (
  LastModified (..),
  Session (Session),
  SessionName,
  safeMkSessionName,
 )
import Primer.Gen.API (genExprTreeOpts)
import Primer.Gen.Core.Raw (
  ExprGen,
  evalExprGen,
  genExpr,
  genGVarName,
  genLVarName,
  genModuleName,
  genName,
  genTyConName,
  genType,
  genValConName,
 )
import Primer.Name (Name)
import Primer.OpenAPI ()
import Primer.Pagination (NonNeg, Paginated (Paginated), PaginatedMeta (..), Positive, mkNonNeg, mkPositive)
import Primer.Servant.OpenAPI (API)
import Primer.Server (openAPIInfo)
import Servant.OpenApi.Test (validateEveryToJSON)
import Tasty (Property, property)
import Test.Hspec (Spec)
import Test.QuickCheck (Arbitrary (arbitrary), arbitraryBoundedEnum)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

-- Note: the golden output can be generated by running
--
-- @make openapi.json@
--
-- from the project's top-level directory and copying the output to
-- the path below.
test_golden :: TestTree
test_golden =
  testGroup
    "golden"
    [ goldenVsString "openapi.json" "test/outputs/OpenAPI/openapi.json" $
        pure $
          encodePretty openAPIInfo
    ]

testToJSON :: (ToJSON a, ToSchema a, Show a) => Gen a -> Property
testToJSON g = property $ do
  x <- forAll g
  case validatePrettyToJSON x of
    Nothing -> pure ()
    Just errs -> annotate errs >> failure

genSessionName :: Gen SessionName
genSessionName = safeMkSessionName <$> G.text (R.linear 1 100) G.unicode

tasty_SessionName :: Property
tasty_SessionName = testToJSON genSessionName

genUUID :: Gen UUID
genUUID = fromWords64 <$> G.word64 R.linearBounded <*> G.word64 R.linearBounded

-- Hedgehog doesn't have a generator for 'UTCTime', so we take it from
-- here:
--
-- https://github.com/hedgehogqa/haskell-hedgehog/issues/215#issue-349965804
--
-- Note that this generator is not meant to be extensive. We're just
-- trying to generate reasonably variable (pure) values for 'UTCTime'.
-- It suffices for our very simple needs of ensuring that JSON
-- serialization of our session type works, and it is not our intent
-- to ensure that Aeson's 'UTCTime' serialization is correct, as we'd
-- expect that to have been tested upstream.
genLastModified :: MonadGen m => m LastModified
genLastModified = do
  y <- toInteger <$> G.int (R.constant 2000 2023)
  m <- G.int (R.constant 1 12)
  d <- G.int (R.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> G.int (R.constant 0 86401)
  let diffTime = secondsToDiffTime secs
  pure $ LastModified $ UTCTime day diffTime

genSession :: Gen Session
genSession = Session <$> genUUID <*> genSessionName <*> genLastModified

tasty_Session :: Property
tasty_Session = testToJSON genSession

-- NB: don't want to use genID, as that is just "next free ID"
tasty_ID :: Property
tasty_ID = testToJSON $ ID <$> G.int (R.linear 0 1000)

tasty_Name :: Property
tasty_Name = testToJSON $ evalExprGen 0 genName

tasty_ModuleName :: Property
tasty_ModuleName = testToJSON $ evalExprGen 0 genModuleName

tasty_TyConName :: Property
tasty_TyConName = testToJSON $ evalExprGen 0 genTyConName

tasty_ValConName :: Property
tasty_ValConName = testToJSON $ evalExprGen 0 genValConName

tasty_GVarName :: Property
tasty_GVarName = testToJSON $ evalExprGen 0 genGVarName

tasty_LVarName :: Property
tasty_LVarName = testToJSON genLVarName

tasty_Tree :: Property
tasty_Tree = testToJSON genTree

-- We only test the trees which we create by viewing either a Type or Expr
genTree :: Gen Tree
genTree = evalExprGen 0 $ G.choice [genExprTree, genTypeTree]

genExprTree :: ExprGen Tree
genExprTree = viewTreeExpr <$> genExprTreeOpts <*> genExpr

genTypeTree :: ExprGen Tree
genTypeTree = viewTreeType <$> genType

tasty_NodeBody :: Property
tasty_NodeBody =
  testToJSON $
    G.choice
      [ TextBody <$> G.text (R.linear 1 20) G.unicode
      , BoxBody <$> genTree
      , pure NoBody
      ]

tasty_NodeFlavor :: Property
tasty_NodeFlavor = testToJSON $ G.enumBounded @_ @NodeFlavor

genDef :: ExprGen Def
genDef = Def <$> genGVarName <*> genExprTree <*> G.maybe genTypeTree

tasty_Def :: Property
tasty_Def = testToJSON $ evalExprGen 0 genDef

genModule :: ExprGen Module
genModule =
  Module
    <$> genModuleName
    <*> G.bool
    <*> G.list (R.linear 0 3) genTyConName
    <*> G.list (R.linear 0 3) genDef

tasty_Module :: Property
tasty_Module = testToJSON $ evalExprGen 0 genModule

genProg :: Gen Prog
genProg = evalExprGen 0 $ Prog <$> G.list (R.linear 0 3) genModule

tasty_Prog :: Property
tasty_Prog = testToJSON genProg

genPositive :: Gen Positive
genPositive = G.just $ mkPositive <$> G.int (R.linear 1 1000)

tasty_Positive :: Property
tasty_Positive = testToJSON genPositive

genNonNeg :: Gen NonNeg
genNonNeg = G.just $ mkNonNeg <$> G.int (R.linear 0 1000)

tasty_NonNeg :: Property
tasty_NonNeg = testToJSON genNonNeg

genPaginatedMeta :: Gen PaginatedMeta
genPaginatedMeta = do
  ti <- genNonNeg
  ps <- genPositive
  fp <- genPositive
  pp <- G.maybe genPositive
  tp <- genPositive
  np <- G.maybe genPositive
  lp <- genPositive
  pure $
    PM
      { totalItems = ti
      , pageSize = ps
      , firstPage = fp
      , prevPage = pp
      , thisPage = tp
      , nextPage = np
      , lastPage = lp
      }

tasty_PaginatedMeta :: Property
tasty_PaginatedMeta = testToJSON genPaginatedMeta

genPaginatedSession :: Gen (Paginated Session)
genPaginatedSession = Paginated <$> genPaginatedMeta <*> G.list (R.linear 0 10) genSession

tasty_Paginated :: Property
tasty_Paginated = testToJSON genPaginatedSession

spec_alljson :: Spec
spec_alljson = validateEveryToJSON (Proxy @API)

-- Orphan instances for validateEveryToJSON
-- NB: these have no shrinking: 'hedgehog' cannot convert hedgehog
-- style shrinking into QuickCheck style shrinking.
instance Arbitrary UUID where
  arbitrary = hedgehog genUUID
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
instance Arbitrary (Paginated Session) where
  arbitrary = hedgehog genPaginatedSession
instance Arbitrary Prog where
  arbitrary = hedgehog genProg
instance Arbitrary ApplyActionBody where
  arbitrary = ApplyActionBody <$> arbitrary <*> arbitrary
instance Arbitrary Available.Option where
  arbitrary = Available.Option <$> arbitrary <*> arbitrary
instance Arbitrary Available.Options where
  arbitrary = Available.Options <$> arbitrary <*> arbitrary
instance Arbitrary Selection where
  arbitrary = Selection <$> arbitrary <*> arbitrary
instance Arbitrary NodeSelection where
  arbitrary = NodeSelection <$> arbitrary <*> arbitrary
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary
instance Arbitrary Level where
  arbitrary = arbitraryBoundedEnum
deriving newtype instance Arbitrary ID
instance Arbitrary Name where
  arbitrary = hedgehog genName
instance Arbitrary NodeType where
  arbitrary = arbitraryBoundedEnum
instance Arbitrary GVarName where
  arbitrary = hedgehog genGVarName
instance Arbitrary Available.InputAction where
  arbitrary = arbitraryBoundedEnum
instance Arbitrary Available.NoInputAction where
  arbitrary = arbitraryBoundedEnum
instance Arbitrary Available.Action where
  arbitrary = either Available.NoInput Available.Input <$> arbitrary
