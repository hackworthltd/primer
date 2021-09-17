{-# LANGUAGE ExistentialQuantification #-}

module Tests.Serialisation where

import Data.Aeson hiding (Error, Result, Success)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Functor
import qualified Data.Map.Strict as Map
import Primer.Action (Action (Move, SetCursor), ActionError (IDNotFound), Movement (Child1))
import Primer.App (
  Log (..),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgAction (BodyAction, MoveToDef),
  ProgError (NoDefSelected),
  Result (..),
  Selection (..),
 )
import Primer.Core (
  Def (..),
  Expr,
  Expr' (EmptyHole),
  ExprMeta,
  ID (..),
  Kind (KFun, KType),
  Meta (..),
  Type' (TApp, TCon, TEmptyHole),
  TypeCache (TCSynthed),
  TypeCacheBoth (TCBoth),
  TypeDef (..),
  TypeMeta,
  ValCon (..),
 )
import Primer.Name (unsafeMkName)
import Primer.Typecheck (SmartHoles (SmartHoles))
import System.FilePath (takeBaseName)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Prelude hiding (log)

-- | Check that encoding the value produces the file.
test_encode :: TestTree
test_encode =
  testGroup "encode" $
    fixtures <&> \(Fixture x path) ->
      goldenVsString (takeBaseName path) path (pure $ encodePretty x)

-- | Check that decoding the file produces the value.
test_decode :: TestTree
test_decode =
  testGroup "decode" $
    fixtures <&> \(Fixture x path) ->
      testCase (takeBaseName path) $ either assertFailure (x @=?) =<< eitherDecodeFileStrict path

-- | A fixture holds some value which is JSON serialisable and path to a
-- fixture file which should contain a JSON representation of that value.
data Fixture = forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Fixture a FilePath

mkFixture :: (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> Fixture
mkFixture name x = Fixture x ("test/fixtures/" <> name <> ".json")

-- | A list of fixtures we will test.
-- When you add a new type to the API, add the corresponding fixture here.
-- (You can generate the fixture file via the frontend - see the README).
fixtures :: [Fixture]
fixtures =
  let id0 = ID 0
      typeMeta :: TypeMeta
      typeMeta = Meta (ID 0) (Just KType) Nothing
      exprMeta :: ExprMeta
      exprMeta = Meta (ID 0) (Just (TCSynthed $ TEmptyHole ())) Nothing
      actionError :: ActionError
      actionError = IDNotFound id0
      expr :: Expr
      expr = EmptyHole exprMeta
      log :: Log
      log = Log [[BodyAction [Move Child1]]]
      def :: Def
      def = Def {defID = 1, defName = "main", defExpr = expr, defType = TEmptyHole typeMeta}
      typeDef :: TypeDef
      typeDef =
        TypeDef
          { typeDefName = "T"
          , typeDefParameters = [("a", KType), ("b", KFun KType KType)]
          , typeDefConstructors = [ValCon "C" [TApp () (TCon () "b") (TCon () "a"), TCon () "Nat"]]
          , typeDefNameHints = []
          }
      progerror :: ProgError
      progerror = NoDefSelected
      progaction :: ProgAction
      progaction = MoveToDef 0
      prog =
        Prog
          { progTypes = [typeDef]
          , progDefs = Map.singleton 1 def
          , progSelection = Just selection
          , progSmartHoles = SmartHoles
          , progLog = log
          }
      selection :: Selection
      selection =
        Selection def $
          Just
            NodeSelection
              { nodeType = BodyNode
              , nodeId = id0
              , meta = Left exprMeta
              }
   in [ mkFixture "id" id0
      , mkFixture "name" (unsafeMkName "x")
      , mkFixture "movement" Child1
      , mkFixture "action" (SetCursor id0)
      , mkFixture "actionerror" actionError
      , mkFixture "type" (TEmptyHole typeMeta)
      , mkFixture "typecache" (TCSynthed $ TEmptyHole ())
      , mkFixture "typecacheboth" (TCBoth (TEmptyHole ()) (TEmptyHole ()))
      , mkFixture "expr" expr
      , mkFixture "kind" KType
      , mkFixture "log" log
      , mkFixture "def" def
      , mkFixture "typeDef" typeDef
      , mkFixture "prog" prog
      , mkFixture "progaction" progaction
      , mkFixture "progerror" progerror
      , mkFixture "selection" selection
      , mkFixture
          "edit_response_1"
          (Error actionError :: Result ActionError Prog)
      , mkFixture "edit_response_2" (Success prog :: Result ActionError Prog)
      ]
