{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Serialization where

import Foreword hiding (log)

import Data.Aeson hiding (Error, Success)
import Data.Aeson.Encode.Pretty (
  Config (confCompare),
  defConfig,
  encodePretty',
 )
import Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.String (String)
import Primer.Action (Action (Move, SetCursor), ActionError (IDNotFound), Movement (Child1))
import Primer.App (
  Log (..),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgAction (BodyAction, MoveToDef),
  ProgError (NoDefSelected),
  Selection (..),
 )
import Primer.Builtins (tNat)
import Primer.Core (
  ASTDef (..),
  ASTTypeDef (..),
  Def (..),
  Expr,
  Expr' (EmptyHole, PrimCon),
  ExprMeta,
  ID (..),
  Kind (KFun, KType),
  Meta (..),
  PrimCon (..),
  Type' (TApp, TCon, TEmptyHole, TVar),
  TypeCache (TCSynthed),
  TypeCacheBoth (TCBoth),
  TypeDef (..),
  TypeMeta,
  ValCon (..),
 )
import Primer.Module (Module (Module, moduleDefs, moduleTypes), mkTypeDefMap)
import Primer.Name (unsafeMkName)
import Primer.Typecheck (SmartHoles (SmartHoles))
import System.FilePath (takeBaseName)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

-- | Check that encoding the value produces the file.
test_encode :: TestTree
test_encode =
  testGroup "encode" $
    fixtures <&> \(Fixture x path) ->
      goldenVsString (takeBaseName path) path (pure $ encodePretty x)
  where
    -- TODO: put this in Foreword. See:
    -- https://github.com/hackworthltd/primer/issues/139#issuecomment-944906914
    encodePretty :: ToJSON a => a -> BL.ByteString
    encodePretty = encodePretty' $ defConfig{confCompare = compare}

-- | Check that decoding the file produces the value.
test_decode :: TestTree
test_decode =
  testGroup "decode" $
    fixtures <&> \(Fixture x path) ->
      testCase (takeBaseName path) $ either assertFailure (x @=?) =<< eitherDecodeFileStrict path

-- | A fixture holds some value which is JSON serializable and path to a
-- fixture file which should contain a JSON representation of that value.
data Fixture = forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Fixture a FilePath

mkFixture :: (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> Fixture
mkFixture name x = Fixture x ("test/outputs/serialization/" <> name <> ".json")

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
      def :: ASTDef
      def = ASTDef{astDefName = "main", astDefExpr = expr, astDefType = TEmptyHole typeMeta}
      typeDef :: TypeDef
      typeDef =
        TypeDefAST
          ASTTypeDef
            { astTypeDefName = "T"
            , astTypeDefParameters = [("a", KType), ("b", KFun KType KType)]
            , astTypeDefConstructors = [ValCon "C" [TApp () (TVar () "b") (TVar () "a"), TCon () tNat]]
            , astTypeDefNameHints = []
            }
      progerror :: ProgError
      progerror = NoDefSelected
      progaction :: ProgAction
      progaction = MoveToDef "main"
      prog =
        Prog
          { progImports = mempty
          , progModule =
              Module
                { moduleTypes = mkTypeDefMap [typeDef]
                , moduleDefs = Map.singleton (astDefName def) (DefAST def)
                }
          , progSelection = Just selection
          , progSmartHoles = SmartHoles
          , progLog = log
          }
      selection :: Selection
      selection =
        Selection (astDefName def) $
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
          (Left actionError :: Either ActionError Prog)
      , mkFixture "edit_response_2" (Right prog :: Either ActionError Prog)
      , mkFixture "prim_char" $ PrimCon @() @() () $ PrimChar 'a'
      , mkFixture "prim_int" $ PrimCon @() @() () $ PrimInt 42
      ]
