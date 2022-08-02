module Tests.Serialization where

import Foreword hiding (log)

import Data.Aeson hiding (Error, Success)
import Data.Aeson.Encode.Pretty (
  Config (confCompare),
  defConfig,
  encodePretty',
 )
import Data.ByteString.Lazy as BL
import Data.Map.Strict qualified as Map
import Data.String (String)
import Primer.Action (Action (Move, SetCursor), ActionError (IDNotFound), Movement (Child1))
import Primer.App (
  EvalResp (EvalResp, evalRespDetail, evalRespExpr, evalRespRedexes),
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
  ModuleName (ModuleName),
  PrimCon (..),
  Type' (TApp, TCon, TEmptyHole, TVar),
  TypeCache (TCSynthed),
  TypeCacheBoth (TCBoth),
  TypeDef (..),
  TypeMeta,
  ValCon (..),
  qualifyName,
 )
import Primer.Eval (
  BetaReductionDetail (
    BetaReductionDetail,
    after,
    argID,
    before,
    bindingName,
    bodyID,
    lambdaID,
    letID,
    types
  ),
  EvalDetail (BetaReduction),
 )
import Primer.Module (Module (Module, moduleDefs, moduleTypes), moduleName)
import Primer.Name (Name, unsafeMkName)
import Primer.Typecheck (SmartHoles (SmartHoles))
import System.FilePath (takeBaseName)
import Test.Tasty hiding (after)
import Test.Tasty.Golden
import Test.Tasty.HUnit
import TestUtils (gvn, vcn)

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
      defName :: Name
      defName = "main"
      def :: ASTDef
      def = ASTDef{astDefExpr = expr, astDefType = TEmptyHole typeMeta}
      typeDef :: TypeDef
      typeDef =
        TypeDefAST
          ASTTypeDef
            { astTypeDefParameters = [("a", KType), ("b", KFun KType KType)]
            , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [TApp () (TVar () "b") (TVar () "a"), TCon () tNat]]
            , astTypeDefNameHints = []
            }
      progerror :: ProgError
      progerror = NoDefSelected
      progaction :: ProgAction
      progaction = MoveToDef $ gvn ["M"] "main"
      modName = ModuleName ["M"]
      prog =
        Prog
          { progImports = mempty
          , progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = Map.singleton "T" typeDef
                  , moduleDefs = Map.singleton defName (DefAST def)
                  }
              ]
          , progSelection = Just selection
          , progSmartHoles = SmartHoles
          , progLog = log
          }
      selection :: Selection
      selection =
        Selection (qualifyName modName defName) $
          Just
            NodeSelection
              { nodeType = BodyNode
              , nodeId = id0
              , meta = Left exprMeta
              }
      reductionDetail :: EvalDetail
      reductionDetail =
        BetaReduction $
          BetaReductionDetail
            { before = expr
            , after = expr
            , bindingName = "x"
            , lambdaID = id0
            , letID = id0
            , argID = id0
            , bodyID = id0
            , types =
                Just (TEmptyHole typeMeta, TEmptyHole typeMeta)
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
      , mkFixture
          "eval_response"
          ( EvalResp
              { evalRespExpr = expr
              , evalRespRedexes = [id0, ID 1]
              , evalRespDetail = reductionDetail
              }
          )
      , mkFixture "prim_char" $ PrimCon @() @() () $ PrimChar 'a'
      , mkFixture "prim_int" $ PrimCon @() @() () $ PrimInt 42
      ]
