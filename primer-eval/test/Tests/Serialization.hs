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
  DefSelection (..),
  EvalResp (EvalResp, evalRespDetail, evalRespExpr, evalRespRedexes),
  Log (..),
  NodeSelection (..),
  NodeType (..),
  Prog (..),
  ProgAction (BodyAction, MoveToDef),
  ProgError (NoDefSelected),
  Selection,
  Selection' (..),
  defaultLog,
 )
import Primer.Builtins (tNat)
import Primer.Core (
  Expr,
  Expr' (EmptyHole, PrimCon),
  ExprMeta,
  ID (..),
  Kind (KFun, KType),
  Meta (..),
  ModuleName (ModuleName),
  PrimCon (..),
  Type' (TEmptyHole),
  TypeCache (TCSynthed),
  TypeCacheBoth (TCBoth),
  TypeMeta,
  qualifyName,
 )
import Primer.Core.DSL (create', tapp, tcon, tvar)
import Primer.Def (
  ASTDef (..),
  Def (..),
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
import Primer.Test.Util (gvn, vcn)
import Primer.TypeDef (
  ASTTypeDef (..),
  TypeDef (..),
  ValCon (..),
 )
import Primer.Typecheck (SmartHoles (SmartHoles))
import System.FilePath (takeBaseName)
import Test.Tasty hiding (after)
import Test.Tasty.Golden
import Test.Tasty.HUnit

-- | A fixture holds some value which is JSON serializable and path to a
-- fixture file which should contain a JSON representation of that value.
data Fixture = forall a. (Eq a, Show a, FromJSON a, ToJSON a) => Fixture a FilePath

mkFixture :: (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> Fixture
mkFixture name x = Fixture x ("test/outputs/serialization/" <> name <> ".json")

-- | A list of eval fixtures we will test.
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
      typeDef :: TypeDef TypeMeta
      typeDef = create' $ do
        f1 <- tvar "b" `tapp` tvar "a"
        f2 <- tcon tNat
        pure $
          TypeDefAST
            ASTTypeDef
              { astTypeDefParameters = [("a", KType), ("b", KFun KType KType)]
              , astTypeDefConstructors = [ValCon (vcn ["M"] "C") [f1, f2]]
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
          , redoLog = defaultLog
          }
      selection :: Selection
      selection =
        SelectionDef $
          DefSelection (qualifyName modName defName) $
            Just
              NodeSelection
                { nodeType = BodyNode
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
            , types = (TEmptyHole typeMeta, TEmptyHole typeMeta)
            }
   in [ mkFixture
          "eval_response"
          ( EvalResp
              { evalRespExpr = expr
              , evalRespRedexes = [id0, ID 1]
              , evalRespDetail = reductionDetail
              }
          )
      ]
