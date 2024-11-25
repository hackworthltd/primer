module Tests.Prog where

import Foreword

import Data.Map.Strict qualified as M
import Primer.App (
  Prog (
    Prog,
    progImports,
    progLog,
    progModules,
    progSelection,
    progSmartHoles,
    redoLog
  ),
  nextProgID,
 )
import Primer.Core (
  ID,
  qualifyName,
 )
import Primer.Core.DSL (
  create',
  kfun,
  khole,
  ktype,
  tEmptyHole,
  tfun,
 )
import Primer.Core.Meta (ModuleName (ModuleName))
import Primer.Examples as Examples
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Name (
  NameCounter,
 )
import Primer.TypeDef (
  ASTTypeDef (ASTTypeDef, astTypeDefConstructors, astTypeDefNameHints, astTypeDefParameters),
  PrimTypeDef (PrimTypeDef, primTypeDefNameHints, primTypeDefParameters),
  TypeDef (TypeDefAST, TypeDefPrim),
  ValCon (ValCon),
 )
import Primer.Typecheck (SmartHoles (NoSmartHoles))
import Test.Tasty.HUnit

expectSuccess :: (Prog, ID, NameCounter) -> Assertion
expectSuccess (p, expectedID, _) = nextProgID p @?= expectedID

unit_nextProgID_exampleEven3Prog :: Assertion
unit_nextProgID_exampleEven3Prog = expectSuccess Examples.even3Prog

-- Yes, even our "bad" examples should have valid next 'ID's.

unit_nextProgID_exampleBadEven3Prog :: Assertion
unit_nextProgID_exampleBadEven3Prog = expectSuccess Examples.badEven3Prog

unit_nextProgID_exampleBadEvenProg :: Assertion
unit_nextProgID_exampleBadEvenProg = expectSuccess Examples.badEvenProg

unit_nextProgID_exampleBadMapProg :: Assertion
unit_nextProgID_exampleBadMapProg = expectSuccess Examples.badMapProg

unit_nextProgID_types :: Assertion
unit_nextProgID_types =
  let p = create' $ do
        let n = ModuleName ["M"]
        kp1 <- kfun khole ktype
        tC <- tfun tEmptyHole tEmptyHole
        let t1 =
              TypeDefAST $
                ASTTypeDef
                  { astTypeDefParameters = [("p1", kp1)]
                  , astTypeDefConstructors = [ValCon (qualifyName n "C") [tC]]
                  , astTypeDefNameHints = mempty
                  }
        kp2 <- khole
        let t2 =
              TypeDefPrim $
                PrimTypeDef
                  { primTypeDefParameters = [("p2", kp2)]
                  , primTypeDefNameHints = mempty
                  }
        let m =
              Module
                { moduleName = n
                , moduleTypes = M.fromList [("T1", t1), ("T2", t2)]
                , moduleDefs = mempty
                }
        pure $
          Prog
            { progImports = []
            , progModules = [m]
            , progSelection = Nothing
            , progSmartHoles = NoSmartHoles
            , progLog = mempty
            , redoLog = mempty
            }
   in nextProgID p @?= 7
