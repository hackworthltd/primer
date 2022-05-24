{-# LANGUAGE NamedFieldPuns #-}

module Tests.Action.Available where

import Foreword

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.Extra (enumerate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Optics (toListOf, (%))
import Primer.Action (ActionName (..), OfferedAction (name))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.Builtins
import Primer.Core (
  ASTDef (..),
  GlobalName (baseName, qualifiedModule),
  HasID (_id),
  ID,
  Kind (KType),
  moduleNamePretty,
  _typeMeta,
 )
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  create,
  emptyHole,
  gvar',
  hole,
  lAM,
  lam,
  letType,
  let_,
  letrec,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  thole,
  tvar,
 )
import Primer.Name (Name (unName))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit ()
import TestUtils (exprIDs, gvn)
import Text.Pretty.Simple (pShowNoColor)

-- | This definition contains every construct in the Primer language.
test_1 :: TestTree
test_1 =
  mkTests
    ASTDef
      { astDefName = gvn ["M"] "1"
      , astDefExpr
      , astDefType
      }
  where
    ((astDefExpr, astDefType), _) = create $ (,) <$> e <*> t
    t =
      tfun
        (tcon tNat)
        ( tforall
            "a"
            KType
            ( tapp
                ( thole
                    ( tapp
                        (tcon tList)
                        tEmptyHole
                    )
                )
                (tvar "a")
            )
        )
    e =
      let_
        "x"
        (con cTrue)
        ( letrec
            "y"
            ( app
                ( hole
                    (con cJust)
                )
                ( hole
                    (gvar' ["M"] "0")
                )
            )
            ( thole
                (tcon tMaybe)
            )
            ( ann
                ( lam
                    "i"
                    ( lAM
                        "β"
                        ( app
                            ( aPP
                                ( letType
                                    "b"
                                    (tcon tBool)
                                    ( aPP
                                        (con cLeft)
                                        (tvar "b")
                                    )
                                )
                                (tvar "β")
                            )
                            ( case_
                                (lvar "i")
                                [ branch
                                    cZero
                                    []
                                    (con cFalse)
                                , branch
                                    cSucc
                                    [
                                      ( "n"
                                      , Nothing
                                      )
                                    ]
                                    ( app
                                        ( app
                                            emptyHole
                                            (lvar "x")
                                        )
                                        (lvar "y")
                                    )
                                ]
                            )
                        )
                    )
                )
                ( tfun
                    (tcon tNat)
                    ( tforall
                        "α"
                        KType
                        ( tapp
                            ( tapp
                                (tcon tEither)
                                (tcon tBool)
                            )
                            (tvar "α")
                        )
                    )
                )
            )
        )

data Output = Output
  { defActions :: [ActionName]
  , bodyActions :: [(ID, [ActionName])]
  , sigActions :: [(ID, [ActionName])]
  }
  deriving (Show)

-- | Golden tests for the available actions at each node of the definition, for each level.
mkTests :: ASTDef -> TestTree
mkTests def =
  let defName = astDefName def
      testName = T.unpack $ moduleNamePretty (qualifiedModule defName) <> "." <> unName (baseName defName)
   in testGroup testName $
        enumerate
          <&> \level ->
            let defActions = map name $ actionsForDef level mempty def
                bodyActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefBody level def id (astDefExpr def)
                        )
                    )
                    . toListOf exprIDs
                    $ astDefExpr def
                sigActions =
                  map
                    ( \id ->
                        ( id
                        , map name $ actionsForDefSig level def id (astDefType def)
                        )
                    )
                    . toListOf (_typeMeta % _id)
                    $ astDefType def
             in goldenVsString (show level) ("test/outputs/available-actions" </> testName </> show level <> ".hs") $
                  pure . BS.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor $
                    Output
                      { defActions
                      , bodyActions
                      , sigActions
                      }
