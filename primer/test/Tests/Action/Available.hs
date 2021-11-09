{-# LANGUAGE NamedFieldPuns #-}

module Tests.Action.Available where

import Foreword

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.Extra (enumerate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Optics (adjoin, toListOf, (%))
import Primer.Action (ActionName (..), OfferedAction (name))
import Primer.Action.Available (actionsForDef, actionsForDefBody, actionsForDefSig)
import Primer.Core (
  Def (..),
  HasID (_id),
  ID,
  Kind (KType),
  _exprMeta,
  _exprTypeMeta,
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
  global,
  hole,
  lAM,
  lam,
  letType,
  let_,
  letrec,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  thole,
  tvar,
  var,
 )
import Primer.Name (Name (unName))
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit ()
import Text.Pretty.Simple (pShowNoColor)

-- | This definition contains every construct in the Primer language.
test_1 :: TestTree
test_1 =
  mkTests
    Def
      { defName = "1"
      , defID
      , defExpr
      , defType
      }
  where
    ((defExpr, defType), defID) = create $ (,) <$> e <*> t
    t =
      tfun
        (tcon "")
        ( tforall
            "a"
            KType
            ( tapp
                ( thole
                    ( tapp
                        (tcon "List")
                        tEmptyHole
                    )
                )
                (tvar "a")
            )
        )
    e =
      let_
        "x"
        (con "True")
        ( letrec
            "y"
            ( app
                ( hole
                    (con "Just")
                )
                ( hole
                    (global 0)
                )
            )
            ( thole
                (tcon "Maybe")
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
                                    (tcon "Bool")
                                    ( aPP
                                        (con "Left")
                                        (tvar "b")
                                    )
                                )
                                (tvar "β")
                            )
                            ( case_
                                (var "i")
                                [ branch
                                    "Zero"
                                    []
                                    (con "False")
                                , branch
                                    "Succ"
                                    [
                                      ( "n"
                                      , Nothing
                                      )
                                    ]
                                    ( app
                                        ( app
                                            emptyHole
                                            (var "x")
                                        )
                                        (var "y")
                                    )
                                ]
                            )
                        )
                    )
                )
                ( tfun
                    (tcon "Nat")
                    ( tforall
                        "α"
                        KType
                        ( tapp
                            ( tapp
                                (tcon "Either")
                                (tcon "Bool")
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
mkTests :: Def -> TestTree
mkTests def =
  let testName = T.unpack $ unName $ defName def
   in testGroup
        testName
        $ map
          ( \level ->
              let defActions = map name $ actionsForDef level mempty def
                  bodyActions =
                    map
                      ( \id ->
                          ( id
                          , map name $ actionsForDefBody level def id (defExpr def)
                          )
                      )
                      . toListOf (_exprMeta % _id `adjoin` _exprTypeMeta % _id)
                      $ defExpr def
                  sigActions =
                    map
                      ( \id ->
                          ( id
                          , map name $ actionsForDefSig level def id (defType def)
                          )
                      )
                      . toListOf (_typeMeta % _id)
                      $ defType def
               in goldenVsString (show level) ("test/outputs/available-actions" </> testName </> show level <> ".hs") $
                    pure . BS.fromStrict . encodeUtf8 . TL.toStrict . pShowNoColor $
                      Output
                        { defActions
                        , bodyActions
                        , sigActions
                        }
          )
          enumerate
