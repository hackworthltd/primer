-- | PROTOTYPE: an example program, so that we can try out pictures without building one up action by action.
module Primer.Miso.Demo (
  demoApp,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Primer.App (App, Prog (progModules), mkApp, newProg)
import Primer.Builtins (cJust, cNothing, tList)
import Primer.Builtins.DSL (list_)
import Primer.Core (Expr, ID)
import Primer.Core.DSL (ann, app, branch, case_, char, con, double, let_, lvar, tapp, tcon)
import Primer.Def (ASTDef (ASTDef), Def (DefAST))
import Primer.Miso.Util (runTC)
import Primer.Module (Module (moduleDefs))
import Primer.Primitives (
  PrimDef (ArcLength, DoubleMinus, DoubleMul, DoubleQuotient),
  cCircle,
  cFill,
  cJoin,
  cLine,
  cLineColour,
  cMakePoint,
  cMoveAlong,
  cOver,
  cPolyline,
  cRGB,
  cRotate,
  cShape,
  cSpin,
  cSpline,
  cTranslate,
  tChar,
  tDouble,
  tPath,
  tPicture,
 )
import Primer.Primitives.DSL (pfun)

-- | 'newApp', with some example definitions added to the main module:
-- - `rollingBall`: a wheel which rolls down a hill without slipping,
--   i.e. it turns through the length of the hill divided by its radius.
-- - `greeting`: a string, for trying out editing through projections.
demoApp :: App
demoApp = mkApp i n prog{progModules = map addDefs $ progModules prog}
  where
    (prog, i0, n0) = newProg
    (defs, (i, n)) = either absurd identity $ runTC (i0, n0) $ do
      rollingBall' <- def rollingBall $ tcon tPicture
      greeting <- def (list_ $ map char "Hello, world!") $ tcon tList `tapp` tcon tChar
      pure [("rollingBall", rollingBall'), ("greeting", greeting)]
    def expr ty = fmap DefAST $ ASTDef <$> expr <*> ty
    addDefs m = m{moduleDefs = Map.fromList defs <> moduleDefs m}

-- | A reproduction of `drawWheel` from the `diagrams-svg` animation examples, minus the moon.
rollingBall :: (MonadFresh ID m) => m Expr
rollingBall =
  let_ "hill" (con cSpline [list_ hillPoints] `ann` tcon tPath)
    . let_ "radius" (double 2 `ann` tcon tDouble)
    . let_ "duration" (double 3 `ann` tcon tDouble)
    $ con
      cOver
      [ list_
          [ con
              cMoveAlong
              [ lvar "hill"
              , lvar "duration"
              , con
                  cTranslate
                  [ double 0
                  , lvar "radius"
                  , con
                      cSpin
                      [ degreesFromRadians $ pfun DoubleMinus `app` double 0 `app` quotient (pfun ArcLength `app` lvar "hill") (lvar "radius")
                      , lvar "duration"
                      , con
                          cLineColour
                          [ colour 0.68 0.3533333333333334 0.12
                          , con cOver [list_ $ con cCircle [lvar "radius"] : map spoke [0 .. 5]]
                          ]
                      ]
                  ]
              ]
          , -- the ground
            con cFill [colour 8.273333333333338e-2 0.28900000000000003 5.1e-2, region [point 30 0, point 0 0]]
          , -- the sky
            con cFill [colour 5.1e-2 0.12239999999999993 0.28900000000000003, region [point 30 20, point 0 20]]
          ]
      ]
  where
    point x y = con cMakePoint [double x, double y]
    colour r g b = con cRGB [double r, double g, double b]
    -- The region between the hill and the given points.
    region ps = con cShape [con cJoin [list_ [lvar "hill", con cPolyline [list_ ps]]]]
    spoke k = con cRotate [double $ 60 * k, con cLine [con cPolyline [list_ [point 0 0, con cMakePoint [lvar "radius", double 0]]]]]
    -- The original's spline through these points is scaled with `scaleToX 30` and `scaleToY 10`, then moved up by 10.
    -- Since splines are preserved by such transformations, we can just apply them to the points (see `pathTrail`).
    hillPoints = [point (10.000000000000002 * x) (1.8038528557176692 * y + 10) | (x, y) <- [(0, 0), (1, -4), (2, 1), (3, 0)]]
    degreesFromRadians a = pfun DoubleMul `app` a `app` double (180 / pi)
    -- Division returns `Nothing` when dividing by zero, in which case we don't turn at all.
    quotient a b =
      case_
        (pfun DoubleQuotient `app` a `app` b)
        [ branch cNothing [] $ double 0
        , branch cJust [("x", Nothing)] $ lvar "x"
        ]
