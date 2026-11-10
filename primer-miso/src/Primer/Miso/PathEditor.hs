{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | A projection for editing paths directly, by dragging their points around.
module Primer.Miso.PathEditor (
  PathEditorOpts (..),
  EditPath (..),
  viewPathEditor,
  pathEditorSize,
  pathLiteral,
) where

import Foreword

import Clay qualified
import Diagrams.Prelude (atParam, unp2)
import Linear (V2 (V2))
import Miso (View)
import Miso.CSS (style_)
import Miso.Event.Types (PointerEvent (offset))
import Miso.Html (div_, onPointerDown, onPointerMove, onPointerUp)
import Miso.Html.Property (class_)
import Miso.String (ms)
import Miso.Svg.Element (polyline_, svg_)
import Miso.Svg.Property (fill_, points_, strokeWidth_, stroke_, viewBox_)
import Primer.Builtins (cCons, cNil)
import Primer.Core (PrimCon (PrimDouble))
import Primer.Miso.Edit (Literal (LitCon, LitPrim))
import Primer.Miso.Projection (pathPoints)
import Primer.Miso.Util (clayToMiso, realToClay)
import Primer.Primitives (Path (..), Point (MakePoint), cJoin, cMakePoint, cPolyline, cSpline, pathTrail)

data PathEditorOpts action = PathEditorOpts
  { bounds :: (V2 Double, V2 Double)
  -- ^ The region of the plane which the editor shows (bottom left and top right corners).
  , edit :: Maybe (EditPath action)
  -- ^ `Nothing` if the path can't be edited.
  }

-- | What we need for editing a path by dragging its points.
data EditPath action = EditPath
  { dragging :: Maybe Int
  -- ^ The index of the point currently being dragged, if any.
  , drag :: Maybe Int -> action
  -- ^ Start (or with `Nothing`, stop) dragging a point.
  , setPath :: Path -> action
  }

-- | The size at which we display the editor.
pathEditorSize :: V2 Double
pathEditorSize = V2 240 160

-- | Replace the point at the given index (in the order of `pathPoints`).
setPathPoint :: Int -> V2 Double -> Path -> Path
setPathPoint i (V2 x y) path = evalState (go path) 0
  where
    go = \case
      Polyline ps -> Polyline <$> traverse point ps
      Spline ps -> Spline <$> traverse point ps
      Join ps -> Join <$> traverse go ps
    point p = do
      j <- get
      put $ j + 1
      pure $ if j == i then MakePoint x y else p

pathLiteral :: Path -> Literal
pathLiteral = \case
  Polyline ps -> LitCon cPolyline [listLit $ map point ps]
  Spline ps -> LitCon cSpline [listLit $ map point ps]
  Join ps -> LitCon cJoin [listLit $ map pathLiteral ps]
  where
    listLit = foldr (\x xs -> LitCon cCons [x, xs]) (LitCon cNil [])
    point (MakePoint x y) = LitCon cMakePoint [LitPrim $ PrimDouble x, LitPrim $ PrimDouble y]

viewPathEditor :: PathEditorOpts action -> Path -> View () () model action
viewPathEditor opts path =
  div_
    [ class_ "path-editor"
    , style_ $ clayToMiso do
        Clay.width $ Clay.px $ realToClay w
        Clay.height $ Clay.px $ realToClay h
    ]
    $ [ svg_
          [viewBox_ $ ms @Text $ "0 0 " <> show w <> " " <> show h]
          [ polyline_
              [ points_ $ ms $ unwords [show sx <> "," <> show sy | V2 sx sy <- toScreen <$> curve]
              , fill_ "none"
              , stroke_ "currentColor"
              , strokeWidth_ "2"
              ]
          ]
      ]
      <> zipWith pointHandle [0 ..] (pathPoints path)
      -- While dragging, an overlay captures pointer events, so that their offsets are relative to the whole editor.
      <> case opts.edit of
        Just e
          | Just i <- e.dragging ->
              [ div_
                  [ class_ "path-drag-overlay"
                  , onPointerMove \ev -> e.setPath $ setPathPoint i (fromScreen ev.offset) path
                  , onPointerUp $ const $ e.drag Nothing
                  ]
                  []
              ]
        _ -> []
  where
    V2 w h = pathEditorSize
    (lo@(V2 loX _), hi@(V2 _ hiY)) = opts.bounds
    -- We scale uniformly, and centre the bounds in the editor.
    scale = let V2 dx dy = hi - lo in min (w / dx) (h / dy)
    V2 cx cy = (V2 w h - ((* scale) <$> (hi - lo))) / 2
    toScreen (V2 x y) = V2 (cx + scale * (x - loX)) (cy + scale * (hiY - y))
    fromScreen (sx, sy) = V2 (loX + (sx - cx) / scale) (hiY - (sy - cy) / scale)
    curve = [let (x, y) = unp2 $ pathTrail path `atParam` (k / 100) in V2 x y | k <- [0 .. 100 :: Double]]
    pointHandle i p =
      let V2 sx sy = toScreen p
       in div_
            ( [ class_ "path-handle"
              , style_ $ clayToMiso do
                  Clay.left $ Clay.px $ realToClay sx
                  Clay.top $ Clay.px $ realToClay sy
              ]
                <> foldMap' (\e -> [onPointerDown $ const $ e.drag $ Just i]) opts.edit
            )
            []
