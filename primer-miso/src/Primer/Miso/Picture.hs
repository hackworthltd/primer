-- | Drawing values of our `Picture` type, using `diagrams`.
module Primer.Miso.Picture (
  renderPicture,
) where

import Foreword hiding (rotate)

import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Diagrams.Backend.SVG (
  AnimationAttrs (..),
  AnimationRepeat (RepeatIndefinitely),
  AnimationRestart (RestartAlways),
  AnimationValues (LinearValues),
  B,
  Options (SVGOptions),
  SVG (SVG),
  TransformAnimation (RotateAnimation),
  animateMotion,
  animateTransform,
 )
import Diagrams.Prelude (
  Diagram,
  V2 (V2),
  atParam,
  circle,
  closeTrail,
  deg,
  dims2D,
  direction,
  fc,
  lc,
  lw,
  mapLoc,
  moveTo,
  pathFromLocTrail,
  phantom,
  rect,
  renderDia,
  rotate,
  rotateTo,
  sRGB,
  scale,
  size,
  strokeLocTrail,
  tangentAtParam,
  translate,
  withEnvelope,
  wrapLine,
  (@@),
 )
import Graphics.Svg (renderBS)
import Miso.String (MisoString, ms)
import Primer.Primitives (Colour (RGB), Picture (..), pathTrail)

-- | Render a picture as an SVG data URL, along with the size at which it should be displayed.
renderPicture :: Picture -> (MisoString, V2 Double)
renderPicture p = ("data:image/svg+xml;base64," <> ms (decodeUtf8 $ B64.encode $ BL.toStrict $ renderBS svg), dims)
  where
    d = pictureToDiagram p
    dims =
      let s = size d
       in -- We scale to fit in a fixed area, keeping the aspect ratio.
          if any (\x -> isNaN x || x <= 0) s then pure maxSize else s ^* minimum (pure maxSize / s)
    svg = let V2 w h = dims in renderDia SVG (SVGOptions (dims2D w h) Nothing "" [] True) d
    maxSize = 200
    v ^* k = (* k) <$> v

pictureToDiagram :: Picture -> Diagram B
pictureToDiagram = \case
  -- `diagrams` divides by zero when given some degenerate shapes, so we guard against those.
  Circle r -> if r > 0 then circle r else mempty
  Rectangle w h -> if w > 0 && h > 0 then rect w h else mempty
  Line path -> strokeLocTrail $ mapLoc wrapLine $ pathTrail path
  -- We only fill shapes, without an outline, which can be drawn separately with `Line` if wanted.
  Shape path -> strokeLocTrail (mapLoc (closeTrail . wrapLine) $ pathTrail path) & lw 0
  Fill c p -> pictureToDiagram p & fc (colour c)
  LineColour c p -> pictureToDiagram p & lc (colour c)
  Translate x y p -> pictureToDiagram p & translate (V2 x y)
  Rotate a p -> pictureToDiagram p & rotate (a @@ deg)
  Scale s p -> if s /= 0 then pictureToDiagram p & scale s else mempty
  Over ps -> foldMap' pictureToDiagram ps
  Spin a t p ->
    let d = pictureToDiagram p
     in animated t d (\k -> d & rotate (k * a @@ deg)) $
          animateTransform (RotateAnimation $ LinearValues [(0 @@ deg, Nothing), (a @@ deg, Nothing)])
  MoveAlong path t p ->
    let d = pictureToDiagram p
        trail = pathTrail path
     in animated t d (\k -> d & rotateTo (direction $ trail `tangentAtParam` k) & moveTo (trail `atParam` k)) $
          animateMotion (pathFromLocTrail $ mapLoc wrapLine trail) True Nothing
  where
    colour (RGB r g b) = sRGB r g b
    -- Animations aren't taken in to account when computing the bounds of a picture,
    -- so we set the bounds from a sample of frames, where `frame` takes a proportion of the way through the animation.
    -- Note that we don't animate at all if the duration isn't positive, since SVG doesn't support that.
    animated t d frame animate =
      if t > 0
        then
          animate (loopingFor t) d
            & withEnvelope (phantom (foldMap' (frame . (/ samples) . fromInteger) [0 .. round samples]) :: Diagram B)
        else d
    samples = 16
    loopingFor t =
      AnimationAttrs
        { animationDuration = t
        , animationBegin = []
        , animationEnd = []
        , animationMin = Nothing
        , animationMax = Nothing
        , animationRestart = RestartAlways
        , animationRepeat = RepeatIndefinitely
        , animationAccumulate = False
        , animationFreeze = False
        }
