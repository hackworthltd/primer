-- | Projections are alternative ways of displaying terms, e.g. showing a list of characters as a string literal.
-- They are purely visual, and don't affect the program itself.
module Primer.Miso.Projection (
  Projection (..),
  ProjectionView (..),
  availableProjection,
  sameKind,
  shownByDefault,
  pathBounds,
  pathPoints,
  projectString,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, toConstr)
import Data.Text qualified as T
import Linear (V2 (V2))
import Primer.Builtins (tList)
import Primer.Core (Expr' (PrimCon), PrimCon (PrimChar), Type' (TApp, TCon))
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Primitives (Colour, Path (..), Picture, Point (MakePoint), exprToColour, exprToDouble, exprToList, exprToPath, exprToPicture, tChar)

data Projection
  = -- | Show a non-empty list of character literals, e.g. @'a' : 'b' : 'c' : []@, as a string literal, e.g. @"abc"@.
    StringProjection
  | -- | Draw a value of our `Picture` type.
    PictureProjection
  | -- | Show a number literal as a slider, ranging between the given bounds.
    -- We store the bounds, rather than computing them from the number, so that they don't change while dragging.
    SliderProjection Double Double
  | -- | Show a literal of our `Colour` type as a swatch, with a colour picker.
    ColourProjection
  | -- | Show a literal of our `Path` type as a curve whose points can be dragged around.
    -- We store the region of the plane to show (left, bottom, right and top), so that it doesn't change while dragging.
    PathProjection Double Double Double Double
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Projection

-- | What a term looks like under a projection.
data ProjectionView
  = StringView Text
  | PictureView Picture
  | NumberView Double
  | ColourView Colour
  | PathView Path

-- | The projection which a term has, if any (with default parameters), along with how it looks under that projection.
-- No term has more than one kind of projection.
-- The term's type should be given where it is known reliably, i.e. not for evaluation results (see `projectString`).
availableProjection :: Maybe (Type' () ()) -> Expr' a b c -> Maybe (Projection, ProjectionView)
availableProjection ty e =
  asum
    [ (StringProjection,) . StringView <$> projectString ty e
    , (PictureProjection,) . PictureView <$> exprToPicture e
    , exprToDouble e <&> \x -> (uncurry SliderProjection $ defaultRange x, NumberView x)
    , (ColourProjection,) . ColourView <$> exprToColour e
    , exprToPath e <&> \p -> let (V2 l b, V2 r t) = pathBounds p in (PathProjection l b r t, PathView p)
    ]
  where
    defaultRange x = if x == 0 then (-10, 10) else (min 0 $ 2 * x, max 0 $ 2 * x)

-- | Whether two projections are of the same kind, ignoring any parameters.
sameKind :: Projection -> Projection -> Bool
sameKind = (==) `on` toConstr

-- | Whether to show terms with this kind of projection as such by default, where we do so (i.e. evaluation results).
-- Projections which are mainly for editing aren't much use there.
shownByDefault :: Projection -> Bool
shownByDefault = \case
  StringProjection -> True
  PictureProjection -> True
  SliderProjection{} -> False
  ColourProjection -> False
  PathProjection{} -> False

-- | View a term as a string literal, if it's a list of character literals.
-- We only consider the empty list to be a string if we're given its type, and that type is @List Char@.
--
-- Otherwise, we don't look at types: it would be cleaner to first check that a term's type is @List Char@, before looking
-- for a literal, but we can't rely on the type caches of evaluation results, which may be stale.
--
-- We reuse `exprToList` from "Primer.Primitives", but other code there for recognising literals (e.g. `exprToNat`)
-- is local to the definitions of particular primitives. We may want to factor more of it out as we add projections.
projectString :: Maybe (Type' () ()) -> Expr' a b c -> Maybe Text
projectString ty e = do
  s <- traverse exprToChar =<< exprToList e
  guard $ not (null s) || isStringType
  pure $ T.pack s
  where
    isStringType = case ty of
      Just (TApp _ (TCon _ l) (TCon _ c)) -> l == tList && c == tChar
      _ -> False
    exprToChar = \case
      PrimCon _ (PrimChar x) -> Just x
      _ -> Nothing

-- | A region of the plane containing the given path's points, with some margin, for a path editor to show.
-- The result is the bottom left and top right corners.
pathBounds :: Path -> (V2 Double, V2 Double)
pathBounds path = case nonEmpty $ pathPoints path of
  Nothing -> (V2 0 0, V2 1 1)
  Just ps ->
    let lo = foldr1 (liftA2 min) ps
        hi = foldr1 (liftA2 max) ps
        -- We avoid an empty region, e.g. for a single point or a horizontal line.
        margin = max 1 . (* 0.15) <$> (hi - lo)
     in (lo - margin, hi + margin)

-- | The points of a path, in order.
pathPoints :: Path -> [V2 Double]
pathPoints = \case
  Polyline ps -> map fromPoint ps
  Spline ps -> map fromPoint ps
  Join ps -> foldMap' pathPoints ps
  where
    fromPoint (MakePoint x y) = V2 x y
