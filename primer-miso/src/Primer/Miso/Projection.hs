-- | Projections are alternative ways of displaying terms, e.g. showing a list of characters as a string literal.
-- They are purely visual, and don't affect the program itself.
module Primer.Miso.Projection (
  Projection (..),
  ProjectionView (..),
  availableProjection,
  projectString,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Text qualified as T
import Primer.Core (Expr' (PrimCon), PrimCon (PrimChar))
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Primitives (Picture, exprToList, exprToPicture)

data Projection
  = -- | Show a non-empty list of character literals, e.g. @'a' : 'b' : 'c' : []@, as a string literal, e.g. @"abc"@.
    StringProjection
  | -- | Draw a value of our `Picture` type.
    PictureProjection
  deriving stock (Eq, Show, Read, Data, Generic, Enum, Bounded)
  deriving (FromJSON, ToJSON) via PrimerJSON Projection

-- | What a term looks like under a projection.
data ProjectionView
  = StringView Text
  | PictureView Picture

-- | The projection which a term has, if any, along with how it looks under that projection.
-- No term has more than one projection.
availableProjection :: Expr' a b c -> Maybe (Projection, ProjectionView)
availableProjection e = asum $ [minBound .. maxBound] <&> \p -> (p,) <$> project p
  where
    project = \case
      StringProjection -> StringView <$> projectString e
      PictureProjection -> PictureView <$> exprToPicture e

-- | View a term as a string literal, if it's a non-empty list of character literals.
-- We don't consider the empty list, since we can't tell whether it's a list of characters without type information.
--
-- It would be cleaner to first check that the term's type is @List Char@, before looking for a literal.
-- But we can't rely on the type caches, since those of evaluation results may be stale.
--
-- We reuse `exprToList` from "Primer.Primitives", but other code there for recognising literals (e.g. `exprToNat`)
-- is local to the definitions of particular primitives. We may want to factor more of it out as we add projections.
projectString :: Expr' a b c -> Maybe Text
projectString e = do
  s <- traverse exprToChar =<< exprToList e
  guard $ not $ null s
  pure $ T.pack s
  where
    exprToChar = \case
      PrimCon _ (PrimChar x) -> Just x
      _ -> Nothing
