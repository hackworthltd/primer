-- | Projections are alternative ways of displaying terms, e.g. showing a list of characters as a string literal.
-- They are purely visual, and don't affect the program itself.
module Primer.Miso.Projection (
  Projection (..),
  projectString,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Text qualified as T
import Primer.Builtins (cCons, cNil)
import Primer.Core (Expr' (Con, PrimCon), PrimCon (PrimChar))
import Primer.JSON (CustomJSON (..), PrimerJSON)

data Projection
  = -- | Show a non-empty list of character literals, e.g. @'a' : 'b' : 'c' : []@, as a string literal, e.g. @"abc"@.
    StringProjection
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Projection

-- | View a term as a string literal, if it's a non-empty list of character literals.
-- We don't consider the empty list, since we can't tell whether it's a list of characters without type information.
--
-- It would be cleaner to first check that the term's type is @List Char@, before looking for a literal.
-- But we can't rely on the type caches, since those of evaluation results may be stale.
--
-- We don't reuse any code from "Primer.Primitives" for recognising literals,
-- since what's there is local to the definitions of particular primitives (see e.g. the `exprToList` helper of `exprToPicture`).
-- We may want to factor some of it out as we add more projections.
projectString :: Expr' a b c -> Maybe Text
projectString e = do
  s <- exprToString e
  guard $ not $ null s
  pure $ T.pack s
  where
    exprToString = \case
      Con _ c [PrimCon _ (PrimChar x), xs] | c == cCons -> (x :) <$> exprToString xs
      Con _ c [] | c == cNil -> Just []
      _ -> Nothing
