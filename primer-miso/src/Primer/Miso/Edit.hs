-- | Editing terms through projections, e.g. typing in to a string literal, or dragging a slider.
-- Such an edit replaces the term with a new literal, which we turn in to ordinary actions,
-- so that the core needs no special support, and the usual type checking, undo etc. apply.
module Primer.Miso.Edit (
  Literal (..),
  editActions,
  stringLiteral,
  colourLiteral,
) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Primer.Action (Action (ConstructPrim, ConstructSaturatedCon, Delete, Move, SetCursor), Movement (ConChild, Parent))
import Primer.Builtins (cCons, cNil)
import Primer.Core (Expr' (Con, PrimCon), GlobalName (baseName, qualifiedModule), HasID, ModuleName (unModuleName), PrimCon (PrimChar, PrimDouble), ValConName, getID)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (unName)
import Primer.Primitives (Colour (RGB), cRGB)

-- | A term built only from constructors and primitive literals.
data Literal
  = LitCon ValConName [Literal]
  | LitPrim PrimCon
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Literal

stringLiteral :: Text -> Literal
stringLiteral = foldr (\c l -> LitCon cCons [LitPrim $ PrimChar c, l]) (LitCon cNil []) . toS @Text @[Char]

colourLiteral :: Colour -> Literal
colourLiteral (RGB r g b) = LitCon cRGB $ LitPrim . PrimDouble <$> [r, g, b]

-- | The actions which replace a term with the given literal.
-- We only replace the parts which differ, so that unchanged nodes keep their IDs, and thus their frontend metadata,
-- such as the projection through which the edit is being made.
-- We also return whether the root itself is replaced, in which case it gets a new ID, and the cursor ends up on it.
editActions :: HasID a => Expr' a b c -> Literal -> ([Action], Bool)
editActions e l = case (e, l) of
  (Con _ c es, LitCon c' ls)
    | c == c' && length es == length ls ->
        (concat $ zipWith (\e' l' -> fst $ editActions e' l') es ls, False)
  (PrimCon _ p, LitPrim p') | p == p' -> ([], False)
  _ -> ([SetCursor $ getID e, Delete] <> construct l, True)
  where
    -- Fill the empty hole under the cursor, leaving the cursor where it started.
    construct = \case
      LitPrim p -> [ConstructPrim p]
      LitCon c ls ->
        ConstructSaturatedCon (unName <$> unModuleName (qualifiedModule c), unName $ baseName c)
          : concat (zipWith (\i l' -> [Move $ ConChild i] <> construct l' <> [Move Parent]) [0 ..] ls)
