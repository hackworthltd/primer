-- NOTE: Unlike the rest of Primer, this entire module was written by an LLM, with little human review.
-- Treat it with appropriate suspicion.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Tidy drawings of trees, following
-- [Drawing Non-layered Tidy Trees in Linear Time](https://ir.cwi.nl/pub/21856) (A.J. van der Ploeg, 2013),
-- which extends the classic algorithm of Reingold and Tilford to nodes of varying heights.
-- Section and line numbers below refer to that paper, and to the Java code in its Appendix C.
--
-- The aesthetic criteria (Section 2) are that nodes don't overlap, children keep their order,
-- parents are centred over their outermost children, identical subtrees are drawn identically,
-- and (optionally, see 'symmetric') the drawing of a mirrored tree is the mirror image of the original drawing.
--
-- Rather than linking contours through the tree with mutable "threads" (Appendix A),
-- we keep each subtree's contours as separate lists, as Section 8.3 suggests for a purely functional implementation.
-- We use plain lists rather than catenable deques, so we lose the guarantee of linear running time,
-- which is no great loss at the sizes of tree we deal with.
module Primer.Miso.Layout (
  tidyLayout,
  TidyOpts (..),
  defaultTidyOpts,
  Layering (..),
) where

import Foreword

import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Tree (Tree (Node), levels)
import Linear (V2 (V2))
import Linear.Affine (Point (P))
import Primer.Miso.Util (P2)

-- | How to position nodes vertically.
data Layering
  = -- | Children start a fixed distance below the bottom of their parent. This is more compact.
    NonLayered
  | -- | All nodes at the same depth share a top, and are treated as being as tall as the tallest of them.
    Layered
  deriving stock (Eq, Show)

data TidyOpts n a = TidyOpts
  { horizontalGap :: n
  -- ^ The minimum horizontal space between nodes.
  , verticalGap :: n
  -- ^ The vertical space between a parent and its children.
  , layering :: Layering
  , symmetric :: Bool
  -- ^ Whether to satisfy the last aesthetic criterion, i.e. mirror symmetry (Section 5).
  -- This spreads smaller subtrees evenly between larger siblings, rather than piling them up against the left one.
  , nodeSize :: a -> V2 n
  -- ^ Width and height.
  }

defaultTidyOpts :: Num n => (a -> V2 n) -> TidyOpts n a
defaultTidyOpts nodeSize =
  TidyOpts
    { horizontalGap = 1
    , verticalGap = 1
    , layering = NonLayered
    , symmetric = True
    , nodeSize
    }

-- | Lay out a tree, annotating each node with the position of its centre.
-- The root's top is at the origin, with the y-axis pointing down.
tidyLayout :: (Fractional n, Ord n) => TidyOpts n a -> Tree a -> Tree (a, P2 n)
tidyLayout opts = place 0 . (.tree) . walk opts.symmetric . boxes opts

-- | A node, along with the box it occupies for the purposes of layout.
-- Following Section 2, the box includes the gaps, so that the algorithm itself needn't worry about spacing.
data Box n a = Box
  { label :: a
  , size :: V2 n
  -- ^ The size of the node itself, as opposed to its box.
  , top :: n
  , width :: n
  , height :: n
  }

boxes :: (Num n, Ord n) => TidyOpts n a -> Tree a -> Tree (Box n a)
boxes opts t = go 0 layerHeights t
  where
    go top hs (Node label ts) =
      let size@(V2 w h) = opts.nodeSize label
          height =
            opts.verticalGap + case (opts.layering, hs) of
              (Layered, h' : _) -> h'
              _ -> h
       in Node Box{label, size, top, width = w + opts.horizontalGap, height} $
            map (go (top + height) $ drop 1 hs) ts
    -- In the layered case, all nodes at the same depth are considered to be equally tall (Section 2).
    layerHeights = map (foldl' max 0 . map ((\(V2 _ h) -> h) . opts.nodeSize)) $ levels t

-- | One side of the outline of a subtree, from top to bottom (Section 3).
-- Each element covers the vertical range from the bottom of the previous one (or the subtree's top) to its own bottom.
type Contour n = [Extent n]

data Extent n = Extent
  { bottom :: n
  , edge :: n
  -- ^ The x-coordinate of this side of the outline, relative to the subtree's root.
  }

-- | A laid-out subtree, with each node's x-coordinate relative to its parent's.
-- This is the result of the paper's @firstWalk@.
data Laid n a = Laid
  { tree :: Tree (Box n a, n)
  , left :: Contour n
  , right :: Contour n
  , lowest :: n
  -- ^ The bottom of the lowest node.
  }

walk :: (Fractional n, Ord n) => Bool -> Tree (Box n a) -> Laid n a
walk symmetric (Node b ts) =
  let self side = Extent{bottom = b.top + b.height, edge = side * b.width / 2}
   in case nonEmpty $ map (walk symmetric) ts of
        Nothing -> Laid{tree = Node (b, 0) [], left = [self (-1)], right = [self 1], lowest = b.top + b.height}
        Just children ->
          let siblings = foldl' (addSibling symmetric) (firstSibling $ NE.head children) (NE.tail children)
              xs = spaceSiblings siblings
              -- `positionRoot` (lines 105-109).
              rootX = (NE.head xs + NE.last xs) / 2
           in Laid
                { tree = Node (b, 0) . toList $ NE.zipWith (\c x -> setX (x - rootX) c.tree) children xs
                , left = self (-1) : move (-rootX) siblings.left
                , right = self 1 : move (-rootX) siblings.right
                , lowest = foldl' max (b.top + b.height) $ (.lowest) <$> children
                }
  where
    setX x (Node (b', _) cs) = Node (b', x) cs

-- | The siblings placed so far, with x-coordinates relative to the first.
data Siblings n = Siblings
  { count :: Int
  , xs :: [n]
  -- ^ In reverse order.
  , left :: Contour n
  , right :: Contour n
  , owners :: [(n, Int)]
  -- ^ Those siblings which have nodes in the right contour, along with their lowest bottoms, latest first.
  -- This is the "IYL" of Section 6, used to find which sibling a node of the right contour belongs to.
  , shift :: IntMap n
  -- ^ The bookkeeping for moving intermediate siblings (Appendix B).
  , change :: IntMap n
  }

firstSibling :: Num n => Laid n a -> Siblings n
firstSibling c =
  Siblings
    { count = 1
    , xs = [0]
    , left = c.left
    , right = c.right
    , owners = [(c.lowest, 0)]
    , shift = mempty
    , change = mempty
    }

-- | Place a subtree to the right of its left siblings (lines 21-27).
addSibling :: (Fractional n, Ord n) => Bool -> Siblings n -> Laid n a -> Siblings n
addSibling symmetric ss c =
  let i = ss.count
      (x, moves) = separate ss.right c.left ss.owners
      ss' = if symmetric then foldl' (distributeExtra i) ss moves else ss
   in ss'
        { count = i + 1
        , xs = x : ss.xs
        , left = ss.left `above` move x c.left
        , right = move x c.right `above` ss.right
        , owners = (c.lowest, i) : dropWhile ((c.lowest >=) . fst) ss.owners
        }

-- | Find how far right a subtree must move, from the position of the first sibling,
-- in order to clear the right contour of its left siblings (lines 42-65).
-- We also return each individual move, along with the sibling which caused it.
separate :: (Num n, Ord n) => Contour n -> Contour n -> [(n, Int)] -> (n, [(Int, n)])
separate = go 0 []
  where
    go x moves (sr : srs) (cl : cls) owners =
      let owners' = dropWhile ((sr.bottom >) . fst) owners
          dist = sr.edge - (cl.edge + x)
          (x', moves') =
            if dist > 0
              then (x + dist, (maybe 0 snd $ listToMaybe owners', dist) : moves)
              else (x, moves)
       in go
            x'
            moves'
            (if sr.bottom <= cl.bottom then srs else sr : srs)
            (if sr.bottom >= cl.bottom then cls else cl : cls)
            owners'
    go x moves _ _ _ = (x, moves)

-- | Having moved sibling @i@ right by @dist@ to clear sibling @si@,
-- arrange for the siblings in between to share the space (lines 119-127).
distributeExtra :: Fractional n => Int -> Siblings n -> (Int, n) -> Siblings n
distributeExtra i ss (si, dist)
  | si == i - 1 = ss
  | otherwise =
      let nr = fromIntegral $ i - si
       in ss
            { shift = IntMap.insertWith (+) (si + 1) (dist / nr) $ IntMap.insertWith (+) i (-dist / nr) ss.shift
            , change = IntMap.insertWith (+) i (-(dist - dist / nr)) ss.change
            }

-- | Apply the moves recorded by 'distributeExtra' (lines 130-137).
-- This never affects the first or last sibling, so it doesn't matter whether we do it before or after positioning the parent.
spaceSiblings :: Num n => Siblings n -> NonEmpty n
spaceSiblings ss =
  fromMaybe (pure 0) . nonEmpty . snd $
    mapAccumL
      ( \(d, modsumdelta) (i, x) ->
          let d' = d + IntMap.findWithDefault 0 i ss.shift
              modsumdelta' = modsumdelta + d' + IntMap.findWithDefault 0 i ss.change
           in ((d', modsumdelta'), x + modsumdelta')
      )
      (0, 0)
      (zip [0 ..] $ reverse ss.xs)

-- | Convert to absolute coordinates (the paper's @secondWalk@).
place :: Fractional n => n -> Tree (Box n a, n) -> Tree (a, P2 n)
place parentX (Node (b, relX) ts) =
  let x = parentX + relX
      V2 _ h = b.size
   in Node (b.label, P $ V2 x $ b.top + h / 2) $ map (place x) ts

move :: Num n => n -> Contour n -> Contour n
move d = map (\e -> e{edge = e.edge + d})

-- | Merge two contours, taking the first where they overlap.
above :: Ord n => Contour n -> Contour n -> Contour n
above = \cases
  [] es -> es
  [e] es -> e : dropWhile ((<= e.bottom) . (.bottom)) es
  (e : es) es' -> e : above es es'
