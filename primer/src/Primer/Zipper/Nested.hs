{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Primer.Zipper.Nested (
  ZipNest (ZipNest),
  unfocusNest,
  mergeNest,
  innerZipNest,
  HasID (..),
  IsZipper (..),
  target,
  _target,
  up,
  down,
  left,
  right,
  top,
  farthest,
  focus,
  unfocus,
  replace,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product (position)
import Data.Generics.Uniplate.Zipper (
  Zipper,
  replaceHole,
  zipper,
 )
import Data.Generics.Uniplate.Zipper qualified as Z
import Optics (Lens', equality', lens, over, traverseOf, view, (%))
import Primer.Core.Meta (
  HasID (..),
 )

-- | A zipper for @small@s embedded in @large@s.
-- We often have a type @large@, where some constructors contain a field
-- of type @small@, where both @large@ and @small@ have interesting
-- tree structure that we would like to navigate with a zipper.  When
-- navigating around the nested @small@ we do not want to lose our place
-- in the wider expression.  This type contains a Zipper for a @small@
-- and a function that will place the unzippered @small@ back into the
-- wider @large@ zipper, keeping its place.
data ZipNest largeZip smallZip small = ZipNest smallZip (small -> largeZip)
  deriving stock (Generic)

instance HasID smallZip => HasID (ZipNest largeZip smallZip small) where
  _id = position @1 % _id

unfocusNest :: IsZipper smallZip small => ZipNest largeZip smallZip small -> largeZip
unfocusNest (ZipNest zs f) = f (unfocus zs)

mergeNest :: IsZipper smallZip small => ZipNest largeZip (ZipNest mediumZip smallZip small) medium
  -> ZipNest largeZip mediumZip medium
mergeNest (ZipNest (ZipNest z f) g) = ZipNest (f $ unfocus z) g

innerZipNest :: ZipNest largeZip smallZip small -> smallZip
innerZipNest (ZipNest zs _) = zs

-- | We want to use zipper-style up, down, left, right, etc. on various
-- 'Zipper's and 'ZipNest's, despite them being very different types.
-- This class enables that, by proxying each method through to the
-- underlying Zipper. @za@ is the user-facing type and @a@ is the
-- type of targets of the internal zipper.
class Data a => IsZipper za a | za -> a where
  asZipper :: Lens' za (Z.Zipper a a)

instance Data a => IsZipper (Z.Zipper a a) a where
  asZipper = equality'

target :: IsZipper za a => za -> a
target = Z.hole . view asZipper

-- | A 'Lens' for the target of a zipper
_target :: IsZipper za a => Lens' za a
_target = lens target (flip replace)

up :: IsZipper za a => za -> Maybe za
up = traverseOf asZipper Z.up

down :: (IsZipper za a) => za -> Maybe za
down = traverseOf asZipper Z.down

left :: IsZipper za a => za -> Maybe za
left = traverseOf asZipper Z.left

right :: IsZipper za a => za -> Maybe za
right = traverseOf asZipper Z.right

top :: IsZipper za a => za -> za
top = farthest up

-- | Move the zipper focus as far in one direction as possible
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go a = maybe a go (f a)

-- | Convert a normal 'Expr' or 'Type' to a cursored one, focusing on the root
focus :: (Data a) => a -> Zipper a a
focus = zipper

-- | Forget the focus of a zipper, obtaining the "whole value"
unfocus :: IsZipper za a => za -> a
unfocus = Z.fromZipper . view asZipper

-- | Replace the node at the cursor with the given value.
replace :: (IsZipper za a) => a -> za -> za
replace = over asZipper . replaceHole

instance IsZipper smallZip elt => IsZipper (ZipNest largeZip smallZip small) elt where
  asZipper = position @1 % asZipper
