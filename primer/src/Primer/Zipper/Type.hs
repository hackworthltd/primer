{-# LANGUAGE FunctionalDependencies #-}

-- | This module contains the zipper type @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper.Type (
  TypeZip,
  TypeZip',
  IsZipper (asZipper),
  focus,
  target,
  _target,
  replace,
  focusOnKind,
  focusOnTy,
  top,
  up,
  down,
  left,
  right,
  farthest,
  search,
  FoldAbove,
  FoldAbove' (FA, current, prior),
  foldAbove,
  foldBelow,
  bindersAboveTy,
  LetTypeBinding' (LetTypeBind),
  LetTypeBinding,
  letTypeBindingName,
  getBoundHereTy',
  getBoundHereTy,
  getBoundHereUpTy,
  getBoundHereDnTy,
  bindersBelowTy,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (
  Zipper,
  replaceHole,
  zipper,
 )
import Data.Generics.Uniplate.Zipper qualified as Z
import Data.Set qualified as S
import Optics (
  over,
  view,
 )
import Optics.Lens (Lens', equality', lens)
import Optics.Traversal (traverseOf)
import Primer.Core.Meta (
  HasID (..),
  ID,
  TyVarName,
  getID,
  unLocalName,
 )
import Primer.Core.Type (
  Kind',
  Type' (TForall, TLet),
  TypeMeta,
 )
import Primer.Name (Name)

type KindZip' c = Zipper (Kind' c) (Kind' c)

type TypeZip' b = Zipper (Type' b) (Type' b)

-- | An ordinary zipper for 'Type's
type TypeZip = TypeZip' TypeMeta

-- | We want to use up, down, left, right, etc. on 'ExprZ' and 'TypeZ',
-- despite them being very different types. This class enables that, by proxying
-- each method through to the underlying Zipper.
-- @za@ is the user-facing type, i.e. 'ExprZ' or 'TypeZ'.
-- @a@ is the type of targets of the internal zipper, i.e. 'Expr' or 'Type'.
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

-- | Convert a normal 'Expr' or 'Type' to a cursored one, focusing on the root
focus :: (Data a) => a -> Zipper a a
focus = zipper

-- | Replace the node at the cursor with the given value.
replace :: (IsZipper za a) => a -> za -> za
replace = over asZipper . replaceHole

-- | Focus on the node with the given 'ID', if it exists in the kind
focusOnKind ::
  (Data c, HasID c) =>
  ID ->
  Kind' c ->
  Maybe (KindZip' c)
focusOnKind i = focusOnKind' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed kind
focusOnKind' ::
  (Data c, HasID c) =>
  ID ->
  KindZip' c ->
  Maybe (KindZip' c)
focusOnKind' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just z
      | otherwise = Nothing

-- | Focus on the node with the given 'ID', if it exists in the type
focusOnTy ::
  (Data b, HasID b) =>
  ID ->
  Type' b ->
  Maybe (Zipper (Type' b) (Type' b))
focusOnTy i = focusOnTy' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed type
focusOnTy' ::
  (Data b, HasID b) =>
  ID ->
  Zipper (Type' b) (Type' b) ->
  Maybe (Zipper (Type' b) (Type' b))
focusOnTy' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just z
      | otherwise = Nothing

-- | Search for a node for which @f@ returns @Just@ something.
-- Performs a depth-first, leftmost-first search.
search :: (IsZipper za a) => (za -> Maybe b) -> za -> Maybe (za, b)
search f z
  | Just x <- f z = Just (z, x)
  | otherwise =
      -- if the node has children, recurse on the leftmost child
      (down z >>= search f . farthest left)
        -- then recurse on the sibling to the right
        <|> (right z >>= search f)

-- | Move the zipper focus as far in one direction as possible
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where go a = maybe a go (f a)

data FoldAbove' a b = FA {prior :: a, current :: b}
type FoldAbove a = FoldAbove' a a

-- | Focus on everything 'up', in order, map each to a monoid, and accumulate.
-- This does not focus on the current target.
-- We keep track of which child we just came up from, as that can be important
-- in applications: e.g. finding enclosing binders, where @let x=e1 in e2@ has
-- two children, but only binds a variable in one of them.
-- NB: 'foldAbove' + 'foldBelow' does not encompass the whole term: it misses
-- siblings.
foldAbove :: (IsZipper za a, Monoid m) => (FoldAbove a -> m) -> za -> m
foldAbove f z = go (target z) (up z)
  where
    go p c = case c of
      Nothing -> mempty
      Just z' -> let cur = target z' in f (FA{prior = p, current = cur}) <> go cur (up z')

-- | Focus on the current thing, and then everything 'below', in depth-first,
-- leftmost-first order;
-- map each to a monoid, and accumulate
-- NB: 'foldAbove' + 'foldBelow' does not encompass the whole term: it misses
-- siblings.
foldBelow :: (IsZipper za a, Monoid m) => (a -> m) -> za -> m
foldBelow f z = f (target z) <> maybe mempty (go . farthest left) (down z)
  where
    go z' = f (target z') <> maybe mempty (go . farthest left) (down z') <> maybe mempty go (right z')

bindersAboveTy :: TypeZip -> S.Set TyVarName
bindersAboveTy = foldAbove getBoundHereUpTy

-- Note that we have two specialisations we care about:
-- bindersBelowTy :: TypeZip -> S.Set Name
-- bindersBelowTy :: Zipper (Type' One) (Type' One) -> S.Set Name
bindersBelowTy :: (Data a, Eq a) => Zipper (Type' a) (Type' a) -> S.Set TyVarName
bindersBelowTy = foldBelow getBoundHereDnTy

-- Get the names bound by this layer of an type for a given child.
getBoundHereUpTy :: Eq a => FoldAbove (Type' a) -> S.Set TyVarName
getBoundHereUpTy e = getBoundHereTy (current e) (Just $ prior e)

-- Get all names bound by this layer of an type, for any child.
getBoundHereDnTy :: Eq a => Type' a -> S.Set TyVarName
getBoundHereDnTy e = getBoundHereTy e Nothing

getBoundHereTy :: Eq a => Type' a -> Maybe (Type' a) -> S.Set TyVarName
getBoundHereTy t prev = S.fromList $ either identity (\(LetTypeBind n _) -> n) <$> getBoundHereTy' t prev

data LetTypeBinding' a = LetTypeBind TyVarName (Type' a)
  deriving stock (Eq, Show)
type LetTypeBinding = LetTypeBinding' TypeMeta

letTypeBindingName :: LetTypeBinding' a -> Name
letTypeBindingName (LetTypeBind n _) = unLocalName n

getBoundHereTy' :: Eq a => Type' a -> Maybe (Type' a) -> [Either TyVarName (LetTypeBinding' a)]
getBoundHereTy' t prev = case t of
  TForall _ v _ _ -> [Left v]
  TLet _ v rhs b ->
    if maybe True (== b) prev
      then [Right $ LetTypeBind v rhs]
      else mempty
  _ -> mempty
