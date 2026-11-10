-- | This module contains the zipper type @TypeZ@, and functions for
--  operating on them.
module Primer.Zipper.Type (
  TypeZip,
  TypeZip',
  focusOnKind,
  KindZip',
  KindZip,
  KindTZ',
  KindTZ,
  unfocusKindT,
  focusOnlyKindT,
  TypeLoc' (..),
  TypeLoc,
  TyBindZ',
  TyBindZ,
  unfocusTypeLoc,
  focusOnTy,
  focusOnTy',
  farthest,
  search,
  FoldAbove,
  FoldAbove' (FA, current, prior),
  foldAbove,
  bindersAboveTy,
  LetTypeBinding' (LetTypeBind),
  LetTypeBinding,
  letTypeBindingName,
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
 )
import Data.Set qualified as S
import Optics (lens, set, view)
import Primer.Core.Meta (
  HasID (..),
  ID,
  TyVarBind',
  TyVarName,
  bindName,
  getID,
  unLocalName,
 )
import Primer.Core.Type (
  Kind',
  KindMeta,
  Type' (TForall),
  TypeMeta,
  scopedChildrenOfTy,
  _typeTyVarBind,
 )
import Primer.Core.Type.Utils (boundVarsTy)
import Primer.Name (Name)
import Primer.Zipper.Nested (
  IsZipper,
  PartZ,
  ZipNest (ZipNest),
  down,
  focus,
  focusPart,
  innerZipNest,
  left,
  replace,
  right,
  target,
  unfocusNest,
  unfocusPart,
  up,
 )

type KindZip' c = Zipper (Kind' c) (Kind' c)

-- | An ordinary zipper for 'Kind's
type KindZip = KindZip' KindMeta

type TypeZip' b c = Zipper (Type' b c) (Type' b c)

-- | An ordinary zipper for 'Type's
type TypeZip = TypeZip' TypeMeta KindMeta

-- | A zipper for kinds inside types
type KindTZ' b c = ZipNest (TypeZip' b c) (KindZip' c) (Kind' c)

type KindTZ = KindTZ' TypeMeta KindMeta

-- | A zipper for bindings of type variables, by a ∀ or type-level let, in types
type TyBindZ' b c = PartZ (TypeZip' b c) (TyVarBind' b)

type TyBindZ = TyBindZ' TypeMeta KindMeta

-- | A specific location in a type (not embedded in an expression).
data TypeLoc' b c
  = -- | A type
    TInType (TypeZip' b c)
  | -- | A kind (inside a ∀)
    TInKind (KindTZ' b c)
  | -- | A binding of a type variable
    TInBind (TyBindZ' b c)
  deriving stock (Generic)

type TypeLoc = TypeLoc' TypeMeta KindMeta

instance (HasID b, HasID c) => HasID (TypeLoc' b c) where
  _id = lens getter setter
    where
      getter = \case
        TInType t -> view _id t
        TInKind k -> view _id k
        TInBind b -> view _id b
      setter l i = case l of
        TInType t -> TInType $ set _id i t
        TInKind k -> TInKind $ set _id i k
        TInBind b -> TInBind $ set _id i b

-- | Shift focus up to the nearest enclosing type.
unfocusTypeLoc :: Data c => TypeLoc' b c -> TypeZip' b c
unfocusTypeLoc = \case
  TInType t -> t
  TInKind k -> unfocusKindT k
  TInBind b -> unfocusPart b

-- | Switch from a 'Kind'-in-'Type' zipper back to an 'Type' zipper.
unfocusKindT :: Data c => KindTZ' b c -> TypeZip' b c
unfocusKindT = unfocusNest

-- | Forget the surrounding type context
focusOnlyKindT :: KindTZ' b c -> KindZip' c
focusOnlyKindT = innerZipNest

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
  (Data b, HasID b, Data c, HasID c) =>
  ID ->
  Type' b c ->
  Maybe (TypeLoc' b c)
focusOnTy i = focusOnTy' i . focus

-- | Focus on the node with the given 'ID', if it exists in the focussed type
focusOnTy' ::
  (Data b, HasID b, Data c, HasID c) =>
  ID ->
  TypeZip' b c ->
  Maybe (TypeLoc' b c)
focusOnTy' i = fmap snd . search matchesID
  where
    matchesID z
      -- If the current target has the correct ID, return that
      | getID (target z) == i = Just $ TInType z
      -- If the current target binds a variable with the correct ID, return that
      | Just b <- focusPart _typeTyVarBind z, getID b == i = Just $ TInBind b
      -- If the current target has a nested kind, search that
      | TForall m a k t <- target z = do
          (zk, _) <- search (guarded (== i) . getID) (focus k)
          pure $ TInKind $ ZipNest zk $ \k' -> replace (TForall m a k' t) z
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
foldAbove :: (IsZipper za a, Monoid m) => (FoldAbove a -> m) -> za -> m
foldAbove f z = go (target z) (up z)
  where
    go p c = case c of
      Nothing -> mempty
      Just z' -> let cur = target z' in f (FA{prior = p, current = cur}) <> go cur (up z')

bindersAboveTy :: TypeZip -> S.Set TyVarName
bindersAboveTy = foldAbove getBoundHereUpTy

-- | The names bound anywhere within the focussed subtree.
bindersBelowTy :: (Data a, Data b) => TypeZip' a b -> S.Set TyVarName
bindersBelowTy = boundVarsTy . target

-- Get the names bound by this layer of an type for a given child.
getBoundHereUpTy :: (Eq a, Eq b) => FoldAbove (Type' a b) -> S.Set TyVarName
getBoundHereUpTy e = getBoundHereTy (current e) (Just $ prior e)

-- Get all names bound by this layer of an type, for any child.
getBoundHereDnTy :: (Eq a, Eq b) => Type' a b -> S.Set TyVarName
getBoundHereDnTy e = getBoundHereTy e Nothing

getBoundHereTy :: (Eq a, Eq b) => Type' a b -> Maybe (Type' a b) -> S.Set TyVarName
getBoundHereTy t prev = S.fromList [bindName b | (c, bs) <- scopedChildrenOfTy t, maybe True (== c) prev, b <- bs]

data LetTypeBinding' a b = LetTypeBind (TyVarBind' a) (Type' a b)
  deriving stock (Eq, Show)
type LetTypeBinding = LetTypeBinding' TypeMeta KindMeta

letTypeBindingName :: LetTypeBinding' a b -> Name
letTypeBindingName (LetTypeBind n _) = unLocalName $ bindName n
