module Primer.Core.Type (
  Type,
  Type' (..),
  Kind,
  Kind' (..),
  TypeMeta,
  _typeMeta,
  _typeMetaLens,
  KindMeta,
  _kindMeta,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Product
import Optics (
  Lens',
  Traversal,
  (%),
 )
import Primer.Core.Meta (
  HasID (..),
  HasMetadata (_metadata),
  Meta,
  TyConName,
  TyVarName,
  Value,
 )
import Primer.JSON

-- | Core types.
--  Type variables are currently represented as text, and we have no compile-time
--  checks on scoping. We may want to introduce de Bruijn indices or use
--  bound/unbound in the future.
type Type = Type' TypeMeta

-- | Type metadata. Each type is optionally annotated with a kind.
-- Currently we don't fill these in during typechecking.
type TypeMeta = Meta (Maybe (Kind' ()))

-- | NB: Be careful with equality -- it is on-the-nose, rather than up-to-alpha: see Subst:alphaEqTy
data Type' a
  = TEmptyHole a
  | THole a (Type' a)
  | TCon a TyConName
  | TFun a (Type' a) (Type' a)
  | TVar a TyVarName
  | TApp a (Type' a) (Type' a)
  | TForall a TyVarName (Kind' ()) (Type' a)
  | -- | TLet is a let binding at the type level.
    -- It is currently only constructed automatically during evaluation -
    -- the student can't directly make it.
    TLet
      a
      -- | bound variable
      TyVarName
      -- | type the variable is bound to; the variable itself is not in scope, this is a non-recursive let
      (Type' a)
      -- | body of the let; binding scopes over this
      (Type' a)
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (Type' a)
  deriving anyclass (NFData)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal (Type' a) (Type' b) a b
_typeMeta = param @0

-- | A lens on to the metadata of a type.
-- Note that unlike '_typeMeta', this is shallow i.e. it does not recurse in to sub-expressions.
-- And for this reason, it cannot be type-changing.
_typeMetaLens :: Lens' (Type' a) a
_typeMetaLens = position @1

-- | Core kinds.
type Kind = Kind' KindMeta

-- | Metadata for kinds.
type KindMeta = Meta ()

data Kind' a
  = KHole a
  | KType a
  | KFun a (Kind' a) (Kind' a)
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (Kind' a)
  deriving anyclass (NFData)

-- | A traversal over the metadata of a kind
_kindMeta :: Traversal (Kind' a) (Kind' b) a b
_kindMeta = param @0

instance HasID a => HasID (Type' a) where
  _id = position @1 % _id

instance HasMetadata (Type' TypeMeta) where
  _metadata = position @1 % typed @(Maybe Value)

instance HasID a => HasID (Kind' a) where
  _id = position @1 % _id

instance HasMetadata (Kind' KindMeta) where
  _metadata = position @1 % typed @(Maybe Value)
