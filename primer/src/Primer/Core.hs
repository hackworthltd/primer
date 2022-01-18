{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module defines the core AST and some functions for operating on it.

module Primer.Core (
  Expr,
  Expr' (..),
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  Def (..),
  HasID (..),
  getID,
  setID,
  HasMetadata (_metadata),
  ID (ID),
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  _chkedAt,
  _synthed,
  Kind (..),
  TypeDef (..),
  typeDefAlg,
  typeDefKind,
  typeDefName,
  typeDefNameHints,
  typeDefParameters,
  AlgTypeDef (..),
  PrimTypeDef (..),
  PrimCon (..),
  PrimFun (..),
  PrimFunError (..),
  ValCon (..),
  valConType,
  boolDef,
  natDef,
  listDef,
  eitherDef,
  Value,
  ExprMeta,
  TypeMeta,
  Meta (Meta),
  _type,
  _exprMeta,
  _exprMetaLens,
  _exprTypeMeta,
  _typeMeta,
  _typeMetaLens,
  defaultTypeDefs,
  primTypeDefs,
  bindName,
  _bindMeta,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Optics (AffineFold, Lens, Lens', Traversal, afailing, lens, set, view, (%))
import Primer.JSON
import Primer.Name (Name)

-- | An identifier for an expression. Every node of the AST has an ID.
newtype ID = ID {unID :: Int}
  deriving (Eq, Generic, Data)
  -- The Ord and Enum instances are useful for tests but we may remove them in
  -- future, so don't use them in app code.
  deriving newtype (Show, Num, Ord, Enum)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (ToJSONKey, FromJSONKey)

data Meta a = Meta ID a (Maybe Value)
  deriving (Generic, Eq, Show, Data, Functor)
  deriving (FromJSON, ToJSON) via VJSON (Meta a)

-- | This lens is called 'type' because 'a' is most commonly a Type, but it will
-- work for any 'a'.
_type :: Lens (Meta a) (Meta b) a b
_type = position @2

-- | Typechecking will add metadata to each node describing its type.
-- Some nodes are purely synthesised, some are purely checked, and some
-- (the "embeddings") are both. These embeddings are synthesisable but in a
-- checkable context, like 'x' in 'f x'.
--
-- Since with type holes the synthesised and checked-at types for embeddings
-- may differ, we record both of them, so downstream consumers can choose which
-- one is better for their needs, rather than having the choice forced upon
-- them.
data TypeCache
  = TCSynthed (Type' ())
  | TCChkedAt (Type' ())
  | TCEmb TypeCacheBoth
  deriving (Eq, Show, Generic, Data)
  deriving (FromJSON, ToJSON) via VJSON TypeCache

-- We were checking at the first, but term was synthesisable and synth'd the
-- second We don't inline this into TypeCache because then we would get partial
-- functions from tcChkedAt and tcSynthed. We really want to name these fields
-- though, to make it clear what each one is!
data TypeCacheBoth = TCBoth {tcChkedAt :: Type' (), tcSynthed :: Type' ()}
  deriving (Eq, Show, Generic, Data)
  deriving (FromJSON, ToJSON) via VJSON TypeCacheBoth

-- TODO `_chkedAt` and `_synthed` should be `AffineTraversal`s,
-- but there is currently no `failing` for AffineTraversals, only for AffineFolds (`afailing`).
-- See https://github.com/well-typed/optics/pull/393

-- | An affine fold getting TCChkedAt or TCEmb's chked-at field
_chkedAt :: AffineFold TypeCache (Type' ())
_chkedAt = #_TCChkedAt `afailing` (#_TCEmb % #tcChkedAt)

-- | An affine fold getting TCSynthed or TCEmb's synthed field
_synthed :: AffineFold TypeCache (Type' ())
_synthed = #_TCSynthed `afailing` (#_TCEmb % #tcSynthed)

-- Expression metadata. Each expression is annotated with a type (populated by
-- the typechecker). These types aren't part of the program so they themselves
-- have no metadata - we indicate this with the '()' argument.
-- They're optional (i.e. in a 'Maybe') because when
-- modifying the AST in an action we aren't necessarily sure of the type of the
-- nodes we're inserting.
type ExprMeta = Meta (Maybe TypeCache)

-- | The core AST.
--  This is the canonical representation of Primer programs.  It is similar to
--  System F, but with support for empty and non-empty holes.  Each node holds a
--  tuple '(ID, Maybe Value)'. The first element is the ID of the node, and the
--  second element is an optional JSON object of metadata owned by the frontend,
--  which we treat as opaque.
type Expr = Expr' ExprMeta TypeMeta

-- | The generic expression type.
-- a is the type of annotations that are placed on every expression node.
-- b is the type of annotations that are placed on every type node.
-- Most of the backend fixes a ~ b ~ ID.
-- The typechecker produces a ~ (ID, Type' ()), b ~ ID.
data Expr' a b
  = Hole a (Expr' a b)
  | EmptyHole a
  | Ann a (Expr' a b) (Type' b)
  | App a (Expr' a b) (Expr' a b)
  | APP a (Expr' a b) (Type' b)
  | Con a Name
  | Lam a Name (Expr' a b)
  | LAM a Name (Expr' a b)
  | Var a Name
  | GlobalVar a ID
  | Let a Name (Expr' a b) (Expr' a b)
  | -- | LetType binds a type to a name in some expression.
    -- It is currently only constructed automatically during evaluation -
    -- the student can't directly make it.
    LetType a Name (Type' b) (Expr' a b)
  | Letrec a Name (Expr' a b) (Type' b) (Expr' a b)
  | Case a (Expr' a b) [CaseBranch' a b] -- See Note [Case]
  | PrimCon a PrimCon
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Expr' a b)

-- Note [Case]
-- We use a list for compatibility and ease of JSON
-- serialization/deserialization.
-- It would potentially be worth moving to some other structure here.
--
-- INVARIANT: branches are sorted in order of constructor in data declaration
-- We may wish to relax this decision later.
-- This is enforced in the typechecker. The purpose of this invariant is
-- twofold: having a canonical/normalised AST and making the typechecker a bit
-- simpler as we don't have to worry about looking up constructors and whether
-- we have got exactly one branch per constructor.

-- | A traversal over the metadata of an expression.
_exprMeta :: forall a b c. Traversal (Expr' a b) (Expr' c b) a c
_exprMeta = param @1

-- | A lens on to the metadata of an expression.
-- Note that unlike '_exprMeta', this is shallow i.e. it does not recurse in to sub-expressions.
-- And for this reason, it cannot be type-changing.
_exprMetaLens :: Lens' (Expr' a b) a
_exprMetaLens = position @1

-- | A traversal over the type metadata of an expression
_exprTypeMeta :: forall a b c. Traversal (Expr' a b) (Expr' a c) b c
_exprTypeMeta = param @0

type CaseBranch = CaseBranch' ExprMeta TypeMeta

data CaseBranch' a b
  = CaseBranch
      Name
      -- ^ constructor
      [Bind' a]
      -- ^ constructor parameters.
      -- Ideally this would be '[Bind' (Meta TypeCache)]' since we always know the types of branch
      -- bindings. Unfortunately that breaks generic traversals like '_exprMeta'.
      (Expr' a b)
      -- ^ right hand side
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (CaseBranch' a b)

-- | Variable bindings
-- These are used in case branches to represent the binding of a variable.
-- They aren't currently used in lambdas or lets, but in the future that may change.
data Bind' a = Bind a Name
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Bind' a)

bindName :: Bind' a -> Name
bindName (Bind _ n) = n

-- | A type-modifying lens for the metadata of a Bind.
_bindMeta :: forall a b. Lens (Bind' a) (Bind' b) a b
_bindMeta = position @1

-- | Core types.
--  Type variables are currently represented as text, and we have no compile-time
--  checks on scoping. We may want to introduce de Bruijn indices or use
--  bound/unbound in the future.
type Type = Type' TypeMeta

-- | Type metadata. Each type is optionally annotated with a kind.
-- Currently we don't fill these in during typechecking.
type TypeMeta = Meta (Maybe Kind)

-- | NB: Be careful with equality -- it is on-the-nose, rather than up-to-alpha: see Subst:alphaEqTy
data Type' a
  = TEmptyHole a
  | THole a (Type' a)
  | TCon a Name
  | TFun a (Type' a) (Type' a)
  | TVar a Name
  | TApp a (Type' a) (Type' a)
  | TForall a Name Kind (Type' a)
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Type' a)

-- | A traversal over the metadata of a type
_typeMeta :: Traversal (Type' a) (Type' b) a b
_typeMeta = param @0

-- | A lens on to the metadata of a type.
-- Note that unlike '_typeMeta', this is shallow i.e. it does not recurse in to sub-expressions.
-- And for this reason, it cannot be type-changing.
_typeMetaLens :: Lens' (Type' a) a
_typeMetaLens = position @1

-- | Core kinds.
data Kind = KHole | KType | KFun Kind Kind
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON Kind

-- | A class for types which have an ID.
-- This makes it easier to change the underlying metadata representation without
-- breaking code that needs to work with IDs, because they use this class
-- instead of hardcoding paths to IDs or using chained 'HasType' instances,
-- which can lead to ambiguity errors. These instances are undecidable but
-- they're contained to this module at least (and are in reality not
-- problematic).
class HasID a where
  _id :: Lens' a ID

instance HasType ID a => HasID (Expr' a b) where
  _id = position @1 % typed @ID

instance HasType ID a => HasID (Type' a) where
  _id = position @1 % typed @ID

instance HasType ID a => HasID (Bind' a) where
  _id = position @1 % typed @ID

instance HasID (Meta a) where
  _id = position @1

-- This instance is used in 'Primer.Zipper', but it would be an orphan if we defined it there.
instance HasID a => HasID (Zipper a a) where
  _id = lens getter setter
    where
      getter = view _id . hole
      setter z i =
        let t = hole z
         in replaceHole (set _id i t) z

-- | Get the ID of the given expression or type
getID :: HasID a => a -> ID
getID = view _id

-- | Set the ID of the given expression or type.
-- | Don't use this function outside of tests, since you could cause ID clashes.
setID :: HasID a => ID -> a -> a
setID = set _id

-- | A class for types which have metadata.
-- This exists for the same reasons that 'HasID' does
class HasMetadata a where
  _metadata :: Lens' a (Maybe Value)

instance HasMetadata (Meta a) where
  _metadata = position @3

instance HasMetadata (Expr' ExprMeta b) where
  _metadata = position @1 % typed @(Maybe Value)

instance HasMetadata (Type' TypeMeta) where
  _metadata = position @1 % typed @(Maybe Value)

instance HasMetadata (Bind' ExprMeta) where
  _metadata = position @1 % typed @(Maybe Value)

-- | A top-level definition
data Def = Def
  { defID :: ID
  , defName :: Name
  , defExpr :: Expr
  , defType :: Type
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Def

{- HLINT ignore "Use newtype instead of data" -}
data PrimCon
  = PrimChar Char
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimCon

data PrimFun = PrimFun
  { primFunType :: forall m. MonadFresh ID m => m Type
  , primFunDef :: forall m. MonadFresh ID m => [Expr] -> Either PrimFunError (m Expr)
  }

data PrimFunError
  = -- | We have attempted to apply a primitive function to ill-typed args.
    PrimFunTypeError
      Name
      -- ^ Function name
      [Expr]
      -- ^ Arguments
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimFunError

data TypeDef
  = TypeDefPrim PrimTypeDef
  | TypeDefAlg AlgTypeDef
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON TypeDef

-- | Definition of a primitive data type
data PrimTypeDef = PrimTypeDef
  { primTypeDefName :: Name
  , primTypeDefParameters :: [(Name, Kind)]
  , primTypeDefNameHints :: [Name]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimTypeDef

-- | Definition of an algebraic data type
--
-- Consider the type T = AlgTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data AlgTypeDef = AlgTypeDef
  { algTypeDefName :: Name
  , algTypeDefParameters :: [(Name, Kind)] -- These names scope over the constructors
  , algTypeDefConstructors :: [ValCon]
  , algTypeDefNameHints :: [Name]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON AlgTypeDef

data ValCon = ValCon
  { valConName :: Name
  , valConArgs :: [Type' ()]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ValCon

valConType :: AlgTypeDef -> ValCon -> Type' ()
valConType td vc =
  let ret = foldl' (\t (n, _) -> TApp () t (TVar () n)) (TCon () (algTypeDefName td)) (algTypeDefParameters td)
      args = foldr (TFun ()) ret (valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (algTypeDefParameters td)
   in foralls

typeDefName :: TypeDef -> Name
typeDefName = \case
  TypeDefPrim t -> primTypeDefName t
  TypeDefAlg t -> algTypeDefName t
typeDefNameHints :: TypeDef -> [Name]
typeDefNameHints = \case
  TypeDefPrim t -> primTypeDefNameHints t
  TypeDefAlg t -> algTypeDefNameHints t
typeDefParameters :: TypeDef -> [(Name, Kind)]
typeDefParameters = \case
  TypeDefPrim t -> primTypeDefParameters t
  TypeDefAlg t -> algTypeDefParameters t
typeDefAlg :: TypeDef -> Maybe AlgTypeDef
typeDefAlg = \case
  TypeDefPrim _ -> Nothing
  TypeDefAlg t -> Just t
typeDefKind :: TypeDef -> Kind
typeDefKind =
  foldr (KFun . snd) KType . \case
    TypeDefPrim t -> primTypeDefParameters t
    TypeDefAlg t -> algTypeDefParameters t

defaultTypeDefs :: [TypeDef]
defaultTypeDefs =
  map
    TypeDefAlg
    [boolDef, natDef, listDef, maybeDef, pairDef, eitherDef]
    <> map
      TypeDefPrim
      primTypeDefs

primTypeDefs :: [PrimTypeDef]
primTypeDefs =
  [ PrimTypeDef
      { primTypeDefName = "Char"
      , primTypeDefParameters = []
      , primTypeDefNameHints = ["c"]
      }
  ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this set.
    _ = \case
      PrimChar _ -> ()

-- | A definition of the Bool type
boolDef :: AlgTypeDef
boolDef =
  AlgTypeDef
    { algTypeDefName = "Bool"
    , algTypeDefParameters = []
    , algTypeDefConstructors =
        [ ValCon "True" []
        , ValCon "False" []
        ]
    , algTypeDefNameHints = ["p", "q"]
    }

-- | A definition of the Nat type
natDef :: AlgTypeDef
natDef =
  AlgTypeDef
    { algTypeDefName = "Nat"
    , algTypeDefParameters = []
    , algTypeDefConstructors =
        [ ValCon "Zero" []
        , ValCon "Succ" [TCon () "Nat"]
        ]
    , algTypeDefNameHints = ["i", "j", "n", "m"]
    }

-- | A definition of the List type
listDef :: AlgTypeDef
listDef =
  AlgTypeDef
    { algTypeDefName = "List"
    , algTypeDefParameters = [("a", KType)]
    , algTypeDefConstructors =
        [ ValCon "Nil" []
        , ValCon "Cons" [TVar () "a", TApp () (TCon () "List") (TVar () "a")]
        ]
    , algTypeDefNameHints = ["xs", "ys", "zs"]
    }

-- | A definition of the Maybe type
maybeDef :: AlgTypeDef
maybeDef =
  AlgTypeDef
    { algTypeDefName = "Maybe"
    , algTypeDefParameters = [("a", KType)]
    , algTypeDefConstructors =
        [ ValCon "Nothing" []
        , ValCon "Just" [TVar () "a"]
        ]
    , algTypeDefNameHints = ["mx", "my", "mz"]
    }

-- | A definition of the Pair type
pairDef :: AlgTypeDef
pairDef =
  AlgTypeDef
    { algTypeDefName = "Pair"
    , algTypeDefParameters = [("a", KType), ("b", KType)]
    , algTypeDefConstructors = [ValCon "MakePair" [TVar () "a", TVar () "b"]]
    , algTypeDefNameHints = []
    }

-- | A definition of the Either type
eitherDef :: AlgTypeDef
eitherDef =
  AlgTypeDef
    { algTypeDefName = "Either"
    , algTypeDefParameters = [("a", KType), ("b", KType)]
    , algTypeDefConstructors = [ValCon "Left" [TVar () "a"], ValCon "Right" [TVar () "b"]]
    , algTypeDefNameHints = []
    }
