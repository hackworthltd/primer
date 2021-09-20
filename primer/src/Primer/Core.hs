{-# LANGUAGE DeriveFunctor #-}
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
  Kind (..),
  TypeDef (..),
  typeDefKind,
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
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
  defaultTypeDefs,
  bindName,
) where

import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Data.List (foldl')
import GHC.Generics hiding (Meta)
import Optics (Lens', Traversal, lens, set, view, (%))
import Primer.JSON
import Primer.Name (Name)

-- | An identifier for an expression. Every node of the AST has an ID.
newtype ID = ID {unID :: Int}
  deriving (Eq, Generic, Data)
  -- The Ord and Enum instances are useful for tests but we may remove them in
  -- future, so don't use them in app code.
  deriving newtype (Show, Num, Ord, Enum)
  deriving (FromJSON, ToJSON) via VJSON ID

instance ToJSONKey ID

instance FromJSONKey ID

data Meta a = Meta ID a (Maybe Value)
  deriving (Generic, Eq, Show, Data, Functor)
  deriving (FromJSON, ToJSON) via VJSON (Meta a)

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
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Expr' a b)

-- Note [Case]
-- We use a list for compatibility and ease of JSON
-- serialisation/deserialisation.
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

-- | User defined types
--
-- Consider the type T = TypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data TypeDef = TypeDef
  { typeDefName :: Name
  , typeDefParameters :: [(Name, Kind)] -- These names scope over the constructors
  , typeDefConstructors :: [ValCon]
  , typeDefNameHints :: [Name]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON TypeDef

data ValCon = ValCon
  { valConName :: Name
  , valConArgs :: [Type' ()]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ValCon

typeDefKind :: TypeDef -> Kind
typeDefKind = foldr (KFun . snd) KType . typeDefParameters

valConType :: TypeDef -> ValCon -> Type' ()
valConType td vc =
  let ret = foldl' (\t (n, _) -> TApp () t (TVar () n)) (TCon () (typeDefName td)) (typeDefParameters td)
      args = foldr (TFun ()) ret (valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (typeDefParameters td)
   in foralls

defaultTypeDefs :: [TypeDef]
defaultTypeDefs = [boolDef, natDef, listDef, maybeDef, pairDef, eitherDef]

-- | A definition of the Bool type
boolDef :: TypeDef
boolDef =
  TypeDef
    { typeDefName = "Bool"
    , typeDefParameters = []
    , typeDefConstructors =
        [ ValCon "True" []
        , ValCon "False" []
        ]
    , typeDefNameHints = ["p", "q"]
    }

-- | A definition of the Nat type
natDef :: TypeDef
natDef =
  TypeDef
    { typeDefName = "Nat"
    , typeDefParameters = []
    , typeDefConstructors =
        [ ValCon "Zero" []
        , ValCon "Succ" [TCon () "Nat"]
        ]
    , typeDefNameHints = ["i", "j", "n", "m"]
    }

-- | A definition of the List type
listDef :: TypeDef
listDef =
  TypeDef
    { typeDefName = "List"
    , typeDefParameters = [("a", KType)]
    , typeDefConstructors =
        [ ValCon "Nil" []
        , ValCon "Cons" [TVar () "a", TApp () (TCon () "List") (TVar () "a")]
        ]
    , typeDefNameHints = ["xs", "ys", "zs"]
    }

-- | A definition of the Maybe type
maybeDef :: TypeDef
maybeDef =
  TypeDef
    { typeDefName = "Maybe"
    , typeDefParameters = [("a", KType)]
    , typeDefConstructors =
        [ ValCon "Nothing" []
        , ValCon "Just" [TVar () "a"]
        ]
    , typeDefNameHints = ["mx", "my", "mz"]
    }

-- | A definition of the Pair type
pairDef :: TypeDef
pairDef =
  TypeDef
    { typeDefName = "Pair"
    , typeDefParameters = [("a", KType), ("b", KType)]
    , typeDefConstructors = [ValCon "MakePair" [TVar () "a", TVar () "b"]]
    , typeDefNameHints = []
    }

-- | A definition of the Either type
eitherDef :: TypeDef
eitherDef =
  TypeDef
    { typeDefName = "Either"
    , typeDefParameters = [("a", KType), ("b", KType)]
    , typeDefConstructors = [ValCon "Left" [TVar () "a"], ValCon "Right" [TVar () "b"]]
    , typeDefNameHints = []
    }
