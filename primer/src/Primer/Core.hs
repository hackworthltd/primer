{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedLabels #-}

-- This module defines the core AST and some functions for operating on it.

module Primer.Core (
  Expr,
  Expr' (..),
  Bind,
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  module Primer.Core.Meta,
  module Primer.Core.Type,
  TypeCache (..),
  TypeCacheBoth (..),
  _chkedAt,
  _synthed,
  PrimCon (..),
  ExprMeta,
  _exprMeta,
  _exprMetaLens,
  _exprTypeMeta,
  bindName,
  _bindMeta,
  typesInExpr,
) where

import Foreword

import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Optics (
  AffineFold,
  AffineTraversal',
  Lens,
  Lens',
  Traversal,
  afailing,
  atraversalVL,
  (%),
 )
import Primer.Core.Meta (
  GVarName,
  GlobalName (baseName, qualifiedModule),
  GlobalNameKind (..),
  HasID (..),
  HasMetadata (_metadata),
  ID (ID),
  LVarName,
  LocalName (LocalName, unLocalName),
  LocalNameKind (..),
  Meta (Meta),
  ModuleName (ModuleName, unModuleName),
  TmVarRef (..),
  TyConName,
  TyVarName,
  ValConName,
  Value,
  getID,
  globalNamePretty,
  mkSimpleModuleName,
  moduleNamePretty,
  qualifyName,
  setID,
  trivialMeta,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _type,
 )
import Primer.Core.Type (
  Kind (..),
  Type,
  Type' (..),
  TypeMeta,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.JSON

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
  deriving stock (Eq, Show, Read, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeCache
  deriving anyclass (NFData)

-- We were checking at the first, but term was synthesisable and synth'd the
-- second We don't inline this into TypeCache because then we would get partial
-- functions from tcChkedAt and tcSynthed. We really want to name these fields
-- though, to make it clear what each one is!
data TypeCacheBoth = TCBoth {tcChkedAt :: Type' (), tcSynthed :: Type' ()}
  deriving stock (Eq, Show, Read, Generic, Data)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeCacheBoth
  deriving anyclass (NFData)

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
  = Hole a (Expr' a b) -- See Note [Holes and bidirectionality]
  | EmptyHole a
  | Ann a (Expr' a b) (Type' b)
  | App a (Expr' a b) (Expr' a b)
  | APP a (Expr' a b) (Type' b)
  | Con a ValConName [Expr' a b] -- See Note [Checkable constructors]
  | Lam a LVarName (Expr' a b)
  | LAM a TyVarName (Expr' a b)
  | Var a TmVarRef
  | Let
      a
      LVarName
      -- ^ bound variable
      (Expr' a b)
      -- ^ value the variable is bound to
      (Expr' a b)
      -- ^ expression the binding scopes over
  | -- | LetType binds a type to a name in some expression.
    -- It is currently only constructed automatically during evaluation -
    -- the student can't directly make it.
    LetType
      a
      TyVarName
      -- ^ bound variable
      (Type' b)
      -- ^ value the variable is bound to
      (Expr' a b)
      -- ^ expression the binding scopes over
  | Letrec
      a
      LVarName
      -- ^ bound variable
      (Expr' a b)
      -- ^ value the variable is bound to; the variable itself is in scope, as this is a recursive let
      (Type' b)
      -- ^ type of the bound variable (variable is not in scope in this type)
      (Expr' a b)
      -- ^ body of the let; binding scopes over this
  | Case a (Expr' a b) [CaseBranch' a b] -- See Note [Case]
  | PrimCon a PrimCon
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (Expr' a b)
  deriving anyclass (NFData)

-- Note [Holes and bidirectionality]
--
-- A @EmptyHole@ (often denoted @?@) is similar to the notion of a
-- typed hole from Haskell or goal from Agda: these are parts of the
-- program that have not yet been filled in. They are allowed to have
-- any type (technically they have type @TEmptyHole@, which is
-- analogous to the "dynamic" type from gradual typing).
--
-- @Hole@s (often denoted @{? e ?}@, where the @e@ is the inner
-- expression), aka "non-empty holes" are similar, but they wrap
-- another @Expr@: this is a "typing mismatch". They allow (similarly
-- to Agda's goals) step-by-step building up a complex term that will
-- eventually get inserted into that position in the program, without
-- forcing it to be well-typed at every intermediate step. This is
-- also similar to the idea of "blame" from the blame calculus.
--
-- From the "outside", both sorts of holes behave the same:
-- intuitively, they behave as if they had any type whatsoever.
-- (Indeed, they can seem to have multiple types simultaneously:
-- @let x = ? in x && (x > 2)@ is well-typed, although there is no
-- concrete term that can fill in the hole). This is achieved by them
-- being synthesisable (and thus can appear in any context: it is not
-- required for the context to say what type is expected), and they
-- synthesise the type @TEmptyHole@ which is consistent with (roughly,
-- silently coercible with) any type.
--
-- From the "inside" of a non-empty hole, there is a choice to be made
-- about typing. How does one typecheck @{? e ?}@ since we have no
-- information about what type @e@ should have. The choice our system
-- makes (following Hazel) is to require @e@ to be synthesisable. Thus
-- one cannot put a lambda directly inside a hole: @{? λx. x ?}@ is
-- ill-typed. One would have to annotate this lambda (but could
-- annotate with a hole): @{? λx.x : ? ?}@. The other possible choice
-- is to require the wrapped expression to be checkable against the
-- hole type: this is mildly more permissive since a bare lambda is
-- now allowed inside a hole, but not much more so since anything that
-- checks against a hole type can be annotated with a hole type to
-- become synthesisable.

-- Note [Checkable constructors]
--
-- Our calculus is heavily inspired by bidirectional type systems
-- (especially McBride's principled rendition). In particular we treat
-- constructors very differently to functions.
--
-- We represent a constructor-applied-to-a-spine as a thing (and can
-- only check its type), where we insist that it is fully saturated.
-- Thus whilst `Cons` is a term, it is ill-typed. The only well-formed
-- `Cons` usages are `Cons x xs`, which checks against `List A`
-- when `A ∋ x` and `List A ∋ xs`.
-- (Note that there are no type arguments here!)
--
-- Whilst this may be a bit inconsistent with the treatment of
-- functions, it has the advantage of symmetry with construction and
-- matching. (I.e. every time one sees a particular constructor, it
-- has the same form: a head of that constructor, and the same number
-- of (term) fields.)
--
-- As an example, `List Int ∋ Cons 2 Nil`, but `Cons` and `Cons 2` are ill-typed.
-- Thus one needs to be aware of the difference between, say,
-- a globally-defined function, and a constructor "of the same type".
-- For example, one can partially apply an addition function and map it
-- across a list: `map (1 +) [2,3]` is well-typed, but one cannot map
-- the `Succ` constructor in the same way, or partially-apply the `Cons` constructor.
-- (Notice, however, that since one will always know what type one is
-- considering, the constructor does not need any type applications
-- corresponding to the parameters of its datatype.)
-- Clearly one could eta-expand, (and if necessary add an annotation) to
-- use as constructor non-saturatedly: e.g. write `map (λn . Succ n) [2,3]`.

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
      ValConName
      -- ^ constructor
      [Bind' a]
      -- ^ constructor parameters.
      -- Ideally this would be '[Bind' (Meta TypeCache)]' since we always know the types of branch
      -- bindings. Unfortunately that breaks generic traversals like '_exprMeta'.
      (Expr' a b)
      -- ^ right hand side
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (CaseBranch' a b)
  deriving anyclass (NFData)

-- | Variable bindings
-- These are used in case branches to represent the binding of a variable.
-- They aren't currently used in lambdas or lets, but in the future that may change.
type Bind = Bind' ExprMeta

data Bind' a = Bind a LVarName
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (Bind' a)
  deriving anyclass (NFData)

bindName :: Bind' a -> LVarName
bindName (Bind _ n) = n

-- | A type-modifying lens for the metadata of a Bind.
_bindMeta :: forall a b. Lens (Bind' a) (Bind' b) a b
_bindMeta = position @1

-- | Note that this does not recurse in to sub-expressions or sub-types.
typesInExpr :: AffineTraversal' (Expr' a b) (Type' b)
typesInExpr = atraversalVL $ \point f -> \case
  Ann m e ty -> Ann m e <$> f ty
  APP m e ty -> APP m e <$> f ty
  LetType m x ty e -> (\ty' -> LetType m x ty' e) <$> f ty
  Letrec m x b ty e -> (\ty' -> Letrec m x b ty' e) <$> f ty
  e -> point e

instance HasID a => HasID (Expr' a b) where
  _id = position @1 % _id

instance HasID a => HasID (Bind' a) where
  _id = position @1 % _id

instance HasMetadata (Expr' ExprMeta b) where
  _metadata = position @1 % typed @(Maybe Value)

instance HasMetadata (Bind' ExprMeta) where
  _metadata = position @1 % typed @(Maybe Value)

data PrimCon
  = PrimChar Char
  | PrimInt Integer
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PrimCon
  deriving anyclass (NFData)
