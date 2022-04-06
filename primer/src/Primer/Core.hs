{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module defines the core AST and some functions for operating on it.

module Primer.Core (
  Expr,
  Expr' (..),
  Bind' (..),
  TmVarRef (..),
  varRefName,
  CaseBranch,
  CaseBranch' (..),
  Def (..),
  defName,
  defType,
  ASTDef (..),
  defAST,
  PrimDef (..),
  defPrim,
  HasID (..),
  getID,
  setID,
  HasMetadata (_metadata),
  ID (ID),
  GlobalNameKind (..),
  GlobalName (baseName),
  qualifyName,
  unsafeMkGlobalName,
  TyConName,
  ValConName,
  GVarName,
  LocalNameKind (..),
  LocalName (LocalName, unLocalName),
  unsafeMkLocalName,
  LVarName,
  TyVarName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  _chkedAt,
  _synthed,
  Kind (..),
  TypeDef (..),
  typeDefAST,
  typeDefKind,
  typeDefName,
  typeDefNameHints,
  typeDefParameters,
  ASTTypeDef (..),
  PrimTypeDef (..),
  PrimCon (..),
  primConName,
  PrimFun (..),
  primFunType,
  ExprAnyFresh (..),
  PrimFunError (..),
  ValCon (..),
  valConType,
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
  bindName,
  _bindMeta,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh (fresh))
import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Optics (AffineFold, Lens, Lens', Traversal, afailing, lens, set, view, (%))
import Primer.JSON
import Primer.Name (Name, unsafeMkName)

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

-- | Tags for 'GlobalName'
data GlobalNameKind
  = ATyCon
  | AValCon
  | ADefName

-- | Global names are currently the same as 'Name's, but will shortly contain
-- a module prefix also. They are tagged with what sort of name they are.
newtype GlobalName (k :: GlobalNameKind) = GlobalName {baseName :: Name}
  deriving (Eq, Ord, Generic, Data)
  deriving newtype (Show, IsString)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

unsafeMkGlobalName :: Text -> GlobalName k
unsafeMkGlobalName = GlobalName . unsafeMkName

-- | Currently just wraps the name, but shortly will take another
-- argument for a module prefix
qualifyName :: Name -> GlobalName k
qualifyName = GlobalName

type TyConName = GlobalName 'ATyCon
type ValConName = GlobalName 'AValCon
type GVarName = GlobalName 'ADefName

-- | Tags for 'LocalName'
data LocalNameKind
  = ATmVar
  | ATyVar

-- | A newtype wrapper around a 'Name', tracking that the name refers
-- to a local variable. The tag says which sort of variable (term or
-- type) this is.
newtype LocalName (k :: LocalNameKind) = LocalName {unLocalName :: Name}
  deriving (Eq, Ord, Show, Data, Generic)
  deriving (IsString) via Name
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Name

unsafeMkLocalName :: Text -> LocalName k
unsafeMkLocalName = LocalName . unsafeMkName

type LVarName = LocalName 'ATmVar
type TyVarName = LocalName 'ATyVar

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
  | Con a ValConName -- See Note [Synthesisable constructors]
  | Lam a LVarName (Expr' a b)
  | LAM a TyVarName (Expr' a b)
  | Var a TmVarRef
  | Let a LVarName (Expr' a b) (Expr' a b)
  | -- | LetType binds a type to a name in some expression.
    -- It is currently only constructed automatically during evaluation -
    -- the student can't directly make it.
    LetType a TyVarName (Type' b) (Expr' a b)
  | Letrec a LVarName (Expr' a b) (Type' b) (Expr' a b)
  | Case a (Expr' a b) [CaseBranch' a b] -- See Note [Case]
  | PrimCon a PrimCon
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Expr' a b)

-- | A reference to a variable.
data TmVarRef
  = GlobalVarRef GVarName
  | LocalVarRef LVarName
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON TmVarRef

varRefName :: TmVarRef -> Name
varRefName = \case
  GlobalVarRef (GlobalName n) -> n
  LocalVarRef (LocalName n) -> n

-- Note [Synthesisable constructors]
-- Whilst our calculus is heavily inspired by bidirectional type systems
-- (especially McBride's principled rendition), we do not treat constructors
-- in this fashion. We view constructors as synthesisable terms
-- ("eliminations"), rather than checkable terms ("constructions").
-- This is for user-experience purposes: we are attempting a pedagogic
-- system where the user-facing code is close to the core language, and
-- we believe that the bidirectional style would be confusing and/or
-- annoyingly restrictive in this particular instance.
--
-- We follow the traditional non-bidirectional view of constructors here:
-- a constructor is a term in-and-of itself (and one can infer its type).
-- Thus one has `Cons` is a term, and we can derive the synthesis
-- judgement `Cons ∈ ∀a. a -> List a -> List a`.
--
-- For comparison, the bidirectional view would be that constructors must
-- always be fully applied, and one can only subject them to a typechecking
-- judgement where the type is an input.
-- Thus `List Int ∋ Cons 2 Nil`, but `Cons` and `Cons 2` are ill-typed.
-- Under this view, one needs to be aware of the difference between, say,
-- a globally-defined function, and a constructor "of the same type".
-- For example, one can partially apply an addition function and map it
-- across a list: `map (1 +) [2,3]` is well-typed, but one cannot map
-- the `Succ` constructor in the same way.
-- (Notice, however, that since one will always know what type one is
-- considering, the constructor does not need any type applications
-- corresponding to the parameters of its datatype.)
-- Clearly one could eta-expand, (and if necessary add an annotation) to
-- use as constructor non-saturatedly: e.g. write `map (λn . Succ n) [2,3]`.
--
-- In effect, we just bake this translation into the core. To do this, we
-- require constructor names to be unique across different types.

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
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (CaseBranch' a b)

-- | Variable bindings
-- These are used in case branches to represent the binding of a variable.
-- They aren't currently used in lambdas or lets, but in the future that may change.
data Bind' a = Bind a LVarName
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON (Bind' a)

bindName :: Bind' a -> LVarName
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
  | TCon a TyConName
  | TFun a (Type' a) (Type' a)
  | TVar a TyVarName
  | TApp a (Type' a) (Type' a)
  | TForall a TyVarName Kind (Type' a)
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

data Def
  = DefPrim PrimDef
  | DefAST ASTDef
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON Def

-- | A primitive, built-in definition
data PrimDef = PrimDef
  { primDefName :: GVarName
  -- ^ Used for display, and to link to an entry in `allPrimDefs`
  , primDefType :: Type
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimDef

-- | A top-level definition, built from an 'Expr'
data ASTDef = ASTDef
  { astDefName :: GVarName
  , astDefExpr :: Expr
  , astDefType :: Type
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ASTDef

defName :: Def -> GVarName
defName = \case
  DefPrim d -> primDefName d
  DefAST d -> astDefName d
defType :: Def -> Type
defType = \case
  DefPrim d -> primDefType d
  DefAST d -> astDefType d
defAST :: Def -> Maybe ASTDef
defAST = \case
  DefPrim _ -> Nothing
  DefAST t -> Just t
defPrim :: Def -> Maybe PrimDef
defPrim = \case
  DefPrim t -> Just t
  DefAST _ -> Nothing

data PrimCon
  = PrimChar Char
  | PrimInt Integer
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimCon

-- | The name of the type to which this primitive constructor belongs.
-- This should be a key in `allPrimTypeDefs`.
primConName :: PrimCon -> TyConName
primConName = \case
  PrimChar _ -> "Char"
  PrimInt _ -> "Int"

data PrimFun = PrimFun
  { primFunTypes :: forall m. MonadFresh ID m => m ([Type], Type)
  -- ^ the function's arguments and return type
  , primFunDef :: [Expr' () ()] -> Either PrimFunError ExprAnyFresh
  }

primFunType :: forall m. MonadFresh ID m => PrimFun -> m Type
primFunType pf = do
  (args, res) <- primFunTypes pf
  foldrM f res args
  where
    f x y = do
      id <- fresh
      pure $ TFun (Meta id Nothing Nothing) x y

-- TODO with `-XImpredicativeTypes` in GHC 9.2, we can turn this in to a type synonym, then inline it
-- see https://github.com/hackworthltd/primer/issues/189
newtype ExprAnyFresh = ExprAnyFresh (forall m. MonadFresh ID m => m Expr)

data PrimFunError
  = -- | We have attempted to apply a primitive function to invalid args.
    PrimFunError
      GVarName
      -- ^ Function name
      [Expr' () ()]
      -- ^ Arguments
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimFunError

data TypeDef
  = TypeDefPrim PrimTypeDef
  | TypeDefAST ASTTypeDef
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON TypeDef

-- | Definition of a primitive data type
data PrimTypeDef = PrimTypeDef
  { primTypeDefName :: TyConName
  , primTypeDefParameters :: [Kind]
  , primTypeDefNameHints :: [Name]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON PrimTypeDef

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef = ASTTypeDef
  { astTypeDefName :: TyConName
  , astTypeDefParameters :: [(TyVarName, Kind)] -- These names scope over the constructors
  , astTypeDefConstructors :: [ValCon]
  , astTypeDefNameHints :: [Name]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ASTTypeDef

data ValCon = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' ()]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ValCon

valConType :: ASTTypeDef -> ValCon -> Type' ()
valConType td vc =
  let ret = foldl' (\t (n, _) -> TApp () t (TVar () n)) (TCon () (astTypeDefName td)) (astTypeDefParameters td)
      args = foldr (TFun ()) ret (valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (astTypeDefParameters td)
   in foralls

typeDefName :: TypeDef -> TyConName
typeDefName = \case
  TypeDefPrim t -> primTypeDefName t
  TypeDefAST t -> astTypeDefName t
typeDefNameHints :: TypeDef -> [Name]
typeDefNameHints = \case
  TypeDefPrim t -> primTypeDefNameHints t
  TypeDefAST t -> astTypeDefNameHints t
typeDefParameters :: TypeDef -> [Kind]
typeDefParameters = \case
  TypeDefPrim t -> primTypeDefParameters t
  TypeDefAST t -> snd <$> astTypeDefParameters t
typeDefAST :: TypeDef -> Maybe ASTTypeDef
typeDefAST = \case
  TypeDefPrim _ -> Nothing
  TypeDefAST t -> Just t
typeDefKind :: TypeDef -> Kind
typeDefKind = foldr KFun KType . typeDefParameters
