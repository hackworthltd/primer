{-# LANGUAGE RecordWildCards #-}

module Primer.Core.Meta (
  HasID (..),
  getID,
  HasMetadata (_metadata),
  ID (ID),
  ModuleName (ModuleName, unModuleName),
  mkSimpleModuleName,
  moduleNamePretty,
  globalNamePretty,
  GlobalNameKind (..),
  GlobalName (qualifiedModule, baseName),
  qualifyName,
  unsafeMkGlobalName,
  TyConName,
  ValConName,
  GVarName,
  LocalNameKind (..),
  LocalName (LocalName, unLocalName),
  TmVarRef (..),
  unsafeMkLocalName,
  LVarName,
  TyVarName,
  Bind' (..),
  LVarBind',
  TyVarBind',
  bindName,
  _bindMeta,
  _bindName,
  Value,
  Meta (Meta),
  trivialMeta,
  trivialMetaUnit,
  _type,
  PrimCon (..),
  Pattern (..),
) where

import Foreword

import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Data.String (IsString (fromString))
import Optics (
  Lens,
  Lens',
  equality',
  lens,
  set,
  view,
  (%),
 )
import Primer.JSON
import Primer.Name (Name, unName, unsafeMkName)

-- | An identifier for an expression. Every node of the AST has an ID.
--
-- Note that we may remove the 'Ord', 'Enum', and/or 'Bounded'
-- instances in future implementations, so you should try not to rely
-- on them. (Internally, we rely on 'Ord' and 'Bounded', but that may
-- change in the future and is more or less not visible to external
-- consumers of this type.)
newtype ID = ID {unID :: Int}
  deriving stock (Eq, Generic, Data)
  deriving newtype (Show, Read, Num, Ord, Enum, Bounded)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (ToJSONKey, FromJSONKey)
  deriving anyclass (NFData)

data Meta a = Meta !ID a (Maybe Value)
  deriving stock (Generic, Eq, Ord, Show, Read, Data, Functor)
  deriving (FromJSON, ToJSON) via PrimerJSON (Meta a)
  deriving anyclass (NFData)

-- | This lens is called 'type' because 'a' is most commonly a Type, but it will
-- work for any 'a'.
_type :: Lens (Meta a) (Meta b) a b
_type = position @2

trivialMeta :: ID -> Meta (Maybe a)
trivialMeta id = Meta id Nothing Nothing

trivialMetaUnit :: ID -> Meta ()
trivialMetaUnit id = Meta id () Nothing

newtype ModuleName = ModuleName {unModuleName :: NonEmpty Name}
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via NonEmpty Name
  deriving anyclass (NFData)

-- | Helper function for simple (non-hierarchical) module names.
mkSimpleModuleName :: Name -> ModuleName
mkSimpleModuleName n = ModuleName $ n :| []

moduleNamePretty :: ModuleName -> Text
moduleNamePretty = mconcat . intersperse "." . toList . fmap unName . unModuleName

globalNamePretty :: GlobalName k -> Text
globalNamePretty GlobalName{..} = moduleNamePretty qualifiedModule <> "." <> unName baseName

-- | Tags for 'GlobalName'
data GlobalNameKind
  = ATyCon
  | AValCon
  | ADefName

-- | Global names are fully qualified with a module name.
-- They are tagged with what sort of name they are.
data GlobalName (k :: GlobalNameKind) = GlobalName
  { qualifiedModule :: ModuleName
  , baseName :: Name
  }
  deriving stock (Eq, Ord, Generic, Data, Show, Read)
  deriving (FromJSON, ToJSON) via PrimerJSON (GlobalName k)
  deriving anyclass (NFData, FromJSONKey, ToJSONKey)

-- | Construct a name from a Text. This is called unsafe because there are no
-- guarantees about whether the name refers to anything that is in scope.
unsafeMkGlobalName :: (NonEmpty Text, Text) -> GlobalName k
unsafeMkGlobalName (m, n) = GlobalName (ModuleName $ fmap unsafeMkName m) (unsafeMkName n)

qualifyName :: ModuleName -> Name -> GlobalName k
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
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (IsString) via Name
  deriving (FromJSON, ToJSON) via Name
  deriving anyclass (NFData)

unsafeMkLocalName :: Text -> LocalName k
unsafeMkLocalName = LocalName . unsafeMkName

type LVarName = LocalName 'ATmVar
type TyVarName = LocalName 'ATyVar

-- | A binding occurrence of a local variable, such as the @x@ in @λx. e@, or in a case branch @C x -> e@.
-- Bindings have their own metadata, and in particular their own IDs,
-- so that they can be referred to (e.g. selected) independently of the construct which introduces them.
-- The metadata of a term variable's binding is that of an expression, and holds the variable's type,
-- whereas that of a type variable's binding is that of a type, and holds the variable's kind.
data Bind' (k :: LocalNameKind) a = Bind a (LocalName k)
  deriving stock (Eq, Ord, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (Bind' k a)
  deriving anyclass (NFData)

-- | For conveniently writing bindings with no metadata, e.g. in types such as @Type' () ()@.
instance a ~ () => IsString (Bind' k a) where
  fromString = Bind () . fromString

-- | A binding of a term variable.
type LVarBind' = Bind' 'ATmVar

-- | A binding of a type variable.
type TyVarBind' = Bind' 'ATyVar

bindName :: Bind' k a -> LocalName k
bindName (Bind _ n) = n

-- | A type-modifying lens for the metadata of a binding.
_bindMeta :: Lens (Bind' k a) (Bind' k b) a b
_bindMeta = position @1

-- | A lens for the name of a binding.
_bindName :: Lens' (Bind' k a) (LocalName k)
_bindName = position @2

-- | A reference to a variable.
data TmVarRef
  = GlobalVarRef GVarName
  | LocalVarRef LVarName
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON TmVarRef
  deriving anyclass (NFData)

-- | A class for types which have an ID.
-- This makes it easier to change the underlying metadata representation without
-- breaking code that needs to work with IDs, because they use this class
-- instead of hardcoding paths to IDs or using chained 'HasType' instances,
-- which can lead to ambiguity errors.
class HasID a where
  _id :: Lens' a ID

instance HasID ID where
  _id = equality'

instance HasID (Meta a) where
  _id = position @1

instance (HasID a, HasID b) => HasID (Either a b) where
  _id =
    lens
      (either getID getID)
      (flip $ \id -> bimap (set _id id) (set _id id))

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

-- | A class for types which have metadata.
-- This exists for the same reasons that 'HasID' does
class HasMetadata a where
  _metadata :: Lens' a (Maybe Value)

instance HasMetadata (Meta a) where
  _metadata = position @3

instance HasID a => HasID (Bind' k a) where
  _id = position @1 % _id

instance HasMetadata a => HasMetadata (Bind' k a) where
  _metadata = position @1 % _metadata

-- PrimCon is not really "Meta", but is placed here to keep the module graph shallower
data PrimCon
  = PrimChar Char
  | PrimInt Integer
  | -- | Literals are always finite, and division by zero is ruled out by the types of our primitives,
    -- so non-finite values (which do not play well with `Eq` or JSON) can only arise via overflow.
    PrimDouble Double
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PrimCon
  deriving anyclass (NFData)

-- | @Pattern@s are what case branches match on. We only support shallow
-- matches, so these record the constructor name (arguments are bound by the
-- @Case@ node, rather than the pattern).
data Pattern
  = -- Pattern is not really "Meta", but is placed here to keep the module graph shallower
    PatCon ValConName
  | PatPrim PrimCon
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Pattern
  deriving anyclass (NFData)
