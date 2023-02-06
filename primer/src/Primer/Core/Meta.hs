{-# LANGUAGE RecordWildCards #-}

module Primer.Core.Meta (
  HasID (..),
  getID,
  setID,
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
  Value,
  Meta (Meta),
  trivialMeta,
  _type,
) where

import Foreword

import Data.Aeson (Value)
import Data.Data (Data)
import Data.Generics.Product
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Zipper (Zipper, hole, replaceHole)
import Optics (
  Lens,
  Lens',
  equality',
  lens,
  set,
  view,
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
  deriving newtype (Show, Num, Ord, Enum, Bounded)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (ToJSONKey, FromJSONKey)
  deriving anyclass (NFData)

data Meta a = Meta ID a (Maybe Value)
  deriving stock (Generic, Eq, Show, Data, Functor)
  deriving (FromJSON, ToJSON) via PrimerJSON (Meta a)
  deriving anyclass (NFData)

-- | This lens is called 'type' because 'a' is most commonly a Type, but it will
-- work for any 'a'.
_type :: Lens (Meta a) (Meta b) a b
_type = position @2

trivialMeta :: ID -> Meta (Maybe a)
trivialMeta id = Meta id Nothing Nothing

newtype ModuleName = ModuleName {unModuleName :: NonEmpty Name}
  deriving stock (Eq, Ord, Show, Data, Generic)
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
  deriving stock (Eq, Ord, Generic, Data, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON (GlobalName k)
  deriving anyclass (NFData)

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
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving (IsString) via Name
  deriving (FromJSON, ToJSON) via Name
  deriving anyclass (NFData)

unsafeMkLocalName :: Text -> LocalName k
unsafeMkLocalName = LocalName . unsafeMkName

type LVarName = LocalName 'ATmVar
type TyVarName = LocalName 'ATyVar

-- | A reference to a variable.
data TmVarRef
  = GlobalVarRef GVarName
  | LocalVarRef LVarName
  deriving stock (Eq, Show, Data, Generic)
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
