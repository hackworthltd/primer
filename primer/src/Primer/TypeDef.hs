{-# LANGUAGE OverloadedLabels #-}

module Primer.TypeDef (
  TypeDef (..),
  ValCon (..),
  TypeDefMap,
  typeDefAST,
  typeDefKind,
  typeDefNameHints,
  typeDefParameters,
  ASTTypeDef (..),
  PrimTypeDef (..),
  valConType,
  _typedefFields,
  forgetTypeDefMetadata,
  generateTypeDefIDs,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Data (Data)
import Optics (Traversal, over, traverseOf, traversed, (%))
import Primer.Core.Meta (
  ID,
  TyConName,
  TyVarName,
  ValConName,
 )
import Primer.Core.Transform (mkTAppCon)
import Primer.Core.Type (
  Kind (KFun, KType),
  Type' (TForall, TFun, TVar),
  TypeMeta,
 )
import Primer.Core.Utils (forgetTypeMetadata, generateTypeIDs)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Name (Name)

data TypeDef b
  = TypeDefPrim PrimTypeDef
  | TypeDefAST (ASTTypeDef b)
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDef b)
  deriving anyclass (NFData)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map TyConName (TypeDef ())

-- | Definition of a primitive data type
data PrimTypeDef = PrimTypeDef
  { primTypeDefParameters :: [(TyVarName, Kind)]
  , primTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PrimTypeDef
  deriving anyclass (NFData)

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef b = ASTTypeDef
  { astTypeDefParameters :: [(TyVarName, Kind)] -- These names scope over the constructors
  , astTypeDefConstructors :: [ValCon b]
  , astTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (ASTTypeDef b)
  deriving anyclass (NFData)

data ValCon b = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' b]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (ValCon b)
  deriving anyclass (NFData)

valConType :: TyConName -> ASTTypeDef () -> ValCon () -> Type' ()
valConType tc td vc =
  let ret = mkTAppCon tc (TVar () . fst <$> astTypeDefParameters td)
      args = foldr (TFun ()) ret (forgetTypeMetadata <$> valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (astTypeDefParameters td)
   in foralls

typeDefNameHints :: TypeDef b -> [Name]
typeDefNameHints = \case
  TypeDefPrim t -> primTypeDefNameHints t
  TypeDefAST t -> astTypeDefNameHints t
typeDefParameters :: TypeDef b -> [(TyVarName, Kind)]
typeDefParameters = \case
  TypeDefPrim t -> primTypeDefParameters t
  TypeDefAST t -> astTypeDefParameters t
typeDefAST :: TypeDef b -> Maybe (ASTTypeDef b)
typeDefAST = \case
  TypeDefPrim _ -> Nothing
  TypeDefAST t -> Just t
typeDefKind :: TypeDef b -> Kind
typeDefKind = foldr (KFun . snd) KType . typeDefParameters

-- | A traversal over the contstructor fields in an typedef.
_typedefFields :: Traversal (TypeDef b) (TypeDef c) (Type' b) (Type' c)
_typedefFields = #_TypeDefAST % #astTypeDefConstructors % traversed % #valConArgs % traversed

forgetTypeDefMetadata :: TypeDef b -> TypeDef ()
forgetTypeDefMetadata = over _typedefFields forgetTypeMetadata

generateTypeDefIDs :: MonadFresh ID m => TypeDef () -> m (TypeDef TypeMeta)
generateTypeDefIDs = traverseOf _typedefFields generateTypeIDs
