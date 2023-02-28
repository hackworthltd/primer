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
) where

import Foreword

import Data.Data (Data)
import Primer.Core.Meta (
  TyConName,
  TyVarName,
  ValConName,
 )
import Primer.Core.Transform (mkTAppCon)
import Primer.Core.Type (
  Kind (KFun, KType),
  Type' (TForall, TFun, TVar),
 )
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Name (Name)

data TypeDef
  = TypeDefPrim PrimTypeDef
  | TypeDefAST ASTTypeDef
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON TypeDef
  deriving anyclass (NFData)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map TyConName TypeDef

-- | Definition of a primitive data type
data PrimTypeDef = PrimTypeDef
  { primTypeDefParameters :: [Kind]
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
data ASTTypeDef = ASTTypeDef
  { astTypeDefParameters :: [(TyVarName, Kind)] -- These names scope over the constructors
  , astTypeDefConstructors :: [ValCon]
  , astTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ASTTypeDef
  deriving anyclass (NFData)

data ValCon = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' ()]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ValCon
  deriving anyclass (NFData)

valConType :: TyConName -> ASTTypeDef -> ValCon -> Type' ()
valConType tc td vc =
  let ret = mkTAppCon tc (TVar () . fst <$> astTypeDefParameters td)
      args = foldr (TFun ()) ret (valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (astTypeDefParameters td)
   in foralls

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
