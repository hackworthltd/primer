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
import Optics (Field2 (_2), Traversal, over, traversalVL, traverseOf, traversed, (%))
import Primer.Core.Meta (
  ID,
  TyConName,
  TyVarName,
  ValConName,
 )
import Primer.Core.Transform (mkTAppCon)
import Primer.Core.Type (
  Kind' (KFun, KType),
  KindMeta,
  Type' (TForall, TFun, TVar),
  TypeMeta,
 )
import Primer.Core.Utils (forgetKindMetadata, forgetTypeMetadata, generateKindIDs, generateTypeIDs)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Name (Name)

data TypeDef b c
  = TypeDefPrim (PrimTypeDef c)
  | TypeDefAST (ASTTypeDef b c)
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (TypeDef b c)
  deriving anyclass (NFData)

-- | A mapping of global names to 'TypeDef's.
type TypeDefMap = Map TyConName (TypeDef () ())

-- | Definition of a primitive data type
data PrimTypeDef c = PrimTypeDef
  { primTypeDefParameters :: [(TyVarName, Kind' c)]
  , primTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (PrimTypeDef c)
  deriving anyclass (NFData)

-- | Definition of an algebraic data type
--
-- Consider the type T = ASTTypeDef "T" [("a",TYPE),("b",TYPE->TYPE)] [ValCon "C" [b a, Nat]]
-- The kind of the type is TYPE{\-a-\} -> (TYPE -> TYPE){\-b-\} -> TYPE{\-always returns a type-\}
-- The type of the constructor is C :: forall a:TYPE. forall b:(TYPE->TYPE). b a -> Nat -> T a b
data ASTTypeDef b c = ASTTypeDef
  { astTypeDefParameters :: [(TyVarName, Kind' c)] -- These names scope over the constructors
  , astTypeDefConstructors :: [ValCon b ()]
  , astTypeDefNameHints :: [Name]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (ASTTypeDef b c)
  deriving anyclass (NFData)

data ValCon b c = ValCon
  { valConName :: ValConName
  , valConArgs :: [Type' b c]
  }
  deriving stock (Eq, Show, Read, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (ValCon b c)
  deriving anyclass (NFData)

valConType :: TyConName -> ASTTypeDef () () -> ValCon () () -> Type' () ()
valConType tc td vc =
  let ret = mkTAppCon tc (TVar () . fst <$> astTypeDefParameters td)
      args = foldr (TFun () . forgetTypeMetadata) ret (valConArgs vc)
      foralls = foldr (\(n, k) t -> TForall () n k t) args (astTypeDefParameters td)
   in foralls

typeDefNameHints :: TypeDef b c -> [Name]
typeDefNameHints = \case
  TypeDefPrim t -> primTypeDefNameHints t
  TypeDefAST t -> astTypeDefNameHints t
typeDefParameters :: TypeDef b c -> [(TyVarName, Kind' c)]
typeDefParameters = \case
  TypeDefPrim t -> primTypeDefParameters t
  TypeDefAST t -> astTypeDefParameters t
typeDefAST :: TypeDef b c -> Maybe (ASTTypeDef b c)
typeDefAST = \case
  TypeDefPrim _ -> Nothing
  TypeDefAST t -> Just t
typeDefKind :: TypeDef b () -> Kind' ()
typeDefKind = foldr (KFun () . snd) (KType ()) . typeDefParameters

-- | A traversal over the contstructor fields in an typedef.
_typedefFields :: Traversal (TypeDef b c) (TypeDef b' c) (Type' b ()) (Type' b' ())
_typedefFields =
  #_TypeDefAST
    % #astTypeDefConstructors
    % traversed
    % #valConArgs
    % traversed

_typedefParamKinds :: Traversal (TypeDef b c) (TypeDef b c') (Kind' c) (Kind' c')
_typedefParamKinds = traversalVL $ \f -> \case
  TypeDefPrim td -> TypeDefPrim <$> traverseOf (#primTypeDefParameters % traversed % _2) f td
  TypeDefAST td -> TypeDefAST <$> traverseOf (#astTypeDefParameters % traversed % _2) f td

forgetTypeDefMetadata :: TypeDef b c -> TypeDef () ()
forgetTypeDefMetadata =
  over _typedefFields forgetTypeMetadata
    . over _typedefParamKinds forgetKindMetadata

generateTypeDefIDs :: MonadFresh ID m => TypeDef () () -> m (TypeDef TypeMeta KindMeta)
generateTypeDefIDs =
  traverseOf _typedefFields generateTypeIDs
    <=< traverseOf _typedefParamKinds generateKindIDs
