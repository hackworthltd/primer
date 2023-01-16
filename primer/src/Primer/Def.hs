module Primer.Def (
  Def (..),
  DefMap,
  ASTDef (..),
  defAST,
  defPrim,
  defType,
) where

import Foreword

import Data.Data (Data)
import Primer.Core (
  Expr,
  GVarName,
  Type,
  Type',
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Primitives (PrimDef, primDefType)

data Def
  = DefPrim PrimDef
  | DefAST ASTDef
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Def
  deriving anyclass (NFData)

defType :: Def -> Type' ()
defType = \case
  DefPrim d -> primDefType d
  DefAST d -> forgetTypeMetadata $ astDefType d

-- | A mapping of global names to 'Def's.
type DefMap = Map GVarName Def

-- | A top-level definition, built from an 'Expr'
data ASTDef = ASTDef
  { astDefExpr :: Expr
  , astDefType :: Type
  }
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ASTDef
  deriving anyclass (NFData)

defAST :: Def -> Maybe ASTDef
defAST = \case
  DefPrim _ -> Nothing
  DefAST t -> Just t
defPrim :: Def -> Maybe PrimDef
defPrim = \case
  DefPrim t -> Just t
  DefAST _ -> Nothing
