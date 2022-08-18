module Primer.Module (
  Module (..),
  qualifyTyConName,
  moduleTypesQualified,
  qualifyDefName,
  moduleDefsQualified,
  insertDef,
  deleteDef,
  renameModule,
  renameModule',
  nextModuleID,
  builtinModule,
  primitiveModule,
) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Data.List.Extra (enumerate)
import Data.Map (delete, insert, mapKeys, member)
import Data.Map qualified as M
import Primer.Builtins (
  boolDef,
  builtinModuleName,
  eitherDef,
  listDef,
  maybeDef,
  natDef,
  pairDef,
  tBool,
  tEither,
  tList,
  tMaybe,
  tNat,
  tPair,
 )
import Primer.Core (
  GVarName,
  GlobalName (baseName),
  ID,
  ModuleName,
  TyConName,
  qualifyName,
 )
import Primer.Def (
  Def (..),
  DefMap,
 )
import Primer.Def.Utils (nextID)
import Primer.JSON (
  CustomJSON (CustomJSON),
  FromJSON,
  PrimerJSON,
  ToJSON,
 )
import Primer.Name (Name)
import Primer.Primitives (allPrimTypeDefs, primDefName, primitiveModuleName)
import Primer.TypeDef (TypeDef (..), TypeDefMap)

data Module = Module
  { moduleName :: ModuleName
  , moduleTypes :: Map Name TypeDef
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Module

qualifyTyConName :: Module -> Name -> TyConName
qualifyTyConName m = qualifyName (moduleName m)

moduleTypesQualified :: Module -> TypeDefMap
moduleTypesQualified m = mapKeys (qualifyTyConName m) $ moduleTypes m

qualifyDefName :: Module -> Name -> GVarName
qualifyDefName m = qualifyName (moduleName m)

moduleDefsQualified :: Module -> DefMap
moduleDefsQualified m = mapKeys (qualifyDefName m) $ moduleDefs m

-- | This assumes that the definition has the correct name to be inserted
-- into the module. I.e. @qualifiedModule (defName d) == moduleName m@.
insertDef :: Module -> Name -> Def -> Module
insertDef m n d = m{moduleDefs = insert n d $ moduleDefs m}

-- | Returns 'Nothing' if (and only if) the definition was not found in the module
deleteDef :: Module -> GVarName -> Maybe Module
deleteDef m d =
  if d `member` moduleDefsQualified m
    then Just $ m{moduleDefs = delete (baseName d) (moduleDefs m)}
    else Nothing

-- | Renames a module and any references to it (in the given 'Traversable' of
-- modules). Returns 'Nothing' if the requested new name is in use
-- (as the name of one of the modules, references are not detected)
renameModule :: Traversable t => ModuleName -> ModuleName -> t Module -> Maybe (t Module)
renameModule fromName toName = traverse rn1
  where
    rn1 m =
      if moduleName m == toName
        then Nothing
        else pure $ renameModule' fromName toName m

-- | Renames all occurrences of the given 'ModuleName'. This does not
-- detect name clashes, see 'renameModule'
renameModule' :: Data a => ModuleName -> ModuleName -> a -> a
renameModule' fromName toName = transformBi (\n -> if n == fromName then toName else n)

-- | Given a 'Module', return the next 'ID' that's safe to use when
-- editing its definitions.
--
-- Note: do not rely on the implementation of this function, as it may
-- change in the future.
nextModuleID :: Module -> ID
nextModuleID m = foldl' (\id_ d -> max (nextID d) id_) minBound (moduleDefs m)

-- | This module depends on the builtin module, due to some terms referencing builtin types.
-- It contains all primitive types and terms.
primitiveModule :: Module
primitiveModule =
  Module
    { moduleName = primitiveModuleName
    , moduleTypes = TypeDefPrim <$> M.mapKeys baseName allPrimTypeDefs
    , moduleDefs = M.fromList $ [(primDefName def, DefPrim def) | def <- enumerate]
    }

builtinModule :: Module
builtinModule =
  Module
    { moduleName = builtinModuleName
    , moduleTypes =
        M.fromList
          [ (baseName tBool, TypeDefAST boolDef)
          , (baseName tNat, TypeDefAST natDef)
          , (baseName tList, TypeDefAST listDef)
          , (baseName tMaybe, TypeDefAST maybeDef)
          , (baseName tPair, TypeDefAST pairDef)
          , (baseName tEither, TypeDefAST eitherDef)
          ]
    , moduleDefs = mempty
    }
