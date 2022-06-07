module Primer.Module (
  Module (..),
  mkTypeDefMap,
  qualifyTyConName,
  moduleTypesQualified,
  qualifyDefName,
  moduleDefsQualified,
  insertDef,
  deleteDef,
  renameModule,
  renameModule',
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Map (delete, insert, mapKeys, member)
import qualified Data.Map as M
import Foreword
import Primer.Core (
  Def,
  DefMap,
  GVarName,
  GlobalName (baseName),
  ModuleName,
  TyConName,
  TypeDef,
  qualifyName,
  typeDefName,
 )
import Primer.JSON
import Primer.Name (Name)

data Module = Module
  { moduleName :: ModuleName
  , -- Invariant: the names are consistent: keys cache the names in the Defs.
    -- In particular, if (n,d) is in the moduleDefs map,
    -- then "qualifyDefName m n == defName d"
    moduleTypes :: Map Name TypeDef
  , moduleDefs :: Map Name Def -- The current program: a set of definitions indexed by Name
  }
  deriving (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via VJSON Module

-- | Create a mapping of name to typedef for use in modules.
-- Ensures that @baseName $ typeDefName (mkTypeDefMap ! n) == n@
-- Assumes that all the typedefs have the same @qualifiedModule@ part to their name.
mkTypeDefMap :: [TypeDef] -> Map Name TypeDef
mkTypeDefMap defs = M.fromList $ map (\d -> (baseName $ typeDefName d, d)) defs

qualifyTyConName :: Module -> Name -> TyConName
qualifyTyConName m = qualifyName (moduleName m)

moduleTypesQualified :: Module -> Map TyConName TypeDef
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
