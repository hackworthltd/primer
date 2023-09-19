{-# LANGUAGE OverloadedLabels #-}

module Primer.Def.Utils (nextID, nextIDTypeDef, globalInUse, typeInUse) where

import Foreword

import Data.Data (Data)
import Data.Generics.Uniplate.Operations (universe)
import Data.Set qualified as Set
import Optics (anyOf, folded, foldlOf', to, toListOf, (%), _2)
import Primer.Core (Expr' (..), KindMeta, Type' (..), TypeMeta, typesInExpr)
import Primer.Core.Meta (GVarName, ID, TyConName)
import Primer.Core.Type.Utils (kindIDs)
import Primer.Core.Utils (exprIDs, freeGlobalVars, typeIDs)
import Primer.Def (ASTDef (..), Def (..))
import Primer.TypeDef (ASTTypeDef (..), PrimTypeDef (PrimTypeDef), TypeDef (..), ValCon (..))

-- | Given a 'Def', return its next 'ID'.
--
-- Note: do not rely on the implementation of this function, as it may
-- change in the future.
nextID :: Def -> ID
nextID (DefAST (ASTDef e t)) =
  let eid = foldlOf' exprIDs max minBound e
      tid = foldlOf' typeIDs max minBound t
   in succ $ max eid tid
nextID (DefPrim _) = 0
{-# INLINE nextID #-}

-- | Given a 'TypeDef', return its next 'ID'.
--
-- Note: do not rely on the implementation of this function, as it may
-- change in the future.
nextIDTypeDef :: TypeDef TypeMeta KindMeta -> ID
nextIDTypeDef (TypeDefAST (ASTTypeDef ps vcs _)) =
  let pid = foldlOf' (folded % _2 % kindIDs) max minBound ps
      vcid = foldlOf' (folded % #valConArgs % folded % typeIDs) max minBound vcs
   in succ $ max pid vcid
nextIDTypeDef (TypeDefPrim (PrimTypeDef ps _)) =
  succ $ foldlOf' (folded % _2 % kindIDs) max minBound ps
{-# INLINE nextIDTypeDef #-}

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % #_DefAST % #astDefExpr % to freeGlobalVars)
    (Set.member v)

-- | Is this type (including any of its constructors) in use in the given definitions?
typeInUse :: (Foldable f, Foldable g, Data a', Ord a', Data b') => TyConName -> ASTTypeDef a b -> f (TypeDef a' b') -> g Def -> Bool
typeInUse defName def ts ds =
  anyOf
    (folded % #_TypeDefAST % to tyConsInTypeDef)
    (Set.member defName . Set.map fst)
    ts
    || anyOf
      (folded % #_DefAST % to tyConsInDef)
      (Set.member defName . Set.map fst)
      ds
    || anyOf
      (folded % #_DefAST % to valConsInDef)
      (\s -> any ((`elem` Set.map fst s) . valConName) $ astTypeDefConstructors def)
      ds
  where
    tyConsInExpr =
      Set.unions . map tyConsInType . concatMap (toListOf typesInExpr) . universe
    tyConsInType t =
      Set.fromList [(n, m) | TCon m n <- universe t]
    tyConsInDef d =
      tyConsInExpr (astDefExpr d) `Set.union` tyConsInType (astDefType d)
    tyConsInTypeDef =
      Set.unions . map (Set.unions . map tyConsInType . valConArgs) . astTypeDefConstructors
    valConsInExpr e =
      Set.fromList [(n, m) | Con m n _ <- universe e]
    valConsInDef =
      valConsInExpr . astDefExpr
