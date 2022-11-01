{-# LANGUAGE OverloadedLabels #-}

module Primer.Def.Utils (nextID, globalInUse) where

import Foreword

import Data.Set qualified as Set
import Optics (anyOf, folded, foldlOf', to, (%))
import Primer.Core.Meta (GVarName, ID)
import Primer.Core.Utils (exprIDs, freeGlobalVars, typeIDs)
import Primer.Def (ASTDef (..), Def (..))

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

globalInUse :: Foldable f => GVarName -> f Def -> Bool
globalInUse v =
  anyOf
    (folded % #_DefAST % #astDefExpr % to freeGlobalVars)
    (Set.member v)
