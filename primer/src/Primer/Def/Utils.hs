module Primer.Def.Utils (nextID) where

import Foreword

import Optics (foldlOf')
import Primer.Core (ID)
import Primer.Core.Utils (exprIDs, typeIDs)
import Primer.Def (ASTDef (ASTDef), Def (..))

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
