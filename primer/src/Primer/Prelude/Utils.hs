module Primer.Prelude.Utils (apps, modName) where

import Control.Monad.Fresh (MonadFresh)
import Data.Foldable (foldl1)
import Primer.Core (Expr, ID, ModuleName, mkSimpleModuleName)
import Primer.Core.DSL (app)

-- | Helper function for functions with multiple arguments
apps :: MonadFresh ID m => m Expr -> [m Expr] -> m Expr
apps f args = foldl1 app (f : args)

modName :: ModuleName
modName = mkSimpleModuleName "Prelude"
