module Primer.Primitives.DSL (pfun) where

import Control.Monad.Fresh (MonadFresh)
import Foreword
import Primer.Core (Expr, ID)
import Primer.Core.DSL (gvar)
import Primer.Primitives (PrimDef, primitiveGVar)

pfun :: MonadFresh ID m => PrimDef -> m Expr
pfun = gvar . primitiveGVar
