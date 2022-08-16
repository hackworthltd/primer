module Primer.Prelude.Integer (max, maxDef) where

import Control.Monad.Fresh (MonadFresh)
import Foreword (Applicative (pure), ($))
import Primer.Builtins qualified as B
import Primer.Core (GVarName, ID, qualifyName)
import Primer.Core.DSL (branch, case_, gvar, lam, lvar, tcon, tfun)
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Utils (apps, modName)
import Primer.Primitives (PrimDef (..), primDefName, primitiveGVar, tInt)

max :: GVarName
max = qualifyName modName "max"

maxDef :: MonadFresh ID m => m Def
maxDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (apps (gvar $ primitiveGVar $ primDefName IntLTE) [lvar "x", lvar "y"])
              [ branch B.cTrue [] $ lvar "y"
              , branch B.cFalse [] $ lvar "x"
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_
