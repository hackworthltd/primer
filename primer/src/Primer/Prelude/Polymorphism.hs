module Primer.Prelude.Polymorphism (
  id,
  idDef,
  const,
  constDef,
) where

import Foreword hiding (const)

import Control.Monad.Fresh (MonadFresh)
import Primer.Core (GVarName, ID, Kind (KType), qualifyName)
import Primer.Core.DSL (lAM, lam, lvar, tforall, tfun, tvar)
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Utils (modName)

id :: GVarName
id = qualifyName modName "id"

idDef :: MonadFresh ID m => m Def
idDef = do
  type_ <- tforall "a" KType $ tvar "a" `tfun` tvar "a"
  term <- lAM "a" $ lam "x" (lvar "x")
  pure $ DefAST $ ASTDef term type_

const :: GVarName
const = qualifyName modName "const"

constDef :: MonadFresh ID m => m Def
constDef = do
  type_ <- tforall "a" KType $ tvar "a" `tfun` tforall "b" KType (tvar "b" `tfun` tvar "a")
  term <- lAM "a" $ lam "x" $ lAM "b" $ lam "y" (lvar "x")
  pure $ DefAST $ ASTDef term type_
