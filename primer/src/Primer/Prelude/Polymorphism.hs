module Primer.Prelude.Polymorphism (
  id,
  idDef,
  const,
  constDef,
  map,
  mapDef,
) where

import Foreword hiding (const, map)

import Control.Monad.Fresh (MonadFresh)
import Primer.Builtins (cCons, cNil)
import Primer.Builtins.DSL (
  listOf,
 )
import Primer.Core (GVarName, ID, Kind (KType), qualifyName)
import Primer.Core.DSL (
  aPP,
  app,
  apps,
  apps',
  branch,
  case_,
  con,
  gvar,
  lAM,
  lam,
  lvar,
  tforall,
  tfun,
  tvar,
 )
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

map :: GVarName
map = qualifyName modName "map"

mapDef :: MonadFresh ID m => m Def
mapDef = do
  type_ <-
    tforall "a" KType $
      tforall "b" KType $
        (tvar "a" `tfun` tvar "b")
          `tfun` (listOf (tvar "a") `tfun` listOf (tvar "b"))
  term <-
    lAM "a" $
      lAM "b" $
        lam "f" $
          lam "xs" $
            case_
              (lvar "xs")
              [ branch cNil [] (con cNil `aPP` tvar "b")
              , branch cCons [("y", Nothing), ("ys", Nothing)] $
                  let fy = app (lvar "f") (lvar "y")
                      fys = apps' (gvar map) [Right $ tvar "a", Right $ tvar "b", Left $ lvar "f", Left $ lvar "ys"]
                   in apps (con cCons `aPP` tvar "b") [fy, fys]
              ]
  pure $ DefAST $ ASTDef term type_
