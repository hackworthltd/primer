module Primer.Prelude.Polymorphism (
  id,
  idDef,
  const,
  constDef,
  map,
  mapDef,
  foldr,
  foldrDef,
  sum,
  sumDef,
  product,
  productDef,
) where

import Foreword hiding (const, foldr, map, product, sum)

import Control.Monad.Fresh (MonadFresh)
import Primer.Builtins (cCons, cNil)
import Primer.Builtins qualified as B
import Primer.Core (GVarName, ID, Kind (KType), Type, qualifyName)
import Primer.Core.DSL (aPP, app, apps, apps', branch, case_, con, gvar, int, lAM, lam, lvar, tapp, tcon, tforall, tfun, tvar)
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Utils (modName)
import Primer.Primitives (PrimDef (..), tInt)
import Primer.Primitives.DSL (pfun)

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

foldr :: GVarName
foldr = qualifyName modName "foldr"

foldrDef :: MonadFresh ID m => m Def
foldrDef = do
  type_ <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` (tvar "b" `tfun` tvar "b")) `tfun` (tvar "b" `tfun` (listOf (tvar "a") `tfun` tvar "b"))
  term <-
    lAM "a" $
      lAM "b" $
        lam "f" $
          lam "y" $
            lam "xs" $
              case_
                (lvar "xs")
                [ branch cNil [] (lvar "y")
                , branch
                    cCons
                    [("x'", Nothing), ("xs'", Nothing)]
                    ( let foldxs' = apps' (gvar foldr) [Right $ tvar "a", Right $ tvar "b", Left $ lvar "f", Left $ lvar "y", Left $ lvar "xs'"]
                       in apps (lvar "f") [lvar "x'", foldxs']
                    )
                ]
  pure $ DefAST $ ASTDef term type_

sum :: GVarName
sum = qualifyName modName "sum"

sumDef :: MonadFresh ID m => m Def
sumDef = do
  type_ <- listOf (tcon tInt) `tfun` tcon tInt
  term <- lam "ns" $ apps' (gvar foldr) [Right $ tcon tInt, Right $ tcon tInt, Left $ pfun IntAdd, Left $ int 0, Left $ lvar "ns"]
  pure $ DefAST $ ASTDef term type_

listOf :: MonadFresh ID m => m Type -> m Type
listOf = tapp (tcon B.tList)

product :: GVarName
product = qualifyName modName "product"

productDef :: MonadFresh ID m => m Def
productDef = do
  type_ <- listOf (tcon tInt) `tfun` tcon tInt
  term <- lam "ns" $ apps' (gvar foldr) [Right $ tcon tInt, Right $ tcon tInt, Left $ pfun IntMul, Left $ int 1, Left $ lvar "ns"]
  pure $ DefAST $ ASTDef term type_
