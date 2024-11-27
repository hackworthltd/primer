module Primer.Prelude.Integer (
  max,
  maxDef,
  min,
  minDef,
  negate,
  negateDef,
  abs,
  absDef,
  gcdHelper,
  gcdHelperDef,
  gcd,
  gcdDef,
  lcm,
  lcmDef,
  even,
  evenDef,
  odd,
  oddDef,
  sum,
  sumDef,
  product,
  productDef,
) where

import Control.Monad.Fresh (MonadFresh)
import Foreword (
  Applicative (pure),
  Either (Left, Right),
  map,
  ($),
  (.),
 )
import Primer.Builtins (tBool)
import Primer.Builtins qualified as B
import Primer.Builtins.DSL (
  listOf,
 )
import Primer.Core (GVarName, ID, qualifyName)
import Primer.Core.DSL (
  app,
  apps,
  apps',
  branch,
  case_,
  gvar,
  int,
  lam,
  let_,
  lvar,
  tcon,
  tfun,
 )
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Logic (not)
import Primer.Prelude.Polymorphism (foldr)
import Primer.Prelude.Utils (modName)
import Primer.Primitives (PrimDef (..), tInt)
import Primer.Primitives.DSL (pfun)

min :: GVarName
min = qualifyName modName "min"

minDef :: MonadFresh ID m => m Def
minDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (apps (pfun IntLTE) [lvar "x", lvar "y"])
              [ branch B.cTrue [] $ lvar "x"
              , branch B.cFalse [] $ lvar "y"
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

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
              (apps (pfun IntLTE) [lvar "x", lvar "y"])
              [ branch B.cTrue [] $ lvar "y"
              , branch B.cFalse [] $ lvar "x"
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

negate :: GVarName
negate = qualifyName modName "negate"

negateDef :: MonadFresh ID m => m Def
negateDef = do
  type_ <- tcon tInt `tfun` tcon tInt
  term <-
    lam
      "x"
      (apps (pfun IntMinus) [int 0, lvar "x"])
  pure $ DefAST $ ASTDef term type_

abs :: GVarName
abs = qualifyName modName "abs"

absDef :: MonadFresh ID m => m Def
absDef = do
  type_ <- tcon tInt `tfun` tcon tInt
  term <-
    lam
      "x"
      (apps (gvar max) [lvar "x", app (gvar negate) (lvar "x")])
  pure $ DefAST $ ASTDef term type_

gcd :: GVarName
gcd = qualifyName modName "gcd"

gcdDef :: MonadFresh ID m => m Def
gcdDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( apps
              (gvar gcdHelper)
              $ map (app (gvar abs) . lvar) ["x", "y"]
          )
      )
  pure $ DefAST $ ASTDef term type_

gcdHelper :: GVarName
gcdHelper = qualifyName modName "gcdHelper"

gcdHelperDef :: MonadFresh ID m => m Def
gcdHelperDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (apps (pfun IntEq) [lvar "y", int 0])
              [ branch B.cTrue [] (lvar "x")
              , branch
                  B.cFalse
                  []
                  ( apps
                      (gvar gcdHelper)
                      [ lvar "y"
                      , apps
                          (pfun IntRem)
                          [lvar "x", lvar "y"]
                      ]
                  )
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

lcm :: GVarName
lcm = qualifyName modName "lcm"

lcmDef :: MonadFresh ID m => m Def
lcmDef = do
  type_ <- tcon tInt `tfun` (tcon tInt `tfun` tcon tInt)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              ( apps
                  (pfun IntEq)
                  [apps (pfun IntMul) [lvar "x", lvar "y"], int 0]
              )
              [ branch B.cTrue [] $ int 0
              , branch
                  B.cFalse
                  []
                  ( let_
                      "m"
                      (apps (gvar gcd) [lvar "x", lvar "y"])
                      ( apps
                          (pfun IntQuot)
                          [ app (gvar abs) $
                              apps
                                (pfun IntMul)
                                [lvar "x", lvar "y"]
                          , lvar "m"
                          ]
                      )
                  )
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

even :: GVarName
even = qualifyName modName "even"

evenDef :: MonadFresh ID m => m Def
evenDef = do
  type_ <- tcon tInt `tfun` tcon tBool
  term <-
    lam
      "x"
      ( apps
          (pfun IntEq)
          [int 0, apps (pfun IntRem) [lvar "x", int 2]]
      )
  pure $ DefAST $ ASTDef term type_

odd :: GVarName
odd = qualifyName modName "odd"

oddDef :: MonadFresh ID m => m Def
oddDef = do
  type_ <- tcon tInt `tfun` tcon tBool
  term <-
    lam
      "x"
      (app (gvar not) (app (gvar even) (lvar "x")))
  pure $ DefAST $ ASTDef term type_

sum :: GVarName
sum = qualifyName modName "sum"

sumDef :: MonadFresh ID m => m Def
sumDef = do
  type_ <- listOf (tcon tInt) `tfun` tcon tInt
  term <- lam "ns" $ apps' (gvar foldr) [Right $ tcon tInt, Right $ tcon tInt, Left $ pfun IntAdd, Left $ int 0, Left $ lvar "ns"]
  pure $ DefAST $ ASTDef term type_

product :: GVarName
product = qualifyName modName "product"

productDef :: MonadFresh ID m => m Def
productDef = do
  type_ <- listOf (tcon tInt) `tfun` tcon tInt
  term <- lam "ns" $ apps' (gvar foldr) [Right $ tcon tInt, Right $ tcon tInt, Left $ pfun IntMul, Left $ int 1, Left $ lvar "ns"]
  pure $ DefAST $ ASTDef term type_
