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
) where

import Control.Monad.Fresh (MonadFresh)
import Foreword (Applicative (pure), map, ($), (.))
import Primer.Builtins (tBool)
import Primer.Builtins qualified as B
import Primer.Core (GVarName, ID, qualifyName)
import Primer.Core.DSL (app, branch, case_, gvar, int, lam, let_, lvar, tcon, tfun)
import Primer.Def (ASTDef (..), Def (..))
import Primer.Prelude.Utils (apps, modName)
import Primer.Primitives (PrimDef (..), primDefName, primitiveGVar, tInt)

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
              (apps (gvar $ primitiveGVar $ primDefName IntLTE) [lvar "x", lvar "y"])
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
              (apps (gvar $ primitiveGVar $ primDefName IntLTE) [lvar "x", lvar "y"])
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
      (apps (gvar $ primitiveGVar $ primDefName IntMinus) [int 0, lvar "x"])
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
              (apps (gvar $ primitiveGVar $ primDefName IntEq) [lvar "y", int 0])
              [ branch B.cTrue [] (lvar "x")
              , branch
                  B.cFalse
                  []
                  ( apps
                      (gvar gcdHelper)
                      [ lvar "y"
                      , apps
                          (gvar $ primitiveGVar $ primDefName IntRem)
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
                  (gvar $ primitiveGVar $ primDefName IntEq)
                  [apps (gvar $ primitiveGVar $ primDefName IntMul) [lvar "x", lvar "y"], int 0]
              )
              [ branch B.cTrue [] $ int 0
              , branch
                  B.cFalse
                  []
                  ( let_
                      "m"
                      (apps (gvar gcd) [lvar "x", lvar "y"])
                      ( apps
                          (gvar $ primitiveGVar $ primDefName IntQuot)
                          [ app (gvar abs) $
                              apps
                                (gvar $ primitiveGVar $ primDefName IntMul)
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
          (gvar $ primitiveGVar $ primDefName IntEq)
          [int 0, apps (gvar $ primitiveGVar $ primDefName IntRem) [lvar "x", int 2]]
      )
  pure $ DefAST $ ASTDef term type_
