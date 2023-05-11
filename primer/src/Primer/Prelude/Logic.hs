module Primer.Prelude.Logic (not, notDef, and, andDef, or, orDef, xor, xorDef, implies, impliesDef) where

import Foreword hiding (and, not, or, xor)

import Control.Monad.Fresh (MonadFresh)
import Primer.Builtins qualified as B
import Primer.Core (
  GVarName,
  ID,
  qualifyName,
 )
import Primer.Core.DSL (
  app,
  apps,
  branch,
  case_,
  con0,
  gvar,
  lam,
  lvar,
  tcon,
  tfun,
 )
import Primer.Def (
  ASTDef (ASTDef),
  Def (DefAST),
 )
import Primer.Prelude.Utils (modName)

not :: GVarName
not = qualifyName modName "not"

-- | The function `not :: Bool -> Bool`.
notDef :: MonadFresh ID m => m Def
notDef = do
  type_ <- tcon B.tBool `tfun` tcon B.tBool
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (con0 B.cFalse)
          , branch B.cFalse [] (con0 B.cTrue)
          ]
      )
  pure $ DefAST $ ASTDef term type_

and :: GVarName
and = qualifyName modName "and"

andDef :: MonadFresh ID m => m Def
andDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch
              B.cTrue
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] (con0 B.cTrue)
                      , branch B.cFalse [] (con0 B.cFalse)
                      ]
                  )
              )
          , branch B.cFalse [] (lam "y" $ con0 B.cFalse)
          ]
      )
  pure $ DefAST $ ASTDef term type_

or :: GVarName
or = qualifyName modName "or"

orDef :: MonadFresh ID m => m Def
orDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( case_
          (lvar "x")
          [ branch B.cTrue [] (lam "y" $ con0 B.cTrue)
          , branch
              B.cFalse
              []
              ( lam
                  "y"
                  ( case_
                      (lvar "y")
                      [ branch B.cTrue [] $ con0 B.cTrue
                      , branch B.cFalse [] $ con0 B.cFalse
                      ]
                  )
              )
          ]
      )
  pure $ DefAST $ ASTDef term type_

xor :: GVarName
xor = qualifyName modName "xor"

xorDef :: MonadFresh ID m => m Def
xorDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( apps
              (gvar and)
              [ apps (gvar or) [lvar "x", lvar "y"]
              , app
                  (gvar not)
                  ( apps
                      (gvar and)
                      [lvar "x", lvar "y"]
                  )
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_

implies :: GVarName
implies = qualifyName modName "implies"

impliesDef :: MonadFresh ID m => m Def
impliesDef = do
  type_ <- tcon B.tBool `tfun` (tcon B.tBool `tfun` tcon B.tBool)
  term <-
    lam
      "x"
      ( lam
          "y"
          ( case_
              (lvar "x")
              [ branch B.cTrue [] (case_ (lvar "y") [branch B.cTrue [] $ con0 B.cTrue, branch B.cFalse [] $ con0 B.cFalse])
              , branch B.cFalse [] (case_ (lvar "y") [branch B.cTrue [] $ con0 B.cTrue, branch B.cFalse [] $ con0 B.cTrue])
              ]
          )
      )
  pure $ DefAST $ ASTDef term type_
