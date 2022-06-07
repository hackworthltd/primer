-- |
--Module      : Primer.Examples
--Description : Example Primer types, terms, and programs.
--Copyright   : (c) 2022, Hackworth Ltd
--License     : AGPL 3.0 or later
--Stability   : experimental
--Portability : portable
--
--Examples of Primer types, terms, and programs, written in Primer's
--Haskell DSL.
--
--Because it contains many names that conflict with common Haskell
--names, this module is intended to be imported qualified.
module Primer.Examples (
  -- * Functions as top-level definitions.

  -- | In Primer, top-level definitions names must be qualified by
  -- their module name. These examples take a 'ModuleName' so that the
  -- 'Def's they return can be resued in multiple contexts.
  map,
  map',
  even,
  odd,
  comprehensive,
) where

import Foreword hiding (
  even,
  map,
  odd,
 )

import Control.Monad.Fresh (MonadFresh)
import qualified Primer.Builtins as B
import Primer.Core (
  ASTDef (ASTDef),
  Def (DefAST),
  GVarName,
  ID,
  Kind (KType),
  ModuleName (unModuleName),
  qualifyName,
 )
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  emptyHole,
  gvar,
  gvar',
  hole,
  lAM,
  lam,
  letType,
  let_,
  letrec,
  lvar,
  tEmptyHole,
  tapp,
  tcon,
  tforall,
  tfun,
  thole,
  tvar,
 )

-- | The polymorphic function @map@ (over @List a@ as defined by
-- 'listDef').
map :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
map modName =
  let this = qualifyName modName "map"
   in do
        type_ <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` tvar "b") `tfun` ((tcon B.tList `tapp` tvar "a") `tfun` (tcon B.tList `tapp` tvar "b"))
        term <-
          lAM "a" $
            lAM "b" $
              lam "f" $
                lam "xs" $
                  case_
                    (lvar "xs")
                    [ branch B.cNil [] $
                        con B.cNil `aPP` tvar "b"
                    , branch B.cCons [("y", Nothing), ("ys", Nothing)] $
                        con B.cCons `aPP` tvar "b" `app` (lvar "f" `app` lvar "y") `app` (gvar this `aPP` tvar "a" `aPP` tvar "b" `app` lvar "f" `app` lvar "ys")
                    ]
        pure (this, DefAST $ ASTDef term type_)

-- | The polymorphic function @map@ (over @List a@ as defined by
-- 'listDef'), implemented using a worker.
map' :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
map' modName = do
  type_ <- tforall "a" KType $ tforall "b" KType $ (tvar "a" `tfun` tvar "b") `tfun` ((tcon B.tList `tapp` tvar "a") `tfun` (tcon B.tList `tapp` tvar "b"))
  let worker =
        lam "xs" $
          case_
            (lvar "xs")
            [ branch B.cNil [] $ con B.cNil `aPP` tvar "b"
            , branch B.cCons [("y", Nothing), ("ys", Nothing)] $
                con B.cCons `aPP` tvar "b" `app` (lvar "f" `app` lvar "y") `app` (lvar "go" `app` lvar "ys")
            ]
  term <-
    lAM "a" $
      lAM "b" $
        lam "f" $
          letrec "go" worker ((tcon B.tList `tapp` tvar "a") `tfun` (tcon B.tList `tapp` tvar "b")) $
            lvar "go"
  pure (qualifyName modName "map", DefAST $ ASTDef term type_)

-- | The function @odd@, defined over the inductive natural number
-- type @Natural@ as defined by 'natDef'.
--
-- Note that this function is mutually recursive on @even@.
odd :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
odd modName = do
  type_ <- tcon B.tNat `tfun` tcon B.tBool
  term <-
    lam "x" $
      case_
        (lvar "x")
        [ branch B.cZero [] $ con B.cFalse
        , branch B.cSucc [("n", Nothing)] $ gvar (qualifyName modName "even") `app` lvar "n"
        ]
  pure (qualifyName modName "odd", DefAST $ ASTDef term type_)

-- | The function @even@, defined over the inductive natural number
-- type @Natural@ as defined by 'natDef'.
--
-- Note that this function is mutually recursive on @odd@.
even :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
even modName = do
  type_ <- tcon B.tNat `tfun` tcon B.tBool
  term <-
    lam "x" $
      case_
        (lvar "x")
        [ branch B.cZero [] $ con B.cTrue
        , branch B.cSucc [("n", Nothing)] $ gvar (qualifyName modName "odd") `app` lvar "n"
        ]
  pure (qualifyName modName "even", DefAST $ ASTDef term type_)

-- | A comprehensive 'Def' containing most of the non-primitive
-- built-in constructs in Primer.
--
-- Note that this 'Def' is nonsensical and is provided only for
-- language coverage.
comprehensive :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
comprehensive modName = do
  type_ <-
    tfun
      (tcon B.tNat)
      ( tforall
          "a"
          KType
          ( tapp
              ( thole
                  ( tapp
                      (tcon B.tList)
                      tEmptyHole
                  )
              )
              (tvar "a")
          )
      )
  term <-
    let_
      "x"
      (con B.cTrue)
      ( letrec
          "y"
          ( app
              ( hole
                  (con B.cJust)
              )
              ( hole
                  (gvar' (unModuleName modName) "unboundName")
              )
          )
          ( thole
              (tcon B.tMaybe)
          )
          ( ann
              ( lam
                  "i"
                  ( lAM
                      "β"
                      ( app
                          ( aPP
                              ( letType
                                  "b"
                                  (tcon B.tBool)
                                  ( aPP
                                      (con B.cLeft)
                                      (tvar "b")
                                  )
                              )
                              (tvar "β")
                          )
                          ( case_
                              (lvar "i")
                              [ branch
                                  B.cZero
                                  []
                                  (con B.cFalse)
                              , branch
                                  B.cSucc
                                  [
                                    ( "n"
                                    , Nothing
                                    )
                                  ]
                                  ( app
                                      ( app
                                          emptyHole
                                          (lvar "x")
                                      )
                                      (lvar "y")
                                  )
                              ]
                          )
                      )
                  )
              )
              ( tfun
                  (tcon B.tNat)
                  ( tforall
                      "α"
                      KType
                      ( tapp
                          ( tapp
                              (tcon B.tEither)
                              (tcon B.tBool)
                          )
                          (tvar "α")
                      )
                  )
              )
          )
      )
  pure (qualifyName modName "comprehensive", DefAST $ ASTDef term type_)
