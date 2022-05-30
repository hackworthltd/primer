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
  ID,
  Kind (KType),
  ModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  aPP,
  app,
  branch,
  case_,
  con,
  gvar,
  lAM,
  lam,
  letrec,
  lvar,
  tapp,
  tcon,
  tforall,
  tfun,
  tvar,
 )

-- | The polymorphic function @map@ (over @List a@ as defined by
-- 'listDef').
map :: MonadFresh ID m => ModuleName -> m Def
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
        pure $ DefAST $ ASTDef this term type_

-- | The polymorphic function @map@ (over @List a@ as defined by
-- 'listDef'), implemented using a worker.
map' :: MonadFresh ID m => ModuleName -> m Def
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
  pure $ DefAST $ ASTDef (qualifyName modName "map'") term type_

-- | The function @odd@, defined over the inductive natural number
-- type @Natural@ as defined by 'natDef'.
--
-- Note that this function is mutually recursive on @even@.
odd :: MonadFresh ID m => ModuleName -> m Def
odd modName = do
  type_ <- tcon B.tNat `tfun` tcon B.tBool
  term <-
    lam "x" $
      case_
        (lvar "x")
        [ branch B.cZero [] $ con B.cFalse
        , branch B.cSucc [("n", Nothing)] $ gvar (qualifyName modName "even") `app` lvar "n"
        ]
  pure $ DefAST $ ASTDef (qualifyName modName "odd") term type_

-- | The function @even@, defined over the inductive natural number
-- type @Natural@ as defined by 'natDef'.
--
-- Note that this function is mutually recursive on @odd@.
even :: MonadFresh ID m => ModuleName -> m Def
even modName = do
  type_ <- tcon B.tNat `tfun` tcon B.tBool
  term <-
    lam "x" $
      case_
        (lvar "x")
        [ branch B.cZero [] $ con B.cTrue
        , branch B.cSucc [("n", Nothing)] $ gvar (qualifyName modName "odd") `app` lvar "n"
        ]
  pure $ DefAST $ ASTDef (qualifyName modName "even") term type_
