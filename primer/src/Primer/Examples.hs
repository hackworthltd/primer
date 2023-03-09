-- |
-- Module      : Primer.Examples
-- Description : Example Primer types, terms, and programs.
-- Copyright   : (c) 2023, Hackworth Ltd
-- License     : AGPL 3.0 or later
-- Stability   : experimental
-- Portability : portable
--
-- Examples of Primer types, terms, and programs, written in Primer's
-- Haskell DSL.
--
-- Because it contains many names that conflict with common Haskell
-- names, this module is intended to be imported qualified.
module Primer.Examples (
  -- * Functions as top-level definitions.

  -- | In Primer, top-level definitions names must be qualified by
  -- their module name. These examples take a 'ModuleName' so that the
  -- 'Def's they return can be resued in multiple contexts.
  not,
  map,
  map',
  even,
  odd,
  comprehensive,
  comprehensiveWellTyped,

  -- * Example modules.
  mapModule,
  evenOddModule,

  -- * Toy example programs, plus their next 'ID' and 'NameCounter'.
  even3Prog,
  mapOddProg,
  mapOddPrimProg,
  badEven3Prog,
  badEvenProg,
  badMapProg,

  -- * Toy 'App's.
  even3App,
  mapOddApp,
) where

import Foreword hiding (
  even,
  map,
  not,
  odd,
 )

import Control.Monad.Fresh (MonadFresh)
import Data.Map.Strict qualified as Map
import Primer.App (
  App,
  Prog (..),
  defaultProg,
  mkApp,
 )
import Primer.Builtins qualified as B
import Primer.Builtins.DSL (
  list_,
  nat,
 )
import Primer.Core (
  GVarName,
  ID,
  Kind (KType),
  ModuleName (unModuleName),
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  aPP,
  ann,
  app,
  branch,
  case_,
  con,
  conSat,
  create,
  emptyHole,
  gvar,
  gvar',
  hole,
  int,
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
  tlet,
  tvar,
 )
import Primer.Def (
  ASTDef (ASTDef),
  Def (DefAST),
 )
import Primer.Module (
  Module (..),
  builtinModule,
  primitiveModule,
 )
import Primer.Name (
  NameCounter,
 )
import Primer.Primitives qualified as P
import Primer.Primitives.DSL (pfun)

-- | The function `not :: Bool -> Bool`.
not :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
not modName =
  let this = qualifyName modName "not"
   in do
        type_ <- tcon B.tBool `tfun` tcon B.tBool
        term <-
          lam
            "x"
            ( case_
                (lvar "x")
                [ branch B.cTrue [] (conSat B.cFalse [] [])
                , branch B.cFalse [] (conSat B.cTrue [] [])
                ]
            )
        pure (this, DefAST $ ASTDef term type_)

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
                        conSat B.cNil [tvar "b"] []
                    , branch B.cCons [("y", Nothing), ("ys", Nothing)] $
                        conSat B.cCons [tvar "b"] [lvar "f" `app` lvar "y", gvar this `aPP` tvar "a" `aPP` tvar "b" `app` lvar "f" `app` lvar "ys"]
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
            [ branch B.cNil [] $ conSat B.cNil [tvar "b"] []
            , branch B.cCons [("y", Nothing), ("ys", Nothing)] $
                conSat B.cCons [tvar "b"] [lvar "f" `app` lvar "y", lvar "go" `app` lvar "ys"]
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
        [ branch B.cZero [] $ conSat B.cFalse [] []
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
        [ branch B.cZero [] $ conSat B.cTrue [] []
        , branch B.cSucc [("n", Nothing)] $ gvar (qualifyName modName "odd") `app` lvar "n"
        ]
  pure (qualifyName modName "even", DefAST $ ASTDef term type_)

-- | A comprehensive 'Def' containing most of the non-primitive
-- built-in constructs in Primer.
--
-- Note that this 'Def' is nonsensical (and not well-typed)
-- and is provided only for language coverage.
comprehensive :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
comprehensive = comprehensive' False

-- | A comprehensive 'Def' containing most of the non-primitive
-- built-in constructs in Primer.
--
-- Note that this 'Def' is nonsensical (but well-typed)
-- and is provided only for language coverage.
--
-- It is similar to 'comprehensive', but with at few ill-typed subtrees
-- replaced with less comprehensive well-typed alternatives.
comprehensiveWellTyped :: MonadFresh ID m => ModuleName -> m (GVarName, Def)
comprehensiveWellTyped = comprehensive' True

-- | A helper for 'comprehensive' and 'comprehensiveWellTyped'
-- which optionally disables a few constructs which are not typeable
-- (namely, an unbound name and letType)
comprehensive' :: MonadFresh ID m => Bool -> ModuleName -> m (GVarName, Def)
comprehensive' typeable modName = do
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
      (conSat B.cTrue [] [])
      ( letrec
          "y"
          ( app
              ( hole
                  (conSat B.cJust [tEmptyHole] [emptyHole])
              )
              ( if typeable then emptyHole else hole $ gvar' (unModuleName modName) "unboundName"
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
                              ( if typeable
                                  then
                                    (lAM "b" $ lam "x" $ conSat B.cLeft [tcon B.tBool, tvar"b"] [lvar"x"])
                                    `ann`
                                    (tforall "b" KType $ tcon B.tBool
                                                  `tfun` (tcon B.tEither `tapp` tcon B.tBool `tapp` tvar "b"))
                                  else
                                    letType
                                      "b"
                                      (tcon B.tBool)
                                      ( conSat B.cLeft [tlet "c" (tvar "b") $ tvar "c"] []
                                      )
                              )
                              (tvar "β")
                          )
                          ( case_
                              (lvar "i")
                              [ branch
                                  B.cZero
                                  []
                                  (conSat B.cFalse [] [])
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

-- | Given a 'ModuleName', return a module with the given name
-- containing @map@ and @map'@, plus the next 'ID' to be used for
-- editing the contents of the module.
mapModule :: ModuleName -> (Module, ID)
mapModule modName =
  let (defs, nextID) = create $ do
        (_, mapDef) <- map modName
        (_, map'Def) <- map' modName
        pure [("map", mapDef), ("map'", map'Def)]
   in ( Module modName mempty $ Map.fromList defs
      , nextID
      )

-- | Given a 'ModuleName', return a module with the given name
-- containing @even@ and @odd@, plus the next 'ID' to be used for
-- editing the contents of the module.
evenOddModule :: ModuleName -> (Module, ID)
evenOddModule modName =
  let (defs, nextID) = create $ do
        (_, evenDef) <- even modName
        (_, oddDef) <- odd modName
        pure [("even", evenDef), ("odd", oddDef)]
   in ( Module modName mempty $ Map.fromList defs
      , nextID
      )

-- | A program whose @main@ asks whether 3 is even.
even3Prog :: (Prog, ID, NameCounter)
even3Prog =
  let modName = mkSimpleModuleName "Even3"
      (defs, nextID) = create $ do
        (_, evenDef) <- even modName
        (_, oddDef) <- odd modName
        even3Def <- do
          type_ <- tcon B.tBool
          term <- gvar (qualifyName modName "even") `app` conSat B.cSucc [] [conSat B.cSucc [] [conSat B.cSucc [] [con B.cZero]]]
          pure $ DefAST $ ASTDef term type_
        let globs = [("even", evenDef), ("odd", oddDef), ("even 3?", even3Def)]
        pure globs
   in ( defaultProg
          { progImports = [builtinModule]
          , progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | A program whose @main@ 'map's 'odd' over a list of 'B.tNat'.
mapOddProg :: Int -> (Prog, ID, NameCounter)
mapOddProg len =
  let modName = mkSimpleModuleName "MapOdd"
      (defs, nextID) = create $ do
        (_, evenDef) <- even modName
        (oddName, oddDef) <- odd modName
        (mapName, mapDef) <- map modName
        mapOddDef <- do
          type_ <- tcon B.tList `tapp` tcon B.tBool
          let lst = list_ B.tNat $ take len $ nat <$> [0 ..]
          term <- gvar mapName `aPP` tcon B.tNat `aPP` tcon B.tBool `app` gvar oddName `app` lst
          pure $ DefAST $ ASTDef term type_
        let globs = [("even", evenDef), ("odd", oddDef), ("map", mapDef), ("mapOdd", mapOddDef)]
        pure globs
   in ( defaultProg
          { progImports = [builtinModule]
          , progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | A program whose @main@ 'map's 'odd' over a list of 'P.tInt'.
-- This is the same as 'mapOddProg', except it works over primitive
-- integers, rather than inductively-defined naturals.
mapOddPrimProg :: Int -> (Prog, ID, NameCounter)
mapOddPrimProg len =
  let modName = mkSimpleModuleName "MapOdd"
      (defs, nextID) = create $ do
        let oddName = qualifyName modName "odd"
        oddDef <- do
          type_ <- tcon P.tInt `tfun` tcon B.tBool
          term <-
            lam "x" $
              case_
                (pfun P.IntRemainder `app` lvar "x" `app` int 2)
                [ branch B.cNothing [] $ con B.cTrue -- this should be impossible (since denominator is obviously non-zero)
                , branch B.cJust [("r", Nothing)] $ pfun P.IntEq `app` lvar "r" `app` int 1
                ]
          pure $ DefAST $ ASTDef term type_
        (mapName, mapDef) <- map modName
        mapOddDef <- do
          type_ <- tcon B.tList `tapp` tcon B.tBool
          let lst = list_ P.tInt $ take len $ int <$> [0 ..]
          term <- gvar mapName `aPP` tcon P.tInt `aPP` tcon B.tBool `app` gvar oddName `app` lst
          pure $ DefAST $ ASTDef term type_
        let globs = [("odd", oddDef), ("map", mapDef), ("mapOdd", mapOddDef)]
        pure globs
   in ( defaultProg
          { progImports = [builtinModule, primitiveModule]
          , progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | A "bad" version of 'even3Prog' where the type of @even 3?@ is
-- specified as @Nat@.
badEven3Prog :: (Prog, ID, NameCounter)
badEven3Prog =
  let modName = mkSimpleModuleName "Even3"
      (defs, nextID) = create $ do
        (_, evenDef) <- even modName
        (_, oddDef) <- odd modName
        even3Def <- do
          type_ <- tcon B.tNat
          term <- gvar (qualifyName modName "even") `app` conSat B.cSucc [] [conSat B.cSucc [] [conSat B.cSucc [] [con B.cZero]]]
          pure $ DefAST $ ASTDef term type_
        let globs = [("even", evenDef), ("odd", oddDef), ("even 3?", even3Def)]
        pure globs
   in ( defaultProg
          { progImports = [builtinModule]
          , progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | A "bad" program that doesn't typecheck because it includes the
-- definition of 'even', but doesn't include 'odd' (they are mutually
-- recursive).
badEvenProg :: (Prog, ID, NameCounter)
badEvenProg =
  let modName = mkSimpleModuleName "BadEven"
      (defs, nextID) = create $ do
        (_, evenDef) <- even modName
        let globs = [("even", evenDef)]
        pure globs
   in ( defaultProg
          { progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | A "bad" program that doesn't typecheck because it defines 'map'
-- but doesn't import 'B.builtinModule'.
badMapProg :: (Prog, ID, NameCounter)
badMapProg =
  let modName = mkSimpleModuleName "BadMap"
      (defs, nextID) = create $ do
        (_, mapDef) <- map modName
        let globs = [("map", mapDef)]
        pure globs
   in ( defaultProg
          { progModules =
              [ Module
                  { moduleName = modName
                  , moduleTypes = mempty
                  , moduleDefs = Map.fromList defs
                  }
              ]
          }
      , nextID
      , toEnum 0
      )

-- | An 'App' containing 'even3Prog'.
even3App :: App
even3App =
  let (p, id_, nc) = even3Prog
   in mkApp id_ nc p

-- | An 'App' containing 'mapOddProg'.
mapOddApp :: App
mapOddApp =
  let (p, id_, nc) = mapOddProg 4
   in mkApp id_ nc p
