{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  allPrimDefs,
  allPrimTypeDefs,
  tInt,
  tChar,
  primitiveGVar,
) where

import Foreword

import Data.Bitraversable (bisequence)
import qualified Data.Map as M
import Numeric.Natural (Natural)
import Primer.Builtins (
  cJust,
  cNothing,
  cSucc,
  cZero,
  tBool,
  tMaybe,
  tNat,
 )
import Primer.Core (
  Expr' (App, Con, PrimCon),
  ExprAnyFresh (..),
  GVarName,
  PrimCon (..),
  PrimFun (..),
  PrimFunError (..),
  PrimTypeDef (..),
  TyConName,
  qualifyName,
 )
import Primer.Core.DSL (
  aPP,
  app,
  bool_,
  char,
  con,
  int,
  maybe_,
  nat,
  tapp,
  tcon,
 )
import Primer.Name (Name)

tChar :: TyConName
tChar = "Char"

tInt :: TyConName
tInt = "Int"

-- | Construct a reference to a primitive definition. For use in tests.
primitiveGVar :: Name -> GVarName
primitiveGVar = qualifyName

-- | Primitive type definitions.
-- There should be one entry here for each constructor of `PrimCon`.
allPrimTypeDefs :: Map TyConName PrimTypeDef
allPrimTypeDefs =
  M.fromList
    [ let name = tChar
       in ( name
          , PrimTypeDef
            { primTypeDefName = name
            , primTypeDefParameters = []
            , primTypeDefNameHints = ["c"]
            }
          )
    , let name = tInt
       in ( name
          , PrimTypeDef
            { primTypeDefName = name
            , primTypeDefParameters = []
            , primTypeDefNameHints = ["i", "j", "k", "m", "n"]
            }
          )
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()

-- | Primitive term definitions.
-- For each of these, we should have a test that the evaluator produces expected results.
allPrimDefs :: Map GVarName PrimFun
allPrimDefs =
  M.fromList
    [ let name = primitiveGVar "toUpper"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tChar] (tcon tChar)
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] ->
                  Right $ ExprAnyFresh $ char $ toUpper c
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "isSpace"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tChar] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] ->
                  Right $ ExprAnyFresh $ bool_ $ isSpace c
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "hexToNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tChar] $ tcon tMaybe `tapp` tcon tNat
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] -> do
                  Right $ ExprAnyFresh $ maybe_ (tcon tNat) nat $ digitToIntSafe c
                  where
                    digitToIntSafe :: Char -> Maybe Natural
                    digitToIntSafe c' = fromIntegral <$> (guard (isHexDigit c') $> digitToInt c')
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "natToHex"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tNat] $ tcon tMaybe `tapp` tcon tChar
            , primFunDef = \case
                [exprToNat -> Just n] ->
                  Right $ ExprAnyFresh $ maybe_ (tcon tChar) char $ intToDigitSafe n
                  where
                    intToDigitSafe :: Natural -> Maybe Char
                    intToDigitSafe n' = guard (0 <= n && n <= 15) $> intToDigit (fromIntegral n')
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "eqChar"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tChar, tcon tChar] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimChar c1), PrimCon _ (PrimChar c2)] ->
                  Right $ ExprAnyFresh $ bool_ $ c1 == c2
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.+"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tInt)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x + y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.-"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tInt)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x - y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.×"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tInt)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x * y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.quotient"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] $ tcon tMaybe `tapp` tcon tInt
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then con cNothing `aPP` tcon tInt
                        else
                          con cJust `aPP` tcon tInt
                            `app` int (x `div` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.remainder"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] $ tcon tMaybe `tapp` tcon tInt
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then con cNothing `aPP` tcon tInt
                        else
                          con cJust `aPP` tcon tInt
                            `app` int (x `mod` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.quot"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tInt)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then int 0
                        else int (x `div` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.rem"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tInt)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then int x
                        else int (x `mod` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.<"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x < y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.≤"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x <= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.>"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x > y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.≥"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x >= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.="
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x == y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.≠"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt, tcon tInt] (tcon tBool)
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x /= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.toNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tInt] $ tcon tMaybe `tapp` tcon tNat
            , primFunDef = \case
                [PrimCon _ (PrimInt x)] ->
                  Right $
                    ExprAnyFresh $
                      if x < 0
                        then con cNothing `aPP` tcon tNat
                        else
                          con cJust
                            `aPP` tcon tNat `app` nat (fromInteger x)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = primitiveGVar "Int.fromNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon tNat] (tcon tInt)
            , primFunDef = \case
                [exprToNat -> Just n] ->
                  Right $ ExprAnyFresh $ int $ fromIntegral n
                xs -> Left $ PrimFunError name xs
            }
          )
    ]
  where
    sequenceTypes args res = bisequence (sequence args, res)
    exprToNat = \case
      Con _ c | c == cZero -> Just 0
      App _ (Con _ c) x | c == cSucc -> succ <$> exprToNat x
      _ -> Nothing
