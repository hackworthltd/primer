{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  allPrimDefs,
  allPrimTypeDefs,
) where

import Foreword

import Data.Bitraversable (bisequence)
import qualified Data.Map as M
import Numeric.Natural (Natural)
import Primer.Core (
  Expr' (App, Con, PrimCon),
  ExprAnyFresh (..),
  PrimCon (..),
  PrimFun (..),
  PrimFunError (..),
  PrimTypeDef (..),
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

-- | Primitive type definitions.
-- There should be one entry here for each constructor of `PrimCon`.
allPrimTypeDefs :: Map Name PrimTypeDef
allPrimTypeDefs =
  M.fromList
    [ let name = "Char"
       in ( name
          , PrimTypeDef
            { primTypeDefName = "Char"
            , primTypeDefParameters = []
            , primTypeDefNameHints = ["c"]
            }
          )
    , let name = "Int"
       in ( name
          , PrimTypeDef
            { primTypeDefName = "Int"
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
allPrimDefs :: Map Name PrimFun
allPrimDefs =
  M.fromList
    [ let name = "toUpper"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Char"] $ tcon "Char"
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] ->
                  Right $ ExprAnyFresh $ char $ toUpper c
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "isSpace"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Char"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] ->
                  Right $ ExprAnyFresh $ bool_ $ isSpace c
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "hexToNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Char"] $ tcon "Maybe" `tapp` tcon "Nat"
            , primFunDef = \case
                [PrimCon _ (PrimChar c)] -> do
                  Right $ ExprAnyFresh $ maybe_ (tcon "Nat") nat $ digitToIntSafe c
                  where
                    digitToIntSafe :: Char -> Maybe Natural
                    digitToIntSafe c' = fromIntegral <$> (guard (isHexDigit c') $> digitToInt c')
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "natToHex"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Nat"] $ tcon "Maybe" `tapp` tcon "Char"
            , primFunDef = \case
                [exprToNat -> Just n] ->
                  Right $ ExprAnyFresh $ maybe_ (tcon "Char") char $ intToDigitSafe n
                  where
                    intToDigitSafe :: Natural -> Maybe Char
                    intToDigitSafe n' = guard (0 <= n && n <= 15) $> intToDigit (fromIntegral n')
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "eqChar"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Char", tcon "Char"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimChar c1), PrimCon _ (PrimChar c2)] ->
                  Right $ ExprAnyFresh $ bool_ $ c1 == c2
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.+"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Int"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x + y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.-"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Int"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x - y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.×"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Int"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ int $ x * y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.quotient"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Maybe" `tapp` tcon "Int"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then con "Nothing" `aPP` tcon "Int"
                        else
                          con "Just" `aPP` tcon "Int"
                            `app` int (x `div` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.remainder"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Maybe" `tapp` tcon "Int"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $
                    ExprAnyFresh $
                      if y == 0
                        then con "Nothing" `aPP` tcon "Int"
                        else
                          con "Just" `aPP` tcon "Int"
                            `app` int (x `mod` y)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.quot"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Int"
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
    , let name = "Int.rem"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Int"
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
    , let name = "Int.<"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x < y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.≤"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x <= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.>"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x > y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.≥"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x >= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.="
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x == y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.≠"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int", tcon "Int"] $ tcon "Bool"
            , primFunDef = \case
                [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
                  Right $ ExprAnyFresh $ bool_ $ x /= y
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.toNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Int"] $ tcon "Maybe" `tapp` tcon "Nat"
            , primFunDef = \case
                [PrimCon _ (PrimInt x)] ->
                  Right $
                    ExprAnyFresh $
                      if x < 0
                        then con "Nothing" `aPP` tcon "Nat"
                        else
                          con "Just"
                            `aPP` tcon "Nat" `app` nat (fromInteger x)
                xs -> Left $ PrimFunError name xs
            }
          )
    , let name = "Int.fromNat"
       in ( name
          , PrimFun
            { primFunTypes = sequenceTypes [tcon "Nat"] $ tcon "Int"
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
      Con _ "Zero" -> Just 0
      App _ (Con _ "Succ") x -> succ <$> exprToNat x
      _ -> Nothing
