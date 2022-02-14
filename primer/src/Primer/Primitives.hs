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
  PrimCon (PrimChar),
  PrimFun (..),
  PrimFunError (..),
  PrimTypeDef (..),
 )
import Primer.Core.DSL (
  bool_,
  char,
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
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()

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
    ]
  where
    sequenceTypes args res = bisequence (sequence args, res)
    exprToNat = \case
      Con _ "Zero" -> Just 0
      App _ (Con _ "Succ") x -> succ <$> exprToNat x
      _ -> Nothing
