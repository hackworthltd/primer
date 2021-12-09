{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (globalPrims) where

import Foreword

import qualified Data.Map as M
import Numeric.Natural (Natural)
import Primer.Core (
  Expr' (App, Con, PrimCon),
  PrimCon (PrimChar),
  PrimFun (..),
  PrimFunError (..),
 )
import Primer.Core.DSL (
  bool_,
  char,
  maybe_,
  nat,
  tapp,
  tcon,
  tfun,
 )
import Primer.Name (Name)

globalPrims :: Map Name PrimFun
globalPrims =
  M.fromList
    [ let name = "toUpper"
       in ( name
          , PrimFun
              { primFunType = tcon "Char" `tfun` tcon "Char"
              , primFunDef = \case
                  [PrimCon _ (PrimChar c)] ->
                    Right $ char $ toUpper c
                  xs -> Left $ PrimFunTypeError name xs
              }
          )
    , let name = "isSpace"
       in ( name
          , PrimFun
              { primFunType = tcon "Char" `tfun` tcon "Bool"
              , primFunDef = \case
                  [PrimCon _ (PrimChar c)] ->
                    Right $ bool_ $ isSpace c
                  xs -> Left $ PrimFunTypeError name xs
              }
          )
    , let name = "hexToNat"
       in ( name
          , PrimFun
              { primFunType = tcon "Char" `tfun` (tcon "Maybe" `tapp` tcon "Nat")
              , primFunDef = \case
                  [PrimCon _ (PrimChar c)] -> do
                    Right $ maybe_ (tcon "Nat") nat $ digitToIntSafe c
                    where
                      digitToIntSafe :: Char -> Maybe Natural
                      digitToIntSafe c' = fromIntegral <$> (guard (isHexDigit c') $> digitToInt c')
                  xs -> Left $ PrimFunTypeError name xs
              }
          )
    , let name = "natToHex"
       in ( name
          , PrimFun
              { primFunType = tcon "Nat" `tfun` (tcon "Maybe" `tapp` tcon "Char")
              , primFunDef = \case
                  [exprToNat -> Just n] ->
                    Right $ maybe_ (tcon "Char") char $ intToDigitSafe n
                    where
                      intToDigitSafe :: Natural -> Maybe Char
                      intToDigitSafe n' = guard (0 <= n && n <= 15) $> intToDigit (fromIntegral n')
                  xs -> Left $ PrimFunTypeError name xs
              }
          )
    , let name = "eqChar"
       in ( name
          , PrimFun
              { primFunType = tcon "Char" `tfun` (tcon "Char" `tfun` tcon "Bool")
              , primFunDef = \case
                  [PrimCon _ (PrimChar c1), PrimCon _ (PrimChar c2)] ->
                    Right $ bool_ $ c1 == c2
                  xs -> Left $ PrimFunTypeError name xs
              }
          )
    ]
  where
    exprToNat = \case
      Con _ "Zero" -> Just 0
      App _ (Con _ "Succ") x -> succ <$> exprToNat x
      _ -> Nothing
