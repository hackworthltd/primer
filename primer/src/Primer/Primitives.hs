{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  PrimDef (..),
  allPrimTypeDefs,
  tInt,
  tChar,
  tDouble,
  primitive,
  primitiveGVar,
  primConName,
  primDefName,
  primDefType,
  primFunDef,
  PrimFunError (..),
  primitiveModuleName,
) where

import Foreword hiding (rotate)

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Data)
import Data.Map qualified as M
import Numeric.Natural (Natural)
import Primer.Builtins (
  cSucc,
  cZero,
  tBool,
  tMaybe,
  tNat,
 )
import Primer.Builtins.DSL (boolAnn, maybeAnn, nat)
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  GlobalName,
  ID,
  ModuleName,
  PrimCon (PrimChar, PrimDouble, PrimInt),
  TyConName,
  Type' (..),
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  ann,
  char,
  double,
  int,
  tcon,
 )
import Primer.Core.Utils (generateIDs)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (Name)
import Primer.Primitives.PrimDef (PrimDef (..))
import Primer.TypeDef (PrimTypeDef (..))

data PrimFunError
  = -- | We have attempted to apply a primitive function to invalid args.
    PrimFunError
      PrimDef
      -- | Arguments
      [Expr' () () ()]
  deriving stock (Eq, Show, Data, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PrimFunError

primitiveModuleName :: ModuleName
primitiveModuleName = mkSimpleModuleName "Primitives"

-- | The name of the type to which this primitive constructor belongs.
-- This should be a key in `allPrimTypeDefs`.
primConName :: PrimCon -> TyConName
primConName = \case
  PrimChar _ -> tChar
  PrimInt _ -> tInt
  PrimDouble _ -> tDouble

primitive :: Name -> GlobalName k
primitive = qualifyName primitiveModuleName

tChar :: TyConName
tChar = primitive "Char"

tInt :: TyConName
tInt = primitive "Int"

tDouble :: TyConName
tDouble = primitive "Double"

-- | Construct a reference to a primitive definition.
primitiveGVar :: PrimDef -> GVarName
primitiveGVar = primitive . primDefName

-- | Primitive type definitions.
-- There should be one entry here for each constructor of `PrimCon`.
allPrimTypeDefs :: Map TyConName (PrimTypeDef ())
allPrimTypeDefs =
  M.fromList
    [ let name = tChar
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = ["c"]
              }
          )
    , let name = tInt
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = ["i", "j", "k", "m", "n"]
              }
          )
    , let name = tDouble
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = ["x", "y", "z"]
              }
          )
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()
      PrimDouble _ -> ()

primDefName :: PrimDef -> Name
primDefName = \case
  ToUpper -> "toUpper"
  IsSpace -> "isSpace"
  HexToNat -> "hexToNat"
  NatToHex -> "natToHex"
  EqChar -> "eqChar"
  IntAdd -> "Int.+"
  IntMinus -> "Int.-"
  IntMul -> "Int.×"
  IntQuotient -> "Int.quotient"
  IntRemainder -> "Int.remainder"
  IntQuot -> "Int.quot"
  IntRem -> "Int.rem"
  IntLT -> "Int.<"
  IntLTE -> "Int.≤"
  IntGT -> "Int.>"
  IntGTE -> "Int.≥"
  IntEq -> "Int.="
  IntNeq -> "Int.≠"
  IntToNat -> "Int.toNat"
  IntFromNat -> "Int.fromNat"
  DoubleAdd -> "Double.+"
  DoubleMinus -> "Double.-"
  DoubleMul -> "Double.×"
  DoubleQuotient -> "Double.quotient"
  DoubleLT -> "Double.<"
  DoubleLTE -> "Double.≤"
  DoubleGT -> "Double.>"
  DoubleGTE -> "Double.≥"
  DoubleEq -> "Double.="
  DoubleNeq -> "Double.≠"
  DoubleFromInt -> "Double.fromInt"
  DoubleFloor -> "Double.floor"
  PrimConst -> "const"

primDefType :: PrimDef -> Type' () ()
primDefType = uncurry (flip $ foldr $ TFun ()) . primFunTypes

primFunTypes :: PrimDef -> ([Type' () ()], Type' () ())
primFunTypes = \case
  ToUpper -> ([c tChar], c tChar)
  IsSpace -> ([c tChar], c tBool)
  HexToNat -> ([c tChar], c tMaybe `a` c tNat)
  NatToHex -> ([c tNat], c tMaybe `a` c tChar)
  EqChar -> ([c tChar, c tChar], c tBool)
  IntAdd -> ([c tInt, c tInt], c tInt)
  IntMinus -> ([c tInt, c tInt], c tInt)
  IntMul -> ([c tInt, c tInt], c tInt)
  IntQuotient -> ([c tInt, c tInt], c tMaybe `a` c tInt)
  IntRemainder -> ([c tInt, c tInt], c tMaybe `a` c tInt)
  IntQuot -> ([c tInt, c tInt], c tInt)
  IntRem -> ([c tInt, c tInt], c tInt)
  IntLT -> ([c tInt, c tInt], c tBool)
  IntLTE -> ([c tInt, c tInt], c tBool)
  IntGT -> ([c tInt, c tInt], c tBool)
  IntGTE -> ([c tInt, c tInt], c tBool)
  IntEq -> ([c tInt, c tInt], c tBool)
  IntNeq -> ([c tInt, c tInt], c tBool)
  IntToNat -> ([c tInt], c tMaybe `a` c tNat)
  IntFromNat -> ([c tNat], c tInt)
  DoubleAdd -> ([c tDouble, c tDouble], c tDouble)
  DoubleMinus -> ([c tDouble, c tDouble], c tDouble)
  DoubleMul -> ([c tDouble, c tDouble], c tDouble)
  DoubleQuotient -> ([c tDouble, c tDouble], c tMaybe `a` c tDouble)
  DoubleLT -> ([c tDouble, c tDouble], c tBool)
  DoubleLTE -> ([c tDouble, c tDouble], c tBool)
  DoubleGT -> ([c tDouble, c tDouble], c tBool)
  DoubleGTE -> ([c tDouble, c tDouble], c tBool)
  DoubleEq -> ([c tDouble, c tDouble], c tBool)
  DoubleNeq -> ([c tDouble, c tDouble], c tBool)
  DoubleFromInt -> ([c tInt], c tDouble)
  DoubleFloor -> ([c tDouble], c tInt)
  -- Arbitrarily limited to `Int` and `Bool` since we our system doesn't allow polymorphic primitives.
  -- Note that this primitive is only for testing anyway.
  PrimConst -> ([c tBool, c tNat], c tBool)
  where
    c = TCon ()
    a = TApp ()

primFunDef :: PrimDef -> [Expr' () () ()] -> Either PrimFunError (forall m. MonadFresh ID m => m Expr)
primFunDef def args = case def of
  ToUpper -> case args of
    [PrimCon _ (PrimChar c)] ->
      Right $ char $ toUpper c
    _ -> err
  IsSpace -> case args of
    [PrimCon _ (PrimChar c)] ->
      Right $ boolAnn (isSpace c)
    _ -> err
  HexToNat -> case args of
    [PrimCon _ (PrimChar c)] -> Right $ maybeAnn (tcon tNat) nat (digitToIntSafe c)
      where
        digitToIntSafe :: Char -> Maybe Natural
        digitToIntSafe c' = fromIntegral <$> (guard (isHexDigit c') $> digitToInt c')
    _ -> err
  NatToHex -> case args of
    [exprToNat -> Just n] ->
      Right $ maybeAnn (tcon tChar) char $ intToDigitSafe n
      where
        intToDigitSafe :: Natural -> Maybe Char
        intToDigitSafe n' = guard (0 <= n && n <= 15) $> intToDigit (fromIntegral n')
    _ -> err
  EqChar -> case args of
    [PrimCon _ (PrimChar c1), PrimCon _ (PrimChar c2)] ->
      Right $ boolAnn $ c1 == c2
    _ -> err
  IntAdd -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ int $ x + y
    _ -> err
  IntMinus -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ int $ x - y
    _ -> err
  IntMul -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ int $ x * y
    _ -> err
  IntQuotient -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        maybeAnn (tcon tInt) int $
          if y == 0
            then Nothing
            else Just $ x `div` y
    _ -> err
  IntRemainder -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        maybeAnn (tcon tInt) int $
          if y == 0
            then Nothing
            else Just $ x `mod` y
    _ -> err
  IntQuot -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        int $
          if y == 0 then 0 else x `div` y
    _ -> err
  IntRem -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        int $
          if y == 0
            then x
            else x `mod` y
    _ -> err
  IntLT -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x < y
    _ -> err
  IntLTE -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x <= y
    _ -> err
  IntGT -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x > y
    _ -> err
  IntGTE -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x >= y
    _ -> err
  IntEq -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x == y
    _ -> err
  IntNeq -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ boolAnn $ x /= y
    _ -> err
  IntToNat -> case args of
    [PrimCon _ (PrimInt x)] ->
      Right $
        maybeAnn (tcon tNat) nat $
          if x < 0
            then Nothing
            else Just $ fromInteger x
    _ -> err
  IntFromNat -> case args of
    [exprToNat -> Just n] ->
      Right $ int $ fromIntegral n
    _ -> err
  DoubleAdd -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ double $ x + y
    _ -> err
  DoubleMinus -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ double $ x - y
    _ -> err
  DoubleMul -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ double $ x * y
    _ -> err
  DoubleQuotient -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $
        maybeAnn (tcon tDouble) double $
          if y == 0
            then Nothing
            else Just $ x / y
    _ -> err
  DoubleLT -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x < y
    _ -> err
  DoubleLTE -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x <= y
    _ -> err
  DoubleGT -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x > y
    _ -> err
  DoubleGTE -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x >= y
    _ -> err
  DoubleEq -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x == y
    _ -> err
  DoubleNeq -> case args of
    [PrimCon _ (PrimDouble x), PrimCon _ (PrimDouble y)] ->
      Right $ boolAnn $ x /= y
    _ -> err
  DoubleFromInt -> case args of
    [PrimCon _ (PrimInt x)] ->
      Right $ double $ fromInteger x
    _ -> err
  DoubleFloor -> case args of
    [PrimCon _ (PrimDouble x)] ->
      Right $ int $ floor x
    _ -> err
  PrimConst -> case args of
    [x, _] ->
      Right $ generateIDs x `ann` tcon tBool
    _ -> err
  where
    exprToNat = \case
      Con _ c [] | c == cZero -> Just 0
      Con _ c [x] | c == cSucc -> succ <$> exprToNat x
      _ -> Nothing
    err = Left $ PrimFunError def args
