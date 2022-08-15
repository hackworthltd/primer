{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  PrimDef (..),
  primitiveModule,
  allPrimTypeDefs,
  tInt,
  tChar,
  primitiveGVar,
  primDefName,
  primDefType,
  defType,
  allPrimDefs,
  primFunDef,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.List.Extra (enumerate)
import Data.Map qualified as M
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
import Primer.Builtins.DSL (bool_, maybe_, nat)
import Primer.Core (
  ASTDef (..),
  Def (..),
  Expr,
  Expr' (App, Con, PrimCon),
  GVarName,
  GlobalName (baseName),
  ID,
  ModuleName,
  PrimCon (..),
  PrimDef (..),
  PrimFunError (..),
  PrimTypeDef (..),
  TyConName,
  Type' (..),
  TypeDef (TypeDefPrim),
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  aPP,
  app,
  char,
  con,
  int,
  tcon,
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes))
import Primer.Name (Name)

primitiveModuleName :: ModuleName
primitiveModuleName = mkSimpleModuleName "Primitives"

primitive :: Name -> GlobalName k
primitive = qualifyName primitiveModuleName

-- | This module depends on the builtin module, due to some terms referencing builtin types.
-- It contains all primitive types and terms.
primitiveModule :: Module
primitiveModule =
  Module
    { moduleName = primitiveModuleName
    , moduleTypes = TypeDefPrim <$> M.mapKeys baseName allPrimTypeDefs
    , moduleDefs = M.fromList $ [(baseName $ primDefName def, DefPrim def) | def <- enumerate]
    }

tChar :: TyConName
tChar = primitive "Char"

tInt :: TyConName
tInt = primitive "Int"

-- | Construct a reference to a primitive definition. For use in tests.
primitiveGVar :: Name -> GVarName
primitiveGVar = primitive

-- | Primitive type definitions.
-- There should be one entry here for each constructor of `PrimCon`.
allPrimTypeDefs :: Map TyConName PrimTypeDef
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
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()

-- | Primitive term definitions.
-- For each of these, we should have a test that the evaluator produces expected results.
allPrimDefs :: Map GVarName PrimDef
allPrimDefs = M.fromList [(primDefName def, def) | def <- enumerate]

primDefName :: PrimDef -> GVarName
primDefName =
  primitiveGVar . \case
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

primDefType :: PrimDef -> Type' ()
primDefType = uncurry (flip $ foldr $ TFun ()) . primFunTypes

defType :: Def -> Type' ()
defType = \case
  DefPrim d -> primDefType d
  DefAST d -> forgetTypeMetadata $ astDefType d

primFunTypes :: PrimDef -> ([Type' ()], Type' ())
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
  where
    c = TCon ()
    a = TApp ()

primFunDef :: PrimDef -> [Expr' () ()] -> Either PrimFunError (forall m. MonadFresh ID m => m Expr)
primFunDef def args = case def of
  ToUpper -> case args of
    [PrimCon _ (PrimChar c)] ->
      Right $ char $ toUpper c
    _ -> err
  IsSpace -> case args of
    [PrimCon _ (PrimChar c)] ->
      Right $ bool_ $ isSpace c
    _ -> err
  HexToNat -> case args of
    [PrimCon _ (PrimChar c)] -> Right $ maybe_ (tcon tNat) nat $ digitToIntSafe c
      where
        digitToIntSafe :: Char -> Maybe Natural
        digitToIntSafe c' = fromIntegral <$> (guard (isHexDigit c') $> digitToInt c')
    _ -> err
  NatToHex -> case args of
    [exprToNat -> Just n] ->
      Right $ maybe_ (tcon tChar) char $ intToDigitSafe n
      where
        intToDigitSafe :: Natural -> Maybe Char
        intToDigitSafe n' = guard (0 <= n && n <= 15) $> intToDigit (fromIntegral n')
    _ -> err
  EqChar -> case args of
    [PrimCon _ (PrimChar c1), PrimCon _ (PrimChar c2)] ->
      Right $ bool_ $ c1 == c2
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
        if y == 0
          then con cNothing `aPP` tcon tInt
          else
            con cJust
              `aPP` tcon tInt
              `app` int (x `div` y)
    _ -> err
  IntRemainder -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        if y == 0
          then con cNothing `aPP` tcon tInt
          else
            con cJust
              `aPP` tcon tInt
              `app` int (x `mod` y)
    _ -> err
  IntQuot -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        if y == 0
          then int 0
          else int (x `div` y)
    _ -> err
  IntRem -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $
        if y == 0
          then int x
          else int (x `mod` y)
    _ -> err
  IntLT -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x < y
    _ -> err
  IntLTE -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x <= y
    _ -> err
  IntGT -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x > y
    _ -> err
  IntGTE -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x >= y
    _ -> err
  IntEq -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x == y
    _ -> err
  IntNeq -> case args of
    [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y)] ->
      Right $ bool_ $ x /= y
    _ -> err
  IntToNat -> case args of
    [PrimCon _ (PrimInt x)] ->
      Right $
        if x < 0
          then con cNothing `aPP` tcon tNat
          else
            con cJust
              `aPP` tcon tNat
              `app` nat (fromInteger x)
    _ -> err
  IntFromNat -> case args of
    [exprToNat -> Just n] ->
      Right $ int $ fromIntegral n
    _ -> err
  where
    exprToNat = \case
      Con _ c | c == cZero -> Just 0
      App _ (Con _ c) x | c == cSucc -> succ <$> exprToNat x
      _ -> Nothing
    err = Left $ PrimFunError def args
