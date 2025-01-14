{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Primer.Primitives (
  PrimDef (..),
  allPrimTypeDefs,
  tInt,
  tChar,
  tAnimation,
  primitive,
  primitiveGVar,
  primConName,
  primDefName,
  primDefType,
  primFunDef,
  PrimFunError (..),
  primitiveModuleName,
  pictureDef,
  tPicture,
  cCircle,
  cRectangle,
  cColour,
  cRotate,
  cTranslate,
  cCompoundPicture,
) where

import Foreword hiding (rotate)

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Data)
import Data.Map qualified as M
import Numeric.Natural (Natural)
import Primer.Builtins (
  cCons,
  cNil,
  cSucc,
  cZero,
  tBool,
  tList,
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
  PrimCon (PrimAnimation, PrimChar, PrimInt),
  TmVarRef (LocalVarRef),
  TyConName,
  Type' (..),
  ValConName,
  mkSimpleModuleName,
  qualifyName,
 )
import Primer.Core.DSL (
  ann,
  char,
  int,
  tcon,
 )
import Primer.Core.Utils (generateIDs)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (Name)
import Primer.Primitives.PrimDef (PrimDef (..))
import Primer.TypeDef (ASTTypeDef (..), PrimTypeDef (..), ValCon (..))

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
  PrimAnimation _ -> tAnimation

primitive :: Name -> GlobalName k
primitive = qualifyName primitiveModuleName

tChar :: TyConName
tChar = primitive "Char"

tInt :: TyConName
tInt = primitive "Int"

tAnimation :: TyConName
tAnimation = primitive "Animation"

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
    , let name = tAnimation
       in ( name
          , PrimTypeDef
              { primTypeDefParameters = []
              , primTypeDefNameHints = []
              }
          )
    ]
  where
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this map.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()
      PrimAnimation _ -> ()

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
  Animate -> "animate"
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
  Animate ->
    -- A loop time, in seconds, and a function from frame number to output.
    -- Note that the number of frames per second is currently hardcoded to 10, and that
    -- ideally we'd use floats here and the function would take a time in seconds as well.
    -- Thus `Animate n p` will denote an animation of `10*n` frames of `0.1`s duration each, for
    -- a total of `n` seconds, and will call `p` with arguments `0,1,...,10*n-1` to compute each frame.
    (
      [ c tInt
      , c tInt `f` c tPicture
      ]
    , c tAnimation
    )
  -- Arbitrarily limited to `Int` and `Bool` since we our system doesn't allow polymorphic primitives.
  -- Note that this primitive is only for testing anyway.
  PrimConst -> ([c tBool, c tNat], c tBool)
  where
    c = TCon ()
    a = TApp ()
    f = TFun ()

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
  Animate -> case args of
    -- Since we only support translating a `Picture` expression to an image once it is in normal form,
    -- this guard will only pass when `picture` has no free variables other than `time`.
    [PrimCon () (PrimInt duration), Lam () time picture]
      | Just _frames <- traverse diagramAtTime [0 .. (duration * 100) `div` frameLength - 1] ->
          -- temporarily disabled due to dependency issues with WASM
          -- we keep around as much as we can without `diagrams` (relies on `fsnotify`, and uses Template Haskell),
          -- or `Rasterific` (relies on `bitvec`, which fails on WASM with GHC <9.8)
          err
      where
        -- Note that this simple substitution hack only allows for trivial functions,
        -- i.e. those where only substitution is needed for the function body to reach a normal form.
        -- Our primitives system doesn't yet support further evaluation here.
        diagramAtTime t = exprToPicture $ substTime (PrimCon () (PrimInt t)) picture
          where
            substTime a = \case
              Var () (LocalVarRef t') | t' == time -> a
              Con () c es -> Con () c $ map (substTime a) es
              e -> e
        -- Values which are hardcoded, for now at least, for the sake of keeping the student-facing API simple.
        -- We keep the frame rate and resolution low to avoid serialising huge GIFs.
        -- gifLooping = LoopingForever
        frameLength = 10 -- in hundredths of a second, as per the GIF spec
        _width :: Int = 160
        _height :: Int = 90
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

pictureDef :: ASTTypeDef () ()
pictureDef =
  ASTTypeDef
    { astTypeDefParameters = []
    , astTypeDefConstructors =
        [ ValCon cCircle [TCon () tInt]
        , ValCon cRectangle [TCon () tInt, TCon () tInt]
        , ValCon cColour [TCon () tInt, TCon () tInt, TCon () tInt, TCon () tPicture]
        , ValCon cRotate [TCon () tInt, TCon () tPicture]
        , ValCon cTranslate [TCon () tInt, TCon () tInt, TCon () tPicture]
        , -- Pictures are ordered foreground to background, i.e. those earlier in the list appear on top.
          ValCon cCompoundPicture [TApp () (TCon () tList) (TCon () tPicture)]
        ]
    , astTypeDefNameHints = []
    }

tPicture :: TyConName
tPicture = primitive "Picture"
cCircle :: ValConName
cCircle = primitive "Circle"
cRectangle :: ValConName
cRectangle = primitive "Rectangle"
cColour :: ValConName
cColour = primitive "Colour"
cRotate :: ValConName
cRotate = primitive "Rotate"
cTranslate :: ValConName
cTranslate = primitive "Translate"
cCompoundPicture :: ValConName
cCompoundPicture = primitive "Compound"

-- | A Haskell model of our built-in `Picture` type.
-- Using this type can make working with pictures more convenient,
-- including by giving us compile-time exhaustiveness checks.
data Picture
  = Circle Integer
  | Rect Integer Integer
  | Colour Integer Integer Integer Picture
  | Rotate Integer Picture
  | Translate Integer Integer Picture
  | CompoundPicture [Picture]

exprToPicture :: Expr' a b c -> Maybe Picture
exprToPicture = \case
  Con _ c [PrimCon _ (PrimInt r)]
    | c == cCircle ->
        Just $ Circle r
  Con _ c [PrimCon _ (PrimInt w), PrimCon _ (PrimInt h)]
    | c == cRectangle ->
        Just $ Rect w h
  Con _ c [PrimCon _ (PrimInt r), PrimCon _ (PrimInt g), PrimCon _ (PrimInt b), exprToPicture -> Just p]
    | c == cColour ->
        Just $ Colour r g b p
  Con _ c [PrimCon _ (PrimInt a), exprToPicture -> Just p]
    | c == cRotate ->
        Just $ Rotate a p
  Con _ c [PrimCon _ (PrimInt x), PrimCon _ (PrimInt y), exprToPicture -> Just p]
    | c == cTranslate ->
        Just $ Translate x y p
  Con _ c [exprToList -> Just (traverse exprToPicture -> Just ps)]
    | c == cCompoundPicture ->
        Just $ CompoundPicture ps
  _ -> Nothing
  where
    exprToList = \case
      Con _ c [] | c == cNil -> Just []
      Con _ c [x, exprToList -> Just xs] | c == cCons -> Just $ x : xs
      _ -> Nothing
