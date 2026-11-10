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
  pictureTypeDefs,
  tColour,
  tPoint,
  tPath,
  tPicture,
  cRGB,
  cMakePoint,
  cPolyline,
  cSpline,
  cJoin,
  cCircle,
  cRectangle,
  cLine,
  cShape,
  cFill,
  cLineColour,
  cTranslate,
  cRotate,
  cScale,
  cOver,
  cSpin,
  cMoveAlong,
  Colour (..),
  Point (..),
  Path (..),
  Picture (..),
  exprToPicture,
  exprToPath,
  exprToList,
  pathTrail,
) where

import Foreword hiding (rotate)

import Control.Monad.Fresh (MonadFresh)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Data (Data)
import Data.Map qualified as M
import Diagrams.Prelude (Line, Located, Trail', V2, cubicSpline, fromVertices, p2, splitAtParam, stdArcLength, unLoc, (*^), (.-.), (.-^))
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
  PrimCon (PrimChar, PrimDouble, PrimInt),
  TyConName,
  Type' (..),
  ValConName,
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
  ArcLength -> "arcLength"
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
  ArcLength -> ([c tPath], c tDouble)
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
  -- PROTOTYPE: this is why the core depends on `diagrams-lib`.
  -- It means we measure exactly the curve which the frontend draws, but ideally a primitive like this would be
  -- defined alongside the rest of the picture support, outside the core, which needs an extensible primitives system.
  ArcLength -> case args of
    [exprToPath -> Just p] ->
      Right $ double $ stdArcLength $ unLoc $ pathTrail p
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

-- | The types used for building pictures.
-- These are not primitive types, but they live here since they are built from 'tDouble', and are needed by 'ArcLength'.
pictureTypeDefs :: [(TyConName, ASTTypeDef () ())]
pictureTypeDefs =
  [ (tColour, simpleTypeDef [ValCon cRGB [c tDouble, c tDouble, c tDouble]])
  , (tPoint, simpleTypeDef [ValCon cMakePoint [c tDouble, c tDouble]])
  , (tPath, simpleTypeDef [ValCon cPolyline [listOf tPoint], ValCon cSpline [listOf tPoint], ValCon cJoin [listOf tPath]])
  ,
    ( tPicture
    , simpleTypeDef
        [ ValCon cCircle [c tDouble]
        , ValCon cRectangle [c tDouble, c tDouble]
        , ValCon cLine [c tPath]
        , ValCon cShape [c tPath]
        , ValCon cFill [c tColour, c tPicture]
        , ValCon cLineColour [c tColour, c tPicture]
        , ValCon cTranslate [c tDouble, c tDouble, c tPicture]
        , ValCon cRotate [c tDouble, c tPicture]
        , ValCon cScale [c tDouble, c tPicture]
        , ValCon cOver [listOf tPicture]
        , ValCon cSpin [c tDouble, c tDouble, c tPicture]
        , ValCon cMoveAlong [c tPath, c tDouble, c tPicture]
        ]
    )
  ]
  where
    c = TCon ()
    listOf = TApp () (c tList) . c
    simpleTypeDef cons =
      ASTTypeDef
        { astTypeDefParameters = []
        , astTypeDefConstructors = cons
        , astTypeDefNameHints = []
        }

tColour :: TyConName
tColour = primitive "Colour"
tPoint :: TyConName
tPoint = primitive "Point"
tPath :: TyConName
tPath = primitive "Path"
tPicture :: TyConName
tPicture = primitive "Picture"

cRGB :: ValConName
cRGB = primitive "RGB"
cMakePoint :: ValConName
cMakePoint = primitive "MakePoint"
cPolyline :: ValConName
cPolyline = primitive "Polyline"
cSpline :: ValConName
cSpline = primitive "Spline"
cJoin :: ValConName
cJoin = primitive "Join"
cCircle :: ValConName
cCircle = primitive "Circle"
cRectangle :: ValConName
cRectangle = primitive "Rectangle"
cLine :: ValConName
cLine = primitive "Line"
cShape :: ValConName
cShape = primitive "Shape"
cFill :: ValConName
cFill = primitive "Fill"
cLineColour :: ValConName
cLineColour = primitive "LineColour"
cTranslate :: ValConName
cTranslate = primitive "Translate"
cRotate :: ValConName
cRotate = primitive "Rotate"
cScale :: ValConName
cScale = primitive "Scale"
cOver :: ValConName
cOver = primitive "Over"
cSpin :: ValConName
cSpin = primitive "Spin"
cMoveAlong :: ValConName
cMoveAlong = primitive "MoveAlong"

-- | A Haskell model of our built-in `Colour` type, with components from 0 to 1.
data Colour = RGB Double Double Double
  deriving stock (Eq, Show)

-- | A Haskell model of our built-in `Point` type.
data Point = MakePoint Double Double
  deriving stock (Eq, Show)

-- | A Haskell model of our built-in `Path` type.
data Path
  = -- | Straight lines between the points.
    Polyline [Point]
  | -- | A smooth curve through the points.
    Spline [Point]
  | -- | One path after another, joined by straight lines where they don't meet.
    Join [Path]
  deriving stock (Eq, Show)

-- | A Haskell model of our built-in `Picture` type.
-- Angles are in degrees, and durations in seconds. Animations loop forever.
data Picture
  = Circle Double
  | Rectangle Double Double
  | -- | Draw a path.
    Line Path
  | -- | Fill the region enclosed by a path, without an outline.
    Shape Path
  | Fill Colour Picture
  | LineColour Colour Picture
  | Translate Double Double Picture
  | Rotate Double Picture
  | Scale Double Picture
  | -- | Pictures earlier in the list appear on top.
    Over [Picture]
  | -- | Rotate by the given angle, over the given duration.
    Spin Double Double Picture
  | -- | Move along the path, at a constant speed, over the given duration, turning to follow its direction.
    MoveAlong Path Double Picture
  deriving stock (Eq, Show)

-- | Interpret a term as a 'Picture', if it is a value of our `Picture` type with no holes, variables etc.
exprToPicture :: Expr' a b c -> Maybe Picture
exprToPicture = \case
  Con _ c [exprToDouble -> Just r] | c == cCircle -> Just $ Circle r
  Con _ c [exprToDouble -> Just w, exprToDouble -> Just h] | c == cRectangle -> Just $ Rectangle w h
  Con _ c [exprToPath -> Just p] | c == cLine -> Just $ Line p
  Con _ c [exprToPath -> Just p] | c == cShape -> Just $ Shape p
  Con _ c [exprToColour -> Just col, exprToPicture -> Just p] | c == cFill -> Just $ Fill col p
  Con _ c [exprToColour -> Just col, exprToPicture -> Just p] | c == cLineColour -> Just $ LineColour col p
  Con _ c [exprToDouble -> Just x, exprToDouble -> Just y, exprToPicture -> Just p] | c == cTranslate -> Just $ Translate x y p
  Con _ c [exprToDouble -> Just a, exprToPicture -> Just p] | c == cRotate -> Just $ Rotate a p
  Con _ c [exprToDouble -> Just s, exprToPicture -> Just p] | c == cScale -> Just $ Scale s p
  Con _ c [exprToList -> Just (traverse exprToPicture -> Just ps)] | c == cOver -> Just $ Over ps
  Con _ c [exprToDouble -> Just a, exprToDouble -> Just t, exprToPicture -> Just p] | c == cSpin -> Just $ Spin a t p
  Con _ c [exprToPath -> Just path, exprToDouble -> Just t, exprToPicture -> Just p] | c == cMoveAlong -> Just $ MoveAlong path t p
  _ -> Nothing

-- | Interpret a term as a 'Path', if it is a value of our `Path` type with no holes, variables etc.
exprToPath :: Expr' a b c -> Maybe Path
exprToPath = \case
  Con _ c [exprToList -> Just (traverse exprToPoint -> Just ps)] | c == cPolyline -> Just $ Polyline ps
  Con _ c [exprToList -> Just (traverse exprToPoint -> Just ps)] | c == cSpline -> Just $ Spline ps
  Con _ c [exprToList -> Just (traverse exprToPath -> Just ps)] | c == cJoin -> Just $ Join ps
  _ -> Nothing

exprToPoint :: Expr' a b c -> Maybe Point
exprToPoint = \case
  Con _ c [exprToDouble -> Just x, exprToDouble -> Just y] | c == cMakePoint -> Just $ MakePoint x y
  _ -> Nothing

exprToColour :: Expr' a b c -> Maybe Colour
exprToColour = \case
  Con _ c [exprToDouble -> Just r, exprToDouble -> Just g, exprToDouble -> Just b] | c == cRGB -> Just $ RGB r g b
  _ -> Nothing

exprToDouble :: Expr' a b c -> Maybe Double
exprToDouble = \case
  PrimCon _ (PrimDouble x) -> Just x
  _ -> Nothing

exprToList :: Expr' a b c -> Maybe [Expr' a b c]
exprToList = \case
  Con _ c [] | c == cNil -> Just []
  Con _ c [x, exprToList -> Just xs] | c == cCons -> Just $ x : xs
  _ -> Nothing

-- | The trail which a 'Path' describes, starting at its first point.
-- This is used by 'ArcLength', as well as for drawing, so that the two agree.
pathTrail :: Path -> Located (Trail' Line V2 Double)
pathTrail = \case
  Polyline ps -> fromVertices $ map toP2 ps
  -- `cubicSpline` needs at least two points.
  -- PROTOTYPE: ideally, this would just be:
  -- Spline ps@(_ : _ : _) -> cubicSpline False $ map toP2 ps
  -- But Firefox computes positions along curves which start flat, like natural cubic splines, wrongly
  -- (https://bugzilla.mozilla.org/show_bug.cgi?id=1507603), which throws off `MoveAlong`.
  -- So we extend the spline backwards along its first segment, by 1% of that segment's length, then cut that extra part
  -- off again, so that the curve no longer starts flat. This changes its shape slightly.
  Spline ps@(p0 : p1 : _) ->
    let p0' = toP2 p0
        extra = p0' .-^ (0.01 *^ (toP2 p1 .-. p0'))
     in snd $ splitAtParam (cubicSpline False $ extra : map toP2 ps) (1 / fromIntegral (length ps))
  Spline ps -> fromVertices $ map toP2 ps
  Join ps -> maybe (fromVertices []) sconcat $ nonEmpty $ map pathTrail ps
  where
    toP2 (MakePoint x y) = p2 (x, y)
