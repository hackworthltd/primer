-- |
-- This module generates "raw" terms and types.
-- That is, syntax trees which are not (necessarily) well-typed, or even well-scoped.
-- It is however, fast and has good coverage properties.
--
-- For generating well-typed terms, see "Gen.Core.Typed".
module Gen.Core.Raw (
  runExprGen,
  evalExprGen,
  genID,
  genName,
  genKind,
  genType,
  genExpr,
) where

import Foreword

import Hedgehog hiding (Var, check)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (..),
  ID (..),
  Kind (..),
  Meta (..),
  PrimCon (..),
  Type,
  Type' (..),
 )
import Primer.Name (Name, unsafeMkName)

type ExprGen a = StateT ID Gen a

runExprGen :: ID -> ExprGen a -> Gen (a, ID)
runExprGen i m = runStateT m i

evalExprGen :: ID -> ExprGen a -> Gen a
evalExprGen i m = evalStateT m i

genExpr :: ExprGen Expr
genExpr =
  Gen.recursive
    Gen.choice
    [genEmptyHole, genCon, genVar, genGlobalVar]
    [ genHole
    , genAnn
    , genApp
    , genAPP
    , genLam
    , genLAM
    , genLet
    , genLetType
    , genLetrec
    , genCase
    , genPrim
    ]

genEmptyHole :: ExprGen Expr
genEmptyHole = EmptyHole <$> genMeta

genHole :: ExprGen Expr
genHole = Hole <$> genMeta <*> genExpr

genAnn :: ExprGen Expr
genAnn = Ann <$> genMeta <*> genExpr <*> genType

genApp :: ExprGen Expr
genApp = App <$> genMeta <*> genExpr <*> genExpr

genAPP :: ExprGen Expr
genAPP = APP <$> genMeta <*> genExpr <*> genType

genCon :: ExprGen Expr
genCon = Con <$> genMeta <*> genName

genLam :: ExprGen Expr
genLam = Lam <$> genMeta <*> genName <*> genExpr

genLAM :: ExprGen Expr
genLAM = LAM <$> genMeta <*> genName <*> genExpr

genVar :: ExprGen Expr
genVar = Var <$> genMeta <*> genName

genGlobalVar :: ExprGen Expr
genGlobalVar = GlobalVar <$> genMeta <*> genName

genLet :: ExprGen Expr
genLet = Let <$> genMeta <*> genName <*> genExpr <*> genExpr

genLetType :: ExprGen Expr
genLetType = LetType <$> genMeta <*> genName <*> genType <*> genExpr

genLetrec :: ExprGen Expr
genLetrec = Letrec <$> genMeta <*> genName <*> genExpr <*> genType <*> genExpr

genCase :: ExprGen Expr
genCase = Case <$> genMeta <*> genExpr <*> Gen.list (Range.linear 0 5) genBranch
  where
    genBranch = CaseBranch <$> genName <*> Gen.list (Range.linear 0 5) genBind <*> genExpr
    genBind = Bind <$> genMeta <*> genName

genPrim :: ExprGen Expr
genPrim = PrimCon <$> genMeta <*> genPrimCon
  where
    intBound = fromIntegral (maxBound :: Word64) -- arbitrary
    genPrimCon :: (StateT ID Gen PrimCon)
    genPrimCon =
      Gen.choice
        [ PrimChar <$> Gen.unicodeAll
        , PrimInt <$> Gen.integral (Range.linear (-intBound) intBound)
        ]
    -- This ensures that when we modify the constructors of `PrimCon` (i.e. we add/remove primitive types),
    -- we are alerted that we need to update this generator.
    _ = \case
      PrimChar _ -> ()
      PrimInt _ -> ()

genType :: ExprGen Type
genType =
  Gen.recursive
    Gen.choice
    [ TEmptyHole <$> genMeta
    , TCon <$> genMeta <*> genName
    , TVar <$> genMeta <*> genName
    ]
    [ THole <$> genMeta <*> genType
    , TFun <$> genMeta <*> genType <*> genType
    , TApp <$> genMeta <*> genType <*> genType
    , TForall <$> genMeta <*> genName <*> genKind <*> genType
    ]

genKind :: ExprGen Kind
genKind = Gen.recursive Gen.choice [pure KType, pure KHole] [KFun <$> genKind <*> genKind]

genMeta :: ExprGen (Meta (Maybe a))
genMeta = Meta <$> genID <*> pure Nothing <*> pure Nothing

genID :: ExprGen ID
genID = do
  i <- get
  put (i + 1)
  pure $ i + 1

genName :: ExprGen Name
genName = unsafeMkName <$> Gen.text (Range.linear 1 10) Gen.alpha
