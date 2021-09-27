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
  Expr,
  Expr' (..),
  ID (..),
  Kind (..),
  Meta (..),
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
genExpr = Gen.recursive Gen.choice [genEmptyHole, genCon] [genHole, genAnn, genApp]

genEmptyHole :: ExprGen Expr
genEmptyHole = EmptyHole <$> genMeta

genHole :: ExprGen Expr
genHole = Hole <$> genMeta <*> genExpr

genAnn :: ExprGen Expr
genAnn = Ann <$> genMeta <*> genExpr <*> genType

genApp :: ExprGen Expr
genApp = App <$> genMeta <*> genExpr <*> genExpr

genCon :: ExprGen Expr
genCon = Con <$> genMeta <*> genName

-- | Note: we currently don't generate @TVar@, @TApp@ or @TForall@, because most
-- of the system doesn't support them.
genType :: ExprGen Type
genType =
  Gen.recursive
    Gen.choice
    [ TEmptyHole <$> genMeta
    , TCon <$> genMeta <*> genName
    ]
    [ TFun <$> genMeta <*> genType <*> genType
    ]

genKind :: ExprGen Kind
genKind = Gen.recursive Gen.choice [pure KType] [KFun <$> genKind <*> genKind]

genMeta :: ExprGen (Meta (Maybe a))
genMeta = Meta <$> genID <*> pure Nothing <*> pure Nothing

genID :: ExprGen ID
genID = do
  i <- get
  put (i + 1)
  pure $ i + 1

genName :: ExprGen Name
genName = unsafeMkName <$> Gen.text (Range.linear 1 10) Gen.alpha
