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
  genLVarName,
  genTyVarName,
  genTyConName,
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
  LVarName,
  LocalName (LocalName),
  Meta (..),
  PrimCon (..),
  TmVarRef (..),
  TyConName,
  TyVarName,
  Type,
  Type' (..),
  ValConName,
  qualifyName,
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
    [genEmptyHole, genCon, genLocalVar, genGlobalVar]
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

genValConName :: ExprGen ValConName
genValConName = qualifyName <$> genName

genCon :: ExprGen Expr
genCon = Con <$> genMeta <*> genValConName

genLam :: ExprGen Expr
genLam = Lam <$> genMeta <*> genLVarName <*> genExpr

genLAM :: ExprGen Expr
genLAM = LAM <$> genMeta <*> genTyVarName <*> genExpr

genLocalVar :: ExprGen Expr
genLocalVar = Var <$> genMeta <*> (LocalVarRef <$> genLVarName)

genGlobalVar :: ExprGen Expr
genGlobalVar = Var <$> genMeta <*> (GlobalVarRef . qualifyName <$> genName)

genLet :: ExprGen Expr
genLet = Let <$> genMeta <*> genLVarName <*> genExpr <*> genExpr

genLetType :: ExprGen Expr
genLetType = LetType <$> genMeta <*> genTyVarName <*> genType <*> genExpr

genLetrec :: ExprGen Expr
genLetrec = Letrec <$> genMeta <*> genLVarName <*> genExpr <*> genType <*> genExpr

genCase :: ExprGen Expr
genCase = Case <$> genMeta <*> genExpr <*> Gen.list (Range.linear 0 5) genBranch
  where
    genBranch = CaseBranch <$> genValConName <*> Gen.list (Range.linear 0 5) genBind <*> genExpr
    genBind = Bind <$> genMeta <*> genLVarName

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
    , TCon <$> genMeta <*> genTyConName
    , TVar <$> genMeta <*> genTyVarName
    ]
    [ THole <$> genMeta <*> genType
    , TFun <$> genMeta <*> genType <*> genType
    , TApp <$> genMeta <*> genType <*> genType
    , TForall <$> genMeta <*> genTyVarName <*> genKind <*> genType
    ]

genTyConName :: ExprGen TyConName
genTyConName = qualifyName <$> genName

genKind :: ExprGen Kind
genKind = Gen.recursive Gen.choice [pure KType, pure KHole] [KFun <$> genKind <*> genKind]

genMeta :: ExprGen (Meta (Maybe a))
genMeta = Meta <$> genID <*> pure Nothing <*> pure Nothing

genID :: ExprGen ID
genID = do
  i <- get
  put (i + 1)
  pure $ i + 1

genName :: MonadGen m => m Name
genName = unsafeMkName <$> Gen.frequency [(9, fixed), (1, random)]
  where
    fixed = Gen.element ["x", "y", "z", "foo", "bar"]
    random = Gen.text (Range.linear 1 10) Gen.alpha

genLVarName :: MonadGen m => m LVarName
genLVarName = LocalName <$> genName

genTyVarName :: MonadGen m => m TyVarName
genTyVarName = LocalName <$> genName
