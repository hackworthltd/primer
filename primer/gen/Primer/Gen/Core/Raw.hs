-- |
-- This module generates "raw" terms and types.
-- That is, syntax trees which are not (necessarily) well-typed, or even well-scoped.
-- It is however, fast and has good coverage properties.
--
-- For generating well-typed terms, see "Primer.Gen.Core.Typed".
module Primer.Gen.Core.Raw (
  ExprGen,
  runExprGen,
  evalExprGen,
  genID,
  genName,
  genModuleName,
  genLVarName,
  genTyVarName,
  genTyConName,
  genValConName,
  genGVarName,
  genKind,
  genType,
  genExpr,
) where

import Foreword

import Hedgehog hiding (Var, check)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  CaseFallback' (CaseExhaustive),
  Expr,
  Expr' (..),
  GVarName,
  ID (..),
  Kind (..),
  LVarName,
  LocalName (LocalName),
  Meta (..),
  ModuleName (ModuleName),
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

genModuleName :: MonadGen m => m ModuleName
genModuleName =
  ModuleName
    <$> Gen.frequency
      [ (9, pure $ "M" :| [])
      , (1, Gen.nonEmpty (Range.linear 1 3) genName)
      ]

genValConName :: MonadGen m => m ValConName
genValConName = qualifyName <$> genModuleName <*> genName

genCon :: ExprGen Expr
genCon =
  Gen.recursive
    Gen.choice
    [genCon' (pure [])]
    [genCon' (Gen.list (Range.linear 0 5) genExpr)]
  where
    genCon' tms = Con <$> genMeta <*> genValConName <*> tms

genLam :: ExprGen Expr
genLam = Lam <$> genMeta <*> genLVarName <*> genExpr

genLAM :: ExprGen Expr
genLAM = LAM <$> genMeta <*> genTyVarName <*> genExpr

genLocalVar :: ExprGen Expr
genLocalVar = Var <$> genMeta <*> (LocalVarRef <$> genLVarName)

genGlobalVar :: ExprGen Expr
genGlobalVar = Var <$> genMeta <*> (GlobalVarRef <$> genGVarName)

genGVarName :: MonadGen m => m GVarName
genGVarName = qualifyName <$> genModuleName <*> genName

genLet :: ExprGen Expr
genLet = Let <$> genMeta <*> genLVarName <*> genExpr <*> genExpr

genLetType :: ExprGen Expr
genLetType = LetType <$> genMeta <*> genTyVarName <*> genType <*> genExpr

genLetrec :: ExprGen Expr
genLetrec = Letrec <$> genMeta <*> genLVarName <*> genExpr <*> genType <*> genExpr

genCase :: ExprGen Expr
genCase = Case <$> genMeta <*> genExpr <*> Gen.list (Range.linear 0 5) genBranch <*> Gen.choice [pure CaseExhaustive]
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
    , TLet <$> genMeta <*> genTyVarName <*> genType <*> genType
    ]

genTyConName :: MonadGen m => m TyConName
genTyConName = qualifyName <$> genModuleName <*> genName

genKind :: MonadGen m => m Kind
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
