-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL (
  emptyHole,
  hole,
  ann,
  app,
  aPP,
  conSat,
  con,
  con0,
  con1,
  lvar,
  gvar,
  var,
  lam,
  lAM,
  let_,
  letrec,
  letType,
  case_,
  branch,
  prim,
  char,
  int,
  tEmptyHole,
  thole,
  tcon,
  tforall,
  tlet,
  tfun,
  tapp,
  tvar,
  meta,
  meta',
  create,
  create',
  setMeta,
  S,
  tcon',
  conSat',
  con',
  gvar',
  branch',
  apps,
  apps',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Optics (set)
import Primer.Core (
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  Expr,
  Expr' (..),
  GVarName,
  ID,
  LVarName,
  ModuleName (ModuleName),
  PrimCon (..),
  TmVarRef (..),
  TyVarName,
  Type,
  TypeCache,
  ValConName,
  Value,
  qualifyName,
  _metadata,
 )
import Primer.Core.DSL.Meta (S, create, create', meta, meta')
import Primer.Core.DSL.Type (
  tEmptyHole,
  tapp,
  tcon,
  tcon',
  tforall,
  tfun,
  thole,
  tlet,
  tvar,
 )
import Primer.Name (Name)

setMeta :: Functor m => Value -> m Expr -> m Expr
setMeta m e = set _metadata (Just m) <$> e

app :: MonadFresh ID m => m Expr -> m Expr -> m Expr
app e1 e2 = App <$> meta <*> e1 <*> e2

-- | `app` for multiple args
apps :: MonadFresh ID m => m Expr -> [m Expr] -> m Expr
apps e es = apps' e (map Left es)

-- | `apps` for expressions and types
apps' :: MonadFresh ID m => m Expr -> [Either (m Expr) (m Type)] -> m Expr
apps' = foldl' app'
  where
    app' e (Left e') = e `app` e'
    app' e (Right t) = e `aPP` t

aPP :: MonadFresh ID m => m Expr -> m Type -> m Expr
aPP e t = APP <$> meta <*> e <*> t

hole :: MonadFresh ID m => m Expr -> m Expr
hole e = Hole <$> meta <*> e

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> meta <*> e <*> t

con :: MonadFresh ID m => ValConName -> m Expr
con c = Con <$> meta <*> pure c <*> pure [] <*> pure []

-- TODO (saturated constructors) once saturation is enforced, this will be
-- renamed to con, and the current con will be removed (since it creates
-- unsaturated constructors)
conSat :: MonadFresh ID m => ValConName -> [m Type] -> [m Expr] -> m Expr
conSat c tys tms = Con <$> meta <*> pure c <*> sequence tys <*> sequence tms

-- | Create a constructor of arity zero.
-- (This condition is not checked here.
--  If used with a constructor which has fields,
--  then the typechecker will complain, when run.)
con0 :: MonadFresh ID m => ValConName -> m Expr
con0 c = conSat c [] []


-- | Create a constructor of arity one.
-- (This condition is not checked here.
--  If used with a constructor which has fields,
--  then the typechecker will complain, when run.)
con1 :: MonadFresh ID m => ValConName -> m Expr -> m Expr
con1 c t = conSat c [] [t]

lvar :: MonadFresh ID m => LVarName -> m Expr
lvar v = Var <$> meta <*> pure (LocalVarRef v)

gvar :: MonadFresh ID m => GVarName -> m Expr
gvar name = Var <$> meta <*> pure (GlobalVarRef name)

var :: MonadFresh ID m => TmVarRef -> m Expr
var v = Var <$> meta <*> pure v

lam :: MonadFresh ID m => LVarName -> m Expr -> m Expr
lam v e = Lam <$> meta <*> pure v <*> e

lAM :: MonadFresh ID m => TyVarName -> m Expr -> m Expr
lAM v e = LAM <$> meta <*> pure v <*> e

let_ :: MonadFresh ID m => LVarName -> m Expr -> m Expr -> m Expr
let_ v a b = Let <$> meta <*> pure v <*> a <*> b

letrec :: MonadFresh ID m => LVarName -> m Expr -> m Type -> m Expr -> m Expr
letrec v a tA b = Letrec <$> meta <*> pure v <*> a <*> tA <*> b

letType :: MonadFresh ID m => TyVarName -> m Type -> m Expr -> m Expr
letType v t e = LetType <$> meta <*> pure v <*> t <*> e

case_ :: MonadFresh ID m => m Expr -> [m CaseBranch] -> m Expr
case_ e brs = Case <$> meta <*> e <*> sequence brs

branch :: MonadFresh ID m => ValConName -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch c vs e = CaseBranch c <$> mapM binding vs <*> e
  where
    binding (name, ty) = Bind <$> meta' ty <*> pure name

prim :: MonadFresh ID m => PrimCon -> m Expr
prim p = PrimCon <$> meta <*> pure p

char :: MonadFresh ID m => Char -> m Expr
char = prim . PrimChar

int :: MonadFresh ID m => Integer -> m Expr
int = prim . PrimInt

con' :: MonadFresh ID m => NonEmpty Name -> Name -> m Expr
con' m n = con $ qualifyName (ModuleName m) n

-- TODO (saturated constructors) once saturation is enforced, this will be
-- renamed to con', and the current con' will be removed (since it creates
-- unsaturated constructors)
conSat' :: MonadFresh ID m => NonEmpty Name -> Name -> [m Type] -> [m Expr] -> m Expr
conSat' m n = conSat $ qualifyName (ModuleName m) n

gvar' :: MonadFresh ID m => NonEmpty Name -> Name -> m Expr
gvar' m n = gvar $ qualifyName (ModuleName m) n

branch' :: MonadFresh ID m => (NonEmpty Name, Name) -> [(LVarName, Maybe TypeCache)] -> m Expr -> m CaseBranch
branch' (m, n) = branch $ qualifyName (ModuleName m) n
