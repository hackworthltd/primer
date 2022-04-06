-- | These functions allow you to create Core expressions easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL (
  emptyHole,
  hole,
  ann,
  app,
  aPP,
  con,
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
  char,
  int,
  bool_,
  nat,
  maybe_,
  list_,
  tEmptyHole,
  thole,
  tcon,
  tforall,
  tfun,
  tapp,
  tvar,
  meta,
  meta',
  create,
  setMeta,
  S,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Numeric.Natural (Natural)
import Optics (set)
import Primer.Core (
  Bind' (..),
  CaseBranch,
  CaseBranch' (..),
  Expr,
  Expr' (..),
  GVarName,
  ID,
  Kind,
  LVarName,
  Meta (..),
  PrimCon (..),
  TmVarRef (..),
  TyConName,
  TyVarName,
  Type,
  Type' (..),
  TypeCache,
  ValConName,
  Value,
  _metadata,
 )

newtype S a = S {unS :: State ID a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFresh ID S where
  fresh = S $ do
    i <- get
    put (i + 1)
    pure i

-- | Evaluate a DSL expression with a starting ID of 0, producing an 'Expr' and
-- the next available fresh 'ID'. You should only need to use this in tests.
create :: S a -> (a, ID)
create = flip runState 0 . unS

setMeta :: Functor m => Value -> m Expr -> m Expr
setMeta m e = set _metadata (Just m) <$> e

app :: MonadFresh ID m => m Expr -> m Expr -> m Expr
app e1 e2 = App <$> meta <*> e1 <*> e2

aPP :: MonadFresh ID m => m Expr -> m Type -> m Expr
aPP e t = APP <$> meta <*> e <*> t

hole :: MonadFresh ID m => m Expr -> m Expr
hole e = Hole <$> meta <*> e

emptyHole :: MonadFresh ID m => m Expr
emptyHole = EmptyHole <$> meta

ann :: MonadFresh ID m => m Expr -> m Type -> m Expr
ann e t = Ann <$> meta <*> e <*> t

con :: MonadFresh ID m => ValConName -> m Expr
con c = Con <$> meta <*> pure c

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

char :: MonadFresh ID m => Char -> m Expr
char c = PrimCon <$> meta <*> pure (PrimChar c)

int :: MonadFresh ID m => Integer -> m Expr
int n = PrimCon <$> meta <*> pure (PrimInt n)

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> meta

thole :: MonadFresh ID m => m Type -> m Type
thole t = THole <$> meta <*> t

tcon :: MonadFresh ID m => TyConName -> m Type
tcon t = TCon <$> meta <*> pure t

tforall :: MonadFresh ID m => TyVarName -> Kind -> m Type -> m Type
tforall v k t = TForall <$> meta <*> pure v <*> pure k <*> t

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> meta <*> a <*> b

tapp :: MonadFresh ID m => m Type -> m Type -> m Type
tapp a b = TApp <$> meta <*> a <*> b

tvar :: MonadFresh ID m => TyVarName -> m Type
tvar v = TVar <$> meta <*> pure v

meta :: MonadFresh ID m => m (Meta (Maybe a))
meta = meta' Nothing

meta' :: MonadFresh ID m => a -> m (Meta a)
meta' a = Meta <$> fresh <*> pure a <*> pure Nothing

-- These functions rely on particular types being in scope.
bool_ :: MonadFresh ID m => Bool -> m Expr
bool_ b = con $ if b then "True" else "False"
nat :: MonadFresh ID m => Natural -> m Expr
nat = \case
  0 -> con "Zero"
  n -> app (con "Succ") $ nat (n - 1)
maybe_ :: MonadFresh ID m => m Type -> (a -> m Expr) -> Maybe a -> m Expr
maybe_ t f = \case
  Nothing -> con "Nothing" `aPP` t
  Just x -> con "Just" `aPP` t `app` f x
list_ :: MonadFresh ID m => TyConName -> [m Expr] -> m Expr
list_ t =
  foldr
    ( \a b ->
        con "Cons"
          `aPP` tcon t
          `app` a
          `app` b
    )
    (con "Nil" `aPP` tcon t)
