-- | These functions allow you to create Core types easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Type (
  tEmptyHole,
  thole,
  tcon,
  tforall,
  tlet,
  tfun,
  tapp,
  tvar,
  tcon',
  khole,
  ktype,
  kfun,
  khole',
  ktype',
  kfun',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Primer.Core.DSL.Meta (kmeta, meta)
import Primer.Core.Meta (
  ID,
  ModuleName (ModuleName),
  TyConName,
  TyVarName,
  qualifyName,
 )
import Primer.Core.Type (
  Kind,
  Kind' (KFun, KHole, KType),
  Type,
  Type' (..),
 )
import Primer.Name (Name)

tEmptyHole :: MonadFresh ID m => m Type
tEmptyHole = TEmptyHole <$> meta

thole :: MonadFresh ID m => m Type -> m Type
thole t = THole <$> meta <*> t

tcon :: MonadFresh ID m => TyConName -> m Type
tcon t = TCon <$> meta <*> pure t

tforall :: MonadFresh ID m => TyVarName -> m Kind -> m Type -> m Type
tforall v k t = TForall <$> meta <*> pure v <*> k <*> t

tlet :: MonadFresh ID m => TyVarName -> m Type -> m Type -> m Type
tlet v t b = TLet <$> meta <*> pure v <*> t <*> b

tfun :: MonadFresh ID m => m Type -> m Type -> m Type
tfun a b = TFun <$> meta <*> a <*> b

tapp :: MonadFresh ID m => m Type -> m Type -> m Type
tapp a b = TApp <$> meta <*> a <*> b

tvar :: MonadFresh ID m => TyVarName -> m Type
tvar v = TVar <$> meta <*> pure v

-- | A helper for use in testsuite. With OverloadedStrings one can use literals
-- for both arguments
tcon' :: MonadFresh ID m => NonEmpty Name -> Name -> m Type
tcon' m n = tcon $ qualifyName (ModuleName m) n

khole :: MonadFresh ID m => m Kind
khole = KHole <$> kmeta

ktype :: MonadFresh ID m => m Kind
ktype = KType <$> kmeta

kfun :: MonadFresh ID m => m Kind -> m Kind -> m Kind
kfun a b = KFun <$> kmeta <*> a <*> b

khole' :: MonadFresh ID m => m Kind
khole' = khole

ktype' :: MonadFresh ID m => m Kind
ktype' = ktype

kfun' :: MonadFresh ID m => m Kind -> m Kind -> m Kind
kfun' = kfun
