-- | These functions allow you to create Core types easily, without having
-- to worry about generating unique IDs.
module Primer.Core.DSL.Type (
  tEmptyHole,
  thole,
  tcon,
  tforall,
  tfun,
  tapp,
  tvar,
  tcon',
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Primer.Core.Meta (
  ID,
  Meta (..),
  ModuleName (ModuleName),
  TyConName,
  TyVarName,
  qualifyName,
 )
import Primer.Core.Type (
  Kind,
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

-- | A helper for use in testsuite. With OverloadedStrings one can use literals
-- for both arguments
tcon' :: MonadFresh ID m => NonEmpty Name -> Name -> m Type
tcon' m n = tcon $ qualifyName (ModuleName m) n
