{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Things which should really be upstreamed rather than living in this project.
module Primer.Miso.Util (
  startAppWithSavedState,
  P2,
  unitX,
  unit_X,
  unitY,
  unit_Y,
  tcBasicProg,
  runTC,
  TypeT,
  TermMeta',
  NodeSelectionT,
  ExprMetaT,
  TypeMetaT,
  KindMetaT,
  ASTDefT (..),
  kindsInType,
  bindingsInExpr,
  typeBindingsInExpr,
  bindingsInType,
  nodeSelectionType,
) where

import Foreword hiding (zero)

import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (FromJSON, ToJSON)
import Linear (Additive, R1 (_x), R2 (_y), V2, zero)
import Linear.Affine (Point (..), unP)
import Miso (
  App (initialAction, model, subs, update, view),
  JSM,
  getLocalStorage,
  mapSub,
  setLocalStorage,
  startApp,
  (<#),
 )
import Optics (
  AffineTraversal',
  Field1 (_1),
  Field2 (_2),
  atraversalVL,
  lensVL,
  (.~),
  (^.),
 )
import Optics.State.Operators ((<<%=))
import Primer.App (NodeSelection (meta), Prog, progCxt)
import Primer.Core (
  Expr' (LAM, Lam, Let, LetType, Letrec),
  ID,
  Kind' (KType),
  LVarName,
  Meta,
  TyVarName,
  Type' (TEmptyHole, TForall, THole, TLet),
  TypeCache (..),
  TypeCacheBoth (TCBoth, tcChkedAt, tcSynthed),
  _type,
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (ASTDef (..), astDefExpr)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (NameCounter)
import Primer.Typecheck (ExprT, TypeError, check, checkKind)

{- Miso -}

-- https://github.com/dmjio/miso/issues/749
startAppWithSavedState :: forall model action. (Eq model, FromJSON model, ToJSON model) => Miso.App model action -> JSM ()
startAppWithSavedState app = do
  savedModel <-
    eitherM (\e -> liftIO $ putStrLn ("saved state not loaded: " <> e) >> pure Nothing) (pure . Just) $
      getLocalStorage storageKey
  startApp
    app
      { model = fromMaybe app.model savedModel
      , update = \case
          Nothing -> pure
          Just a -> \m -> do
            m' <- first Just $ app.update a m
            m' <# do
              setLocalStorage storageKey m'
              pure Nothing
      , subs = mapSub Just <$> app.subs
      , view = fmap Just . app.view
      , initialAction = Just app.initialAction
      }
  where
    storageKey = "miso-app-state"

{- Linear -}

-- defined in `diagrams` but no clear reason why they aren't in `linear` itself
-- https://github.com/ekmett/linear/issues/180
type P2 = Point V2
unitX :: (R1 v, Additive v, Num n) => v n
unitX = zero & lensVL _x .~ 1
unit_X :: (R1 v, Additive v, Num n) => v n
unit_X = zero & lensVL _x .~ (-1)
unitY :: (R2 v, Additive v, Num n) => v n
unitY = zero & lensVL _y .~ 1
unit_Y :: (R2 v, Additive v, Num n) => v n
unit_Y = zero & lensVL _y .~ (-1)

-- https://github.com/ekmett/linear/issues/181
-- this style would be simplest but isn't possible due to the implementation of `OverloadedRecordDot`:
-- instance R1 t => HasField "x" (t a) a where
--     getField = flip (^.) $ lensVL _x
-- this might be too ad-hoc to get accepted upstream:
instance HasField "x" (V2 a) a where
  getField = (^. lensVL _x)
instance (HasField "x" (f a) a) => HasField "x" (Point f a) a where
  getField = getField @"x" . unP
instance HasField "y" (V2 a) a where
  getField = (^. lensVL _y)
instance (HasField "y" (f a) a) => HasField "y" (Point f a) a where
  getField = getField @"y" . unP

{- Primer -}

-- `tcWholeProg` throws away information by not returning a prog containing `ExprT`s
-- we use `check` since, for whatever reason, `synth` deletes the case branches in `map`
tcBasicProg :: Prog -> ASTDef -> Either TypeError ASTDefT
tcBasicProg p ASTDef{..} =
  runTC
    . flip (runReaderT @_ @(M TypeError)) (progCxt p)
    $ ASTDefT
      <$> check (forgetTypeMetadata astDefType) astDefExpr
      <*> checkKind (KType ()) astDefType

-- TODO this is all basically copied from unexposed parts of Primer library - find a way to expose
newtype M e a = M {unM :: StateT (ID, NameCounter) (Except e) a}
  deriving newtype (Functor, Applicative, Monad, MonadError e)
instance MonadFresh ID (M e) where
  fresh = M $ _1 <<%= succ
instance MonadFresh NameCounter (M e) where
  fresh = M $ _2 <<%= succ
runTC :: M e a -> Either e a
runTC = runExcept . flip evalStateT (0, toEnum 0) . (.unM)

-- analogous with `ExprT`/`TypeT`
-- type KindT = Kind' KindMetaT
-- type SelectionT = Selection' (Either ExprMetaT (Either TypeMetaT KindMetaT))
type TypeT = Type' TypeMetaT KindMetaT -- TODO actually exists in Primer lib but is hidden
type TermMeta' a b c = Either a (Either b c) -- TODO make this a proper sum type
type NodeSelectionT = NodeSelection (TermMeta' ExprMetaT TypeMetaT KindMetaT)
type ExprMetaT = Meta TypeCache
type TypeMetaT = Meta (Kind' ())
type KindMetaT = Meta ()
data ASTDefT = ASTDefT {expr :: ExprT, sig :: TypeT} -- TODO parameterise `ASTDef` etc.?
  deriving stock (Eq, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ASTDefT

-- analogous to `typesInExpr`
kindsInType :: AffineTraversal' (Type' a b) (Kind' b)
kindsInType = atraversalVL $ \point f -> \case
  TForall m a k t -> flip (TForall m a) t <$> f k
  e -> point e

-- TODO if we had first-class bindings, we could probably implement all of these generically
bindingsInExpr :: AffineTraversal' (Expr' a b c) LVarName
bindingsInExpr = atraversalVL $ \point f -> \case
  Lam m v e -> f v <&> \v' -> Lam m v' e
  Let m v e1 e2 -> f v <&> \v' -> Let m v' e1 e2
  Letrec m v e1 t e2 -> f v <&> \v' -> Letrec m v' e1 t e2
  e -> point e
typeBindingsInExpr :: AffineTraversal' (Expr' a b c) TyVarName
typeBindingsInExpr = atraversalVL $ \point f -> \case
  LAM m v e -> f v <&> \v' -> LAM m v' e
  LetType m v t e -> f v <&> \v' -> LetType m v' t e
  e -> point e
bindingsInType :: AffineTraversal' (Type' a b) TyVarName
bindingsInType = atraversalVL $ \point f -> \case
  TForall m v k t -> f v <&> \v' -> TForall m v' k t
  TLet m v t1 t2 -> f v <&> \v' -> TLet m v' t1 t2
  e -> point e

-- TODO generalise to full selections and DRY with `getSelectionTypeOrKind` from `primer-api`
nodeSelectionType :: NodeSelectionT -> Either (Type' () ()) (Either (Kind' ()) ())
nodeSelectionType =
  bimap
    (getAPIType . (^. _type))
    (bimap (^. _type) (^. _type))
    . (.meta)
  where
    -- copied directly from innards of `getSelectionTypeOrKind`
    getAPIType :: TypeCache -> Type' () ()
    getAPIType = \case
      TCSynthed t -> t
      TCChkedAt t -> t
      TCEmb (TCBoth{tcSynthed, tcChkedAt})
        -- If this node is an embedding, we have a choice of two types to report.
        -- We choose the one that is not a hole;
        | isHole tcSynthed -> tcChkedAt
        | isHole tcChkedAt -> tcSynthed
        -- if neither is a hole (in which case the two are consistent), we choose the synthed type
        | otherwise -> tcSynthed
      where
        isHole :: Type' a b -> Bool
        isHole = \case
          THole{} -> True
          TEmptyHole{} -> True
          _ -> False
