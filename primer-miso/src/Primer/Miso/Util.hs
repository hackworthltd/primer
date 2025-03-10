{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Things which should really be upstreamed rather than living in this project.
module Primer.Miso.Util (
  componentWithSavedState,
  ComponentWithSavedState,
  mailComponentWithSavedState,
  embedWithId,
  clayToMiso,
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
  ModuleT (..),
  kindsInType,
  bindingsInExpr,
  typeBindingsInExpr,
  bindingsInType,
  nodeSelectionType,
  DefSelectionT,
  realToClay,
) where

import Foreword hiding (zero)

import Clay qualified
import Clay.Stylesheet qualified as Clay
import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Tuple.Extra (both)
import Linear (Additive, R1 (_x), R2 (_y), V2, zero)
import Linear.Affine (Point (..), unP)
import Miso (
  App (initialAction, model, subs, update, view),
  Component,
  ComponentOptions (..),
  JSM,
  View,
  component,
  componentOptions,
  consoleLog,
  embedWith,
  getLocalStorage,
  id_,
  mail,
  mapSub,
  setLocalStorage,
  text,
  (<#),
 )
import Miso.String (MisoString, ms)
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
import Primer.App (DefSelection, NodeSelection (meta), Prog, progCxt)
import Primer.Core (
  Expr' (LAM, Lam, Let, LetType, Letrec),
  ID,
  Kind' (KType),
  LVarName,
  Meta,
  ModuleName,
  TyVarName,
  Type' (TEmptyHole, TForall, THole, TLet),
  TypeCache (..),
  TypeCacheBoth (TCBoth, tcChkedAt, tcSynthed),
  _type,
 )
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (ASTDef (..), astDefExpr, defAST)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Module (Module (moduleName), moduleDefs)
import Primer.Name (Name, NameCounter)
import Primer.Typecheck (ExprT, TypeError, check, checkKind)

{- Miso -}

data ComponentWithSavedStateAction m a
  = Start
  | SetLoadedState m
  | InnerAction a
  | NoOp
data ComponentWithSavedStateModel m
  = NotStarted
  | Running m
  deriving stock (Eq)
type ComponentWithSavedState i m a =
  Component
    i
    (ComponentWithSavedStateModel m)
    (ComponentWithSavedStateAction m a)

mailComponentWithSavedState :: ComponentWithSavedState name m a -> a -> JSM ()
mailComponentWithSavedState c = mail c . InnerAction

-- https://github.com/dmjio/miso/issues/749
componentWithSavedState ::
  forall id model action.
  (KnownSymbol id, FromJSON model, ToJSON model) =>
  Miso.App model action ->
  ComponentWithSavedState id model action
componentWithSavedState app =
  component
    app
      { model = NotStarted
      , update = \case
          Start ->
            const $
              NotStarted
                <# ( fmap (SetLoadedState . fromMaybe app.model)
                      . eitherM
                        (\e -> consoleLog ("saved state not loaded: " <> ms e) >> pure Nothing)
                        (pure . Just)
                      $ getLocalStorage @model storageKey
                   )
          SetLoadedState m -> const $ pure $ Running m
          InnerAction a -> \case
            NotStarted ->
              NotStarted <# do
                consoleLog $ "warning: componentWithSavedState received action before fully loading: " <> componentId
                pure NoOp
            Running m -> do
              m' <- first InnerAction $ app.update a m
              Running m' <# do
                setLocalStorage storageKey m
                pure NoOp
          NoOp -> pure
      , subs = mapSub InnerAction <$> app.subs
      , view = \case
          Running m -> fmap InnerAction . app.view $ m
          NotStarted -> text "loading saved state"
      , initialAction = Start
      }
  where
    componentId = ms $ symbolVal $ Proxy @id
    storageKey = "miso-app-state-" <> componentId

embedWithId :: forall name m a action. (Eq m, KnownSymbol name) => Component name m a -> View action
embedWithId = flip embedWith componentOptions{attributes = [id_ $ ms $ symbolVal $ Proxy @name]}

{- Clay -}

-- https://github.com/sebastiaanvisser/clay/issues/208
-- note that we silently ignore non-properties, and modifiers on properties
-- what we really want is for Clay property functions to return something much more precise than `Css`
-- but this would be a big breaking change, and Clay is really designed primarily for generating stylesheets
clayToMiso :: Clay.Css -> Map MisoString MisoString
clayToMiso =
  Map.fromList
    . concatMap \case
      Clay.Property _modifiers (Clay.Key k) (Clay.Value v) -> (,) <$> allPrefixes k <*> allPrefixes v
        where
          allPrefixes = \case
            Clay.Prefixed ts -> map (uncurry (<>) . both ms) ts
            Clay.Plain t -> pure $ ms t
      _ -> []
    . Clay.runS

realToClay :: Real a => a -> Clay.Number
realToClay = Clay.Number . realToFixed

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
tcBasicProg :: Prog -> Module -> Either TypeError ModuleT
tcBasicProg p m =
  runTC
    . flip (runReaderT @_ @(M TypeError)) (progCxt p)
    $ ModuleT (moduleName m) <$> for (Map.mapMaybe defAST $ moduleDefs m) \ASTDef{..} ->
      ASTDefT
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
type DefSelectionT = DefSelection (TermMeta' ExprMetaT TypeMetaT KindMetaT)
type NodeSelectionT = NodeSelection (TermMeta' ExprMetaT TypeMetaT KindMetaT)
type ExprMetaT = Meta TypeCache
type TypeMetaT = Meta (Kind' ())
type KindMetaT = Meta ()
data ASTDefT = ASTDefT {expr :: ExprT, sig :: TypeT} -- TODO parameterise `ASTDef` etc.?
  deriving stock (Eq, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ASTDefT
data ModuleT = ModuleT -- TODO include type defs and primitives
  { name :: ModuleName
  , defs :: Map Name ASTDefT
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ModuleT

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
