{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Things which should really be upstreamed rather than living in this project.
module Primer.Miso.Util (
  startAppWithSavedState,
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
  availableForSelection,
  astDefTtoAstDef,
  defSelectionTtoDefSelection,
  optToName,
  stringToOpt,
  defSelectionTtoDefSelection',
  assumeDefHasTypeCheckInfo,
) where

import Foreword hiding (zero)

import Clay qualified
import Clay.Stylesheet qualified as Clay
import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh (MonadFresh (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
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
  sequenceOf,
  (%),
  (%~),
  (.~),
  (^.),
 )
import Optics.State.Operators ((<<%=))
import Primer.Action.Available (Action)
import Primer.Action.Available qualified as Available
import Primer.App (DefSelection (..), Editable, Level, NodeSelection (..), NodeType (..), Prog, tcWholeProgWithImports)
import Primer.Core (
  Expr' (LAM, Lam, Let, LetType, Letrec),
  ExprMeta,
  GlobalName,
  ID,
  Kind',
  KindMeta,
  LVarName,
  LocalName,
  Meta,
  ModuleName,
  TyVarName,
  Type' (TEmptyHole, TForall, THole, TLet),
  TypeCache (..),
  TypeCacheBoth (TCBoth, tcChkedAt, tcSynthed),
  TypeMeta,
  getID,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _exprMeta,
  _exprTypeMeta,
  _type,
  _typeMeta,
 )
import Primer.Def (ASTDef (..), DefMap, astDefExpr)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Name (Name, NameCounter)
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck (ExprT, TypeError, exprTtoExpr, typeTtoType)

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

{- Clay -}

-- https://github.com/sebastiaanvisser/clay/issues/208
-- note that we silently ignore non-properties, and modifiers on properties
-- what we really want is for Clay property functions to return something much more precise than `Css`
-- but this would be a big breaking change, and Clay is really designed primarily for generating stylesheets
clayToMiso :: Clay.Css -> Map Text Text
clayToMiso =
  Map.fromList
    . concatMap \case
      Clay.Property _modifiers (Clay.Key k) (Clay.Value v) -> (,) <$> allPrefixes k <*> allPrefixes v
        where
          allPrefixes = \case
            Clay.Prefixed ts -> map (uncurry (<>)) ts
            Clay.Plain t -> pure t
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

tcBasicProg :: Prog -> Either TypeError Prog
tcBasicProg p = runTC $ tcWholeProgWithImports p

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

-- TODO this isn't quite analogous to `forgetMetadata`...
-- are there other places where we do this sort of thing?
astDefTtoAstDef :: ASTDefT -> ASTDef
astDefTtoAstDef def =
  ASTDef
    { astDefExpr = exprTtoExpr def.expr
    , astDefType = typeTtoType def.sig
    }
defSelectionTtoDefSelection :: DefSelectionT -> DefSelection ID
defSelectionTtoDefSelection = fmap getID
defSelectionTtoDefSelection' :: DefSelectionT -> DefSelection (Either ExprMeta (Either TypeMeta KindMeta))
defSelectionTtoDefSelection' = fmap (bimap exprMetaTtoExprMeta (bimap typeMetaTtoTypeMeta kindMetaTtoKindMeta))
exprMetaTtoExprMeta :: ExprMetaT -> ExprMeta
exprMetaTtoExprMeta = _type %~ Just
typeMetaTtoTypeMeta :: TypeMetaT -> TypeMeta
typeMetaTtoTypeMeta = _type %~ Just
kindMetaTtoKindMeta :: KindMetaT -> KindMeta
kindMetaTtoKindMeta = identity

-- TODO this is where it gets interesting
-- if we find in practice that this is never `Nothing` (because typechecking happens on creation and actions?)
-- then that means it should be possible to refactor all the internal types so we can ensure everythins is always TCed
assumeDefHasTypeCheckInfo :: ASTDef -> Maybe ASTDefT
assumeDefHasTypeCheckInfo def = do
  expr <- sequenceOf (_exprMeta % _type) (astDefExpr def) >>= sequenceOf (_exprTypeMeta % _type)
  sig <- sequenceOf (_typeMeta % _type) (astDefType def)
  pure ASTDefT{expr, sig}

availableForSelection ::
  TypeDefMap ->
  DefMap ->
  Level ->
  Editable ->
  ASTDef ->
  DefSelectionT ->
  [Action]
availableForSelection tydefs defs level editable def defSel = case defSel.node of
  Nothing -> Available.forDef defs level editable defSel.def
  Just nodeSel -> case nodeSel.nodeType of
    BodyNode -> Available.forBody tydefs level editable (astDefExpr def) (getID nodeSel)
    SigNode -> Available.forSig level editable (astDefType def) (getID nodeSel)

-- TODO improve core API so this becomes unnecessary?
-- the whole `context` thing is a bit weird - it really does just mean module
-- this would mean our first instance of breaking the REST API, unless we definte an API-level option type instead
-- note that context can mean a primitive rather than a local name
optToName :: Available.Option -> Either (LocalName l) (GlobalName g)
optToName opt =
  -- TODO how to avoid unsafety? I guess just never throw away the fact these come from variables in the first place
  maybe
    (Left . unsafeMkLocalName) -- this function is used for presented choices - we know these are vars not chars or ints
    (curry $ Right . unsafeMkGlobalName)
    opt.context
    opt.option
stringToOpt :: Text -> Available.Option
stringToOpt t =
  Available.Option
    t
    Nothing
    -- TODO another reason why this is a stupid API is that this field is meaningless here
    -- there should really be different types for presented options and submissions
    -- I did notice this while working on the old frontend but never got around to fixing it
    True
