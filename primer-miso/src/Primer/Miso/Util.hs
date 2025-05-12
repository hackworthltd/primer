{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  optToName,
  stringToOpt,
  assumeDefHasTypeCheckInfo,
  findASTDef,
  assumeDefSelectionHasTypeCheckInfo,
  selectedDefName,
  runMutationWithNullDb,
) where

import Foreword hiding (zero)

import Clay qualified
import Clay.Stylesheet qualified as Clay
import Control.Concurrent.STM (atomically, newTBQueueIO)
import Control.Monad.Extra (eitherM)
import Control.Monad.Fresh (MonadFresh (..))
import Control.Monad.Log (WithSeverity)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bitraversable (bitraverse)
import Data.Map qualified as Map
import Data.UUID.Types qualified as UUID
import GHC.Base (error)
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
  traverseOf,
  (%),
  (.~),
  (^.),
 )
import Optics.State.Operators ((<<%=))
import Primer.API (APILog, Env (Env), edit, runPrimerM)
import Primer.Action.Available (Action)
import Primer.Action.Available qualified as Available
import Primer.App (
  DefSelection (..),
  Editable,
  Level,
  MutationRequest,
  NodeSelection (..),
  NodeType (..),
  Selection,
  Selection' (..),
  TypeDefSelection (..),
 )
import Primer.App qualified
import Primer.Core (
  Expr' (LAM, Lam, Let, LetType, Letrec),
  ExprMeta,
  GVarName,
  GlobalName,
  HasID,
  ID,
  Kind',
  KindMeta,
  LVarName,
  LocalName,
  Meta,
  ModuleName,
  TyConName,
  TyVarName,
  Type' (TEmptyHole, TForall, THole, TLet),
  TypeCache (..),
  TypeCacheBoth (TCBoth, tcChkedAt, tcSynthed),
  TypeMeta,
  getID,
  globalNamePretty,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _exprMeta,
  _exprTypeMeta,
  _type,
  _typeMeta,
 )
import Primer.Database qualified as DB
import Primer.Def (ASTDef (..), Def (..), DefMap, astDefExpr)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Log (runPureLogT)
import Primer.Name (Name, NameCounter)
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck (ExprT, exprTtoExpr, typeTtoType)
import StmContainers.Map qualified as StmMap

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

-- TODO keep an eye out for whether these ever return `Nothing` in practice
-- the current hypothesis is that, because we already typecheck on initialisation and on performing each action,
-- the app only ever deals with typechecked expressions
-- and that it is therefore possible that we could refactor the library to assert this in the Haskell types
-- note that we try to always work with typechecked expressions in the frontend,
-- as being able to assume that metadata is always available makes the core application code simpler
assumeDefHasTypeCheckInfo :: ASTDef -> Maybe ASTDefT
assumeDefHasTypeCheckInfo def = do
  expr <- sequenceOf (_exprMeta % _type) (astDefExpr def) >>= sequenceOf (_exprTypeMeta % _type)
  sig <- sequenceOf (_typeMeta % _type) (astDefType def)
  pure ASTDefT{expr, sig}
assumeDefSelectionHasTypeCheckInfo :: DefSelection (Either ExprMeta (Either TypeMeta KindMeta)) -> Maybe DefSelectionT
assumeDefSelectionHasTypeCheckInfo =
  traverseOf #node $ traverse $ traverseOf #meta $ bitraverse (sequenceOf _type) $ bitraverse (sequenceOf _type) pure

-- see `assumeDefHasTypeCheckInfo`
-- sometimes we need to discard that extra type-level information,
-- in order to get an input for various Primer library functions
astDefTtoAstDef :: ASTDefT -> ASTDef
astDefTtoAstDef def = ASTDef{astDefExpr = exprTtoExpr def.expr, astDefType = typeTtoType def.sig}

-- this is potentially a better API then the one which `Primer.Action.Available` currently exports
availableForSelection ::
  HasID a =>
  TypeDefMap ->
  DefMap ->
  Level ->
  Editable ->
  ASTDef ->
  DefSelection a ->
  [Action]
availableForSelection tydefs defs level editable def defSel = case defSel.node of
  Nothing -> Available.forDef defs level editable defSel.def
  Just nodeSel -> case nodeSel.nodeType of
    BodyNode -> Available.forBody tydefs level editable (astDefExpr def) (getID nodeSel)
    SigNode -> Available.forSig level editable (astDefType def) (getID nodeSel)

-- this part of the actions API needs a re-think
-- (it was perhaps too motivated by what was convenient for our old TypeScript frontend):
-- `context` is a bit weird, and basically just means module (lack of context can mean local name or primitive)
-- `optToName` is used for presented choices - we know these are variables, not chars or ints
-- use of `unsafeMk` functions is necessary even though we need never convert from names to text in the first place
-- `matchesType` is meaningless in `stringToOpt` - we should use different types for presented options and submissions
optToName :: Available.Option -> Either (LocalName l) (GlobalName g)
optToName opt =
  maybe
    (Left . unsafeMkLocalName)
    (curry $ Right . unsafeMkGlobalName)
    opt.context
    opt.option
stringToOpt :: Text -> Available.Option
stringToOpt t = Available.Option t Nothing True

-- should DRY with function of some name in `primer-api`
-- ultimately probably only want the more general `findASTTypeOrTermDef`
findASTDef :: Map GVarName (Editable, Def) -> GVarName -> Either Text (Editable, ASTDef)
findASTDef allDefs def = case allDefs Map.!? def of
  Nothing -> Left $ "unknown def: " <> globalNamePretty def
  Just (_, DefPrim _) -> Left $ "unexpected primitive def: " <> globalNamePretty def
  Just (editable, DefAST d) -> pure (editable, d)

selectedDefName :: Selection -> Either GVarName TyConName
selectedDefName = \case
  SelectionDef d -> Left d.def
  SelectionTypeDef d -> Right d.def

-- this is a bit messy, but it's only temporary since we will soon want a proper database
runMutationWithNullDb :: MutationRequest -> Primer.App.App -> IO (Seq (WithSeverity APILog), Primer.App.App)
runMutationWithNullDb req app = do
  let
    -- these dummy values are enough given that we use the null DB, in one shot, with no other concurrent users
    version = "dummy-version"
    sid = UUID.nil
    queueBound = 1
  lastModified <- DB.getCurrentTime
  sessions <- atomically do
    m <- StmMap.new
    StmMap.insert (DB.SessionData app DB.defaultSessionName lastModified) sid m
    pure m
  dbOpQueue <- newTBQueueIO queueBound
  let runReq = runPureLogT . runPrimerM (edit @_ @APILog sid req) $ Env sessions dbOpQueue version
  let runDB = DB.runNullDb sessions $ DB.serve $ DB.ServiceCfg dbOpQueue version
  (_res, logs) <- either absurd (first $ first Right) <$> race runDB runReq
  res <- atomically $ StmMap.lookup sid sessions -- returning `_res` would mean discarding the ID counter state
  pure (logs, maybe (error "impossible: ") (.sessionApp) res)
