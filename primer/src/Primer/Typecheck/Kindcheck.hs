module Primer.Typecheck.Kindcheck (
  KindError (..),
  checkKind,
  synthKind,
  Type,
  Kind,
  TypeT,
  KindOrType (..),
  extendLocalCxtTy,
  extendLocalCxtTys,
  lookupLocalTy,
  localTyVars,
  consistentKinds,
  annotate,
) where

import Foreword

import Optics ((%), (.~))
import Control.Monad.Fresh (MonadFresh)
import Control.Monad.NestedError (MonadNestedError, throwError')
import Data.Map qualified as Map
import Primer.Core.DSL.Meta (meta')
import Primer.Core.Meta (_type,ID, LocalName (LocalName), Meta (Meta), TyVarName, unLocalName)
import Primer.Core.Type (
  Kind' (KFun, KHole, KType),
  _kindMeta,
  Type' (TApp, TCon, TEmptyHole, TForall, TFun, THole, TLet, TVar),
 )
import Primer.Name (NameCounter)
import Primer.TypeDef (typeDefKind)
import Primer.Typecheck.Cxt (Cxt (localCxt, smartHoles, typeDefs), Kind, KindOrType (K, T), Type)
import Primer.Typecheck.KindError (
  KindError (
    InconsistentKinds,
    KindDoesNotMatchArrow,
    TLetUnsupported,
    TyVarWrongSort,
    UnknownTypeConstructor,
    UnknownTypeVariable
  ),
 )
import Primer.Typecheck.SmartHoles (SmartHoles (NoSmartHoles, SmartHoles))
import Primer.Core.Type.Utils (forgetKindMetadata)

-- | A shorthand for the constraints needed when kindchecking
type KindM e m =
  ( Monad m
  , MonadReader Cxt m -- has access to a typing context, and SmartHoles option
  , MonadFresh ID m -- can generate fresh IDs
  -- can generate fresh names (needed for "smart holes" and polymorphism)
  , MonadFresh NameCounter m
  , MonadNestedError KindError e m -- can throw kind errors
  )

type TypeT = Type' (Meta (Kind' ())) (Meta ())

lookupLocalTy :: TyVarName -> Cxt -> Either KindError Kind
lookupLocalTy v cxt = case Map.lookup (unLocalName v) $ localCxt cxt of
  Just (K k) -> Right k
  Just (T _) -> Left $ TyVarWrongSort (unLocalName v)
  Nothing -> Left $ UnknownTypeVariable v

localTyVars :: Cxt -> Map TyVarName Kind
localTyVars = Map.mapKeys LocalName . Map.mapMaybe (\case K k -> Just k; T _ -> Nothing) . localCxt

extendLocalCxtTy :: (TyVarName, Kind) -> Cxt -> Cxt
extendLocalCxtTy (name, k) cxt = cxt{localCxt = Map.insert (unLocalName name) (K k) (localCxt cxt)}

extendLocalCxtTys :: [(TyVarName, Kind)] -> Cxt -> Cxt
extendLocalCxtTys x cxt = cxt{localCxt = Map.fromList (bimap unLocalName K <$> x) <> localCxt cxt}

-- Synthesise a kind for the given type
-- TypeHoles are always considered to have kind KHole - a kind hole.
-- When SmartHoles is on, we essentially remove all holes, and re-insert where
-- necessary.
-- However, we take care not to remove a non-empty hole only to immediately
-- re-insert it, since this would needlessly change its ID, resulting in
-- problems if an action left the cursor on such a hole: "lost ID after
-- typechecking". For example, consider (numbers are denoting IDs inside the
-- metadata)
--   synthKind $ TApp 0 (THole 1 (TCon 2 Bool)) t
-- If we removed the hole, we would then note that Bool does not have an arrow
-- kind, and so wrap it in a hole again, returning something like
--   TApp 0 (THole 3 (TCon 2 Bool)) t
-- A similar thing would happen with
--   synthKind $ TApp 0 (TCon 1 List) (THole 2 (TCon 3 List))
-- because we do not have checkKind KType List
synthKind :: KindM e m => Type' (Meta a) (Meta b) -> m (Kind, TypeT)
synthKind = \case
  TEmptyHole m -> pure (KHole (), TEmptyHole (annotate (KHole ()) m))
  THole m t -> do
    sh <- asks smartHoles
    (k, t') <- synthKind t
    case sh of
      NoSmartHoles -> pure (KHole (), THole (annotate (KHole ()) m) t')
      SmartHoles -> pure (k, t')
  TCon m c -> do
    typeDef <- asks (Map.lookup c . typeDefs)
    case typeDef of
      Nothing -> throwError' $ UnknownTypeConstructor c
      Just def -> let k = typeDefKind def in pure (k, TCon (annotate k m) c)
  TFun m a b -> do
    a' <- checkKind (KType ()) a
    b' <- checkKind (KType ()) b
    pure (KType (), TFun (annotate (KType ()) m) a' b')
  TVar m v -> do
    asks (lookupLocalTy v) >>= \case
      Right k -> pure (k, TVar (annotate k m) v)
      Left err -> throwError' err
  TApp ma (THole mh s) t -> do
    -- If we didn't have this special case, we might remove this hole (in a
    -- recursive call), only to reintroduce it again with a different ID
    -- TODO: ugly and duplicated...
    sh <- asks smartHoles
    (k, s') <- synthKind s
    case (matchArrowKind k, sh) of
      (_, NoSmartHoles) -> checkKind (KHole ()) t >>= \t' -> pure (KHole (), TApp (annotate (KHole ()) ma) (THole (annotate (KHole ()) mh) s') t')
      (Nothing, SmartHoles) -> checkKind (KHole ()) t >>= \t' -> pure (KHole (), TApp (annotate (KHole ()) ma) (THole (annotate (KHole ()) mh) s') t')
      (Just (k1, k2), SmartHoles) -> checkKind k1 t >>= \t' -> pure (k2, TApp (annotate k2 ma) s' t')
  TApp m s t -> do
    sh <- asks smartHoles
    (k, s') <- synthKind s
    case (matchArrowKind k, sh) of
      (Nothing, NoSmartHoles) -> throwError' $ KindDoesNotMatchArrow k
      (Nothing, SmartHoles) -> do
        sWrap <- THole <$> meta' (KHole ()) <*> pure s'
        t' <- checkKind (KHole ()) t
        pure (KHole (), TApp (annotate (KHole ()) m) sWrap t')
      (Just (k1, k2), _) -> checkKind k1 t >>= \t' -> pure (k2, TApp (annotate k2 m) s' t')
  TForall m n k t -> do
    t' <- local (extendLocalCxtTy (n, forgetKindMetadata k)) $ checkKind (KType ()) t
    pure (KType (), TForall (annotate (KType ()) m) n (k & _kindMeta % _type .~ ()) t')
  TLet{} -> throwError' TLetUnsupported

checkKind :: KindM e m => Kind -> Type' (Meta a) (Meta b) -> m TypeT
checkKind k (THole m t) = do
  -- If we didn't have this special case, we might remove this hole (in a
  -- recursive call), only to reintroduce it again with a different ID
  -- TODO: ugly and duplicated...
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (_, NoSmartHoles) -> pure $ THole (annotate (KHole ()) m) t'
    (True, SmartHoles) -> pure t'
    (False, SmartHoles) -> pure $ THole (annotate (KHole ()) m) t'
checkKind k t = do
  sh <- asks smartHoles
  (k', t') <- synthKind t
  case (consistentKinds k k', sh) of
    (True, _) -> pure t'
    (False, NoSmartHoles) -> throwError' $ InconsistentKinds k k'
    (False, SmartHoles) -> THole <$> meta' (KHole ()) <*> pure t'

-- | Extend the metadata of an 'Expr' or 'Type'
-- (usually with a 'TypeCache' or 'Kind')
annotate :: b -> Meta a -> Meta b
annotate t (Meta i _ v) = Meta i t v

matchArrowKind :: Kind -> Maybe (Kind, Kind)
matchArrowKind (KHole ()) = pure (KHole (), KHole ())
matchArrowKind (KType ()) = Nothing
matchArrowKind (KFun () k1 k2) = pure (k1, k2)

consistentKinds :: Kind -> Kind -> Bool
consistentKinds (KHole ()) _ = True
consistentKinds _ (KHole ()) = True
consistentKinds (KType ()) (KType ()) = True
consistentKinds (KFun () k1 k2) (KFun () k1' k2') = consistentKinds k1 k1' && consistentKinds k2 k2'
consistentKinds _ _ = False
