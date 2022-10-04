{-# LANGUAGE ImpredicativeTypes #-}

module Primer.Typecheck.Utils (
  TypeDefError (..),
  TypeDefInfo (..),
  getTypeDefInfo,
  getTypeDefInfo',
  instantiateValCons,
  instantiateValCons',
  lookupConstructor,
  mkTAppCon,
  decomposeTAppCon,
  substituteTypeVars,
  maybeTypeOf,
  typeOf,
  _typecache,
  getGlobalNames,
  getGlobalBaseNames,
) where

import Foreword

import Control.Arrow ((&&&))
import Control.Monad.Fresh (MonadFresh)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Tuple.Extra (fst3)
import Optics (Lens', view, (%))
import Primer.Core (Expr, Expr', GlobalName (baseName, qualifiedModule), ModuleName, TypeCache, _exprMetaLens)
import Primer.Core.Meta (Meta, TyConName, TyVarName, ValConName, _type)
import Primer.Core.Type (Kind, Type' (TApp, TCon, TEmptyHole, THole))
import Primer.Name (Name, NameCounter)
import Primer.Subst (substTy)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefConstructors, astTypeDefParameters),
  TypeDef (TypeDefAST, TypeDefPrim),
  TypeDefMap,
  ValCon (valConArgs, valConName),
  typeDefAST,
  typeDefParameters,
 )
import Primer.Typecheck.Cxt (Cxt, globalCxt, typeDefs)

-- We assume that constructor names are unique, returning the first one we find
lookupConstructor :: TypeDefMap -> ValConName -> Maybe (ValCon, TyConName, ASTTypeDef)
lookupConstructor tyDefs c =
  let allCons = do
        (tc, TypeDefAST td) <- M.assocs tyDefs
        vc <- astTypeDefConstructors td
        pure (vc, tc, td)
   in find ((== c) . valConName . fst3) allCons

-- | @mkTAppCon C [X,Y,Z] = C X Y Z@
mkTAppCon :: TyConName -> [Type' ()] -> Type' ()
mkTAppCon c = foldl' (TApp ()) (TCon () c)

data TypeDefError
  = TDIHoleType -- a type hole
  | TDINotADT -- e.g. a function type etc
  | TDIUnknown TyConName -- not in scope
  | TDINotSaturated -- e.g. @List@ or @List a b@ rather than @List a@

data TypeDefInfo a = TypeDefInfo [Type' a] TyConName TypeDef -- instantiated parameters, and the typedef (with its name), i.e. [Int] are the parameters for @List Int@

getTypeDefInfo :: MonadReader Cxt m => Type' a -> m (Either TypeDefError (TypeDefInfo a))
getTypeDefInfo t = reader $ flip getTypeDefInfo' t . typeDefs

getTypeDefInfo' :: TypeDefMap -> Type' a -> Either TypeDefError (TypeDefInfo a)
getTypeDefInfo' _ (TEmptyHole _) = Left TDIHoleType
getTypeDefInfo' _ (THole _ _) = Left TDIHoleType
getTypeDefInfo' tydefs ty =
  case decomposeTAppCon ty of
    Nothing -> Left TDINotADT
    Just (tycon, params) -> do
      case M.lookup tycon tydefs of
        Nothing -> Left $ TDIUnknown tycon
        Just tydef
          -- this check would be redundant if we were sure that the input type
          -- were of kind KType, alternatively we should do kind checking here
          | length (typeDefParameters tydef) /= length params -> Left TDINotSaturated
          | otherwise -> Right $ TypeDefInfo params tycon tydef

-- | Takes a particular instance of a parameterised type (e.g. @List Nat@), and
-- extracts both both the raw typedef (e.g. @List a = Nil | Cons a (List a)@)
-- and the constructors with instantiated argument types
-- (e.g. @Nil : List Nat ; Cons : Nat -> List Nat -> List Nat@)
instantiateValCons ::
  (MonadFresh NameCounter m, MonadReader Cxt m) =>
  Type' () ->
  m (Either TypeDefError (TyConName, ASTTypeDef, [(ValConName, [Type' ()])]))
instantiateValCons t = do
  tds <- asks typeDefs
  let instCons = instantiateValCons' tds t
      -- Because @(,,) a b@ does not have a Traversable instance
      -- we reassociate so we use the one of @(,) a@
      reassoc (a, b, c) = ((a, b), c)
      reassoc' ((a, b), c) = (a, b, c)
      sequence4 =
        fmap (getCompose . getCompose . getCompose . getCompose)
          . sequence
          . Compose
          . Compose
          . Compose
          . Compose
  -- We eta-expand here to deal with simplified subsumption
  fmap (fmap reassoc') $ sequence4 $ fmap (fmap (fmap $ fmap $ \x -> x) . reassoc) instCons
{- HLINT ignore instantiateValCons "Use id" -}

-- | As 'instantiateValCons', but pulls out the relevant bits of the monadic
-- context into an argument
instantiateValCons' ::
  TypeDefMap ->
  Type' () ->
  Either TypeDefError (TyConName, ASTTypeDef, [(ValConName, forall m. MonadFresh NameCounter m =>  [m (Type' ())])])
instantiateValCons' tyDefs t =
  getTypeDefInfo' tyDefs t
    >>= \(TypeDefInfo params tc def) -> case def of
      TypeDefPrim _ -> Left TDINotADT
      TypeDefAST tda -> do
        let defparams = map fst $ astTypeDefParameters tda
            f :: ValCon -> (ValConName, forall m. MonadFresh NameCounter m => [m (Type' ())])
            -- eta expand to deal with shallow subsumption
            f c = (valConName c, map (\a -> substituteTypeVars (zip defparams params) a) $ valConArgs c)
        pure (tc, tda, map f $ astTypeDefConstructors tda) -- map f $ astTypeDefConstructors tda)
{- HLINT ignore instantiateValCons' "Avoid lambda" -}


substituteTypeVars :: MonadFresh NameCounter m => [(TyVarName, Type' ())] -> Type' () -> m (Type' ())
substituteTypeVars = flip $ foldrM (uncurry substTy)

-- | Decompose @C X Y Z@ to @(C,[X,Y,Z])@
decomposeTAppCon :: Type' a -> Maybe (TyConName, [Type' a])
decomposeTAppCon ty = do
  (con, args) <- go ty
  pure (con, reverse args)
  where
    go (TCon _ con) = Just (con, [])
    go (TApp _ t s) = do
      (con, args) <- go t
      pure (con, s : args)
    go _ = Nothing

-- | Get the (potentially absent) type of an 'Expr'
maybeTypeOf :: Expr -> Maybe TypeCache
maybeTypeOf = view _typecache

-- | A lens for the type annotation of an 'Expr' or 'ExprT'
_typecache :: Lens' (Expr' (Meta a) b) a
_typecache = _exprMetaLens % _type

-- | Get the type of an 'ExprT'
typeOf :: Expr' (Meta TypeCache) (Meta Kind) -> TypeCache
typeOf = view _typecache

-- Helper to create fresh names
getGlobalNames :: MonadReader Cxt m => m (S.Set (ModuleName, Name))
getGlobalNames = do
  tyDefs <- asks typeDefs
  topLevel <- asks $ S.fromList . map f . M.keys . globalCxt
  let ctors =
        Map.foldMapWithKey
          ( \t def ->
              S.fromList $
                (f t :) $
                  map (f . valConName) $
                    maybe [] astTypeDefConstructors $
                      typeDefAST def
          )
          tyDefs
  pure $ S.union topLevel ctors
  where
    f = qualifiedModule &&& baseName

getGlobalBaseNames :: MonadReader Cxt m => m (S.Set Name)
getGlobalBaseNames = S.map snd <$> getGlobalNames
