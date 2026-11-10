module Primer.ZipperCxt (
  localVariablesInScopeExpr,
  extractLocalsExprZ,
  extractLocalsTypeZ,
  variablesInScopeTy,
  ShadowedVarsExpr (M),
  ShadowedVarsTy (N),
) where

import Foreword

import Data.Set qualified as Set
import Optics ((%), (^.))
import Primer.Core (
  Bind' (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName,
  GlobalName (baseName),
  Kind' (KHole),
  LVarName,
  LocalName (unLocalName),
  Meta (Meta),
  SomeBind',
  TyVarName,
  Type,
  Type' (..),
  TypeCache (..),
  TypeCacheBoth (..),
  TypeMeta,
  bindName,
  scopedChildrenOf,
  scopedChildrenOfTy,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Utils (forgetKindMetadata, forgetTypeMetadata)
import Primer.Zipper (
  ExprZ,
  FoldAbove,
  TypeLoc,
  TypeLoc' (TInType),
  TypeZ,
  asZipper,
  current,
  foldAbove,
  prior,
  unfocusType,
  unfocusTypeLoc,
 )

-- Helper for variablesInScopeExpr: collect variables, most local first,
-- eliding shadowed variables
-- ["shadowed" here means by name, even though even though internally they are
-- in different namespaces; thus a lambda-bound variable "main" will shadow the
-- globally-defined "main"]
data ShadowedVarsExpr
  = M
      -- | Local type variables
      [(TyVarName, Kind' ())]
      -- | Local term variables
      [(LVarName, Type' () ())]
      -- | Global variables
      [(GVarName, Type' () ())]
  deriving stock (Eq, Show)

instance Semigroup ShadowedVarsExpr where
  M ty1 tm1 gl1 <> M ty2 tm2 gl2 = M (ty1 <> ty2') (tm1 <> tm2') (gl1 <> gl2')
    where
      names1 =
        Set.fromList (map (unLocalName . fst) ty1)
          <> Set.fromList (map (unLocalName . fst) tm1)
          <> Set.fromList (map (baseName . fst) gl1)
      ty2' = filter (flip Set.notMember names1 . unLocalName . fst) ty2
      tm2' = filter (flip Set.notMember names1 . unLocalName . fst) tm2
      gl2' = filter (flip Set.notMember names1 . baseName . fst) gl2

instance Monoid ShadowedVarsExpr where
  mempty = M mempty mempty mempty

-- | Collect the typing context for the focused node.
-- We do this by walking back up the tree, collecting variables as we cross
-- binders.
-- The first list is local type variables, the second list is local term variables.
-- We remove shadowed variables.
-- Note that type/kind information is extracted from the TypeCache.
localVariablesInScopeExpr ::
  Either ExprZ TypeZ ->
  ([(TyVarName, Kind' ())], [(LVarName, Type' () ())])
localVariablesInScopeExpr exprOrTy =
  let M tyvars tmvars _globs = either extractLocalsExprZ extractLocalsTypeZ exprOrTy
   in (reverse tyvars, reverse tmvars) -- keep most-global first

extractLocalsTypeZ :: TypeZ -> ShadowedVarsExpr
extractLocalsTypeZ z =
  let x = variablesInScopeTy $ TInType $ z ^. asZipper
      y = unfocusType z
   in -- walkUpExpr will extract binders strictly containing y
      -- (i.e. if y=λs.t, then 's' won't be reported). Since no
      -- construct both contains a type and binds a variable, this
      -- will not miss anything.
      M (reverse x) [] [] <> extractLocalsExprZ y

extractLocalsExprZ :: ExprZ -> ShadowedVarsExpr
extractLocalsExprZ = foldAbove getBoundHere
  where
    getBoundHere :: FoldAbove Expr -> ShadowedVarsExpr
    getBoundHere e = mconcat [fromBinding (current e) b | (c, bs) <- scopedChildrenOf (current e), c == Left (prior e), b <- bs]

    -- The types and kinds of bound variables are cached in their bindings.
    -- For letrecs and let types, we instead use the annotation and bound type respectively, which are the source of truth.
    fromBinding :: Expr -> SomeBind' ExprMeta TypeMeta -> ShadowedVarsExpr
    fromBinding parent = \case
      Left (Bind m x) -> M [] [(x, case parent of { Letrec _ _ _ ty _ -> forgetTypeMetadata ty; _ -> typeOrHole m })] []
      Right (Bind m a) -> M [(a, case parent of { LetType _ _ ty _ -> kindOrHoleOf ty; _ -> fromMaybe (KHole ()) $ m ^. _type })] [] []

    -- If a node has no type annotation we assign it type TEmptyHole
    typeOrHole :: Meta (Maybe TypeCache) -> Type' () ()
    typeOrHole (Meta _ t _) = typeOrHole' t

    typeOrHole' :: Maybe TypeCache -> Type' () ()
    typeOrHole' = maybe (TEmptyHole ()) uncache

    -- Extract a Type from a TypeCache
    uncache :: TypeCache -> Type' () ()
    uncache (TCSynthed t) = t
    uncache (TCChkedAt t) = t
    uncache (TCEmb TCBoth{tcSynthed = t}) = t

-- If a type has no cached kind we report it has kind KHole
kindOrHoleOf :: Type -> Kind' ()
kindOrHoleOf t = fromMaybe (KHole ()) $ t ^. _typeMetaLens % _type

-- Helper for variablesInScopeTy: collect variables, most local first, eliding
-- shadowed vars, as with 'ShadowedVarsExpr'
newtype ShadowedVarsTy = N [(TyVarName, Kind' ())]
  deriving stock (Eq, Show)

instance Semigroup ShadowedVarsTy where
  N ty1 <> N ty2 = N (ty1 <> ty2')
    where
      names1 = Set.fromList (map fst ty1)
      ty2' = filter (flip Set.notMember names1 . fst) ty2

instance Monoid ShadowedVarsTy where
  mempty = N mempty

-- | As for 'variablesInScopeExpr', but when you are focussed somewhere inside
-- a type, rather than somewhere inside an expr
-- Note that kind information is extracted from the cached kind (for 'TLet')
variablesInScopeTy :: TypeLoc -> [(TyVarName, Kind' ())]
variablesInScopeTy z =
  let N vs = foldAbove getBoundHere $ unfocusTypeLoc z -- no bindings in kinds
   in reverse vs -- keep most-global first
  where
    getBoundHere :: FoldAbove Type -> ShadowedVarsTy
    getBoundHere t = N [(bindName b, kindHere (current t)) | (c, bs) <- scopedChildrenOfTy (current t), c == prior t, b <- bs]
    -- The kind of the variable bound by a type.
    kindHere = \case
      TForall _ _ k _ -> forgetKindMetadata k
      TLet _ _ t' _ -> kindOrHoleOf t'
      _ -> KHole ()
