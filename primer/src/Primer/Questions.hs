{-# LANGUAGE GADTs #-}

-- | Logic for answering questions about the program.
module Primer.Questions (
  Question (..),
  variablesInScopeExpr,
  variablesInScopeTy,
  ShadowedVarsExpr (..), -- only exported for testing
  ShadowedVarsTy (..), -- only exported for testing
  generateNameExpr,
  generateNameTy,
  generateNameTyAvoiding,
  uniquify,
) where

import Foreword

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Primer.Core (
  GVarName,
  ID,
  Kind' (KFun, KType),
  LVarName,
  TyVarName,
  Type' (..),
 )
import Primer.Core.Transform (decomposeTAppCon)
import Primer.Def (
  DefMap,
  defType,
 )
import Primer.Name (Name, unName, unsafeMkName)
import Primer.Name.Fresh (mkAvoidForFreshName, mkAvoidForFreshNameTy, mkAvoidForFreshNameTypeZ)
import Primer.TypeDef (typeDefNameHints)
import Primer.Typecheck.Cxt (Cxt, typeDefs)
import Primer.Zipper (
  BindLoc' (BindCase),
  KindTZ,
  Loc,
  Loc' (InBind, InExpr, InKind, InType),
  TypeZip,
  unfocusCaseBind,
  unfocusKind,
 )
import Primer.ZipperCxt (
  ShadowedVarsExpr (M),
  ShadowedVarsTy (N),
  extractLocalsExprZ,
  extractLocalsTypeZ,
  variablesInScopeTy,
 )

-- | The type of questions which return information about the program, but do not
-- modify it.
data Question a where
  -- Given the Name of a definition and the ID of a type or expression in that
  -- definition, what variables are in scope at the expression?
  -- Nested pairs: to make serialization to PS work easily
  VariablesInScope ::
    GVarName ->
    ID ->
    Question
      ( ( [(TyVarName, Kind' ())]
        , [(LVarName, Type' () ())]
        )
      , [(GVarName, Type' () ())]
      )
  GenerateName ::
    GVarName ->
    ID ->
    Either (Maybe (Type' () ())) (Maybe (Kind' ())) ->
    Question [Name]

-- | Collect the typing context for the focused node.
-- We do this by walking back up the tree, collecting variables as we cross
-- binders.
-- We also return any global variables which are in scope (which currently is all of them)
-- The first list is local type variables, the second list is local term variables,
-- the third is globals.
variablesInScopeExpr ::
  DefMap ->
  Loc ->
  ([(TyVarName, Kind' ())], [(LVarName, Type' () ())], [(GVarName, Type' () ())])
variablesInScopeExpr defs loc =
  let locals = case loc of
        InExpr ze -> extractLocalsExprZ ze
        InType zt -> extractLocalsTypeZ zt
        InKind zk -> extractLocalsTypeZ $ unfocusKind zk
        InBind (BindCase zb) -> extractLocalsExprZ $ unfocusCaseBind zb
      globals = Map.assocs $ fmap defType defs
      M tyvars tmvars globs = locals <> M [] [] globals
   in (reverse tyvars, reverse tmvars, globs) -- keep most-global first

generateNameExpr ::
  MonadReader Cxt m =>
  Either (Maybe (Type' () ())) (Maybe (Kind' ())) ->
  Loc ->
  m [Name]
-- NB: it makes perfect sense to ask for a type variable (Either is Right)
-- in a term context (Loc is InExpr): we could be inserting a LAM.
-- It doesn't make sense to ask for a term variable in a type context,
-- but it also doesn't harm to support it.
generateNameExpr tk z = uniquifyMany <$> getAvoidSet z <*> baseNames tk

generateNameTy ::
  MonadReader Cxt m =>
  Either (Maybe (Type' () ())) (Maybe (Kind' ())) ->
  Either TypeZip KindTZ ->
  m [Name]
generateNameTy = generateNameTyAvoiding []

generateNameTyAvoiding ::
  MonadReader Cxt m =>
  [Name] ->
  Either (Maybe (Type' () ())) (Maybe (Kind' ())) ->
  Either TypeZip KindTZ ->
  m [Name]
-- It doesn't really make sense to ask for a term variable (Left) here, but
-- it doesn't harm to support it
generateNameTyAvoiding avoiding tk z =
  uniquifyMany <$> ((Set.fromList avoiding <>) <$> mkAvoidForFreshNameTy z) <*> baseNames tk

baseNames ::
  MonadReader Cxt m =>
  Either (Maybe (Type' () ())) (Maybe (Kind' ())) ->
  m [Name]
baseNames tk = do
  tys <- asks typeDefs
  pure $ case tk of
    Left (Just ty)
      | Just c <- headCon ty
      , Just hints@(_ : _) <- typeDefNameHints <$> Map.lookup c tys ->
          hints
    Left (Just TFun{}) -> ["f", "g", "h"]
    Left _ -> ["x", "y", "z"]
    Right (Just (KType{})) -> ["α", "β", "γ"]
    Right (Just (KFun{})) -> ["f", "m", "t"]
    Right _ -> ["α", "β", "γ"]
  where
    headCon = fmap fst . decomposeTAppCon

getAvoidSet :: MonadReader Cxt m => Loc -> m (Set.Set Name)
getAvoidSet = \case
  InExpr ze -> mkAvoidForFreshName ze
  InType zt -> mkAvoidForFreshNameTypeZ zt
  InKind zk -> mkAvoidForFreshNameTypeZ $ unfocusKind zk
  InBind (BindCase zb) -> mkAvoidForFreshName $ unfocusCaseBind zb

-- | Adds a numeric suffix to a name to be distinct from a given set.
-- (If the name is already distinct then return it unmodified.)
uniquify :: Set.Set Name -> Name -> Name
uniquify avoid = snd . uniquify' avoid

-- A helper for uniquify and uniquifyMany
-- We do not use Name.freshName as we don't want a global fresh counter
-- (and we want to control the base name)
uniquify' :: Set.Set Name -> Name -> (Integer, Name)
uniquify' avoid = go
  where
    -- Replace use of `unsafeHead` here. See:
    -- https://github.com/hackworthltd/primer/issues/147
    go n = unsafeHead [(i, f n i) | i <- [0 ..], f n i `Set.notMember` avoid]
    f :: Name -> Integer -> Name
    f n = \case
      0 -> n
      i -> unsafeMkName $ unName n <> show i

-- | Adds a numeric suffix to each name so they are distinct from the given set.
-- Returns the thus-constructed names in order of their added suffix.
uniquifyMany :: Set.Set Name -> [Name] -> [Name]
uniquifyMany avoid ns = map snd $ sort $ uniquify' avoid <$> ns
