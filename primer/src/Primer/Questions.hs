-- Logic for answering API questions

module Primer.Questions
  ( variablesInScopeExpr,
    variablesInScopeTy,
    ShadowedVarsExpr (..), -- only exported for testing
    ShadowedVarsTy (..), -- only exported for testing
    generateNameExpr,
    generateNameTy,
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Primer.Action (mkAvoidForFreshName, mkAvoidForFreshNameTy)
import Primer.Core
  ( Def (..),
    ID,
    Kind (KFun, KType),
    Type' (..),
    TypeDef (typeDefNameHints),
  )
import Primer.Name (Name, unName, unsafeMkName)
import Primer.Typecheck (Cxt, decomposeTAppCon, getGlobalNames, typeDefs)
import Primer.Zipper
  ( ExprZ,
    TypeZ,
    TypeZip,
    bindersAboveTy,
    bindersBelowTy,
  )
import Primer.ZipperCxt
  ( ShadowedVarsExpr (M),
    ShadowedVarsTy (N),
    extractLocalsExprZ,
    extractLocalsTypeZ,
    forgetMetadata,
    variablesInScopeTy,
  )

-- | Collect the typing context for the focused node.
-- We do this by walking back up the tree, collecting variables as we cross
-- binders.
-- We also return any global variables which are in scope (which currently is all of them)
-- The first list is local type variables, the second list is local term variables,
-- the third is globals.
variablesInScopeExpr ::
  Map ID Def ->
  Either ExprZ TypeZ ->
  ([(Name, Kind)], [(Name, Type' ())], [(ID, Name, Type' ())])
variablesInScopeExpr defs exprOrTy =
  let locals = either extractLocalsExprZ extractLocalsTypeZ exprOrTy
      globals = Map.elems $ fmap (\(Def id_ n _ ty) -> (id_, n, forgetMetadata ty)) defs
      M tyvars tmvars globs = locals <> M [] [] globals
   in (reverse tyvars, reverse tmvars, globs) -- keep most-global first

generateNameExpr ::
  MonadReader Cxt m =>
  Either (Maybe (Type' ())) (Maybe Kind) ->
  Either ExprZ TypeZ ->
  m [Name]
-- NB: it makes perfect sense to ask for a type variable (first Either is Right)
-- in a term context (second Either is Left): we could be inserting a LAM.
-- It doesn't make sense to ask for a term variable in a type context,
-- but it also doesn't harm to support it.
generateNameExpr tk z = uniquify <$> getAvoidSet z <*> baseNames tk

generateNameTy ::
  MonadReader Cxt m =>
  Either (Maybe (Type' ())) (Maybe Kind) ->
  TypeZip ->
  m [Name]
-- It doesn't really make sense to ask for a term variable (Left) here, but
-- it doesn't harm to support it
generateNameTy tk z = uniquify <$> getAvoidSetTy z <*> baseNames tk

baseNames ::
  MonadReader Cxt m =>
  Either (Maybe (Type' ())) (Maybe Kind) ->
  m [Name]
baseNames tk = do
  tys <- asks typeDefs
  pure $ case tk of
    Left (Just ty)
      | Just c <- headCon ty,
        Just hints@(_ : _) <- typeDefNameHints <$> Map.lookup c tys ->
        hints
    Left (Just TFun {}) -> ["f", "g", "h"]
    Left _ -> ["x", "y", "z"]
    Right (Just KType) -> ["α", "β", "γ"]
    Right (Just (KFun _ _)) -> ["f", "m", "t"]
    Right _ -> ["α", "β", "γ"]
  where
    headCon = fmap fst . decomposeTAppCon

getAvoidSet :: MonadReader Cxt m => Either ExprZ TypeZ -> m (Set.Set Name)
getAvoidSet = \case
  Left ze -> mkAvoidForFreshName ze
  Right zt -> mkAvoidForFreshNameTy zt

getAvoidSetTy :: MonadReader Cxt m => TypeZip -> m (Set.Set Name)
getAvoidSetTy z = do
  globals <- getGlobalNames
  pure $ bindersAboveTy z <> bindersBelowTy z <> globals

-- We do not use Name.freshName as we don't want a global fresh counter
-- (and we want to control the base name)
uniquify :: Set.Set Name -> [Name] -> [Name]
uniquify avoid ns = map snd $ sort $ map go ns
  where
    go n = head [(i, f n i) | i <- [0 ..], f n i `Set.notMember` avoid]
    f :: Name -> Integer -> Name
    f n = \case
      0 -> n
      i -> unsafeMkName $ unName n <> pack (show i)
