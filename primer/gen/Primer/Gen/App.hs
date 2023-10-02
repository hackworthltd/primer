-- ApplicativeDo: generators shrink much better if applicative (though much of
-- this module is inherently monadic)
{-# LANGUAGE ApplicativeDo #-}

-- |
-- This module generates well-typed 'Prog's
-- It is however, slow and the distribution is not very even.
module Primer.Gen.App (
  genProg,
  genApp,
) where

import Control.Monad.Fresh (MonadFresh (fresh))
import Primer.App (
  App,
  Prog (
    Prog,
    progImports,
    progLog,
    progModules,
    progSelection,
    progSmartHoles,
    redoLog
  ),
  defaultLog,
  mkApp,
  tcWholeProgWithImports,
 )
import Primer.Core (GlobalName (baseName), Kind' (KType), ModuleName (ModuleName), qualifyName)
import Primer.Core.Utils (forgetTypeMetadata, generateIDs, generateTypeIDs)
import Primer.Def (ASTDef (ASTDef), Def (DefAST), defType)
import Primer.Module (Module (Module, moduleDefs, moduleName, moduleTypes), moduleDefsQualified, moduleTypesQualified)
import Primer.Name (Name, unsafeMkName)
import Primer.Typecheck (
  Cxt,
  SmartHoles,
  TypeError,
  extendGlobalCxt,
  extendTypeDefCxt,
 )

import Primer.Gen.Core.Typed (
  WT,
  freshNameForCxt,
  genChk,
  genList,
  genTypeDefGroup,
  genWTType,
  isolateWT,
 )

import Hedgehog (GenT, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Data.Map qualified as M
import GHC.Base (error)

import Foreword hiding (mod)
import Primer.TypeDef (generateTypeDefIDs)

-- | Generate a whole 'Prog', with empty log
-- Note that the result will be well-typed, but not necessarily
-- "smartholes-normal", in the sense that typechecking with smartholes
-- enabled may modify it (e.g. by eliding redundant holes)
genProg :: SmartHoles -> [Module] -> GenT WT Prog
genProg sh initialImports = local (extendCxtByModules initialImports) $ do
  imports <- telescope (Range.linear 0 2) (local . extendCxtByModule) (genModule "I")
  home <- local (extendCxtByModules imports) $ telescope (Range.linear 1 2) (local . extendCxtByModule) (genModule "M")
  pure
    $ Prog
      { progImports = initialImports <> imports
      , progModules = home
      , progSelection = Nothing
      , progSmartHoles = sh
      , progLog = defaultLog
      , redoLog = defaultLog
      }
  where
    telescope :: MonadGen m => Range.Range Int -> (a -> m [a] -> m [a]) -> (Int -> m a) -> m [a]
    telescope n f m = Gen.int n >>= \n' -> telescope' n' 0 f m
    telescope' :: MonadGen m => Int -> Int -> (a -> m [a] -> m [a]) -> (Int -> m a) -> m [a]
    telescope' nMax n k m
      | n >= nMax = pure []
      | otherwise = do
          x <- m n
          rest <- k x $ telescope' nMax (n + 1) k m
          pure $ x : rest
    extendCxtByModule :: Module -> Cxt -> Cxt
    extendCxtByModule = extendCxtByModules . pure
    extendCxtByModules :: [Module] -> Cxt -> Cxt
    extendCxtByModules ms =
      extendTypeDefCxt (foldMap' moduleTypesQualified ms)
        . extendGlobalCxt (M.toList . fmap (forgetTypeMetadata . defType) $ foldMap' moduleDefsQualified ms)
    genModule :: Name -> Int -> GenT WT Module
    genModule prefix index = do
      let mn = ModuleName $ prefix :| [unsafeMkName $ show index]
      tds' <- genTypeDefGroup $ Just mn
      tds <- traverse (\(n, d) -> (n,) <$> generateTypeDefIDs d) tds'
      defs <- local (extendTypeDefCxt $ M.fromList tds') (genASTDefGroup mn)
      pure
        $ Module
          { moduleName = mn
          , moduleTypes = M.fromList $ first baseName <$> tds
          , moduleDefs = defs
          }

-- Generate a mutually-recursive group of term definitions
genASTDefGroup :: ModuleName -> GenT WT (Map Name Def)
genASTDefGroup mod = do
  nts <- genList 5 $ (\n t -> (qualifyName mod n, t)) <$> freshNameForCxt <*> genWTType (KType ())
  nTyTms <- local (extendGlobalCxt nts) $ for nts $ \(n, ty) -> (n,ty,) <$> genChk ty
  fmap M.fromList . for nTyTms $ \(n, ty, tm) -> do
    tm' <- generateIDs tm
    ty' <- generateTypeIDs ty
    pure (baseName n, DefAST $ ASTDef tm' ty')

-- | Generate an 'App' that is "smartholes-normal": typechecking with the
-- app's smartholes setting will not modify it
-- (up to alpha equality in TypeCaches).
genApp :: SmartHoles -> [Module] -> GenT WT App
genApp sh initialImports = do
  -- Since we know that typechecking is idempotent (tasty_tcWholeProg_idempotent),
  -- we also know that the output of 'genApp' is "smartholes-normal".
  p <- runExceptT @TypeError . tcWholeProgWithImports =<< genProg sh initialImports
  let p' = case p of
        Left err ->
          -- genProg always generates a well-typed App (see tasty_genProg_well_formed)
          -- so we will just crash if something has gone wrong.
          error $ show err
        Right p'' -> p''
  i <- lift $ isolateWT fresh
  nc <- lift $ isolateWT fresh
  pure $ mkApp i nc p'
