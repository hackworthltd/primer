{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Primer.Import (
  ImportError (..),
  importFromApp',
  ImportActionConfig (..),
) where

import Control.Monad.NestedError (MonadNestedError (throwError'))
import Data.Generics.Uniplate.Data (transformM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Foreword
import Optics (
  AffineTraversal',
  Field2 (_2),
  traverseOf,
  traversed,
  (%),
  (%~),
  (.~),
 )
import Primer.App.Core (
  App (appProg),
  Prog (progTypes),
 )
import Primer.Core (
  Type',
  TypeDef,
  ValCon (valConArgs, valConName),
  astTypeDefConstructors,
  typeDefAST,
  typeDefKind,
  typeDefName,
  _typeDefName,
 )
import Primer.Core.Utils (alphaEqTy)
import Primer.JSON
import Primer.Name (Name)
import Primer.Typecheck (mkTypeDefMap)
import Primer.Utils (distinct')

data ImportError
  = -- | Cannot both import and rename a type
    ImportRenameType Name
  | -- | Cannot import two types under the same name, or something that clashes with an existing type
    DuplicateTypes [Name]
  | -- | Cannot import two ctors under the same name, or something that clashes with an existing ctors
    DuplicateCtors [Name]
  | -- | Cannot import something that does not exist
    UnknownImportedType Name
  | -- | Cannot import something that does not exist (args: an imported type, and its non-existent constructor)
    UnknownImportedCtor Name Name
  | -- | We cannot currently rename primitive types (as the types of primitive constructors are
    -- hardcoded by name)
    CannotRenamePrimitiveType Name
  | -- | Have asked to import a primitive type, but also to rewrite its constructors
    PrimitiveTypeHasNoCtor Name
  | -- | Must import all constructors of any imported type (args: an imported type, and a non-imported constructor)
    MissedImportCtor Name Name
  | -- | Cannot rewrite a dep which does not exist (although, since it cannot be referred to, nothing would go wrong if we allowed it...)
    UnknownRewrittenSrcType Name
  | -- | Cannot rewrite into a type that does not exist
    UnknownRewrittenTgtType Name
  | -- | Tried to rewrite the first arg to the second, but they are of different kinds
    RewriteTypeKindMismatch Name Name
  | -- | Tried to rewrite a primitive to a non-primitive, or vice versa
    RewriteTypePrimitiveMismatch Name Name
  | -- | Must rewrite all constructors of any rewritten type: args are type its (first) ctor that were missed
    MissedRewriteCtor Name Name
  | -- | In a rewritten type (first arg), cannot rewrite two ctors to the same thing (second arg)
    DuplicateRewriteCtor Name [Name]
  | -- | The constructor rewrites for this type rewriting did not cover all constructors of the target
    RewriteCtorsNotSurjective Name Name
  | -- | We attempted to rewrite a ctor that does not exist. Args: the type, and its non-existent ctor
    RewriteCtorSrcNotExist Name Name
  | -- | We attempted to rewrite into a ctor that does not exist. Args: the type, and its non-existent ctor
    RewriteCtorTgtNotExist Name Name
  | -- | This type was referenced in the source, but we have neither imported it nor rewritten it
    ReferencedTypeNotHandled Name
  | -- | @RewrittenCtorTypeDiffers ty1 con1 ty2 con2@: we tried to rewrite @ty1@ into @ty2@, mapping @con1@ to @con2@, but their types do not match
    RewrittenCtorTypeDiffers Name Name Name Name
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON ImportError

-- | How to rename a type, and each of its ctors
type TypeImportDetails = Map Name (Name, Map Name Name)

-- | The generic "import" is
-- "Given
--  - an App (actually, Prog) 'A'
--  - a bunch of type/term defs in 'A' and some free local names
--  - a mapping from dependencies of the stuff in 'A' to appropriate local stuff
-- we will copy the code out of 'A' into your own app, renaming as asked"
--
-- The keys of the iacImport* maps must exist in the appropriate external App
-- The values of the iacImport* maps must be free (type/term names) in the current App
--   For types, the fst of the value must be free, and the snd map must be
--     keys are exactly the ctors of the type in 'A', values are free constructor names
-- The keys of iacDeps* must cover all free vars of the defs in 'A' named by iacImport*
--   For types, the constructor map must be a bijection
-- The values of iacDeps* must exist in the current App, and have the same Kind/Type (after substituting types according to iacDeps) as the key does.
--
-- (The reason we require a bijection for rewriting constructors is for
--  translating pattern matches:
-- - It needs to be injective for the translation, as previously distinguished
--   cases are now identified
-- - It needs to be surjective for the translated match to be exhaustive)
--
-- We will typecheck the final result, but this should pass if the conditions above are satisfied
data ImportActionConfig = IAC
  { iacImportRenamingTypes :: TypeImportDetails
  , iacDepsTypes :: TypeImportDetails
  , iacImportRenamingTerms :: ()
  , iacDepsTerms :: ()
  }

-- | Computes the new program with a copy of the requested imports,
-- but does not typecheck it
--
-- See 'App.importFromApp' for a version that typechecks and updates an App
-- state.
importFromApp' ::
  (MonadNestedError ImportError e m, MonadReader App m) =>
  Prog ->
  ImportActionConfig ->
  m Prog
importFromApp' srcProg iac = do
  curProg <- asks appProg
  newTypeDefs <- getImportTypesFromApp curProg (srcProg, iac)
  pure $ curProg & #progTypes %~ (++ newTypeDefs)

getImportTypesFromApp :: forall m e. MonadNestedError ImportError e m => Prog -> (Prog, ImportActionConfig) -> m [TypeDef]
getImportTypesFromApp curProg (srcProg, IAC{iacImportRenamingTypes, iacDepsTypes}) =
  {- We check requirements, to give nice error messages, rather than "typechecker says no"
       - Things need to exist in srcProg:
         - types in iacImportRenamingTypes
         - constructors in iacImportRenamingTypes
         - types/constructors in iacDepsTypes
         - all these can only be mentioned once (either one import, or one rename-deps)
       - Things that we are creating need to have names distinct from each other and everything in curProg
         - types/constructors in iacImportRenamingTypes
       - Referenced things (i.e. types appearing in imported constructor args) need to be "covered": either
         - imported, no further checks
         - or renamed as a dep: then their type/kind must match
       - iacImportRenamingTypes and iacDepsTypes: for each type T ~> (T',ctorMap)
         - The keys of the ctorMap are exactly the ctors of T
  -}
  do
    -- Not allowed to both import and dep-rename some type
    for_ (Map.intersection iacImportRenamingTypes iacDepsTypes) $ throwError' . ImportRenameType . fst

    -- things we create have distinct names, and don't appear in curProg
    let createdTypes = fst <$> Map.elems iacImportRenamingTypes
    let createdCtors = concatMap (Map.elems . snd) $ Map.elems iacImportRenamingTypes
    let extantTypes = typeDefName <$> progTypes curProg
    let extantCtors =
          concatMap (fmap valConName . astTypeDefConstructors) $
            mapMaybe typeDefAST $
              progTypes curProg
    case distinct' $ createdTypes ++ extantTypes of
      Left dups -> throwError' $ DuplicateTypes dups
      Right _ -> pure ()
    case distinct' $ createdCtors ++ extantCtors of
      Left dups -> throwError' $ DuplicateCtors dups
      Right _ -> pure ()

    -- make renaming map, and grab the typedefs, with the new type/ctor name. NB: the argument types still need rewriting!
    let srcTypes = mkTypeDefMap $ progTypes srcProg
    (tyrn1, _ctorrn1, tydefs) <- getAp $
      flip Map.foldMapWithKey iacImportRenamingTypes $ \srcTyName (tgtTyName, ctorMap) -> Ap $ do
        -- imported type exists in srcProg
        srcTy <-
          lookupOrThrow srcTyName srcTypes $
            UnknownImportedType srcTyName
        -- must import all the constructors (and nothing else)
        case typeDefAST srcTy of
          Nothing -> do
            unless (srcTyName == tgtTyName) $ throwError' $ CannotRenamePrimitiveType srcTyName
            unless (Map.null ctorMap) $ throwError' $ PrimitiveTypeHasNoCtor srcTyName
          Just srcADT -> do
            let srcCtors = Set.fromList (valConName <$> astTypeDefConstructors srcADT)
            let importedCtors = Map.keysSet ctorMap
            -- cannot import a non-existant constructor
            for_ (importedCtors Set.\\ srcCtors) $
              throwError' . UnknownImportedCtor srcTyName

        renamedTy <-
          traverseOf
            (#_TypeDefAST % #astTypeDefConstructors % traversed % #valConName)
            -- we must import all constructors
            (\c -> lookupOrThrow c ctorMap $ MissedImportCtor srcTyName c)
            $ srcTy & _typeDefName .~ tgtTyName

        pure
          ( Map.singleton srcTyName tgtTyName
          , ctorMap
          , [renamedTy]
          )

    -- renaming map for deps rewriting
    let curTypes = mkTypeDefMap $ progTypes curProg
    (tyrn2, _ctorrn2, toCheckRewriteCtorsTypes) <- getAp $
      flip Map.foldMapWithKey iacDepsTypes $ \srcTyName (tgtTyName, ctorMap) -> Ap $ do
        -- the rewritten thing must exist in the imported program
        -- (nothing would go wrong if we elided this check, but it catches
        -- that our caller is broken)
        srcTy <-
          lookupOrThrow srcTyName srcTypes $
            UnknownRewrittenSrcType srcTyName
        -- We must rewrite it to an existing type ...
        tgtTy <-
          lookupOrThrow tgtTyName curTypes $
            UnknownRewrittenTgtType tgtTyName
        -- ... of the same kind
        unless (typeDefKind srcTy == typeDefKind tgtTy) $
          throwError' $ RewriteTypeKindMismatch srcTyName tgtTyName

        (_ctorPerm, toCheckRewriteCtorsTypes) <- case (typeDefAST srcTy, typeDefAST tgtTy) of
          (Nothing, Nothing) -> do
            unless (srcTyName == tgtTyName) $ throwError' $ CannotRenamePrimitiveType srcTyName
            -- primitives have no constructors
            pure (Map.empty, [])
          (Nothing, Just _) -> throwError' $ RewriteTypePrimitiveMismatch srcTyName tgtTyName
          (Just _, Nothing) -> throwError' $ RewriteTypePrimitiveMismatch srcTyName tgtTyName
          (Just srcADT, Just tgtADT) -> do
            let mkCtorMap ty = Map.fromList (map (\c -> (valConName c, c)) $ astTypeDefConstructors ty)
            let srcCtorMap = mkCtorMap srcADT
            let tgtCtorMap = mkCtorMap tgtADT
            let srcCtors = Map.keysSet srcCtorMap
            let rewriteSrcCtors = Map.keysSet ctorMap
            -- must rewrite every constructor
            for_ (srcCtors Set.\\ rewriteSrcCtors) $ throwError' . MissedRewriteCtor srcTyName
            -- cannot rewrite two ctors to the same thing (else cannot translate exhaustive pattern matching)
            case distinct' $ Map.elems ctorMap of
              Left dups -> throwError' $ DuplicateRewriteCtor srcTyName dups
              Right _ -> pure ()
            -- must hit all of the target ctors (else cannot translate exhaustive pattern matching)
            unless (Map.keysSet tgtCtorMap `Set.isSubsetOf` Set.fromList (Map.elems ctorMap)) $
              throwError' $ RewriteCtorsNotSurjective srcTyName tgtTyName

            -- for each s->t, check s and t exist respectively, and types same
            -- however, we need to rename types to check the ctor types match
            -- which we cannot do yet, as we have not built the renaming map,
            -- so actually just record what we need to check
            toCheck <- for (Map.toList ctorMap) $ \(srcCtor, tgtCtor) -> do
              s <-
                lookupOrThrow srcCtor srcCtorMap $
                  RewriteCtorSrcNotExist srcTyName srcCtor
              t <-
                lookupOrThrow tgtCtor tgtCtorMap $
                  RewriteCtorTgtNotExist tgtTyName tgtCtor
              pure (srcTyName, s, tgtTyName, t)
            pure
              ( Map.singleton
                (valConName <$> astTypeDefConstructors srcADT)
                (valConName <$> astTypeDefConstructors tgtADT)
              , toCheck
              )

        pure
          ( Map.singleton srcTyName tgtTyName
          , ctorMap
          , toCheckRewriteCtorsTypes
          )

    let tyrn = Map.union tyrn1 tyrn2

    -- Rename the types referenced:
    -- everywhere we see a 'TCon', replace it with its image in the 'tyrn' map
    -- (We throw an error if they do not exist in our
    -- renaming map, i.e. the imported types, or the rewritten deps)
    let rewriteTCons :: Type' () -> m (Type' ())
        rewriteTCons = transformM $
          traverseOf tconName $ \tc ->
            lookupOrThrow tc tyrn $ ReferencedTypeNotHandled tc

    -- Now check that rewritten constructors have the same type
    for_ toCheckRewriteCtorsTypes $ \(srcTy, srcCtor, tgtTy, tgtCtor) -> do
      srcCtorArgs <- traverse rewriteTCons $ valConArgs srcCtor
      let tgtCtorArgs = valConArgs tgtCtor
      unless
        ( length srcCtorArgs == length tgtCtorArgs
            && and (zipWith alphaEqTy srcCtorArgs tgtCtorArgs)
        )
        $ throwError' $
          RewrittenCtorTypeDiffers
            srcTy
            (valConName srcCtor)
            tgtTy
            (valConName tgtCtor)

    -- Rename the types referenced. These are now ready to insert into curProg
    traverseOf
      (traversed % #_TypeDefAST % #astTypeDefConstructors % traversed % #valConArgs % traversed)
      rewriteTCons
      tydefs
  where
    lookupOrThrow :: Ord k => k -> Map k v -> ImportError -> m v
    lookupOrThrow k m e = case m Map.!? k of
      Nothing -> throwError' e
      Just v -> pure v

    tconName :: AffineTraversal' (Type' ()) Name
    tconName = #_TCon % _2
