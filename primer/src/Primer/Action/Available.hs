{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
  OfferedAction (..),
  InputAction (..),
  NoInputAction (..),
  Level (..),
  inputAction,
  ActionOption (..),
  ActionOptions (..),
  InputActionError (..),
) where

import Foreword

import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics (
  (%),
  (^.),
  (^?),
  _Just,
 )

import Primer.Action.Priorities qualified as P
import Primer.Core (
  Editable (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Level (..),
  Meta (..),
  ModuleName (unModuleName),
  Type,
  Type' (..),
  getID,
  unLocalName,
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _synthed,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Utils (freeVars)
import Primer.Def (
  ASTDef (..),
  Def,
  DefMap,
  defAST,
 )
import Primer.Def.Utils (globalInUse)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (unName)
import Primer.Questions (generateNameExpr, generateNameTy, variablesInScopeExpr, variablesInScopeTy)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefConstructors),
  TypeDef (TypeDefAST),
  TypeDefMap,
  ValCon (valConArgs),
  typeDefAST,
  valConName,
 )
import Primer.Typecheck (
  Cxt,
  TypeDefError (TDIHoleType),
  TypeDefInfo (TypeDefInfo),
  getTypeDefInfo',
 )
import Primer.Zipper (
  SomeNode (..),
  findNodeWithParent,
  findType,
  focusOn,
  focusOnTy,
  locToEither,
 )

data OfferedAction
  = NoInput NoInputAction
  | Input InputAction
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via PrimerJSON OfferedAction

data NoInputAction
  = MakeCase
  | ConvertLetToLetrec
  | ConstructApp
  | ConstructAPP
  | ConstructAnn
  | RemoveAnn
  | FinishHole
  | EnterHole
  | ConstructFun
  | AddInput
  | ConstructTypeApp
  | DuplicateDef
  | Raise
  | RaiseType
  | DeleteDef
  | DeleteExpr
  | DeleteType
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NoInputAction
data InputAction
  = MakeLambda
  | UseVar
  | SaturatedFunction
  | MakeLet
  | MakeLetRec
  | ConstructBigLambda
  | UseTypeVar
  | ConstructForall
  | RenameDef
  | RenamePatternVar
  | RenameLambda
  | RenameLAM
  | RenameLetBinding
  | RenameForall
  | UseValueCon -- TODO sort these (incl. use sites) - were at bottom because qualified
  | UseSaturatedValueCon
  | UseTypeCon
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON InputAction

actionsForDef ::
  Level ->
  -- | All existing definitions, along with their mutability
  Map GVarName (Editable, Def) ->
  -- | The name of a definition in the map
  GVarName ->
  [OfferedAction]
actionsForDef l defs defName = prioritySort l $ catMaybes [rename, duplicate, delete]
  where
    rename = do
      -- TODO can this be simplified?
      _ <- getEditableASTDef defs defName
      pure $ Input RenameDef

    duplicate = do
      _ <- getEditableASTDef defs defName
      pure $ NoInput DuplicateDef

    delete = do
      -- Ensure it is not in use, otherwise the action will not succeed
      _ <- getEditableDef defs defName
      guard $ not $ globalInUse defName $ Map.delete defName $ fmap snd defs
      pure $ NoInput DeleteDef

-- | Given the body of a Def and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefBody ::
  TypeDefMap ->
  Level ->
  Editable ->
  ID ->
  Expr ->
  [OfferedAction]
actionsForDefBody _ _ NonEditable _ _ = mempty
actionsForDefBody tydefs l Editable id expr = prioritySort l $ case findNodeWithParent id expr of
  Nothing -> mempty
  Just (ExprNode e, p) ->
    let raiseAction = case p of
          Nothing -> [] -- at root already, cannot raise
          Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
          _ -> [NoInput Raise]
     in basicActionsForExpr tydefs l e <> raiseAction
  Just (TypeNode t, p) ->
    let raiseAction = case p of
          Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
          _ -> [NoInput Raise]
     in basicActionsForType l t <> compoundActionsForType t <> raiseAction
  Just (CaseBindNode _, _) -> [Input RenamePatternVar]

-- | Given a the type signature of a Def and the ID of a node in it,
-- return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  Editable ->
  ID ->
  Type ->
  [OfferedAction]
actionsForDefSig _ NonEditable _ _ = mempty
actionsForDefSig l Editable id ty = prioritySort l $ case findType id ty of
  Nothing -> mempty
  Just t ->
    basicActionsForType l t
      <> compoundActionsForType t
      <> mwhen (id /= getID ty) [NoInput RaiseType]

-- | Given an expression, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForExpr :: TypeDefMap -> Level -> Expr -> [OfferedAction]
basicActionsForExpr tydefs l expr = case expr of
  EmptyHole{} -> universalActions <> emptyHoleActions
  Hole{} -> defaultActions <> holeActions
  Ann{} -> defaultActions <> annotationActions
  Lam{} -> defaultActions <> lambdaActions
  LAM{} -> defaultActions <> bigLambdaActions
  Let _ v e _ -> defaultActions <> letActions v e
  Letrec{} -> defaultActions <> letRecActions
  e -> let _ = e ^. _exprMetaLens in defaultActions <> annotate
  where
    m = expr ^. _exprMetaLens
    annotate = mwhen (l == Expert) [NoInput ConstructAnn]
    emptyHoleActions = case l of
      Beginner ->
        [ Input UseVar
        , Input UseValueCon
        ]
      _ ->
        [ Input UseVar
        , Input SaturatedFunction
        , Input UseValueCon
        , Input UseSaturatedValueCon
        , Input MakeLet
        , Input MakeLetRec
        , NoInput EnterHole
        ]
          <> annotate
    holeActions = NoInput FinishHole : annotate
    annotationActions = mwhen (l == Expert) [NoInput RemoveAnn]
    lambdaActions = Input RenameLambda : annotate
    bigLambdaActions = annotate <> mwhen (l == Expert) [Input RenameLAM]
    letActions v e =
      [Input RenameLetBinding]
        <> munless (unLocalName v `Set.member` freeVars e) [NoInput ConvertLetToLetrec]
        <> annotate
    letRecActions = Input RenameLetBinding : annotate
    universalActions =
      let both = case l of
            Beginner ->
              [ Input MakeLambda
              ]
            Intermediate ->
              [ Input MakeLambda
              , NoInput ConstructApp
              ]
            Expert ->
              [ NoInput ConstructApp
              , NoInput ConstructAPP
              , Input MakeLambda
              , Input ConstructBigLambda
              ]
          -- We assume that the input program is type-checked, in order to
          -- filter some actions by Syn/Chk
          synthTy = m ^? _type % _Just % _synthed
          synOnly ty = case getTypeDefInfo' tydefs ty of
            Left TDIHoleType{} -> Just $ NoInput MakeCase
            Right (TypeDefInfo _ _ TypeDefAST{}) -> Just $ NoInput MakeCase
            _ -> Nothing
       in (synOnly =<< synthTy) ?: both
    defaultActions = universalActions <> [NoInput DeleteExpr]

-- | Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> Type -> [OfferedAction]
basicActionsForType l = \case
  TEmptyHole{} -> universalActions <> [Input UseTypeCon] <> mwhen (l == Expert) [Input UseTypeVar]
  TForall{} -> defaultActions <> mwhen (l == Expert) [Input RenameForall]
  _ -> defaultActions
  where
    universalActions =
      [NoInput ConstructFun]
        <> mwhen
          (l == Expert)
          [ Input ConstructForall
          , NoInput ConstructTypeApp
          ]
    defaultActions = universalActions <> [NoInput DeleteType]

-- TODO inline

-- | These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: Type' (Meta a) -> [OfferedAction]
compoundActionsForType ty = case ty of
  TFun _m _ _ -> [NoInput AddInput]
  _ -> []

-- TODO just combine these and take a `OfferedAction`?
priorityNoInputAction :: NoInputAction -> Level -> Int
priorityNoInputAction = \case
  MakeCase -> P.makeCase
  ConvertLetToLetrec -> P.makeLetRecursive
  ConstructApp -> P.applyFunction
  ConstructAPP -> P.applyType
  ConstructAnn -> P.annotateExpr
  RemoveAnn -> P.removeAnnotation
  FinishHole -> P.finishHole
  EnterHole -> P.enterHole
  ConstructFun -> P.constructFunction
  AddInput -> P.addInput
  ConstructTypeApp -> P.constructTypeApp
  DuplicateDef -> P.duplicate
  Raise -> P.raise
  RaiseType -> P.raise
  DeleteDef -> P.delete
  DeleteExpr -> P.delete
  DeleteType -> P.delete
priorityInputAction :: InputAction -> Level -> Int
priorityInputAction = \case
  MakeLambda -> P.makeLambda
  UseVar -> P.useVar
  SaturatedFunction -> P.useFunction
  MakeLet -> P.makeLet
  MakeLetRec -> P.makeLetrec
  ConstructBigLambda -> P.makeTypeAbstraction
  UseTypeVar -> P.useTypeVar
  ConstructForall -> P.constructForall
  RenameDef -> P.rename
  RenamePatternVar -> P.rename
  RenameLambda -> P.rename
  RenameLAM -> P.rename
  RenameLetBinding -> P.rename
  RenameForall -> P.rename
  UseValueCon -> P.useValueCon
  UseSaturatedValueCon -> P.useSaturatedValueCon
  UseTypeCon -> P.useTypeCon

data ActionOption = ActionOption
  { option :: Text
  , context :: Maybe (NonEmpty Text)
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionOption

data ActionOptions = ActionOptions
  { options :: [ActionOption]
  , free :: Bool
  -- ^ allow free text input, rather than just selections from the list
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrimerJSON ActionOptions

-- getInput :: OfferedAction -> (ActionOptions, [ProgAction])
-- getInput :: Level -> OfferedAction -> (ActionOptions, [ProgAction])
-- TODO obviously don't use `Text` for errors
-- TODO but can we avoid errors here completely by shifting more responsibility to `ProgAction`
-- TODO fewer args?
-- inputAction ::
--   Map GVarName (a, Def) ->
--   ASTDef ->
--   GVarName ->
--   ID ->
--   Level ->
--   Either Expr Type ->
--   OfferedAction ->
--   (ActionOptions, Either Text [ProgAction])

-- TODO the `Maybe ID` here is awkward - some actions require ID's and others don't, we should reflect this in the types
inputAction ::
  TypeDefMap ->
  DefMap ->
  ASTDef ->
  Cxt ->
  Level ->
  Maybe ID ->
  InputAction ->
  Either InputActionError ActionOptions
inputAction typeDefs defs def cxt level mid = \case
  MakeLambda -> do
    options <- genName'
    -- q <- handleQuestion $ GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
    pure ActionOptions{options, free = True}
  UseVar -> do
    -- TODO DRY next 10 lines or so
    (_types, locals, globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> (if level == Beginner then noFunctions locals else locals)
        optionsGlob = fromGlobal . fst <$> (if level == Beginner then noFunctions globals else globals)
        options = optionsLoc <> optionsGlob
    pure ActionOptions{options, free = False}
  SaturatedFunction -> do
    (_types, locals, globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> onlyFunctions locals
        optionsGlob = fromGlobal . fst <$> onlyFunctions globals
        options = optionsLoc <> optionsGlob
    pure ActionOptions{options, free = False}
  MakeLet -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  MakeLetRec -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  ConstructBigLambda -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  UseTypeVar -> do
    (types, _locals, _globals) <- varsInScope'
    let options = flip ActionOption Nothing . unName . unLocalName . fst <$> types
    pure ActionOptions{options, free = False}
  ConstructForall -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  RenameDef ->
    pure ActionOptions{options = [], free = True}
  RenamePatternVar -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  RenameLambda -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  RenameLAM -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  RenameLetBinding -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  RenameForall -> do
    options <- genName'
    pure ActionOptions{options, free = True}
  UseValueCon ->
    let options = map (fromGlobal . valConName) . (if level == Beginner then noFunctionsCon else identity) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  UseSaturatedValueCon ->
    let options = map (fromGlobal . valConName) . onlyFunctionsCon . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  UseTypeCon ->
    let options = fromGlobal . fst <$> Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  where
    -- TODO there's some repetition here (EDIT: though not much, after inlining and simplifying) from `handleQuestion` (and `focusNodeDefs`, which uses too concrete an error type)
    genName' = do
      id <- maybe (throwError NoID) pure mid
      -- TODO by always passing `Nothing`, we lose good name hints - see `baseNames`
      -- we previously passed `Just` for only AMakeLambda,AConstructBigLambda,ARenamePatternVar,ARenameLambda,ARenameLAM,ARenameLetBinding,ARenameForall
      -- not  MakeLet,MakeLetRec,ConstructForall
      typeKind <-
        (fst <$> findNodeWithParent id (astDefExpr def)) <|> (TypeNode <$> findType id (astDefType def))
          & maybe
            (throwError IDNotFound)
            ( \case
                ExprNode e -> pure $ Left $ do
                  tc <- e ^. _exprMetaLens % _type
                  (tc ^? _chkedAt) <|> (tc ^? _synthed) -- TODO which to prioritise? ask Ben
                TypeNode t -> pure $ Right $ t ^. _typeMetaLens % _type
                CaseBindNode b -> pure $ Left $ do
                  tc <- b ^. _bindMeta % _type
                  -- TODO DRY this line with above
                  (tc ^? _chkedAt) <|> (tc ^? _synthed)
            )
      names <-
        focusNode id <&> \case
          Left zE -> generateNameExpr typeKind zE
          Right zT -> generateNameTy typeKind zT
      pure $ flip ActionOption Nothing . unName <$> runReader names cxt
    varsInScope' = do
      id <- maybe (throwError NoID) pure mid
      node <- focusNode id
      pure $ case node of
        Left zE -> variablesInScopeExpr defs zE
        Right zT -> (variablesInScopeTy zT, [], [])
    fromGlobal n = ActionOption{option = unName $ baseName n, context = Just $ map unName $ unModuleName $ qualifiedModule n}
    focusNode id =
      let mzE = locToEither <$> focusOn id (astDefExpr def)
          mzT = focusOnTy id $ astDefType def
       in case fmap Left mzE <|> fmap Right mzT of
            Nothing -> throwError IDNotFound
            Just x -> pure x
data InputActionError -- TODO can we somehow remove this? maybe combine with `ActionError`
  = IDNotFound
  | NoID
  deriving (Show)

getEditableDef :: Ord k => Map k (Editable, b) -> k -> Maybe b
getEditableDef defs defName = do
  (m, d) <- Map.lookup defName defs
  case m of
    NonEditable -> Nothing
    Editable -> pure d
getEditableASTDef :: Ord k => Map k (Editable, Def) -> k -> Maybe ASTDef
getEditableASTDef defs defName = defAST =<< getEditableDef defs defName

prioritySort ::
  Level ->
  [OfferedAction] ->
  [OfferedAction]
prioritySort l = sortOn $ \case
  NoInput x -> priorityNoInputAction x l
  Input x -> priorityInputAction x l

-- TODO is each of these only used once?
noFunctions :: [(a1, Type' a2)] -> [(a1, Type' a2)]
noFunctions = filter $ \case
  (_, TFun{}) -> False
  _ -> True
onlyFunctions :: [(a1, Type' a2)] -> [(a1, Type' a2)]
onlyFunctions = filter $ \case
  (_, TFun{}) -> True
  _ -> False
noFunctionsCon :: [ValCon] -> [ValCon]
noFunctionsCon = filter $ null . valConArgs
onlyFunctionsCon :: [ValCon] -> [ValCon]
onlyFunctionsCon = filter $ not . null . valConArgs
