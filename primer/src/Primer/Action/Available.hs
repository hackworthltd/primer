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
  QualifiedText (..),
  fromQualifiedText,
  toQualifiedText,
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
  _exprMetaLens,
  _synthed,
  _type,
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

data ActionOption = ActionOption
  { option :: Text
  , qualification :: Maybe (NonEmpty Text) -- TODO hmm - this is isomorphic to normal list - too cute? also rename
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionOption

-- TODO work out what to do with this - bite the bullet and do the DB migration?
-- TODO as we only send `ActionOptionsChooseQualified` _to_ the client, I guess those can be the proper types e.g. `TyConName`
data QualifiedText = QualifiedText
  { context :: NonEmpty Text
  , text :: Text
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON QualifiedText
toQualifiedText :: (NonEmpty Text, Text) -> QualifiedText
toQualifiedText (context, text) = QualifiedText{..}
fromQualifiedText :: QualifiedText -> (NonEmpty Text, Text)
fromQualifiedText QualifiedText{..} = (context, text)

data ActionOptions = ActionOptions
  { options :: [ActionOption]
  , free :: Bool
  -- ^ allow free text input, rather than just selections from the list
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrimerJSON ActionOptions

data OfferedAction
  = NoInputAction NoInputAction
  | InputAction InputAction
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via PrimerJSON OfferedAction

-- TODO rename constructors - descriptive names, also drop the prefix and we'll always qualify
data NoInputAction
  = AMakeCase
  | AConvertLetToLetrec
  | AConstructApp
  | AConstructAPP
  | AConstructAnn
  | ARemoveAnn
  | AFinishHole
  | AEnterHole
  | AConstructFun
  | AAddInput
  | AConstructTypeApp
  | ADuplicateDef
  | ARaise
  | ARaiseType
  | ADeleteDef
  | ADeleteExpr
  | ADeleteType
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NoInputAction
data InputAction -- TODO rename for consistency
  = AMakeLambda
  | AUseVar
  | ASaturatedFunction
  | AMakeLet
  | AMakeLetRec
  | AConstructBigLambda
  | AUseTypeVar
  | AConstructForall
  | ARenameDef
  | ARenamePatternVar
  | ARenameLambda
  | ARenameLAM
  | ARenameLetBinding
  | ARenameForall
  | AUseValueCon -- TODO sort these (incl. use sites) - were at bottom because qualified
  | AUseSaturatedValueCon
  | AUseTypeCon
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
      pure $ InputAction ARenameDef

    duplicate = do
      _ <- getEditableASTDef defs defName
      pure $ NoInputAction ADuplicateDef

    delete = do
      -- Ensure it is not in use, otherwise the action will not succeed
      _ <- getEditableDef defs defName
      guard $ not $ globalInUse defName $ Map.delete defName $ fmap snd defs
      pure $ NoInputAction ADeleteDef

-- | Given the body of a Def and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefBody ::
  TypeDefMap ->
  Level ->
  Editable ->
  ID ->
  Expr ->
  [OfferedAction]
actionsForDefBody _ _ NonEditable _ _ = mempty
actionsForDefBody tydefs l Editable id expr =
  let raiseAction' = NoInputAction ARaise
   in prioritySort l $ case findNodeWithParent id expr of
        Nothing -> mempty
        Just (ExprNode e, p) ->
          let raiseAction = case p of
                Nothing -> [] -- at root already, cannot raise
                Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
                _ -> [raiseAction']
           in basicActionsForExpr tydefs l e <> raiseAction
        Just (TypeNode t, p) ->
          let raiseAction = case p of
                Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
                _ -> [raiseAction']
           in (basicActionsForType l t <> compoundActionsForType t)
                <> raiseAction
        Just (CaseBindNode _, _) -> [InputAction ARenamePatternVar]

-- | Given a the type signature of a Def and the ID of a node in it,
-- return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  Editable ->
  ID ->
  Type ->
  [OfferedAction]
actionsForDefSig _ NonEditable _ _ = mempty
actionsForDefSig l Editable id ty =
  let raiseAction =
        [ NoInputAction ARaiseType
        | id /= getID ty
        ]
   in prioritySort l $ case findType id ty of
        Nothing -> mempty
        Just t ->
          basicActionsForType l t
            <> compoundActionsForType t
            <> raiseAction

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
    annotate = mwhen (l == Expert) [NoInputAction AConstructAnn]
    emptyHoleActions = case l of
      Beginner ->
        [ InputAction AUseVar
        , InputAction AUseValueCon
        ]
      _ ->
        [ InputAction AUseVar
        , InputAction ASaturatedFunction
        , InputAction AUseValueCon
        , InputAction AUseSaturatedValueCon
        , InputAction AMakeLet
        , InputAction AMakeLetRec
        , NoInputAction AEnterHole
        ]
          <> annotate
    holeActions = NoInputAction AFinishHole : annotate
    annotationActions = mwhen (l == Expert) [NoInputAction ARemoveAnn]
    lambdaActions = InputAction ARenameLambda : annotate
    bigLambdaActions = annotate <> mwhen (l == Expert) [InputAction ARenameLAM]
    letActions v e =
      [InputAction ARenameLetBinding]
        <> munless (unLocalName v `Set.member` freeVars e) [NoInputAction AConvertLetToLetrec]
        <> annotate
    letRecActions = InputAction ARenameLetBinding : annotate
    universalActions =
      let both = case l of
            Beginner ->
              [ InputAction AMakeLambda
              ]
            Intermediate ->
              [ InputAction AMakeLambda
              , NoInputAction AConstructApp
              ]
            Expert ->
              [ NoInputAction AConstructApp
              , NoInputAction AConstructAPP
              , InputAction AMakeLambda
              , InputAction AConstructBigLambda
              ]
          -- We assume that the input program is type-checked, in order to
          -- filter some actions by Syn/Chk
          synthTy = m ^? _type % _Just % _synthed
          synOnly ty = case getTypeDefInfo' tydefs ty of
            Left TDIHoleType{} -> Just $ NoInputAction AMakeCase
            Right (TypeDefInfo _ _ TypeDefAST{}) -> Just $ NoInputAction AMakeCase
            _ -> Nothing
       in (synOnly =<< synthTy) ?: both
    defaultActions = universalActions <> [NoInputAction ADeleteExpr]

-- | Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> Type -> [OfferedAction]
basicActionsForType l = \case
  TEmptyHole{} -> universalActions <> [InputAction AUseTypeCon] <> mwhen (l == Expert) [InputAction AUseTypeVar]
  TForall{} -> defaultActions <> mwhen (l == Expert) [InputAction ARenameForall]
  _ -> defaultActions
  where
    universalActions =
      [NoInputAction AConstructFun]
        <> mwhen
          (l == Expert)
          [ InputAction AConstructForall
          , NoInputAction AConstructTypeApp
          ]
    defaultActions = universalActions <> [NoInputAction ADeleteType]

-- TODO inline

-- | These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: Type' (Meta a) -> [OfferedAction]
compoundActionsForType ty = case ty of
  TFun _m _ _ -> [NoInputAction AAddInput]
  _ -> []

-- TODO just combine these and take a `OfferedAction`?
priorityNoInputAction :: NoInputAction -> Level -> Int
priorityNoInputAction = \case
  AMakeCase -> P.makeCase
  AConvertLetToLetrec -> P.makeLetRecursive
  AConstructApp -> P.applyFunction
  AConstructAPP -> P.applyType
  AConstructAnn -> P.annotateExpr
  ARemoveAnn -> P.removeAnnotation
  AFinishHole -> P.finishHole
  AEnterHole -> P.enterHole
  AConstructFun -> P.constructFunction
  AAddInput -> P.addInput
  AConstructTypeApp -> P.constructTypeApp
  ADuplicateDef -> P.duplicate
  ARaise -> P.raise
  ARaiseType -> P.raise
  ADeleteDef -> P.delete
  ADeleteExpr -> P.delete
  ADeleteType -> P.delete
priorityInputAction :: InputAction -> Level -> Int
priorityInputAction = \case
  AMakeLambda -> P.makeLambda
  AUseVar -> P.useVar
  ASaturatedFunction -> P.useFunction
  AMakeLet -> P.makeLet
  AMakeLetRec -> P.makeLetrec
  AConstructBigLambda -> P.makeTypeAbstraction
  AUseTypeVar -> P.useTypeVar
  AConstructForall -> P.constructForall
  ARenameDef -> P.rename
  ARenamePatternVar -> P.rename
  ARenameLambda -> P.rename
  ARenameLAM -> P.rename
  ARenameLetBinding -> P.rename
  ARenameForall -> P.rename
  AUseValueCon -> P.useValueCon
  AUseSaturatedValueCon -> P.useSaturatedValueCon
  AUseTypeCon -> P.useTypeCon

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
  AMakeLambda -> do
    options <- genName' False
    -- q <- handleQuestion $ GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
    pure ActionOptions{options, free = True}
  AUseVar -> do
    -- TODO DRY next 10 lines or so
    (_types, locals, globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> (if level == Beginner then noFunctions locals else locals)
        optionsGlob = fromGlobal . fst <$> (if level == Beginner then noFunctions globals else globals)
        options = optionsLoc <> optionsGlob
    pure ActionOptions{options, free = False}
  ASaturatedFunction -> do
    (_types, locals, globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> onlyFunctions locals
        optionsGlob = fromGlobal . fst <$> onlyFunctions globals
        options = optionsLoc <> optionsGlob
    pure ActionOptions{options, free = False}
  AMakeLet -> do
    options <- genName' False
    pure ActionOptions{options, free = True}
  AMakeLetRec -> do
    options <- genName' False
    pure ActionOptions{options, free = True}
  AConstructBigLambda -> do
    options <- genName' True
    pure ActionOptions{options, free = True}
  AUseTypeVar -> do
    (types, _locals, _globals) <- varsInScope'
    let options = flip ActionOption Nothing . unName . unLocalName . fst <$> types
    pure ActionOptions{options, free = False}
  AConstructForall -> do
    options <- genName' True
    pure ActionOptions{options, free = True}
  ARenameDef ->
    pure ActionOptions{options = [], free = True}
  ARenamePatternVar -> do
    options <- genName' True
    pure ActionOptions{options, free = True}
  ARenameLambda -> do
    options <- genName' False
    pure ActionOptions{options, free = True}
  ARenameLAM -> do
    options <- genName' True
    pure ActionOptions{options, free = True}
  ARenameLetBinding -> do
    options <- genName' False
    pure ActionOptions{options, free = True}
  ARenameForall -> do
    options <- genName' True
    pure ActionOptions{options, free = True}
  AUseValueCon ->
    let options = map (fromGlobal . valConName) . (if level == Beginner then noFunctionsCon else identity) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  AUseSaturatedValueCon ->
    let options = map (fromGlobal . valConName) . onlyFunctionsCon . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  AUseTypeCon ->
    let options = fromGlobal . fst <$> Map.toList typeDefs
     in pure ActionOptions{options, free = False}
  where
    -- TODO by always passing `Nothing`, we lose good name hints - see `baseNames` (we previously passed `Just` for only AMakeLambda,AConstructBigLambda,ARenamePatternVar,ARenameLambda,ARenameLAM,ARenameLetBinding,ARenameForall)
    -- TODO there's some repetition here (EDIT: though not much, after inlining and simplifying) from `handleQuestion` (and `focusNodeDefs`, which uses too concrete an error type)
    genName' tk = do
      id <- maybe (throwError NoID) pure mid
      names <-
        focusNode id <&> \case
          Left zE -> generateNameExpr typeKind zE
          Right zT -> generateNameTy typeKind zT
      pure $ flip ActionOption Nothing . unName <$> runReader names cxt
      where
        typeKind = if tk then Right Nothing else Left Nothing
    varsInScope' = do
      id <- maybe (throwError NoID) pure mid
      node <- focusNode id
      pure $ case node of
        Left zE -> variablesInScopeExpr defs zE
        Right zT -> (variablesInScopeTy zT, [], [])
    fromGlobal n = ActionOption{option = unName $ baseName n, qualification = Just $ map unName $ unModuleName $ qualifiedModule n}
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
  NoInputAction x -> priorityNoInputAction x l
  InputAction x -> priorityInputAction x l

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
