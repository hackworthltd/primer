{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use section" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
  OfferedAction (..),
  OfferedActionChooseQualified (..),
  OfferedActionChooseText (..),
  OfferedActionChooseOrEnterText (..),
  FunctionFiltering (..),
  SomeAction (..),
  InputAction (..),
  NoInputAction (..),
  InputActionQualified (..),
  Level (..),
  ActionRequest (..),
  ActionRequestText (..),
  ActionRequestQualified (..),
  inputAction,
  inputActionQualified,
  mkAction,
  QualifiedText (..),
  fromQualifiedText,
  toQualifiedText,
) where

import Foreword

import Data.Aeson.Types (ToJSON (toJSON))
import Data.Data (Data)
import Data.Kind qualified as Kind
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics (
  to,
  view,
  (%),
  (^.),
  (^?),
  _Just,
 )
import Primer.Action (
  Action (..),
  Level (..),
  Movement (..),
  ProgAction (..),
  nameString,
  uniquifyDefName,
 )
import Primer.Action.Actions ()
import Primer.Action.Priorities qualified as P
import Primer.App (Editable (Editable, NonEditable), MonadQueryApp, NodeType (..), ProgError (SelectionNoID), globalInUse, handleQuestion)
import Primer.Core (
  Bind' (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind,
  LVarName,
  Meta (..),
  ModuleName (unModuleName),
  TmVarRef (..),
  TyConName,
  Type,
  Type' (..),
  TypeCache (..),
  getID,
  unLocalName,
  unsafeMkLocalName,
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _id,
  _synthed,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Transform (unfoldFun)
import Primer.Core.Utils (forgetTypeMetadata, freeVars)
import Primer.Def (
  ASTDef (..),
  Def,
  DefMap,
  defAST,
 )
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name, unName)
import Primer.Questions (Question (..), generateNameExpr)
import Primer.TypeDef (
  ASTTypeDef (astTypeDefConstructors),
  TypeDef (TypeDefAST),
  TypeDefMap,
  ValCon (valConArgs),
  typeDefAST,
  valConName,
 )
import Primer.Typecheck (
  TypeDefError (TDIHoleType),
  TypeDefInfo (TypeDefInfo),
  getTypeDefInfo',
 )
import Primer.Zipper (
  BindLoc' (BindCase),
  Loc' (InBind, InExpr, InType),
  caseBindZFocus,
  focusOn,
  focusOnTy,
  target,
  unfocusCaseBind,
  unfocusType,
  up,
 )

-- TODO work out what to do with this - bite the bullet and do the DB migration?
-- TODO as we only send `OfferedActionChooseQualified` _to_ the client, I guess those can be the proper types e.g. `TyConName`
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

-- deriving (Show, Generic)
-- deriving (ToJSON, FromJSON) via PrimerJSON OfferedAction

-- TODO split `InputRequired` up - `InputAction` implies the `Bool` and whether the `[Text]` is empty
-- TODO rip out questions - kind of meaningless now that this all goes on in backend
-- TODO move to `Primer.Action`? incl. child types
data OfferedAction -- TODO rename incl. constructors and subtypes
  = NoInputRequired NoInputAction
  | ChooseQualified OfferedActionChooseQualified
  | ChooseText OfferedActionChooseText -- TODO unused - we should use this when there are never going to be options
  | EnterText InputAction
  | ChooseOrEnterText OfferedActionChooseOrEnterText
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON OfferedAction

-- TODO we need another for variables, which may be qualified or unqualified (see also `tInputTmVar`) - actually maybe just modify `ChooseQualified` to `Choose`?
-- TODO we would "inline" all these types in to `OfferedAction`, but `openapi3` wouldn't like that
data OfferedActionChooseQualified = OfferedActionChooseQualified
  { options :: [QualifiedText]
  , action :: InputActionQualified
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON OfferedActionChooseQualified
data OfferedActionChooseText = OfferedActionChooseText
  { options :: [Text]
  , action :: InputAction
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON OfferedActionChooseText
data OfferedActionChooseOrEnterText = OfferedActionChooseOrEnterText
  { options :: [Text]
  , action :: InputAction
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON OfferedActionChooseOrEnterText

-- data InputType = Input | NoInput
-- type NoInputAction' = OfferedAction0 NoInput
-- type InputAction' = OfferedAction0 Input
-- data OfferedAction0 (k :: InputType) where
--   C1 :: OfferedAction0 NoInput
-- deriving instance Generic (OfferedAction0 k)

data SomeAction
  = NoInputAction NoInputAction
  | InputAction InputAction
  | InputActionQualified InputActionQualified

-- TODO more constructors
-- TODO reorder constructors logically
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
  deriving (Show, Generic)
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
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON InputAction
data InputActionQualified -- TODO rename this and others
  = AUseValueCon
  | AUseSaturatedValueCon
  | AUseTypeCon
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON InputActionQualified

-- TODO use a GADT for actions, so this becomes `(Action a, a)`? awkward for JSON instances - in theory I don't see any reason we can't derive Generic when GADTs are only used for phantom types, but the closest discussion I've found is this (and its linked issues): https://gitlab.haskell.org/ghc/ghc/-/issues/8560
-- TODO GADT for actions would also be nice as we could tag by `Expr`/`Sig`/`Def` (EDIT: actually this gets complex because they're not mutually exclusive e.g. all sig actions can also appear in bodies)
-- TODO ditch the type and make these separate API calls / functions
data ActionRequest
  = ActionRequestSimple NoInputAction -- TODO rename: `ActionRequestNoInput ActionNoInput`
  | ActionRequestText ActionRequestText
  | ActionRequestQualified ActionRequestQualified
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionRequest

-- TODO rename constructors?
-- TODO parameterise? awkward for serialisation?
data ActionRequestText = MkActionRequestText
  { action :: InputAction
  , option :: Text -- TODO or number from list? would that require backend to remember some state?
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionRequestText
data ActionRequestQualified = MkActionRequestQualified
  { action :: InputActionQualified
  , option :: QualifiedText
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionRequestQualified

-- | Filter on variables and constructors according to whether they
-- have a function type.
data FunctionFiltering
  = Everything
  | OnlyFunctions
  | NoFunctions

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
-- This is probably useful elsewhere, but we currently just need it here
data SomeNode a b
  = ExprNode (Expr' (Meta a) (Meta b))
  | TypeNode (Type' (Meta b))
  | -- | If/when we model all bindings with 'Bind'', we will want to generalise this.
    CaseBindNode (Bind' (Meta a))

actionsForDef ::
  Level ->
  -- | All existing definitions, along with their mutability
  Map GVarName (Editable, Def) ->
  -- | The name of a definition in the map
  GVarName ->
  [SomeAction]
actionsForDef l defs defName = prioritySort l $ catMaybes [rename, duplicate, delete]
  where
    rename = do
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
  GVarName ->
  Editable ->
  ID ->
  Expr ->
  [SomeAction]
actionsForDefBody _ _ _ NonEditable _ _ = mempty
actionsForDefBody tydefs l defName mut@Editable id expr =
  let raiseAction' = NoInputAction ARaise
   in prioritySort l $ case findNodeWithParent id expr of
        Nothing -> mempty
        Just (ExprNode e, p) ->
          let raiseAction = case p of
                Nothing -> [] -- at root already, cannot raise
                Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
                _ -> [raiseAction']
           in basicActionsForExpr tydefs l defName e <> raiseAction
        Just (TypeNode t, p) ->
          let raiseAction = case p of
                Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
                _ -> [raiseAction']
           in (basicActionsForType l defName t <> compoundActionsForType l t)
                <> raiseAction
        Just (CaseBindNode b, _) -> actionsForBinding l defName mut b

-- | Given a the type signature of a Def and the ID of a node in it,
-- return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  GVarName ->
  Editable ->
  ID ->
  Type ->
  [SomeAction]
actionsForDefSig _ _ NonEditable _ _ = mempty
actionsForDefSig l defName Editable id ty =
  let raiseAction =
        [ NoInputAction ARaiseType
        | id /= getID ty
        ]
   in prioritySort l $ case findType id ty of
        Nothing -> mempty
        Just t ->
          basicActionsForType l defName t
            <> compoundActionsForType l t
            <> raiseAction

-- | Bindings support just one action: renaming.
actionsForBinding ::
  Level ->
  GVarName ->
  Editable ->
  Bind' (Meta (Maybe TypeCache)) ->
  [SomeAction]
actionsForBinding _ _ NonEditable _ = mempty
actionsForBinding l defName Editable b =
  [ InputAction ARenamePatternVar
  ]

-- | Find a node in the AST by its ID, and also return its parent
findNodeWithParent ::
  (Data a, Data b, Eq a) =>
  ID ->
  Expr' (Meta a) (Meta b) ->
  Maybe (SomeNode a b, Maybe (SomeNode a b))
findNodeWithParent id x = do
  z <- focusOn id x
  Just $ case z of
    InExpr ez -> (ExprNode $ target ez, ExprNode . target <$> up ez)
    InType tz ->
      ( TypeNode $ target tz
      , Just $
          maybe
            (ExprNode $ target $ unfocusType tz)
            (TypeNode . target)
            (up tz)
      )
    InBind (BindCase bz) -> (CaseBindNode $ caseBindZFocus bz, Just . ExprNode . target . unfocusCaseBind $ bz)

-- | Find a sub-type in a larger type by its ID.
findType :: forall b. Data b => ID -> Type' (Meta b) -> Maybe (Type' (Meta b))
findType id ty = target <$> focusOnTy id ty

-- | Given an expression, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForExpr :: TypeDefMap -> Level -> GVarName -> Expr -> [SomeAction]
basicActionsForExpr tydefs l defName expr = case expr of
  EmptyHole{} -> universalActions <> emptyHoleActions
  Hole{} -> defaultActions <> holeActions
  Ann{} -> defaultActions <> annotationActions
  Lam{} -> defaultActions <> lambdaActions
  LAM{} -> defaultActions <> bigLambdaActions
  Let _ v e _ -> defaultActions <> letActions v e
  Letrec _ _ _ t _ -> defaultActions <> letRecActions (Just t)
  e -> let _ = e ^. _exprMetaLens in defaultActions ++ expert annotateExpression
  where
    m = expr ^. _exprMetaLens

    insertVariable = InputAction AUseVar

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    insertVariableSaturatedRefined = InputAction ASaturatedFunction
    annotateExpression = NoInputAction AConstructAnn
    applyFunction = NoInputAction AConstructApp
    applyType = NoInputAction AConstructAPP
    patternMatch = NoInputAction AMakeCase
    makeLambda = InputAction AMakeLambda
    makeTypeAbstraction = InputAction AConstructBigLambda
    useValueConstructor = InputActionQualified AUseValueCon
    useSaturatedRefinedValueConstructor = InputActionQualified AUseSaturatedValueCon
    makeLetBinding = InputAction AMakeLet
    makeLetrec = InputAction AMakeLetRec
    enterHole = NoInputAction AEnterHole
    finishHole = NoInputAction AFinishHole
    removeAnnotation = NoInputAction ARemoveAnn
    renameVariable = InputAction ARenameLambda
    renameTypeVariable = InputAction ARenameLAM
    makeLetRecursive = NoInputAction AConvertLetToLetrec
    renameLet t = InputAction ARenameLetBinding
    deleteExpr = NoInputAction ADeleteExpr
    expert = if l == Expert then (: []) else const []
    emptyHoleActions = case l of
      Beginner ->
        [ insertVariable
        , useValueConstructor
        ]
      _ ->
        [ insertVariable
        , insertVariableSaturatedRefined
        , useValueConstructor
        , useSaturatedRefinedValueConstructor
        , makeLetBinding
        , makeLetrec
        , enterHole
        ]
          ++ expert annotateExpression
    holeActions = finishHole : expert annotateExpression
    annotationActions = expert removeAnnotation
    lambdaActions = renameVariable : expert annotateExpression
    bigLambdaActions = concatMap expert [annotateExpression, renameTypeVariable]
    letActions v e =
      renameLet (e ^? _exprMetaLens % _type % _Just % _synthed)
        : munless (unLocalName v `Set.member` freeVars e) [makeLetRecursive]
          <> expert annotateExpression
    letRecActions t = renameLet t : expert annotateExpression

    -- Actions for every expression node
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    universalActions =
      let both = case l of
            Beginner ->
              [ makeLambda
              ]
            Intermediate ->
              [ makeLambda
              , applyFunction
              ]
            Expert ->
              [ applyFunction
              , applyType
              , makeLambda
              , makeTypeAbstraction
              ]
          synthTy = m ^? _type % _Just % _synthed
          synOnly ty = case getTypeDefInfo' tydefs ty of
            Left TDIHoleType{} -> Just patternMatch
            Right (TypeDefInfo _ _ TypeDefAST{}) -> Just patternMatch
            _ -> Nothing
       in (synOnly =<< synthTy) ?: both

    -- Actions for every expression node except holes and annotations
    defaultActions = universalActions <> [deleteExpr]

(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x : xs
Nothing ?: xs = xs
infixr 5 ?:

-- | Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> GVarName -> Type -> [SomeAction]
basicActionsForType l defName ty = case ty of
  TEmptyHole _ -> universalActions <> emptyHoleActions
  TForall _ _ k _ -> defaultActions <> forAllActions k
  _ -> defaultActions
  where
    m = ty ^. _typeMetaLens
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    constructFunctionType = NoInputAction AConstructFun
    constructPolymorphicType = InputAction AConstructForall
    constructTypeApplication = NoInputAction AConstructAPP
    useTypeConstructor = InputActionQualified AUseTypeCon
    useTypeVariable = InputAction AUseTypeVar
    renameTypeVariable k m' = InputAction ARenameForall
    deleteType = NoInputAction ADeleteType
    emptyHoleActions = [useTypeConstructor] <> expert useTypeVariable
    forAllActions k = expert $ renameTypeVariable k m
    universalActions =
      -- Actions for every type node
      [constructFunctionType]
        <> concatMap
          expert
          [ constructPolymorphicType
          , constructTypeApplication
          ]
    defaultActions = universalActions <> [deleteType] -- Actions for every type node except empty holes
    expert = if l == Expert then (: []) else const []

-- | These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: Level -> Type' (Meta a) -> [SomeAction]
compoundActionsForType l ty = case ty of
  TFun _m a b -> [addFunctionArgument a b]
  _ -> []
  where
    -- This action traverses the function type and adds a function arrow to the end of it,
    -- resulting in a new argument type. The result type is unchanged.
    -- The cursor location is also unchanged.
    -- e.g. A -> B -> C ==> A -> B -> ? -> C
    addFunctionArgument a b = NoInputAction AAddInput

-- TODO just combine these and take a `SomeAction`?
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
priorityInputActionQualified :: InputActionQualified -> Level -> Int
priorityInputActionQualified = \case
  AUseValueCon -> P.useValueCon
  AUseSaturatedValueCon -> P.useSaturatedValueCon
  AUseTypeCon -> P.useTypeCon

-- getInput :: SomeAction -> (OfferedAction, [ProgAction])
-- getInput :: Level -> SomeAction -> (OfferedAction, [ProgAction])
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
--   SomeAction ->
--   (OfferedAction, Either Text [ProgAction])

-- TODO the `Maybe ID` here is awkward - some actions require ID's and others don't, we should reflect this in the types
inputAction :: MonadQueryApp m => Level -> GVarName -> Maybe ID -> InputAction -> m OfferedAction
inputAction level defName mid action = case action of
  AMakeLambda -> do
    options <- genName' False
    -- q <- handleQuestion $ GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  AUseVar -> do
    ((_types, locals), globals) <- varsInScope'
    let options = unName . unLocalName . fst <$> (if level == Beginner then noFunctions locals else locals)
    pure $ ChooseText OfferedActionChooseText{action, options}
  ASaturatedFunction -> do
    ((_types, locals), globals) <- varsInScope'
    let options = unName . unLocalName . fst <$> onlyFunctions locals
    pure $ ChooseText OfferedActionChooseText{action, options}
  AMakeLet -> do
    options <- genName' False
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  AMakeLetRec -> do
    options <- genName' False
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  AConstructBigLambda -> do
    options <- genName' True
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  AUseTypeVar -> do
    ((types, _locals), _globals) <- varsInScope'
    let options = unName . unLocalName . fst <$> types
    pure $ ChooseText OfferedActionChooseText{action, options}
  AConstructForall -> do
    options <- genName' True
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  ARenameDef ->
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options = []}
  ARenamePatternVar -> do
    options <- genName' True
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  ARenameLambda -> do
    options <- genName' False
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  ARenameLAM -> do
    options <- genName' True
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  ARenameLetBinding -> do
    options <- genName' False
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  ARenameForall -> do
    options <- genName' True
    pure $ ChooseOrEnterText OfferedActionChooseOrEnterText{action, options}
  where
    -- TODO `handleQuestion` imported from `App` - wary of import cycles
    -- TODO use `generateNameExpr` directly?
    -- TODO by always passing `Nothing`, we lose good name hints - see `baseNames` (we previously passed `Just` for only AMakeLambda,AConstructBigLambda,ARenamePatternVar,ARenameLambda,ARenameLAM,ARenameLetBinding,ARenameForall)
    genName' tk = do
      id <- maybe (throwError SelectionNoID) pure mid
      unName <<$>> handleQuestion (GenerateName defName id $ if tk then Right Nothing else Left Nothing)
    -- TODO much as above
    varsInScope' = do
      id <- maybe (throwError SelectionNoID) pure mid
      handleQuestion $ VariablesInScope defName id

inputActionQualified :: TypeDefMap -> Level -> InputActionQualified -> OfferedAction
inputActionQualified typeDefs level action = case action of
  AUseValueCon ->
    let options = map (fromGlobal . valConName) . (if level == Beginner then noFunctionsCon else identity) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in ChooseQualified OfferedActionChooseQualified{action, options}
  -- TODO note 1
  AUseSaturatedValueCon ->
    let options = map (fromGlobal . valConName) . (onlyFunctionsCon) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in ChooseQualified OfferedActionChooseQualified{action, options}
  AUseTypeCon ->
    let options = fromGlobal . fst <$> Map.toList typeDefs
     in ChooseQualified OfferedActionChooseQualified{action, options}
  where
    fromGlobal n = toQualifiedText (map unName $ unModuleName $ qualifiedModule n, unName $ baseName n)

-- TODO rename
-- essentially, map a high-level (and serialisation-friendly) action to a sequence of low-level ones
-- TODO ID is unused for a def action - probably indicates we need to split the action type up
mkAction ::
  DefMap ->
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) ->
  ActionRequest ->
  Either Text [ProgAction]
mkAction defs def defName mNodeSel = \case
  ActionRequestSimple AMakeCase ->
    toProgAction [ConstructCase]
  ActionRequestSimple AConvertLetToLetrec ->
    toProgAction [ConvertLetToLetrec]
  ActionRequestSimple AConstructApp ->
    toProgAction [ConstructApp, Move Child2]
  ActionRequestSimple AConstructAPP ->
    toProgAction [ConstructAPP, EnterType]
  ActionRequestSimple AConstructAnn ->
    toProgAction [ConstructAnn]
  ActionRequestSimple ARemoveAnn ->
    toProgAction [RemoveAnn]
  ActionRequestSimple AFinishHole ->
    toProgAction [FinishHole]
  ActionRequestSimple AEnterHole ->
    toProgAction [EnterHole]
  ActionRequestSimple AConstructFun ->
    toProgAction [ConstructArrowL, Move Child1]
  ActionRequestSimple AAddInput -> do
    -- case et of
    --   Right t ->
    --     let unfoldFun' :: Type' a -> (Type' a, [Type' a])
    --         unfoldFun' = undefined

    --         (_resultType, argTypes) = unfoldFun' t

    --         moveToLastArg = replicate (length argTypes) (Move Child2)

    --         moveBack = replicate (length argTypes) (Move Parent)
    --      in toProgAction $ moveToLastArg <> [ConstructArrowR] <> moveBack
    --   _ -> Left "expected TFun"
    -- l <- case et of
    --   Right (TFun _ a b) -> pure $ NE.length $ fst $ unfoldFun a b
    --   _ -> Left "expected TFun"
    -- TODO don't hardcode
    -- it's awkward though - there's a reason this was in its own section before this commit
    let l = 1
    let moveToLastArg = replicate l (Move Child2)
        moveBack = replicate l (Move Parent)
     in toProgAction $ moveToLastArg <> [ConstructArrowR] <> moveBack
  ActionRequestSimple AConstructTypeApp ->
    toProgAction [ConstructTApp, Move Child1]
  ActionRequestSimple ADuplicateDef ->
    let sigID = getID $ astDefType def

        bodyID = getID $ astDefExpr def

        copyName = uniquifyDefName (qualifiedModule defName) (unName (baseName defName) <> "Copy") defs
     in pure
          [ CreateDef (qualifiedModule defName) (Just copyName)
          , CopyPasteSig (defName, sigID) []
          , CopyPasteBody (defName, bodyID) []
          ]
  ActionRequestSimple ARaise -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
  ActionRequestSimple ARaiseType -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteSig (defName, id) [SetCursor id, Move Parent, Delete]]
  ActionRequestSimple ADeleteDef ->
    pure [DeleteDef defName]
  ActionRequestSimple ADeleteExpr ->
    toProgAction [Delete]
  ActionRequestSimple ADeleteType ->
    toProgAction [Delete]
  -- TODO rename `tInput`
  ActionRequestText MkActionRequestText{action, option = tInput} ->
    let -- TODO should we handle "parsing" in to trusted input here
        -- see the comment on `Action` - given that we're now not exposing that type via the API, it should probably use the rich versions
        -- I think we previously were inconsistent, or just hadn't given this much thought

        -- TODO hmm this needn't necessarily be local
        -- and obviously we shouldn't use `unsafeMkLocalName` etc.
        tInputTmVar = LocalVarRef $ unsafeMkLocalName tInput
     in case action of
          AMakeLambda ->
            toProgAction [ConstructLam $ Just tInput]
          AUseVar ->
            toProgAction [ConstructVar tInputTmVar]
          ASaturatedFunction -> do
            oR <- offerRefined
            toProgAction [if oR then InsertRefinedVar tInputTmVar else InsertSaturatedVar tInputTmVar]
          AMakeLet ->
            toProgAction [ConstructLet $ Just tInput]
          AMakeLetRec ->
            toProgAction [ConstructLetrec $ Just tInput]
          AConstructBigLambda ->
            toProgAction [ConstructLAM $ Just tInput]
          AUseTypeVar ->
            toProgAction [ConstructTVar tInput]
          AConstructForall ->
            toProgAction [ConstructTForall $ Just tInput, Move Child1]
          ARenameDef ->
            pure [RenameDef defName tInput]
          ARenamePatternVar ->
            toProgAction [RenameCaseBinding tInput]
          ARenameLambda ->
            toProgAction [RenameLam tInput]
          ARenameLAM ->
            toProgAction [RenameLAM tInput]
          ARenameLetBinding ->
            toProgAction [RenameLet tInput]
          ARenameForall ->
            toProgAction [RenameForall tInput]
  ActionRequestQualified MkActionRequestQualified{action, option = (fromQualifiedText -> option)} -> case action of
    AUseValueCon ->
      toProgAction [ConstructCon option]
    AUseSaturatedValueCon -> do
      -- NB: Exactly one of the saturated and refined actions will be available
      -- (depending on whether we have useful type information to hand).
      -- We put the same labels on each.
      oR <- offerRefined
      toProgAction [if oR then ConstructRefinedCon option else ConstructSaturatedCon option]
    AUseTypeCon ->
      toProgAction [ConstructTCon option]
  where
    toProgAction actions = do
      id <- id'
      sigOrBody <- maybeToEither "no node selection" $ fst <$> mNodeSel
      let a = case sigOrBody of
            SigNode -> SigAction
            BodyNode -> BodyAction
      pure [MoveToDef defName, a $ SetCursor id : actions]
    -- If we have a useful type, offer the refine action, otherwise offer the
    -- saturate action.
    -- TODO I don't really understand the difference that `offerRefined` represents
    -- and maybe we should be moving all this to a higher level anyway - the rest of this function has little logic, and we could potentially avoid error handling
    offerRefined = do
      id <- id'
      case findNodeWithParent id $ astDefExpr def of
        Just (ExprNode e, _) -> pure $ case e ^. _exprMetaLens ^? _type % _Just % _chkedAt of
          Just (TEmptyHole _) -> False
          Just (THole _ _) -> False
          Just _ -> True
          _ -> False
        _ -> Left "expected TypeNode"
    id' = maybeToEither "no node selection" $ snd <$> mNodeSel

getEditableDef defs defName = do
  (m, d) <- Map.lookup defName defs
  case m of
    NonEditable -> Nothing
    Editable -> pure d
getEditableASTDef defs defName = defAST =<< getEditableDef defs defName

withExpr et as = case et of
  Left e -> pure $ as e
  Right _ -> Left "expected expression" -- TODO we should really be able to rule this out statically

prioritySort ::
  Level ->
  [SomeAction] ->
  [SomeAction]
prioritySort l = sortOn $ \case
  NoInputAction x -> priorityNoInputAction x l
  InputAction x -> priorityInputAction x l
  InputActionQualified x -> priorityInputActionQualified x l

-- data Loc = LocDef | LocSig | LocBody
-- type family LocContents loc where
--   LocContents LocDef = ()
--   LocContents LocSig = Type
--   LocContents LocBody = Either Expr Type
-- data Act (freeInputAllowed :: Bool) (location :: Loc) (inputData :: Kind.Type)

-- none, choice only (subtypes - text, var, constructor), choice with free text

-- Extract the source of the function type we were checked at
-- i.e. the type that a lambda-bound variable would have here
lamVarTy = \case
  TFun _ s _ -> pure s
  _ -> Nothing

-- Extract the kind a forall-bound variable would have here
lAMVarKind = \case
  TForall _ _ k _ -> Just k
  _ -> Nothing

-- TODO is each of these only used once?
noFunctions = filter $ \case
  (_, TFun{}) -> False
  _ -> True
onlyFunctions = filter $ \case
  (_, TFun{}) -> True
  _ -> False
noFunctionsCon = filter $ null . valConArgs
onlyFunctionsCon = filter $ not . null . valConArgs
