{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# HLINT ignore "Use section" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
  SomeAction (..),
  InputAction (..),
  NoInputAction (..),
  Level (..),
  inputAction,
  mkActionNoInput,
  mkActionInput,
  QualifiedText (..),
  fromQualifiedText,
  toQualifiedText,
  ActionOption (..),
  OfferedAction (..),
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
  unsafeMkGlobalName,
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

data ActionOption = ActionOption
  { option :: Text
  , qualification :: Maybe (NonEmpty Text) -- TODO hmm - this is isomorphic to normal list - too cute? also rename
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ActionOption

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

-- TODO rename `ActionOptions` or similar? this doesn't seem right as it doesn't contain the action itself
data OfferedAction = OfferedAction
  { options :: [ActionOption]
  , allowFreeText :: Bool
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrimerJSON OfferedAction

data SomeAction
  = NoInputAction NoInputAction
  | InputAction InputAction
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via PrimerJSON SomeAction

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

-- TODO simplify?

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
  [SomeAction]
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
  [SomeAction]
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
basicActionsForExpr :: TypeDefMap -> Level -> Expr -> [SomeAction]
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
basicActionsForType :: Level -> Type -> [SomeAction]
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
compoundActionsForType :: Type' (Meta a) -> [SomeAction]
compoundActionsForType ty = case ty of
  TFun _m _ _ -> [NoInputAction AAddInput]
  _ -> []

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
inputAction :: MonadQueryApp m => TypeDefMap -> Level -> GVarName -> Maybe ID -> InputAction -> m OfferedAction
inputAction typeDefs level defName mid = \case
  AMakeLambda -> do
    options <- genName' False
    -- q <- handleQuestion $ GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
    pure OfferedAction{options, allowFreeText = True}
  AUseVar -> do
    -- TODO DRY next 10 lines or so
    ((_types, locals), globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> (if level == Beginner then noFunctions locals else locals)
        optionsGlob = fromGlobal . fst <$> (if level == Beginner then noFunctions globals else globals)
        options = optionsLoc <> optionsGlob
    pure OfferedAction{options, allowFreeText = False}
  ASaturatedFunction -> do
    ((_types, locals), globals) <- varsInScope'
    let optionsLoc = flip ActionOption Nothing . unName . unLocalName . fst <$> onlyFunctions locals
        optionsGlob = fromGlobal . fst <$> onlyFunctions globals
        options = optionsLoc <> optionsGlob
    pure OfferedAction{options, allowFreeText = False}
  AMakeLet -> do
    options <- genName' False
    pure OfferedAction{options, allowFreeText = True}
  AMakeLetRec -> do
    options <- genName' False
    pure OfferedAction{options, allowFreeText = True}
  AConstructBigLambda -> do
    options <- genName' True
    pure OfferedAction{options, allowFreeText = True}
  AUseTypeVar -> do
    ((types, _locals), _globals) <- varsInScope'
    let options = flip ActionOption Nothing . unName . unLocalName . fst <$> types
    pure OfferedAction{options, allowFreeText = False}
  AConstructForall -> do
    options <- genName' True
    pure OfferedAction{options, allowFreeText = True}
  ARenameDef ->
    pure OfferedAction{options = [], allowFreeText = True}
  ARenamePatternVar -> do
    options <- genName' True
    pure OfferedAction{options, allowFreeText = True}
  ARenameLambda -> do
    options <- genName' False
    pure OfferedAction{options, allowFreeText = True}
  ARenameLAM -> do
    options <- genName' True
    pure OfferedAction{options, allowFreeText = True}
  ARenameLetBinding -> do
    options <- genName' False
    pure OfferedAction{options, allowFreeText = True}
  ARenameForall -> do
    options <- genName' True
    pure OfferedAction{options, allowFreeText = True}
  AUseValueCon ->
    let options = map (fromGlobal . valConName) . (if level == Beginner then noFunctionsCon else identity) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure OfferedAction{options, allowFreeText = False}
  AUseSaturatedValueCon ->
    let options = map (fromGlobal . valConName) . onlyFunctionsCon . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure OfferedAction{options, allowFreeText = False}
  AUseTypeCon ->
    let options = fromGlobal . fst <$> Map.toList typeDefs
     in pure OfferedAction{options, allowFreeText = False}
  where
    -- TODO `handleQuestion` imported from `App` - wary of import cycles
    -- TODO use `generateNameExpr` directly?
    -- TODO by always passing `Nothing`, we lose good name hints - see `baseNames` (we previously passed `Just` for only AMakeLambda,AConstructBigLambda,ARenamePatternVar,ARenameLambda,ARenameLAM,ARenameLetBinding,ARenameForall)
    genName' tk = do
      id <- maybe (throwError SelectionNoID) pure mid
      flip ActionOption Nothing . unName <<$>> handleQuestion (GenerateName defName id $ if tk then Right Nothing else Left Nothing)
    -- TODO much as above
    varsInScope' = do
      id <- maybe (throwError SelectionNoID) pure mid
      handleQuestion $ VariablesInScope defName id
    fromGlobal n = ActionOption{option = unName $ baseName n, qualification = Just $ map unName $ unModuleName $ qualifiedModule n}

-- TODO move out of this module?
-- TODO rename
-- essentially, map a high-level (and serialisation-friendly) action to a sequence of low-level ones
-- TODO ID is unused for a def action - probably indicates we need to split the action type up
mkActionNoInput ::
  DefMap ->
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) -> -- TODO why tuple rather than `NodeSelection`?
  NoInputAction ->
  Either Text [ProgAction]
mkActionNoInput defs def defName mNodeSel = \case
  AMakeCase ->
    toProgAction [ConstructCase]
  AConvertLetToLetrec ->
    toProgAction [ConvertLetToLetrec]
  AConstructApp ->
    toProgAction [ConstructApp, Move Child2]
  AConstructAPP ->
    toProgAction [ConstructAPP, EnterType]
  AConstructAnn ->
    toProgAction [ConstructAnn]
  ARemoveAnn ->
    toProgAction [RemoveAnn]
  AFinishHole ->
    toProgAction [FinishHole]
  AEnterHole ->
    toProgAction [EnterHole]
  AConstructFun ->
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    toProgAction [ConstructArrowL, Move Child1]
  AAddInput -> do
    -- This action traverses the function type and adds a function arrow to the end of it,
    -- resulting in a new argument type. The result type is unchanged.
    -- The cursor location is also unchanged.
    -- e.g. A -> B -> C ==> A -> B -> ? -> C

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
  AConstructTypeApp ->
    toProgAction [ConstructTApp, Move Child1]
  ADuplicateDef ->
    let sigID = getID $ astDefType def

        bodyID = getID $ astDefExpr def

        copyName = uniquifyDefName (qualifiedModule defName) (unName (baseName defName) <> "Copy") defs
     in pure
          [ CreateDef (qualifiedModule defName) (Just copyName)
          , CopyPasteSig (defName, sigID) []
          , CopyPasteBody (defName, bodyID) []
          ]
  ARaise -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
  ARaiseType -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteSig (defName, id) [SetCursor id, Move Parent, Delete]]
  ADeleteDef ->
    pure [DeleteDef defName]
  ADeleteExpr ->
    toProgAction [Delete]
  ADeleteType ->
    toProgAction [Delete]
  where
    -- TODO DRY
    toProgAction actions = do
      id <- id'
      sigOrBody <- maybeToEither "no node selection" $ fst <$> mNodeSel
      let a = case sigOrBody of
            SigNode -> SigAction
            BodyNode -> BodyAction
      pure [MoveToDef defName, a $ SetCursor id : actions]
    -- If we have a useful type, offer the refine action, otherwise offer the
    -- saturate action.
    id' = maybeToEither "no node selection" $ snd <$> mNodeSel

mkActionInput ::
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) ->
  ActionOption ->
  InputAction ->
  Either Text [ProgAction]
-- TODO rename `tInput`
mkActionInput def defName mNodeSel tInput0 = \case
  AMakeLambda -> do
    t <- tInputLocal
    toProgAction [ConstructLam $ Just t]
  AUseVar ->
    toProgAction [ConstructVar tInputTmVar]
  ASaturatedFunction -> do
    oR <- offerRefined
    toProgAction [if oR then InsertRefinedVar tInputTmVar else InsertSaturatedVar tInputTmVar]
  AMakeLet -> do
    t <- tInputLocal
    toProgAction [ConstructLet $ Just t]
  AMakeLetRec -> do
    t <- tInputLocal
    toProgAction [ConstructLetrec $ Just t]
  AConstructBigLambda -> do
    t <- tInputLocal
    toProgAction [ConstructLAM $ Just t]
  AUseTypeVar -> do
    t <- tInputLocal
    toProgAction [ConstructTVar t]
  AConstructForall -> do
    t <- tInputLocal
    toProgAction [ConstructTForall $ Just t, Move Child1]
  ARenameDef -> do
    t <- tInputLocal
    pure [RenameDef defName t]
  ARenamePatternVar -> do
    t <- tInputLocal
    toProgAction [RenameCaseBinding t]
  ARenameLambda -> do
    t <- tInputLocal
    toProgAction [RenameLam t]
  ARenameLAM -> do
    t <- tInputLocal
    toProgAction [RenameLAM t]
  ARenameLetBinding -> do
    t <- tInputLocal
    toProgAction [RenameLet t]
  ARenameForall -> do
    t <- tInputLocal
    toProgAction [RenameForall t]
  AUseValueCon -> do
    o <- option
    toProgAction [ConstructCon o]
  AUseSaturatedValueCon -> do
    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    oR <- offerRefined
    o <- option
    toProgAction [if oR then ConstructRefinedCon o else ConstructSaturatedCon o]
  AUseTypeCon -> do
    o <- option
    toProgAction [ConstructTCon o]
  where
    -- TODO should we handle "parsing" in to trusted input here
    -- see the comment on `Action` - given that we're now not exposing that type via the API, it should probably use the rich versions
    -- I think we previously were inconsistent, or just hadn't given this much thought

    -- TODO obviously we shouldn't use "unsafe" functions
    tInputTmVar = case tInput0.qualification of
      Just q -> GlobalVarRef $ unsafeMkGlobalName (q, tInput0.option)
      Nothing -> LocalVarRef $ unsafeMkLocalName tInput0.option
    -- TODO DRY
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
    tInputLocal = case tInput0.qualification of
      Just _ -> Left $ "unexpected global: " <> show tInput0
      Nothing -> pure tInput0.option
    option = case tInput0.qualification of
      Nothing -> Left "no qual"
      Just q -> pure (q, tInput0.option)

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
  [SomeAction] ->
  [SomeAction]
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
