{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Compute all the possible actions which can be performed on a definition.
-- This module is expected to be imported qualified, due to various potential name clashes.
module Primer.Action.Available (
  Action (..),
  InputAction (..),
  NoInputAction (..),
  forDef,
  forBody,
  forSig,
  Option (..),
  FreeInput (..),
  Options (..),
  options,
  forTypeDef,
  forTypeDefParamNode,
  forTypeDefConsNode,
  forTypeDefConsFieldNode,
) where

import Foreword

import Data.Either.Extra (eitherToMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Extra (fst3)
import Optics (
  to,
  (%),
  (^.),
  (^..),
  (^?),
  _Just,
 )
import Primer.Action.Priorities qualified as P
import Primer.App.Base (
  DefSelection (..),
  Editable (..),
  Level (..),
  NodeSelection (..),
  NodeType (..),
  Selection' (..),
  TypeDefConsFieldSelection (..),
  TypeDefConsSelection (..),
  TypeDefNodeSelection (..),
  TypeDefSelection (..),
  getTypeDefConFieldType,
 )
import Primer.Core (
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  ModuleName (unModuleName),
  Type,
  Type' (..),
  TypeMeta,
  ValConName,
  getID,
  unLocalName,
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _synthed,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Utils (forgetTypeMetadata, freeVars)
import Primer.Def (
  ASTDef (..),
  DefMap,
 )
import Primer.Def.Utils (globalInUse)
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (unName)
import Primer.Primitives (tChar, tInt)
import Primer.Questions (
  generateNameExpr,
  generateNameTy,
  variablesInScopeExpr,
  variablesInScopeTy,
 )
import Primer.TypeDef (
  ASTTypeDef (..),
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
  target,
 )

-- | An offered action.
data Action
  = NoInput NoInputAction
  | Input InputAction
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Action

-- | An action which can be applied without requiring further input.
data NoInputAction
  = MakeCase
  | MakeApp
  | MakeAPP
  | MakeAnn
  | RemoveAnn
  | LetToRec
  | Raise
  | EnterHole
  | RemoveHole
  | DeleteExpr
  | MakeFun
  | AddInput
  | MakeTApp
  | RaiseType
  | DeleteType
  | DuplicateDef
  | DeleteDef
  | AddConField
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NoInputAction

-- | An action which requires extra data (often a name) before it can be applied.
data InputAction
  = MakeCon
  | MakeInt
  | MakeChar
  | MakeVar
  | MakeVarSat
  | MakeLet
  | MakeLetRec
  | MakeLam
  | MakeLAM
  | RenamePattern
  | RenameLet
  | RenameLam
  | RenameLAM
  | MakeTCon
  | MakeTVar
  | MakeForall
  | RenameForall
  | RenameDef
  | RenameType
  | RenameCon
  | RenameTypeParam
  | AddCon
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON InputAction

forDef ::
  DefMap ->
  Level ->
  Editable ->
  GVarName ->
  [Action]
forDef _ _ NonEditable _ = mempty
forDef defs l Editable defName =
  sortByPriority l $
    [Input RenameDef, NoInput DuplicateDef]
      <> mwhen
        -- ensure the definition is not in use, otherwise the action will not succeed
        (not $ globalInUse defName $ Map.delete defName defs)
        [NoInput DeleteDef]

forBody ::
  TypeDefMap ->
  Level ->
  Editable ->
  Expr ->
  ID ->
  [Action]
forBody _ _ NonEditable _ _ = mempty
forBody tydefs l Editable expr id = sortByPriority l $ case findNodeWithParent id expr of
  Nothing -> mempty
  Just (ExprNode e, p) ->
    let raiseAction = case p of
          Nothing -> [] -- at root already, cannot raise
          Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
          _ -> [NoInput Raise]
     in forExpr tydefs l e <> raiseAction
  Just (TypeNode t, p) ->
    let raiseAction = case p of
          Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
          _ -> [NoInput Raise]
     in forType l t <> raiseAction
  Just (CaseBindNode _, _) ->
    [Input RenamePattern]

forSig ::
  Level ->
  Editable ->
  Type ->
  ID ->
  [Action]
forSig _ NonEditable _ _ = mempty
forSig l Editable ty id = sortByPriority l $ case findType id ty of
  Nothing -> mempty
  Just t ->
    forType l t
      <> mwhen (id /= getID ty) [NoInput RaiseType]

forExpr :: TypeDefMap -> Level -> Expr -> [Action]
forExpr tydefs l expr =
  universalActions <> synOnly <> case expr of
    EmptyHole{} ->
      annotate
        <> [ Input MakeVar
           , Input MakeCon
           ]
        <> mwhen (Map.member tInt tydefs) [Input MakeInt]
        <> mwhen (Map.member tChar tydefs) [Input MakeChar]
        <> mwhen
          (l /= Beginner)
          [ Input MakeVarSat
          , Input MakeLet
          , Input MakeLetRec
          , NoInput EnterHole
          ]
    Hole{} ->
      delete
        <> annotate
        <> [NoInput RemoveHole]
    Ann{} ->
      delete
        <> mwhen (l == Expert) [NoInput RemoveAnn]
    Lam{} ->
      delete
        <> annotate
        <> [Input RenameLam]
    LAM{} ->
      delete
        <> annotate
        <> mwhen (l == Expert) [Input RenameLAM]
    Let _ v e _ ->
      delete
        <> annotate
        <> [Input RenameLet]
        <> munless (unLocalName v `Set.member` freeVars e) [NoInput LetToRec]
    Letrec{} ->
      delete
        <> annotate
        <> [Input RenameLet]
    _ ->
      delete
        <> annotate
  where
    universalActions = case l of
      Beginner ->
        [ Input MakeLam
        ]
      Intermediate ->
        [ Input MakeLam
        , NoInput MakeApp
        ]
      Expert ->
        [ NoInput MakeApp
        , NoInput MakeAPP
        , Input MakeLam
        , Input MakeLAM
        ]
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    synOnly =
      expr ^.. _exprMetaLens % _type % _Just % _synthed % to (getTypeDefInfo' tydefs) >>= \case
        Left TDIHoleType{} -> [NoInput MakeCase]
        Right (TypeDefInfo _ _ TypeDefAST{}) -> [NoInput MakeCase]
        _ -> []
    annotate = mwhen (l == Expert) [NoInput MakeAnn]
    delete = [NoInput DeleteExpr]

forType :: Level -> Type -> [Action]
forType l type_ =
  universalActions <> case type_ of
    TEmptyHole{} ->
      [Input MakeTCon] <> mwhen (l == Expert) [Input MakeTVar]
    TForall{} ->
      delete <> mwhen (l == Expert) [Input RenameForall]
    TFun{} ->
      delete <> [NoInput AddInput]
    _ ->
      delete
  where
    universalActions =
      [NoInput MakeFun]
        <> mwhen
          (l == Expert)
          [ Input MakeForall
          , NoInput MakeTApp
          ]
    delete = [NoInput DeleteType]

forTypeDef ::
  Level ->
  Editable ->
  [Action]
forTypeDef _ NonEditable = mempty
forTypeDef l Editable =
  sortByPriority
    l
    [ Input RenameType
    , Input AddCon
    ]

forTypeDefParamNode ::
  Level ->
  Editable ->
  [Action]
forTypeDefParamNode _ NonEditable = mempty
forTypeDefParamNode l Editable =
  sortByPriority
    l
    [ Input RenameTypeParam
    ]

forTypeDefConsNode ::
  Level ->
  Editable ->
  [Action]
forTypeDefConsNode _ NonEditable = mempty
forTypeDefConsNode l Editable =
  sortByPriority
    l
    [ NoInput AddConField
    , Input RenameCon
    ]

forTypeDefConsFieldNode ::
  Level ->
  Editable ->
  ASTTypeDef TypeMeta ->
  ValConName ->
  Int ->
  ID ->
  [Action]
forTypeDefConsFieldNode _ NonEditable _ _ _ _ = mempty
forTypeDefConsFieldNode l Editable def con index id =
  maybe mempty (sortByPriority l . forType l) $
    findType id =<< getTypeDefConFieldType def con index

-- | An input for an 'InputAction'.
data Option = Option
  { option :: Text
  , context :: Maybe (NonEmpty Text)
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON Option

-- | The available sorts of free-form input for an 'InputAction'.
data FreeInput
  = -- | Free-form input is not allowed
    FreeNone
  | -- | A free-form string input is allowed, and represents a variable name
    FreeVarName
  | -- | A free-form string input is allowed, and represents a primitive integer
    FreeInt
  | -- | A free-form string input is allowed, and represents a primitive character
    FreeChar
  deriving stock (Show, Read, Generic, Bounded, Enum)
  deriving (ToJSON, FromJSON) via PrimerJSON FreeInput

-- | The available inputs for an 'InputAction'.
data Options = Options
  { opts :: [Option]
  , free :: FreeInput
  }
  deriving stock (Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Options

options ::
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Level ->
  Either (ASTTypeDef TypeMeta) ASTDef ->
  Selection' ID ->
  InputAction ->
  -- | Returns 'Nothing' if an ID was required but not passed, passed but not found in the tree,
  -- or found but didn't correspond to the expected sort of entity (type/expr/pattern).
  Maybe Options
options typeDefs defs cxt level def0 sel0 = \case
  MakeCon ->
    pure
      . noFree
      . map (globalOpt . valConName . snd)
      . filter (not . (&& level == Beginner) . uncurry hasArgsCon)
      . concatMap (\td -> (td,) <$> astTypeDefConstructors td)
      . mapMaybe (typeDefAST . snd)
      $ Map.toList typeDefs
  MakeInt -> pure Options{opts = [], free = FreeInt}
  MakeChar -> pure Options{opts = [], free = FreeChar}
  MakeVar ->
    varOpts
      <&> noFree . map fst . filter (not . (&& level == Beginner) . hasArgsVar . snd)
  MakeVarSat ->
    varOpts
      <&> noFree . map fst . filter (hasArgsVar . snd)
  MakeLet ->
    freeVar <$> genNames (Left Nothing)
  MakeLetRec ->
    freeVar <$> genNames (Left Nothing)
  MakeLam -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lamVarTy)
  MakeLAM -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Right $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lAMVarKind)
  RenamePattern -> do
    CaseBindNode b <- findNode
    freeVar <$> genNames (Left $ b ^? _bindMeta % _type % _Just % _chkedAt)
  RenameLet ->
    findNode >>= \case
      ExprNode (Let _ _ e _) ->
        freeVar <$> genNames (Left $ e ^? _exprMetaLens % _type % _Just % _synthed)
      ExprNode (Letrec _ _ _ t _) -> freeVar <$> genNames (Left $ Just $ forgetTypeMetadata t)
      ExprNode (LetType _ _ t _) -> freeVar <$> genNames (Right $ t ^. _typeMetaLens % _type)
      TypeNode (TLet _ _ t _) -> freeVar <$> genNames (Right $ t ^. _typeMetaLens % _type)
      _ -> Nothing
  RenameLam -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lamVarTy)
  RenameLAM -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Right $ join $ e ^? _exprMetaLens % _type % _Just % _chkedAt % to lAMVarKind)
  MakeTCon ->
    pure $ noFree $ globalOpt . fst <$> Map.toList typeDefs
  MakeTVar ->
    noFree . map (localOpt . unLocalName . fst) . fst3 <$> varsInScope
  MakeForall ->
    freeVar <$> genNames (Right Nothing)
  RenameForall -> do
    TypeNode (TForall _ _ k _) <- findNode
    freeVar <$> genNames (Right $ Just k)
  RenameDef ->
    pure $ freeVar []
  RenameType ->
    pure $ freeVar []
  RenameCon ->
    pure $ freeVar []
  RenameTypeParam ->
    pure $ freeVar []
  AddCon ->
    pure $ freeVar []
  where
    freeVar opts = Options{opts, free = FreeVarName}
    noFree opts = Options{opts, free = FreeNone}
    localOpt = flip Option Nothing . unName
    globalOpt n =
      Option
        { option = unName $ baseName n
        , context = Just $ map unName $ unModuleName $ qualifiedModule n
        }
    varOpts = do
      (_, locals, globals) <- varsInScope
      pure $
        (first (localOpt . unLocalName) <$> locals)
          <> (first globalOpt <$> globals)
    findNode = case sel0 of
      SelectionDef sel -> do
        nodeSel <- sel.node
        def <- eitherToMaybe def0
        case nodeSel.nodeType of
          BodyNode -> fst <$> findNodeWithParent nodeSel.meta (astDefExpr def)
          SigNode -> TypeNode <$> findType nodeSel.meta (astDefType def)
      SelectionTypeDef sel -> do
        (_, zT) <- conField sel
        pure $ TypeNode $ target zT
    genNames typeOrKind =
      map localOpt . flip runReader cxt <$> case sel0 of
        SelectionDef sel -> do
          z <- focusNode =<< sel.node
          pure $ case z of
            Left zE -> generateNameExpr typeOrKind zE
            Right zT -> generateNameTy typeOrKind zT
        SelectionTypeDef sel -> do
          (_, zT) <- conField sel
          pure $ generateNameTy typeOrKind zT
    varsInScope = case sel0 of
      SelectionDef sel -> do
        nodeSel <- sel.node
        focusNode nodeSel <&> \case
          Left zE -> variablesInScopeExpr defs zE
          Right zT -> (variablesInScopeTy zT, [], [])
      SelectionTypeDef sel -> do
        (def, zT) <- conField sel
        pure (astTypeDefParameters def <> variablesInScopeTy zT, [], [])
    focusNode nodeSel = do
      def <- eitherToMaybe def0
      case nodeSel.nodeType of
        BodyNode -> Left . locToEither <$> focusOn nodeSel.meta (astDefExpr def)
        SigNode -> fmap Right $ focusOnTy nodeSel.meta $ astDefType def
    conField sel = do
      (con, field) <- case sel of
        TypeDefSelection _ (Just (TypeDefConsNodeSelection (TypeDefConsSelection con (Just field)))) ->
          Just (con, field)
        _ -> Nothing
      def <- either Just (const Nothing) def0
      map (def,) $ focusOnTy field.meta =<< getTypeDefConFieldType def con field.index
    -- Extract the source of the function type we were checked at
    -- i.e. the type that a lambda-bound variable would have here
    lamVarTy = \case
      TFun _ s _ -> pure s
      _ -> Nothing
    -- Extract the kind a forall-bound variable would have here
    lAMVarKind = \case
      TForall _ _ k _ -> Just k
      _ -> Nothing
    -- Constructor has either type or value arguments
    hasArgsCon td vc =
      not (null (astTypeDefParameters td)) || not (null (valConArgs vc))
    -- Variable can be applied to something i.e. is a function or a polymorphic value
    hasArgsVar = \case
      TFun{} -> True
      TForall{} -> True
      _ -> False

sortByPriority ::
  Level ->
  [Action] ->
  [Action]
sortByPriority l =
  sortOn $
    ($ l) . \case
      NoInput a -> case a of
        MakeCase -> P.makeCase
        MakeApp -> P.applyFunction
        MakeAPP -> P.applyType
        MakeAnn -> P.annotateExpr
        RemoveAnn -> P.removeAnnotation
        LetToRec -> P.makeLetRecursive
        Raise -> P.raise
        EnterHole -> P.enterHole
        RemoveHole -> P.finishHole
        DeleteExpr -> P.delete
        MakeFun -> P.constructFunction
        AddInput -> P.addInput
        MakeTApp -> P.constructTypeApp
        RaiseType -> P.raise
        DeleteType -> P.delete
        DuplicateDef -> P.duplicate
        DeleteDef -> P.delete
        AddConField -> P.addConField
      Input a -> case a of
        MakeCon -> P.useSaturatedValueCon
        MakeInt -> P.makeInt
        MakeChar -> P.makeChar
        MakeVar -> P.useVar
        MakeVarSat -> P.useFunction
        MakeLet -> P.makeLet
        MakeLetRec -> P.makeLetrec
        MakeLam -> P.makeLambda
        MakeLAM -> P.makeTypeAbstraction
        RenamePattern -> P.rename
        RenameLet -> P.rename
        RenameLam -> P.rename
        RenameLAM -> P.rename
        MakeTCon -> P.useTypeCon
        MakeTVar -> P.useTypeVar
        MakeForall -> P.constructForall
        RenameForall -> P.rename
        RenameDef -> P.rename
        RenameType -> P.rename
        AddCon -> P.addCon
        RenameCon -> P.rename
        RenameTypeParam -> P.rename
