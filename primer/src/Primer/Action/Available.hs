{-# LANGUAGE BlockArguments #-}
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
) where

import Foreword

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
  Editable (..),
  Level (..),
  NodeType (..),
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
 )

-- | An offered action.
data Action
  = NoInput NoInputAction
  | Input InputAction
  deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON) via PrimerJSON Action

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NoInputAction

-- | An action which requires extra data (often a name) before it can be applied.
data InputAction
  = MakeCon
  | MakeConSat
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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)
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
          , Input MakeConSat
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

-- | An input for an 'InputAction'.
data Option = Option
  { option :: Text
  , context :: Maybe (NonEmpty Text)
  }
  deriving (Eq, Show, Generic)
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
  deriving (Show, Generic, Bounded, Enum)
  deriving (ToJSON) via PrimerJSON FreeInput

-- | The available inputs for an 'InputAction'.
data Options = Options
  { opts :: [Option]
  , free :: FreeInput
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrimerJSON Options

options ::
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Level ->
  ASTDef ->
  Maybe (NodeType, ID) ->
  InputAction ->
  -- | Returns 'Nothing' if an ID was required but not passed, passed but not found in the tree,
  -- or found but didn't correspond to the expected sort of entity (type/expr/pattern).
  Maybe Options
options typeDefs defs cxt level def mNodeSel = \case
  MakeCon ->
    pure
      . noFree
      . map (globalOpt . valConName . snd)
      . filter (not . (&& level == Beginner) . uncurry hasArgsCon)
      . concatMap (\td -> (td,) <$> astTypeDefConstructors td)
      . mapMaybe (typeDefAST . snd)
      $ Map.toList typeDefs
  MakeConSat ->
    pure
      . noFree
      . map (globalOpt . valConName . snd)
      . filter (uncurry hasArgsCon)
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
  RenameLet -> do
    ExprNode e <- findNode
    freeVar <$> genNames (Left $ e ^? _exprMetaLens % _type % _Just % _synthed)
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
    TypeNode t <- findNode
    freeVar <$> genNames (Right $ t ^. _typeMetaLens % _type)
  RenameDef ->
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
    findNode = do
      (nt, id) <- mNodeSel
      case nt of
        BodyNode -> fst <$> findNodeWithParent id (astDefExpr def)
        SigNode -> TypeNode <$> findType id (astDefType def)
    genNames typeOrKind = do
      z <- focusNode =<< mNodeSel
      pure $ map localOpt $ flip runReader cxt $ case z of
        Left zE -> generateNameExpr typeOrKind zE
        Right zT -> generateNameTy typeOrKind zT
    varsInScope = do
      nodeSel <- mNodeSel
      focusNode nodeSel <&> \case
        Left zE -> variablesInScopeExpr defs zE
        Right zT -> (variablesInScopeTy zT, [], [])
    focusNode (nt, id) = case nt of
      BodyNode -> Left . locToEither <$> focusOn id (astDefExpr def)
      SigNode -> fmap Right $ focusOnTy id $ astDefType def
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
      Input a -> case a of
        MakeCon -> P.useValueCon
        MakeConSat -> P.useSaturatedValueCon
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
