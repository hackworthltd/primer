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
  Options (..),
  options,
) where

import Foreword

import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics (to, (%), (^.), (^..), (^?), _Just)
import Primer.Action.Priorities qualified as P
import Primer.Core (
  Editable (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Level (..),
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
import Primer.Def (ASTDef (..), DefMap)
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
        <> mwhen
          (l /= Beginner)
          [ Input MakeVar
          , Input MakeVarSat
          , Input MakeCon
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

-- | The available inputs for an 'InputAction'.
data Options = Options
  { opts :: [Option]
  , free :: Bool
  -- ^ Allow free text input, rather than just selections from the list.
  }
  deriving (Show, Generic)
  deriving (ToJSON) via PrimerJSON Options

options ::
  TypeDefMap ->
  DefMap ->
  Cxt ->
  Level ->
  ASTDef ->
  Maybe ID ->
  InputAction ->
  -- | Returns 'Nothing' if an ID was required but not passed, or if an ID was passed but not found in the tree.
  Maybe Options
options typeDefs defs cxt level def mid = \case
  MakeCon ->
    let opts = map (fromGlobal . valConName) . (if level == Beginner then filter $ null . valConArgs else identity) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure Options{opts, free = False}
  MakeConSat ->
    let opts = map (fromGlobal . valConName) . filter (not . null . valConArgs) . concatMap astTypeDefConstructors . mapMaybe (typeDefAST . snd) $ Map.toList typeDefs
     in pure Options{opts, free = False}
  MakeVar -> do
    opts <-
      varOptions
        <&> map fst . filter \case
          -- don't show functions here in beginner mode
          (_, TFun{}) -> level /= Beginner
          _ -> True
    pure Options{opts, free = False}
  MakeVarSat -> do
    opts <-
      varOptions
        <&> map fst . filter \case
          -- only display functions
          (_, TFun{}) -> True
          _ -> False
    pure Options{opts, free = False}
  MakeLet -> do
    opts <- genName'
    pure Options{opts, free = True}
  MakeLetRec -> do
    opts <- genName'
    pure Options{opts, free = True}
  MakeLam -> do
    opts <- genName'
    pure Options{opts, free = True}
  MakeLAM -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenamePattern -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenameLet -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenameLam -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenameLAM -> do
    opts <- genName'
    pure Options{opts, free = True}
  MakeTCon ->
    let opts = fromGlobal . fst <$> Map.toList typeDefs
     in pure Options{opts, free = False}
  MakeTVar -> do
    (types, _locals, _globals) <- varsInScope'
    let opts = flip Option Nothing . unName . unLocalName . fst <$> types
    pure Options{opts, free = False}
  MakeForall -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenameForall -> do
    opts <- genName'
    pure Options{opts, free = True}
  RenameDef ->
    pure Options{opts = [], free = True}
  where
    varOptions = do
      (_types, locals, globals) <- varsInScope'
      let optionsLoc = first (flip Option Nothing . unName . unLocalName) <$> locals
          optionsGlob = first fromGlobal <$> globals
      pure $ optionsLoc <> optionsGlob
    genName' = do
      id <- mid
      let chkOrSyn tc = (tc ^? _chkedAt) <|> (tc ^? _synthed)
      typeKind <-
        ((fst <$> findNodeWithParent id (astDefExpr def)) <|> (TypeNode <$> findType id (astDefType def))) >>= \case
          ExprNode e -> pure $ Left $ do
            tc <- e ^. _exprMetaLens % _type
            chkOrSyn tc
          TypeNode t -> pure $ Right $ t ^. _typeMetaLens % _type
          CaseBindNode b -> pure $ Left $ do
            tc <- b ^. _bindMeta % _type
            chkOrSyn tc
      names <-
        focusNode id <&> \case
          Left zE -> generateNameExpr typeKind zE
          Right zT -> generateNameTy typeKind zT
      pure $ flip Option Nothing . unName <$> runReader names cxt
    varsInScope' = do
      id <- mid
      node <- focusNode id
      pure $ case node of
        Left zE -> variablesInScopeExpr defs zE
        Right zT -> (variablesInScopeTy zT, [], [])
    fromGlobal n = Option{option = unName $ baseName n, context = Just $ map unName $ unModuleName $ qualifiedModule n}
    focusNode id =
      let mzE = locToEither <$> focusOn id (astDefExpr def)
          mzT = focusOnTy id $ astDefType def
       in fmap Left mzE <|> fmap Right mzT

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
