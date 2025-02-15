{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Primer.Miso (start) where

import Foreword

import Clay qualified
import Control.Concurrent.STM (atomically, newTBQueueIO)
import Control.Monad.Catch.Pure (CatchT (runCatchT))
import Control.Monad.Log (PureLoggingT, Severity (..), WithSeverity (..), runPureLoggingT)
import Control.Monad.Writer (Writer, WriterT, runWriterT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (..))
import Data.Default qualified as Default
import Data.Generics.Uniplate.Data (children)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Tuple.Extra ((&&&))
import Data.UUID.Types (UUID)
import Data.UUID.Types qualified as UUID
import GHC.Base (error)
import Linear (Metric (norm), R1 (_x), V2 (V2), unangle)
import Linear.Affine ((.+^), (.-.), (.-^))
import Miso (
  App (
    App,
    events,
    initialAction,
    logLevel,
    model,
    mountPoint,
    subs,
    update,
    view
  ),
  Effect,
  JSM,
  LogLevel (Off),
  View,
  button_,
  class_,
  consoleLog,
  defaultEvents,
  div_,
  form_,
  fromTransition,
  id_,
  img_,
  input_,
  onChange,
  onClick,
  required_,
  scheduleIO,
  scheduleIO_,
  src_,
  startApp,
  style_,
  text,
  type_,
 )
import Miso qualified
import Miso.String (MisoString)
import Optics (Lens', forOf, lens, lensVL, sequenceOf, to, traverseOf, use, (%), (%~), (.~), (^.), (^..), _Just)
import Optics.State.Operators ((.=), (?=))
import Primer.API (APILog, Env (Env), edit, findASTDef, runPrimerM)
import Primer.Action (toProgActionInput, toProgActionNoInput)
import Primer.Action.Available qualified as Available
import Primer.App (
  DefSelection (..),
  Editable (..),
  MutationRequest (..),
  NodeSelection (..),
  NodeType (BodyNode, SigNode),
  Prog (progImports),
  ProgError (ActionError),
  Selection' (SelectionDef, SelectionTypeDef),
  appCurrentState,
  appProg,
  appStateProg,
  handleEditRequest,
  newApp,
  newEmptyApp,
  newProg,
  progAllDefs,
  progAllModules,
  progAllTypeDefs,
  progModules,
  progSelection,
 )
import Primer.App qualified
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  CaseFallback' (CaseExhaustive, CaseFallback),
  Expr' (
    APP,
    Ann,
    Case,
    Con,
    EmptyHole,
    Hole,
    LAM,
    Lam,
    Let,
    LetType,
    Letrec,
    PrimCon,
    Var
  ),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind' (..),
  LVarName,
  LocalName (unLocalName),
  ModuleName,
  Pattern (PatCon, PatPrim),
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  Type' (..),
  getID,
  globalNamePretty,
  mkSimpleModuleName,
  qualifyName,
  typesInExpr,
  unsafeMkGlobalName,
  unsafeMkLocalName,
  _exprMetaLens,
  _kindMetaLens,
  _type,
  _typeMetaLens,
 )
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Database qualified as DB
import Primer.Def (ASTDef, DefMap, defAST)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Miso.Layout (
  slHSep,
  slHeight,
  slVSep,
  slWidth,
  symmLayout',
 )
import Primer.Miso.Util (
  ASTDefT (expr, sig),
  DefSelectionT,
  ModuleT (..),
  NodeSelectionT,
  P2,
  TermMeta',
  assumeDefHasTypeCheckInfo,
  astDefTtoAstDef,
  availableForSelection,
  bindingsInExpr,
  bindingsInType,
  clayToMiso,
  defSelectionTtoDefSelection,
  defSelectionTtoDefSelection',
  kindsInType,
  nodeSelectionType,
  optToName,
  realToClay,
  startAppWithSavedState,
  stringToOpt,
  tcBasicProg,
  typeBindingsInExpr,
 )
import Primer.Module (Module (moduleDefs, moduleName), moduleDefsQualified)
import Primer.Name (Name, unName)
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck (SmartHoles (SmartHoles), buildTypingContext)
import StmContainers.Map qualified as StmMap

start :: JSM ()
start =
  startAppWithSavedState
    App
      { model =
          Model
            { app
            , actionPanelOptionsMode = Nothing
            }
      , update = updateModel
      , view = viewModel
      , subs = []
      , events = defaultEvents
      , initialAction = NoOp "start"
      , mountPoint = Nothing
      , logLevel = Off
      }
  where
    app =
      newApp
        & #currentState
          % #prog
          .~ either
            (error . ("initial program failed to typecheck: " <>) . show)
            identity
            (tcBasicProg $ newApp.currentState.prog)

-- instance {-# OVERLAPPING #-} IsLabel "selection" (Lens' Model (Maybe DefSelectionT))
modelSelection :: Lens' Model (Maybe DefSelectionT)
modelSelection =
  modelProg
    % lens
      ( \model ->
          progSelection
            (model)
            >>= \case
              SelectionDef s -> do
                -- TODO maybe we should error when not typechecked, rather than silently acting like there's no selection
                forOf (#node % _Just % #meta) s \case
                  -- TODO this could surely be simplified
                  Left m -> map Left $ m & sequenceOf _type
                  Right (Left m) -> map (Right . Left) $ m & sequenceOf _type
                  Right (Right m) -> pure $ Right $ Right m
              SelectionTypeDef _ -> Nothing
      )
      ( flip \sel ->
          -- TODO is there a library function for setting the selection?
          -- maybe look at what REST API does
          #progSelection .~ map (SelectionDef . defSelectionTtoDefSelection') sel
      )

-- TODO reuse this in `modelSelection`
modelProg :: Lens' Model Prog
modelProg = #app % #currentState % #prog

data Model = Model
  -- TODO do we need to go all the way to `AppT`, parameterising `Prog` etc. ?
  -- TODO is storing `App` in model a bit too much
  { app :: Primer.App.App
  , -- TODO look in to component-ising the action panel?
    actionPanelOptionsMode :: Maybe (Available.InputAction, Available.Options)
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Model

data Action
  = NoOp Text -- For situations where Miso requires an action, but we don't actually want to do anything.
  | SelectDef GVarName
  | SelectNode NodeSelectionT
  | ShowActionOptions (Available.InputAction, Available.Options)
  | ApplyAction (Either Available.NoInputAction (Available.InputAction, Available.Option))
  | CancelActionInput
  | SetProg Prog
  | SetApp Primer.App.App
  | RunUndo
  | RunRedo
  deriving stock (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
  fromTransition . \case
    NoOp _ -> pure ()
    SelectDef d -> do
      modelSelection ?= DefSelection d Nothing
      #actionPanelOptionsMode .= Nothing
    SelectNode sel -> do
      modelSelection % _Just % #node ?= sel
      #actionPanelOptionsMode .= Nothing
    ShowActionOptions a -> #actionPanelOptionsMode ?= a
    ApplyAction a -> do
      sel <- use modelSelection
      -- TODO what if app is out of date by the time the IO gets scheduled?
      app <- use #app
      case sel of
        Just s -> scheduleIO do
          r <- liftIO $ runAction app (defSelectionTtoDefSelection s) a
          case r of
            Left _ -> pure $ NoOp "running action failed" -- TODO log more? when can this happen? internal only?
            -- TODO if we can avoid IO, just set the new prog here
            -- Right p -> pure $ SetProg p
            Right p -> pure $ SetApp p
        Nothing -> pure () -- TODO log? this shouldn't really happen without weird async delays
      #actionPanelOptionsMode .= Nothing
    -- TODO DRY with above (also has some of the same issues)
    RunUndo -> do
      app <- use #app
      scheduleIO . liftIO $
        runMutationWithNullDb app Undo <&> \case
          Left _ -> NoOp "running undo failed"
          Right p -> SetApp p
      #actionPanelOptionsMode .= Nothing
    RunRedo -> do
      app <- use #app
      scheduleIO . liftIO $
        runMutationWithNullDb app Redo <&> \case
          Left _ -> NoOp "running redo failed"
          Right p -> SetApp p
      #actionPanelOptionsMode .= Nothing
    CancelActionInput ->
      -- TODO hang on - do we just want to do this on every single action?
      -- probably not forever? there could be trivial harmless things I guess
      -- so is there any principled way to decide exactly when to reset?
      #actionPanelOptionsMode .= Nothing
    SetProg p -> modelProg .= p
    SetApp a -> #app .= a

viewModel :: Model -> View Action
viewModel model@Model{..} =
  div_
    [id_ "miso-root"]
    $ [ div_
          [id_ "def-panel"]
          $ (Map.keys $ Map.mapMaybe (traverse defAST) $ progAllDefs $ appStateProg $ appCurrentState app) <&> \def ->
            button_
              [ class_ $ mwhen (Just def == ((.def) <$> selection)) "selected"
              , onClick $ SelectDef def
              ]
              [text $ globalNamePretty def]
      ]
      <> case selection of
        Nothing -> [text "no selection"]
        Just defSel ->
          [ div_
              [ id_ "sig"
              ]
              [ SelectNode . NodeSelection SigNode
                  <$> fst (viewTree (viewTreeType (Just . Right &&& isSelected) def.sig))
              ]
          , div_
              [ id_ "body"
              ]
              [ SelectNode . NodeSelection BodyNode
                  <$> fst (viewTree (viewTreeExpr (Just &&& isSelected) def.expr))
              ]
          , div_
              [ id_ "selection-type"
              ]
              [ fst $ viewTree case defSel.node of
                  Nothing -> viewTreeType mkMeta $ forgetTypeMetadata def.sig
                  Just s -> case nodeSelectionType s of
                    Left t -> viewTreeType mkMeta t
                    Right (Left t) -> viewTreeKind mkMeta t
                    -- TODO this isn't really correct - kinds in Primer don't have kinds
                    Right (Right ()) -> viewTreeKind mkMeta $ KType ()
              ]
          , div_ [id_ "action-panel"] case actionPanelOptionsMode of
              Nothing ->
                availableForSelection tydefs' defs' level editable def' defSel <&> \action ->
                  button_
                    [ onClick case action of
                        Available.NoInput a -> ApplyAction $ Left a
                        Available.Input a ->
                          ShowActionOptions
                            ( a
                            , -- TODO this error shouldn't happen
                              -- because we calculate the options here in order to prevent async issues
                              -- and mitigate any bugs which could cause state changes while in the option-choosing view
                              -- if we had proper error handling here for those unlikely cases,
                              -- we could instead just pass the action here
                              -- and reconstruct the option list on the other side
                              -- which would have the advantage of keeping the model smaller
                              -- which does seem like generally a good principle
                              -- then again performance is generally better this way I guess
                              -- no recomputation of similar things
                              -- another alternative would be doing the computation in the update function
                              -- haven't really thought about pros and cons too much, but it's not a silver bullet
                              fromMaybe (error "couldn't get action options") $
                                Available.options
                                  tydefs'
                                  defs'
                                  (buildTypingContext tydefs' defs' SmartHoles)
                                  level
                                  (Right def')
                                  (SelectionDef $ defSelectionTtoDefSelection defSel)
                                  a
                            )
                    ]
                    [ text case action of
                        -- TODO use proper descriptive text and/or symbols
                        -- take from old frontend? those weren't consistently great
                        -- maybe now is the time to propose some new ones, rather than letting bad ones reach the new app
                        Available.NoInput a -> show a
                        Available.Input a -> show a
                    ]
                where
                  def' = astDefTtoAstDef def
                  -- TODO don't hardcode
                  level = Primer.App.Expert
              Just (action, opts) ->
                ( case opts.free of
                    Available.FreeNone -> []
                    _ ->
                      [ form_
                          -- TODO hitting enter key, rather than clicking button, causes form submission
                          -- i.e. page reload and addition of `/?` to URL
                          -- this makes no difference:
                          -- [Miso.onWithOptions Miso.defaultOptions{Miso.preventDefault = True} "submit" Miso.valueDecoder $ const $ NoOp ""]
                          []
                          [ input_
                              [ type_ "text"
                              , required_ True
                              , onChange $ ApplyAction . Right . (action,) . stringToOpt
                              ]
                          , button_ [] [text "↩"]
                          ]
                      ]
                )
                  <> ( opts.opts <&> \opt@(optToName -> name) ->
                        button_
                          ( [onClick $ ApplyAction $ Right (action, opt)]
                              <> mwhen
                                (opt.matchesType)
                                [class_ "matches-type"]
                          )
                          [ text $ either (unName . unLocalName) globalNamePretty name
                          ]
                     )
                  <> [ button_ [class_ "cancel", onClick CancelActionInput] [text "Cancel"]
                     ]
          , div_
              [id_ "undo-redo-panel"]
              [ button_ [onClick RunUndo] [text "Undo"]
              , button_ [onClick RunRedo] [text "Redo"]
              ]
          ]
          where
            mkMeta = const (Nothing, False)
            isSelected x = (getID <$> defSel.node) == Just (getID x)
            defs = progAllDefs (appProg app)
            tydefs = progAllTypeDefs (appProg app)
            defs' = snd <$> defs
            tydefs' = snd <$> tydefs
            -- TODO better error handling
            def = fromMaybe (error "selected def is not fully typechecked") $ assumeDefHasTypeCheckInfo defNoTC
            (editable, defNoTC) = fromMaybe (error "selected def not found") $ traverse defAST =<< defs !? defSel.def
  where
    selection = model ^. modelSelection

-- TODO `isNothing clickAction` implies `not selected` - we could model this better
-- but in the long run, we intend to have no unselectable nodes anyway
data NodeViewData action = NodeViewData
  { clickAction :: Maybe action
  , selected :: Bool
  , level :: Level
  , opts :: NodeViewOpts action
  }

data NodeViewOpts action
  = SyntaxNode {wide :: Bool, flavor :: Text, text :: Text}
  | HoleNode {empty :: Bool}
  | PrimNode PrimCon
  | ConNode {name :: Name, scope :: ModuleName}
  | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
  | PatternBoxNode (Maybe (View action, V2 Double)) -- `Nothing` indicates that this is a fallback pattern.

data Level
  = Expr
  | Type
  | Kind

viewNodeData :: P2 Double -> V2 Double -> [View action] -> NodeViewData action -> View action
viewNodeData position dimensions edges node = case node.opts of
  PrimNode (PrimAnimation animation) ->
    img_
      [ src_ ("data:img/gif;base64," <> animation)
      , style_ $ clayToMiso do
          Clay.width $ Clay.px $ realToClay dimensions.x
          Clay.height $ Clay.px $ realToClay dimensions.y
      ]
  _ ->
    div_
      ( [ class_ "node"
        , class_ case node.level of
            Expr -> "expr"
            Type -> "type"
            Kind -> "kind"
        , class_ case node.opts of
            SyntaxNode{} -> "syntax"
            _ -> "non-syntax"
        , class_ case node.opts of
            SyntaxNode{flavor} -> flavor
            HoleNode{} -> "hole"
            PrimNode{} -> "prim"
            ConNode{} -> "con"
            VarNode{} -> "var"
            PatternBoxNode{} -> "pattern-box"
        , style_ $ clayToMiso do
            Clay.position Clay.absolute
            Clay.transform $
              Clay.translate
                (Clay.px $ realToClay position.x)
                (Clay.px $ realToClay position.y)
        ]
          <> foldMap' (\a -> [onClick a, class_ "selectable"]) node.clickAction
          <> mwhen node.selected [class_ "selected"]
      )
      $ edges -- Edges come first so that they appear behind contents.
        <> [ div_
              [ class_ "node-contents"
              , style_ $ clayToMiso do
                  Clay.width $ Clay.px $ realToClay dimensions.x
                  Clay.height $ Clay.px $ realToClay dimensions.y
                  Clay.boxSizing Clay.borderBox
                  Clay.display Clay.flex
                  Clay.justifyContent Clay.center
                  Clay.alignItems Clay.center
              ]
              case node.opts of
                PatternBoxNode (Just p) -> [fst p]
                PatternBoxNode Nothing ->
                  [ div_
                      [class_ "fallback-pattern"]
                      -- "🤷🏽‍♀️" is a lexical error: https://gitlab.haskell.org/ghc/ghc/-/issues/25635
                      [text "\x1f937\x1f3fd\x200d\x2640\xfe0f"]
                  ]
                _ ->
                  [ div_
                      [ class_ "node-text"
                      ]
                      [ text case node.opts of
                          SyntaxNode{text = t} -> t
                          HoleNode{empty = e} -> if e then "?" else "⚠️"
                          PrimNode pc -> case pc of
                            PrimChar c' -> show c'
                            PrimInt n -> show n
                          ConNode{name} -> unName name
                          VarNode{name} -> unName name
                      ]
                  ]
           ]

viewTreeExpr ::
  (Data a, Data b, Data c) =>
  (TermMeta' a b c -> (Maybe action, Bool)) ->
  Expr' a b c ->
  Tree.Tree (NodeViewData action)
viewTreeExpr mkMeta e =
  Tree.Node
    (uncurry NodeViewData (mkMeta $ Left $ e ^. _exprMetaLens) Expr nodeView)
    childViews
  where
    nodeView = case e of
      Hole{} -> HoleNode{empty = False}
      EmptyHole{} -> HoleNode{empty = True}
      Ann{} -> SyntaxNode False "ann" ":"
      Primer.App{} -> SyntaxNode False "app" "←"
      APP{} -> SyntaxNode False "type-expr-app" "←"
      Con _ c _ -> ConNode{name = baseName c, scope = qualifiedModule c}
      Lam{} -> SyntaxNode False "lam" "λ"
      LAM{} -> SyntaxNode False "type-lam" "Λ"
      Var _ (GlobalVarRef v) -> VarNode{name = baseName v, mscope = Just $ qualifiedModule v}
      Var _ (LocalVarRef v) -> VarNode{name = unLocalName v, mscope = Nothing}
      Let{} -> SyntaxNode False "let" "let"
      LetType{} -> SyntaxNode False "let-type" "let type"
      Letrec{} -> SyntaxNode False "letrec" "let rec"
      PrimCon _ c -> PrimNode c
      Case{} -> SyntaxNode True "match" "match"
    childViews = case e of
      Case _ scrut branches fb ->
        mconcat
          [ [viewTreeExpr mkMeta scrut]
          , branches <&> \(CaseBranch p bindings r) ->
              Tree.Node
                ( NodeViewData Nothing False Expr
                    $ PatternBoxNode
                    $ Just
                    $ viewTree
                    $ Tree.Node
                      ( NodeViewData Nothing False Expr case p of
                          PatCon c -> ConNode{name = baseName c, scope = qualifiedModule c}
                          PatPrim c -> PrimNode c
                      )
                    $ bindings <&> \(Bind m v) ->
                      Tree.Node
                        (uncurry NodeViewData (mkMeta $ Left m) Expr VarNode{name = unLocalName v, mscope = Nothing})
                        []
                )
                [viewTreeExpr mkMeta r]
          , case fb of
              CaseExhaustive -> []
              CaseFallback r -> [Tree.Node (NodeViewData Nothing False Expr $ PatternBoxNode Nothing) [viewTreeExpr mkMeta r]]
          ]
      _ ->
        mconcat
          [ map (viewTreeBinding Type) (e ^.. typeBindingsInExpr)
          , map (viewTreeBinding Expr) (e ^.. bindingsInExpr)
          , map (viewTreeType (mkMeta . Right)) (e ^.. typesInExpr)
          , map (viewTreeExpr mkMeta) (children e)
          ]
        where
          viewTreeBinding l name = Tree.Node (NodeViewData Nothing False l VarNode{name = unLocalName name, mscope = Nothing}) []

viewTreeType ::
  (Data b, Data c) =>
  (Either b c -> (Maybe action, Bool)) ->
  Type' b c ->
  Tree.Tree (NodeViewData action)
viewTreeType mkMeta t =
  Tree.Node
    (uncurry NodeViewData (mkMeta $ Left $ t ^. _typeMetaLens) Type nodeView)
    childViews
  where
    nodeView = case t of
      TEmptyHole{} -> HoleNode{empty = True}
      THole{} -> HoleNode{empty = True}
      TCon _ c -> ConNode{name = baseName c, scope = qualifiedModule c}
      TFun{} -> SyntaxNode False "type-fun" "→"
      TVar _ v -> VarNode{name = unLocalName v, mscope = Nothing}
      TApp{} -> SyntaxNode False "type-app" "←"
      TForall{} -> SyntaxNode False "forall" "∀"
      TLet{} -> SyntaxNode False "type-let" "let"
    childViews =
      map
        (\name -> Tree.Node (NodeViewData Nothing False Type VarNode{name, mscope = Nothing}) [])
        (t ^.. bindingsInType % to unLocalName)
        <> map (viewTreeKind (mkMeta . Right)) (t ^.. kindsInType)
        <> map (viewTreeType mkMeta) (children t)

viewTreeKind ::
  (Data c) =>
  (c -> (Maybe action, Bool)) ->
  Kind' c ->
  Tree.Tree (NodeViewData action)
viewTreeKind mkMeta k =
  Tree.Node
    (uncurry NodeViewData (mkMeta $ k ^. _kindMetaLens) Kind nodeView)
    childViews
  where
    nodeView = case k of
      KHole{} -> HoleNode{empty = True}
      KType{} -> SyntaxNode False "kind-type" "*"
      KFun{} -> SyntaxNode False "kind-fun" "→"
    childViews = map (viewTreeKind mkMeta) (children k)

viewEdge :: V2 Double -> View action
viewEdge v =
  div_
    [ class_ "edge"
    , style_ $ clayToMiso do
        Clay.position Clay.absolute
        Clay.top $ Clay.pct 50
        Clay.left $ Clay.pct 50
        Clay.transformOrigin [Clay.pct 0]
        Clay.borderStyle Clay.solid
        Clay.transform $ Clay.rotate $ Clay.rad $ realToClay theta
        Clay.width $ Clay.px $ realToClay size
    ]
    []
  where
    theta = unangle v
    size = norm v

viewTree :: Tree (NodeViewData action) -> (View action, V2 Double)
viewTree t =
  ( div_
      [ class_ "tree"
      , style_ $ clayToMiso do
          Clay.minWidth $ Clay.px $ realToClay dimensions.x
          Clay.minHeight $ Clay.px $ realToClay dimensions.y
      ]
      . map fst
      . toList
      $ Tree.foldTree
        ( \((node, v), p) subs ->
            Tree.Node
              ( viewNodeData
                  (p .-^ topLeft .-^ v / 2)
                  v
                  (map (viewEdge . (.-. p) . snd . head) subs)
                  node
              , p
              )
              subs
        )
        nodes
  , dimensions
  )
  where
    dimensions = bottomRight - topLeft
    mins = map (\(v, p) -> p .-^ snd v / 2) nodes
    topLeft = V2 (minimum $ map (.x) mins) (minimum $ map (.y) mins)
    maxs = map (\(v, p) -> p .+^ snd v / 2) nodes
    bottomRight = V2 (maximum $ map (.x) maxs) (maximum $ map (.y) maxs)
    nodes =
      symmLayout' @Double
        ( Default.def
            & (slHSep .~ nodePadding)
            & (slVSep .~ nodePadding)
            & (slWidth .~ \(_, v) -> (-(v.x / 2), v.x / 2))
            & (slHeight .~ \(_, v) -> (-(v.y / 2), v.y / 2))
        )
        $ map (\opts -> (opts, getDimensions opts.opts)) t
    getDimensions = \case
      PatternBoxNode (Just (_, v)) -> v + pure boxPadding
      PatternBoxNode Nothing -> basicDimsSquare + pure boxPadding
      SyntaxNode{wide = False} -> basicDimsSquare
      HoleNode{} -> basicDimsSquare
      _ -> basicDims
      where
        basicDimsSquare = basicDims & lensVL _x .~ basicDims.y
    -- TODO make these configurable
    nodePadding = 20
    boxPadding = 55
    basicDims = V2 80 35

-- TODO obviously one-shotting this doesn't seem great
-- is there any actual point saving what state we can, given that we're using `NullDb`?
-- well, sessions for example is presumably mutated...
-- we could serialise it to a normal map and keep that in the model
-- remember we don't actually care about concurrency for this project
-- runMutationWithNullDb :: Primer.App.App -> MutationRequest -> IO (Either ProgError Prog)
-- runMutationWithNullDb :: Primer.App.App -> MutationRequest -> IO (Either (Either Text ProgError) Prog)
runMutationWithNullDb :: Primer.App.App -> MutationRequest -> IO (Either (Either Text ProgError) Primer.App.App)
runMutationWithNullDb app req = do
  -- TODO are these dummies real enough to work?
  let version = "dummy-version"
      sid = UUID.nil
      queueBound = 10
  lastModified <- DB.getCurrentTime
  sessions <- atomically do
    m <- StmMap.new
    StmMap.insert (DB.SessionData app DB.defaultSessionName lastModified) sid m
    pure m

  dbOpQueue <- newTBQueueIO queueBound
  let runReq =
        map (map discardSeverity)
          . runPureLoggingT
          . runPrimerM @(PureLoggingT (WithSeverity [APILog]) IO) (edit sid req)
          $ Env sessions dbOpQueue version
  let runDB = DB.runNullDb sessions $ DB.serve $ DB.ServiceCfg dbOpQueue version
  -- TODO do something with these return values?
  (_res, _logs) <- either absurd (first $ first Right) <$> race runDB runReq

  -- TODO why am I bothering to use pure logger when we need IO for DB anyway?
  -- ah well actually we could use `runNullDbT` in another monad...
  -- unfortunately `DB.serve` and the `MonadDb (NullDbT m)` instance currently require IO
  -- but I don't think there's any fundamental reason why they have to
  -- we'd have to generalise some STM stuff to use other data structures
  -- it may of course very well not be worth it
  -- of course eventually we won't want null DB anyway, but some sort of local storage DB
  -- which will need to have side effects
  -- although maybe it would be nice if we could separate out the save-to-DB part
  -- and we don't actually need that for a while anyway since we're currently saving all Miso state there anyway

  -- pure res
  -- just inserting the returned prog means we discard the ID counter state, so we do this instead
  res' <- atomically $ StmMap.lookup sid sessions
  pure $ maybeToEither (Left "session not found") $ (.sessionApp) <$> res'

runAction ::
  Primer.App.App ->
  DefSelection ID ->
  Either Available.NoInputAction (Available.InputAction, Available.Option) ->
  -- IO (Either (Either Text ProgError) Prog)
  IO (Either (Either Text ProgError) Primer.App.App)
runAction app sel =
  let
    (_editable, def) =
      either
        -- TODO how impossible is this, and can we fix upstream to throw proper typed errors?
        (const $ error "findASTDef failure in runAction")
        identity
        $ runIdentity
        $ runCatchT
        $ findASTDef defs sel.def
    defs = progAllDefs prog
    prog = appProg app
   in
    either (pure . Left . Right . ActionError) (\as -> runMutationWithNullDb app $ Edit as)
      . \case
        Left action -> toProgActionNoInput (snd <$> defs) (Right def) (SelectionDef sel) action
        Right (action, opt) -> toProgActionInput (Right def) (SelectionDef sel) opt action

-- TODO yeah okay this is a bit weird
-- look at `Primer.PureLogT` instead?
instance Semigroup a => Semigroup (WithSeverity a) where
  WithSeverity s x <> WithSeverity _ x' = WithSeverity s (x <> x')
instance Monoid a => Monoid (WithSeverity a) where
  mempty = WithSeverity Informational mempty
