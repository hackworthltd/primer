{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Primer.Miso (start) where

import Foreword

import Clay qualified
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (..))
import Data.Default qualified as Default
import Data.Generics.Uniplate.Data (children)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Tuple.Extra ((&&&))
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
  scheduleIO_,
  src_,
  style_,
  text,
  type_,
 )
import Miso.String (MisoString)
import Optics (lensVL, to, (%), (.~), (^.), (^..), _Just)
import Optics.State.Operators ((.=), (?=))
import Primer.Action.Available qualified as Available
import Primer.App (
  DefSelection (..),
  Editable (..),
  NodeSelection (..),
  NodeType (BodyNode, SigNode),
  Prog (progImports),
  Selection' (SelectionDef),
  newProg,
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
  _typeMetaLens,
 )
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (DefMap)
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
  astDefTtoAstDef,
  availableForSelection,
  bindingsInExpr,
  bindingsInType,
  clayToMiso,
  defSelectionTtoDefSelection,
  kindsInType,
  nodeSelectionType,
  optToName,
  realToClay,
  startAppWithSavedState,
  stringToOpt,
  tcBasicProg,
  typeBindingsInExpr,
 )
import Primer.Module (Module (moduleName))
import Primer.Name (Name, unName)
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck (SmartHoles (SmartHoles), buildTypingContext)

start :: JSM ()
start =
  startAppWithSavedState
    App
      { model = Model{module_, selection = Nothing, actionPanelOptionsMode = Nothing}
      , update = updateModel
      , view = viewModel
      , subs = []
      , events = defaultEvents
      , initialAction = NoOp "start"
      , mountPoint = Nothing
      , logLevel = Off
      }
  where
    -- TODO we hardcode Prelude as the active module, for the sake of demonstration
    module_ =
      either (error . ("Prelude failed to typecheck: " <>) . show) identity
        . tcBasicProg p
        . fromMaybe (error "prog doesn't contain Prelude")
        . find ((== mkSimpleModuleName "Prelude") . moduleName)
        $ progImports p
      where
        (p, _, _) = newProg

data Model = Model
  { module_ :: ModuleT -- We typecheck everything up front so that we can use `ExprT`, guaranteeing existence of metadata.
  , selection :: Maybe DefSelectionT
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
  -- \| ConsoleLog MisoString -- TODO temporary, for debugging
  deriving stock (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
  fromTransition . \case
    NoOp _ -> pure ()
    SelectDef d -> do
      #selection ?= DefSelection d Nothing
      #actionPanelOptionsMode .= Nothing
    SelectNode sel -> do
      #selection % _Just % #node ?= sel
      #actionPanelOptionsMode .= Nothing
    ShowActionOptions a -> #actionPanelOptionsMode ?= a
    ApplyAction a -> do
      #actionPanelOptionsMode .= Nothing
    -- TODO remove
    -- scheduleIO_ $ consoleLog $ show a
    CancelActionInput ->
      -- TODO hang on - do we just want to do this on every single action?
      -- probably not forever? there could be trivial harmless things I guess
      -- so is there any principled way to decide exactly when to reset?
      #actionPanelOptionsMode .= Nothing

-- ConsoleLog s -> scheduleIO_ $ consoleLog s

viewModel :: Model -> View Action
viewModel Model{..} =
  div_
    [id_ "miso-root"]
    $ [ div_
          [id_ "def-panel"]
          $ Map.keys module_.defs <&> \(qualifyName module_.name -> def) ->
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
                availableForSelection tydefs defs level editable def' defSel <&> \action ->
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
                                  tydefs
                                  defs
                                  (buildTypingContext tydefs defs SmartHoles)
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
                  -- TODO pass all program defs
                  defs = mempty @DefMap
                  tydefs = mempty @TypeDefMap
                  -- TODO don't hardcode
                  level = Primer.App.Expert
                  editable = Editable
              Just (action, opts) ->
                ( case opts.free of
                    Available.FreeNone -> []
                    _ ->
                      [ form_
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
          ]
          where
            mkMeta = const (Nothing, False)
            isSelected x = (getID <$> defSel.node) == Just (getID x)
            -- TODO better error handling
            def = fromMaybe (error "selected def not found") $ module_.defs !? baseName defSel.def

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
