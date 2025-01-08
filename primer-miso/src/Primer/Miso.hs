{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Primer.Miso (start) where

import Foreword

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (..))
import Data.Default qualified as Default
import Data.Foldable (foldMap)
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
  defaultEvents,
  div_,
  fromTransition,
  img_,
  onClick,
  src_,
  style_,
  text,
 )
import Optics (lensVL, to, (%), (.~), (^.), (^..), _Just)
import Optics.State.Operators ((?=))
import Primer.App (
  NodeSelection (..),
  NodeType (BodyNode, SigNode),
  Prog (progImports),
  newProg,
 )
import Primer.App.Base (DefSelection (..))
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
  _exprMetaLens,
  _kindMetaLens,
  _typeMetaLens,
 )
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetTypeMetadata)
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
  bindingsInExpr,
  bindingsInType,
  kindsInType,
  nodeSelectionType,
  startAppWithSavedState,
  tcBasicProg,
  typeBindingsInExpr,
 )
import Primer.Module (Module (moduleName))
import Primer.Name (Name, unName)

start :: JSM ()
start =
  startAppWithSavedState
    App
      { model = Model{module_, selection = Nothing}
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
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Model

data Action
  = NoOp Text -- For situations where Miso requires an action, but we don't actually want to do anything.
  | SelectDef GVarName
  | SelectNode NodeSelectionT
  deriving stock (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel =
  fromTransition . \case
    NoOp _ -> pure ()
    SelectDef d -> #selection ?= DefSelection d Nothing
    SelectNode sel -> #selection % _Just % #node ?= sel

viewModel :: Model -> View Action
viewModel Model{..} =
  div_
    []
    [ div_
        []
        $ Map.keys module_.defs <&> \(qualifyName module_.name -> def) ->
          button_
            [onClick $ SelectDef def]
            [text $ globalNamePretty def]
    , case selection of
        Nothing -> "no selection"
        Just defSel ->
          div_
            [ class_ "canvas"
            ]
            [ SelectNode . NodeSelection SigNode <$> viewTree (viewTreeType (Just . Right &&& isSelected) def.sig)
            , SelectNode . NodeSelection BodyNode <$> viewTree (viewTreeExpr (Just &&& isSelected) def.expr)
            , viewTree case defSel.node of
                Nothing -> viewTreeType mkMeta $ forgetTypeMetadata def.sig
                Just s -> case nodeSelectionType s of
                  Left t -> viewTreeType mkMeta t
                  Right (Left t) -> viewTreeKind mkMeta t
                  -- TODO this isn't really correct - kinds in Primer don't have kinds
                  Right (Right ()) -> viewTreeKind mkMeta $ KType ()
            ]
          where
            mkMeta = const (Nothing, False)
            isSelected x = (getID <$> defSel.node) == Just (getID x)
            -- TODO better error handling
            def = fromMaybe (error "selected def not found") $ module_.defs !? baseName defSel.def
    ]

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
  | PatternBoxNode (Maybe (Measured (View action))) -- `Nothing` indicates that this is a fallback pattern.

data Level
  = Expr
  | Type
  | Kind

viewNodeData :: P2 Double -> V2 Double -> [View action] -> NodeViewData action -> View action
viewNodeData position dimensions edges node = case node.opts of
  PrimNode (PrimAnimation animation) ->
    img_
      [ src_ ("data:img/gif;base64," <> animation)
      , style_ $
          [ ("width", show dimensions.x <> "px")
          , ("height", show dimensions.y <> "px")
          ]
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
        , style_
            [
              ( "transform"
              , "translate("
                  <> show position.x
                  <> "px,"
                  <> show position.y
                  <> "px)"
              )
            ]
        ]
          <> foldMap (\a -> [onClick a, class_ "selectable"]) node.clickAction
          <> mwhen node.selected [class_ "selected"]
      )
      $ (edges <>) -- Edges come first so that they appear behind contents.
        [ div_
            [ class_ "node-contents"
            , style_
                [ ("width", show dimensions.x <> "px")
                , ("height", show dimensions.y <> "px")
                ]
            ]
            case node.opts of
              PatternBoxNode (Just p) -> [p.item]
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
                    $ viewTreeWithDimensions
                    $ ( Tree.Node $ NodeViewData Nothing False Expr case p of
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
    , style_
        [
          ( "transform"
          , "rotate(" <> show theta <> "rad)"
          )
        , ("width", show size <> "px")
        ]
    ]
    []
  where
    theta = unangle v
    size = norm v

viewTree :: Tree (NodeViewData action) -> View action
viewTree = (.item) . viewTreeWithDimensions

viewTreeWithDimensions :: Tree (NodeViewData action) -> Measured (View action)
viewTreeWithDimensions t =
  Measured
    { dimensions
    , item =
        div_
          ( [ style_ $
                [ ("width", show dimensions.x <> "px")
                , ("height", show dimensions.y <> "px")
                ]
            ]
          )
          . map fst
          . toList
          $ Tree.foldTree
            ( \(node, p) subs ->
                Tree.Node
                  ( viewNodeData
                      (p .-^ topLeft .-^ node.dimensions / 2)
                      node.dimensions
                      (map (viewEdge . (.-. p) . snd . head) subs)
                      node.item
                  , p
                  )
                  subs
            )
            nodes
    }
  where
    dimensions = bottomRight - topLeft
    mins = map (\(v, p) -> p .-^ v.dimensions / 2) nodes
    topLeft = V2 (minimum $ map (.x) mins) (minimum $ map (.y) mins)
    maxs = map (\(v, p) -> p .+^ v.dimensions / 2) nodes
    bottomRight = V2 (maximum $ map (.x) maxs) (maximum $ map (.y) maxs)
    nodes =
      symmLayout' @Double
        ( Default.def
            & (slHSep .~ padding)
            & (slVSep .~ padding)
            & (slWidth .~ \node -> (-(node.dimensions.x / 2), node.dimensions.x / 2))
            & (slHeight .~ \node -> (-(node.dimensions.y / 2), node.dimensions.y / 2))
        )
        $ map (\opts -> Measured opts $ getDimensions opts.opts) t
      where
        padding = 20
    getDimensions = \case
      PatternBoxNode (Just p) -> p.dimensions + pure boxPadding
      PatternBoxNode Nothing -> basicDimsSquare + pure boxPadding
      SyntaxNode{wide = False} -> basicDimsSquare
      HoleNode{} -> basicDimsSquare
      _ -> basicDims
      where
        boxPadding = 55
        basicDims = V2 80 35
        basicDimsSquare = basicDims & lensVL _x .~ basicDims.y

data Measured a = Measured
  { item :: a
  , dimensions :: V2 Double
  }
  deriving stock (Generic)
