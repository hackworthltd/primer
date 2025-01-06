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
import Data.Generics.Uniplate.Data (children)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Tree (Tree)
import Data.Tree qualified as Tree
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
import Optics (lensVL, over, to, (%), (.~), (^.), (^..), _Just)
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
  P2,
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
            []
            [ div_
                [ class_ "canvas"
                ]
                [ SelectNode . NodeSelection SigNode <$> viewTree (viewTreeType isSelected def.sig)
                , SelectNode . NodeSelection BodyNode <$> viewTree (viewTreeExpr isSelected def.expr)
                , NoOp "clicked non-interactive node" <$ case defSel.node of
                    Nothing -> viewTree $ viewTreeType (const False) $ forgetTypeMetadata def.sig
                    Just s -> case nodeSelectionType s of
                      Left t -> viewTree $ viewTreeType (const False) t
                      Right (Left t) -> viewTree $ viewTreeKind (const False) t
                      -- TODO this isn't really correct - kinds in Primer don't have kinds
                      Right (Right ()) -> viewTree $ viewTreeKind (const False) $ KType ()
                ]
            ]
          where
            isSelected x = (getID <$> defSel.node) == Just (getID x)
            -- TODO better error handling
            def = fromMaybe (error "selected def not found") $ module_.defs !? baseName defSel.def
    ]

data NodeViewData
  = SyntaxNode {wide :: Bool, flavor :: Text, text :: Text}
  | HoleNode {empty :: Bool}
  | PrimNode PrimCon
  | ConNode {name :: Name, scope :: ModuleName}
  | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
  | PatternBoxNode (forall action. MeasuredView action)

data Level
  = Expr
  | Type
  | Kind

viewNode :: Bool -> Level -> NodeViewData -> MeasuredView action
viewNode selected level opts =
  MeasuredView
    { dimensions
    , view = case opts of
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
            [ class_ "node"
            , class_ $ mwhen selected "selected"
            , class_ case level of
                Expr -> "expr"
                Type -> "type"
                Kind -> "kind"
            , class_ case opts of
                SyntaxNode{} -> "syntax"
                _ -> "non-syntax"
            , class_ case opts of
                SyntaxNode{flavor} -> flavor
                HoleNode{} -> "hole"
                PrimNode{} -> "prim"
                ConNode{} -> "con"
                VarNode{} -> "var"
                PatternBoxNode{} -> "pattern-box"
            , style_ $
                [ ("width", show dimensions.x <> "px")
                , ("height", show dimensions.y <> "px")
                ]
                  <> case opts of
                    HoleNode{} -> [("font-style", "italic")]
                    _ -> []
            ]
            case opts of
              PatternBoxNode p ->
                [ div_
                    [ style_
                        [ ("position", "absolute")
                        , ("top", show (boxPadding / 2) <> "px")
                        ]
                    ]
                    [p.view]
                ]
              _ ->
                [ div_
                    []
                    [ text case opts of
                        SyntaxNode{text = t} -> t
                        HoleNode{empty = e} -> if e then "?" else "⚠️"
                        PrimNode pc -> case pc of
                          PrimChar c' -> show c'
                          PrimInt n -> show n
                        ConNode{name} -> unName name
                        VarNode{name} -> unName name
                    ]
                ]
    }
  where
    boxPadding = 55
    basicDims = V2 80 35
    dimensions = case opts of
      PatternBoxNode p -> p.dimensions + pure boxPadding
      SyntaxNode{wide = False} -> basicDims & lensVL _x .~ basicDims.y
      _ -> basicDims

viewTreeExpr ::
  (Data a, Data b, Data c) =>
  (TermMeta' a b c -> Bool) ->
  Expr' a b c ->
  Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeExpr isSelected e =
  Tree.Node
    ( over #view (div_ [onClick meta] . pure) $
        viewNode (isSelected meta) Expr nodeView
    )
    childViews
  where
    meta = Left $ e ^. _exprMetaLens
    nodeView = case e of
      Hole{} -> HoleNode{empty = False}
      EmptyHole{} -> HoleNode{empty = False}
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
          [ [viewTreeExpr isSelected scrut]
          , branches <&> \(CaseBranch p bindings r) ->
              Tree.Node
                ( viewNode False Expr
                    $ PatternBoxNode
                    $ viewTreeWithDimensions False
                    $ ( Tree.Node $ viewNode False Expr case p of
                          PatCon c -> ConNode{name = baseName c, scope = qualifiedModule c}
                          PatPrim c -> PrimNode c
                      )
                    $ bindings <&> \(Bind m v) ->
                      Tree.Node
                        (viewNode (isSelected $ Left m) Expr VarNode{name = unLocalName v, mscope = Nothing})
                        []
                )
                [viewTreeExpr isSelected r]
          , case fb of
              CaseExhaustive -> []
              CaseFallback r -> [Tree.Node (viewNode False Expr (SyntaxNode False "fallback" "_")) [viewTreeExpr isSelected r]]
          ]
      _ ->
        mconcat
          [ map (viewTreeBinding Type) (e ^.. typeBindingsInExpr)
          , map (viewTreeBinding Expr) (e ^.. bindingsInExpr)
          , map (viewTreeType isSelected) (e ^.. typesInExpr)
          , map (viewTreeExpr isSelected) (children e)
          ]
        where
          viewTreeBinding l name = Tree.Node (viewNode False l VarNode{name = unLocalName name, mscope = Nothing}) []

viewTreeType ::
  (Data b, Data c) =>
  (TermMeta' a b c -> Bool) ->
  Type' b c ->
  Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeType isSelected t =
  Tree.Node
    ( over #view (div_ [onClick meta] . pure) $
        viewNode (isSelected meta) Type nodeView
    )
    childViews
  where
    meta = Right $ Left $ t ^. _typeMetaLens
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
        (\name -> Tree.Node (viewNode False Type VarNode{name, mscope = Nothing}) [])
        (t ^.. bindingsInType % to unLocalName)
        <> map (viewTreeKind isSelected) (t ^.. kindsInType)
        <> map (viewTreeType isSelected) (children t)

viewTreeKind ::
  (Data c) =>
  (TermMeta' a b c -> Bool) ->
  Kind' c ->
  Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeKind isSelected k =
  Tree.Node
    ( over #view (div_ [onClick $ Right $ Right $ k ^. _kindMetaLens] . pure) $
        viewNode (isSelected $ Right $ Right $ k ^. _kindMetaLens) Kind nodeView
    )
    childViews
  where
    nodeView = case k of
      KHole{} -> HoleNode{empty = True}
      KType{} -> SyntaxNode False "kind-type" "*"
      KFun{} -> SyntaxNode False "kind-fun" "→"
    childViews = map (viewTreeKind isSelected) (children k)

-- | Draw an edge from one point to another.
viewEdge :: P2 Double -> P2 Double -> View action
viewEdge p p' =
  div_
    [ class_ "edge"
    , style_
        [
          ( "transform"
          , "translate("
              <> show p.x
              <> "px,"
              <> show p.y
              <> "px) rotate("
              <> show theta
              <> "rad)"
          )
        , ("width", show size <> "px")
        ]
    ]
    []
  where
    v = p' .-. p
    theta = unangle v
    size = norm v

viewTree :: Tree (MeasuredView action) -> View action
viewTree = (.view) . viewTreeWithDimensions True

viewTreeWithDimensions ::
  -- | Apply the same padding we use between nodes to the entire tree.
  -- Should be `False` for nested trees.
  Bool ->
  Tree (MeasuredView action) ->
  MeasuredView action
viewTreeWithDimensions outerPadding t =
  MeasuredView
    { dimensions = bottomRight - topLeft
    , view =
        div_ (mwhen outerPadding [style_ [("padding", show (padding / 2) <> "px")]])
          . map fst
          . toList
          $ Tree.foldTree
            ( \(node, p) subs ->
                Tree.Node
                  ( div_ [] $
                      let offset = p .-^ node.dimensions / 2
                       in div_
                            [ style_
                                [ ("position", "absolute")
                                ,
                                  ( "transform"
                                  , "translate("
                                      <> show offset.x
                                      <> "px,"
                                      <> show offset.y
                                      <> "px)"
                                  )
                                ]
                            ]
                            [node.view]
                            : map (viewEdge p . head . map snd) subs
                  , p
                  )
                  subs
            )
            nodes
    }
  where
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
        t
    padding = 20

data MeasuredView action = MeasuredView
  { view :: View action
  , dimensions :: V2 Double
  }
  deriving stock (Generic)
