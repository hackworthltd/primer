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
  NodeSelection (NodeSelection),
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
import Primer.Miso.Colors (
  blackPrimary,
  bluePrimary,
  blueQuaternary,
  blueSecondary,
  blueTertiary,
  greenPrimary,
  greySecondary,
  redTertiary,
  whitePrimary,
  yellowPrimary,
  yellowTertiary,
 )
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
                [ style_
                    [ ("display", "grid")
                    , ("grid-template-columns", "1fr 1fr 1fr")
                    , ("justify-items", "center")
                    ]
                ]
                [ SelectNode . NodeSelection SigNode <$> viewTree (viewTreeType def.sig)
                , SelectNode . NodeSelection BodyNode <$> viewTree (viewTreeExpr def.expr)
                , NoOp "clicked non-interactive node" <$ case defSel.node of
                    Nothing -> viewTree $ viewTreeType $ forgetTypeMetadata def.sig
                    Just s -> case nodeSelectionType s of
                      Left t -> viewTree $ viewTreeType t
                      Right (Left t) -> viewTree $ viewTreeKind t
                      -- TODO this isn't really correct - kinds in Primer don't have kinds
                      Right (Right ()) -> viewTree $ viewTreeKind $ KType ()
                ]
            ]
          where
            -- TODO better error handling
            def = fromMaybe (error "selected def not found") $ module_.defs !? baseName defSel.def
    ]

data NodeViewData
  = SyntaxNode {wide :: Bool, color :: Text, text :: Text}
  | HoleNode {empty :: Bool}
  | PrimNode PrimCon
  | ConNode {name :: Name, scope :: ModuleName}
  | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
  | PatternBoxNode (forall action. MeasuredView action)

viewNode :: NodeViewData -> Map Text Text -> Map Text Text -> MeasuredView action
viewNode opts extraOuterStyles extraInnerStyles =
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
                  <> extraOuterStyles
                  <> extraInnerStyles
            ]
        _ ->
          div_
            [ style_ $
                [ ("display", "flex")
                , ("justify-content", "center")
                , ("align-items", "center")
                , ("border-style", "solid")
                , ("box-sizing", "border-box")
                , ("border-color", borderColor)
                , ("background-color", backgroundColor)
                , ("width", show dimensions.x <> "px")
                , ("height", show dimensions.y <> "px")
                , ("border-width", ".25rem")
                ]
                  <> case opts of
                    HoleNode{} -> [("font-style", "italic")]
                    _ -> []
                  <> extraOuterStyles
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
                    [ style_ $
                        [ ("overflow", "hidden")
                        , ("text-overflow", "ellipsis")
                        , ("white-space", "nowrap")
                        , ("color", fontColor)
                        ]
                          <> extraInnerStyles
                    ]
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
                where
                  fontColor = case opts of
                    SyntaxNode{} -> whitePrimary
                    _ -> bluePrimary
          where
            borderColor = case opts of
              SyntaxNode{color} -> color
              HoleNode{} -> redTertiary
              PrimNode{} -> greenPrimary
              ConNode{} -> greenPrimary
              VarNode{} -> blueQuaternary
              PatternBoxNode{} -> yellowPrimary
            backgroundColor = case opts of
              SyntaxNode{color} -> color
              PatternBoxNode{} -> yellowTertiary
              _ -> whitePrimary
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
  Expr' a b c ->
  Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeExpr e =
  Tree.Node
    ( over #view (div_ [onClick $ Left $ e ^. _exprMetaLens] . pure) $
        viewNode nodeView rounded []
    )
    childViews
  where
    rounded = [("border-radius", "1.5rem")] -- Curved nodes to indicate value-level expressions.
    nodeView = case e of
      Hole{} -> HoleNode{empty = False}
      EmptyHole{} -> HoleNode{empty = False}
      Ann{} -> SyntaxNode False blackPrimary ":"
      Primer.App{} -> SyntaxNode False blueTertiary "←"
      APP{} -> SyntaxNode False blueTertiary "←"
      Con _ c _ -> ConNode{name = baseName c, scope = qualifiedModule c}
      Lam{} -> SyntaxNode False bluePrimary "λ"
      LAM{} -> SyntaxNode False blueSecondary "Λ"
      Var _ (GlobalVarRef v) -> VarNode{name = baseName v, mscope = Just $ qualifiedModule v}
      Var _ (LocalVarRef v) -> VarNode{name = unLocalName v, mscope = Nothing}
      Let{} -> SyntaxNode False blueQuaternary "let"
      LetType{} -> SyntaxNode False blueQuaternary "let type"
      Letrec{} -> SyntaxNode False blueQuaternary "let rec"
      PrimCon _ c -> PrimNode c
      Case{} -> SyntaxNode True yellowPrimary "match"
    childViews = case e of
      Case _ scrut branches fb ->
        mconcat
          [ [viewTreeExpr scrut]
          , branches <&> \(CaseBranch p bindings r) ->
              Tree.Node
                ( viewNode
                    ( PatternBoxNode
                        ( viewTreeWithDimensions False
                            $ Tree.Node case p of
                              PatCon c -> viewNode ConNode{name = baseName c, scope = qualifiedModule c} rounded []
                              PatPrim c -> viewNode (PrimNode c) rounded []
                            $ bindings
                              <&> \(Bind _ v) ->
                                Tree.Node (viewNode VarNode{name = unLocalName v, mscope = Nothing} rounded []) []
                        )
                    )
                    rounded
                    []
                )
                [viewTreeExpr r]
          , case fb of
              CaseExhaustive -> []
              CaseFallback r -> [Tree.Node (viewNode (SyntaxNode False yellowPrimary "_") [] []) [viewTreeExpr r]]
          ]
      _ ->
        mconcat
          [ map (viewTreeBinding []) (e ^.. typeBindingsInExpr)
          , map (viewTreeBinding rounded) (e ^.. bindingsInExpr)
          , map viewTreeType (e ^.. typesInExpr)
          , map viewTreeExpr (children e)
          ]
        where
          viewTreeBinding as name = Tree.Node (viewNode VarNode{name = unLocalName name, mscope = Nothing} as []) []

viewTreeType ::
  (Data b, Data c) =>
  Type' b c ->
  Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeType t =
  Tree.Node
    ( over #view (div_ [onClick $ Right $ Left $ t ^. _typeMetaLens] . pure) $
        viewNode nodeView [] []
    )
    childViews
  where
    nodeView = case t of
      TEmptyHole{} -> HoleNode{empty = True}
      THole{} -> HoleNode{empty = True}
      TCon _ c -> ConNode{name = baseName c, scope = qualifiedModule c}
      TFun{} -> SyntaxNode False bluePrimary "→"
      TVar _ v -> VarNode{name = unLocalName v, mscope = Nothing}
      TApp{} -> SyntaxNode False blueTertiary "←"
      TForall{} -> SyntaxNode False blueSecondary "∀"
      TLet{} -> SyntaxNode False blueQuaternary "let"
    childViews =
      map
        (\name -> Tree.Node (viewNode VarNode{name, mscope = Nothing} [] []) [])
        (t ^.. bindingsInType % to unLocalName)
        <> map viewTreeKind (t ^.. kindsInType)
        <> map viewTreeType (children t)

viewTreeKind :: (Data c) => Kind' c -> Tree.Tree (MeasuredView (TermMeta' a b c))
viewTreeKind k =
  Tree.Node
    ( over #view (div_ [onClick $ Right $ Right $ k ^. _kindMetaLens] . pure) $
        viewNode
          nodeView
          -- Rotate to indicate kind.
          -- We then scale by (1 + 1/√2)/2 so that dimensions used for layout are a good approximation.
          [("transform", "rotate(45deg) scale(0.854)")]
          -- Rotate the content back to it's correct orientation.
          [("transform", "rotate(-45deg)")]
    )
    childViews
  where
    nodeView = case k of
      KHole{} -> HoleNode{empty = True}
      KType{} -> SyntaxNode False greenPrimary "*"
      KFun{} -> SyntaxNode False bluePrimary "→"
    childViews = map viewTreeKind (children k)

-- | Draw an edge from one point to another.
viewEdge :: P2 Double -> P2 Double -> View action
viewEdge p p' =
  div_
    [ style_
        [ ("position", "absolute")
        , ("transform-origin", "left")
        , ("z-index", "-1")
        ,
          ( "transform"
          , "translate("
              <> show p.x
              <> "px,"
              <> show p.y
              <> "px) rotate("
              <> show theta
              <> "rad)"
          )
        , ("border-style", "solid")
        , ("border-color", greySecondary)
        , ("border-width", ".125rem")
        , ("height", "0px")
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
