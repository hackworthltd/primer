{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Primer.Miso (start) where

import Foreword

import Clay qualified
import Control.Monad.Except (liftEither)
import Control.Monad.Log (Severity (Notice), WithSeverity, msgSeverity)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (..))
import Data.Default qualified as Default
import Data.Generics.Uniplate.Data (children)
import Data.List.Extra (compareLength, partition)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Tuple.Extra (uncurry3)
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
  Checked (Checked),
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
  onChecked,
  onClick,
  required_,
  scheduleIO,
  scheduleIO_,
  src_,
  style_,
  text,
  type_,
 )
import Miso.String (MisoString, fromMisoString, ms)
import Numeric.Natural (Natural)
import Optics (lensVL, to, use, (%), (.~), (^.), (^..))
import Optics.State.Operators ((%=), (.=), (?=))
import Primer.Action (toProgActionInput, toProgActionNoInput)
import Primer.Action.Available qualified as Available
import Primer.App (
  DefSelection (..),
  Editable (..),
  MutationRequest (..),
  NodeSelection (..),
  NodeType (BodyNode, SigNode),
  ProgError (ActionError),
  Selection' (SelectionDef, SelectionTypeDef),
  TypeDefConsFieldSelection (..),
  TypeDefConsSelection (..),
  TypeDefNodeSelection (..),
  TypeDefParamSelection (..),
  TypeDefSelection (..),
  appIdCounter,
  appNameCounter,
  appProg,
  checkAppWellFormed,
  newApp,
  progAllDefs,
  progAllTypeDefs,
  progAllTypeDefsMeta,
  progSelection,
 )
import Primer.App qualified
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  CaseFallback' (CaseExhaustive, CaseFallback),
  Expr,
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
  GlobalName (baseName, qualifiedModule),
  ID (..),
  Kind' (..),
  LocalName (unLocalName),
  Meta (Meta),
  ModuleName,
  Pattern (PatCon, PatPrim),
  PrimCon (..),
  TmVarRef (GlobalVarRef, LocalVarRef),
  Type' (..),
  getID,
  globalNamePretty,
  typesInExpr,
  _exprMetaLens,
  _kindMetaLens,
  _typeMetaLens,
 )
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetTypeMetadata)
import Primer.Def (defAST)
import Primer.Eval (Dir (..), EvalLog, NormalOrderOptions (..), findRedex, redexes, step)
import Primer.Eval.Redex (RunRedexOptions (..), ViewRedexOptions (..))
import Primer.EvalFullStep (EvalFullError (TimedOut), evalFull)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Log (runPureLogT)
import Primer.Miso.Layout (
  slHSep,
  slHeight,
  slVSep,
  slWidth,
  symmLayout',
 )
import Primer.Miso.Util (
  ASTDefT (expr, sig),
  P2,
  SelectionT,
  TermMeta',
  assumeDefHasTypeCheckInfo,
  assumeDefSelectionHasTypeCheckInfo,
  assumeTypeDefHasTypeCheckInfo,
  assumeTypeDefSelectionHasTypeCheckInfo,
  astDefTtoAstDef,
  astTypeDefTtoAstTypeDef,
  availableForSelection,
  bindingsInExpr,
  bindingsInType,
  clayToMiso,
  findASTTypeOrTermDef,
  kindsInType,
  nodeSelectionType,
  optToName,
  readMs,
  realToClay,
  runMutationWithNullDb,
  runTC,
  setSelectionAction,
  showMs,
  startAppWithSavedState,
  stringToOpt,
  typeBindingsInExpr,
 )
import Primer.Name (Name, NameCounter, unName)
import Primer.TypeDef (ASTTypeDef (..), TypeDef (TypeDefAST), ValCon (..), forgetTypeDefMetadata, typeDefAST, typeDefKind)
import Primer.Typecheck (SmartHoles (SmartHoles), buildTypingContext, exprTtoExpr, typeTtoType)

start :: JSM ()
start =
  startAppWithSavedState
    App
      { model =
          Model
            { app = fromRight (error "initial app is not well-formed") $ checkAppWellFormed newApp
            , readOnlySelection = Nothing
            , components =
                ComponentModels
                  { actionPanel =
                      ActionPanelModel
                        { optionsMode = Nothing
                        }
                  , eval =
                      EvalModel
                        { opts =
                            -- TODO we use these values so that the initial visuals are in sync with Miso's state,
                            -- since it's not clear how best to e.g. set checboxes to start active,
                            -- but it would maybe be better to use our defaults from tests instead
                            -- (grouped lets, push and elide, aggressive - see `evalFullTest`),
                            -- or whatever our React frontend used...
                            EvalOpts
                              { normalOrder = UnderBinders
                              , viewRedex =
                                  ViewRedexOptions
                                    { groupedLets = False
                                    , avoidShadowing = False
                                    , aggressiveElision = False
                                    }
                              , runRedex =
                                  RunRedexOptions
                                    { pushAndElide = False
                                    }
                              , stepLimit = 0
                              , dir = Chk
                              }
                        , fullscreen = False
                        , history = []
                        }
                  }
            }
      , update = updateModel
      , view = viewModel
      , subs = []
      , events = defaultEvents
      , initialAction = NoOp "start"
      , mountPoint = Nothing
      , logLevel = Off
      }

data Model = Model
  { app :: Primer.App.App
  , readOnlySelection :: Maybe SelectionT
  -- ^ A non-editable def is being viewed (e.g. from an imported module), rather than the selection in `app`.
  , components :: ComponentModels
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Model

-- TODO When Miso 1.9/2.0 is released, we should take advantage of its component support
-- we can then simplify some code, removing unnecessary error handling etc.
-- this type contains the state which should be component-local
data ComponentModels = ComponentModels
  { actionPanel :: ActionPanelModel
  , eval :: EvalModel
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ComponentModels

{- HLINT ignore "Use newtype instead of data" -}
data ActionPanelModel = ActionPanelModel
  { optionsMode :: Maybe (Available.InputAction, Available.Options)
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ActionPanelModel

data EvalModel = EvalModel
  { opts :: EvalOpts
  , fullscreen :: Bool
  , history :: [(Expr, ([ID], Maybe ID), (ID, NameCounter))]
  -- ^ The head is the result of non-interactive eval, and the rest are the steps which the student has chosen.
  -- Each step includes the produced expression as well as its redexes, and the counter state which produced it.
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EvalModel

data EvalOpts = EvalOpts
  { normalOrder :: NormalOrderOptions
  , viewRedex :: ViewRedexOptions
  , runRedex :: RunRedexOptions
  , stepLimit :: Natural
  , dir :: Dir
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EvalOpts

data Action
  = NoOp MisoString -- For situations where Miso requires an action, but we don't actually want to do anything.
  | Select Editable SelectionT
  | ShowActionOptions (Available.InputAction, Available.Options)
  | ApplyAction (Either Available.NoInputAction (Available.InputAction, Available.Option))
  | CancelActionInput
  | RunUndo
  | RunRedo
  | SetApp Primer.App.App
  | SetEvalOpts (EvalOpts -> EvalOpts)
  | ToggleFullscreenEval
  | ChooseRedex ID
  | StepBackEval

updateModel :: Action -> Model -> Effect Action Model
updateModel =
  fromTransition . \case
    NoOp _ -> pure ()
    Select editable sel -> do
      resetActionPanel
      case editable of
        NonEditable ->
          #readOnlySelection ?= sel
        Editable -> do
          #readOnlySelection .= Nothing
          runMutation $ Edit $ pure $ setSelectionAction sel
    ShowActionOptions a ->
      #components % #actionPanel % #optionsMode ?= a
    ApplyAction actionAndOpts -> do
      prog <- appProg <$> use #app
      let defs = progAllDefs prog
          tydefs = progAllTypeDefsMeta prog
      -- TODO handle errors properly, not just `Either MisoString`
      actionResult <- runExceptT do
        sel <- liftEither $ maybeToEither (Left "no selection for action") $ progSelection prog
        (_editable, def) <-
          liftEither
            . first (Left . ("findASTDef failure in runAction: " <>))
            $ findASTTypeOrTermDef tydefs defs sel
        liftEither $ first (Right . ActionError) case actionAndOpts of
          Left action -> toProgActionNoInput (snd <$> defs) def (getID <$> sel) action
          Right (action, opt) -> toProgActionInput def (getID <$> sel) opt action
      case actionResult of
        Right actions -> runMutation $ Edit actions
        Left e -> scheduleIO_ $ consoleLog $ "running action failed: " <> either identity showMs e
      resetActionPanel
    RunUndo -> do
      runMutation Undo
      resetActionPanel
    RunRedo -> do
      runMutation Redo
      resetActionPanel
    CancelActionInput ->
      resetActionPanel
    SetApp a -> do
      #app .= a
      setEval
    SetEvalOpts f -> do
      #components % #eval % #opts %= f
      setEval
    ToggleFullscreenEval -> #components % #eval % #fullscreen %= not
    ChooseRedex id -> do
      (tydefs, defs, _) <- getDefs <$> use #app
      use (#components % #eval % #history) >>= \case
        [] ->
          -- TODO warn here
          -- this shouldn't be possible
          -- history can only be empty if eval panel is empty (which means no definition is selected)
          -- but `ChooseRedex` is only currently triggered by clicking on a node in that panel
          pure ()
        (currentEvalExpr, _, s0) : _ -> do
          opts <- use $ #components % #eval % #opts
          let ((evalStepResult, evalStepLogs), s1) =
                either absurd identity
                  . runTC s0
                  . runPureLogT
                  $ step opts.viewRedex.avoidShadowing' tydefs defs currentEvalExpr opts.dir id
          scheduleIO_ $ logAllToConsole @EvalLog evalStepLogs
          case evalStepResult of
            Left err -> scheduleIO_ $ consoleLog $ "eval error: " <> show err
            -- TODO do something with `_detail`, i.e. move towards actual eval mode
            -- a first step could be to label redexes with a brief explanation
            -- i.e. match on the `EvalDetail` constructor, without looking at its fields
            Right (expr, _detail) -> do
              let (rxs, redexesLogs) = getRedexes opts tydefs defs expr
              scheduleIO_ $ logAllToConsole redexesLogs
              #components % #eval % #history %= ((expr, rxs, s1) :)
    StepBackEval -> do
      (#components % #eval % #history) %= \case
        [] -> [] -- TODO warn here? this shouldn't happen since we don't display the button for this in this state
        _ : h -> h
  where
    -- TODO the only part of this that should really require `IO` is writing to a database
    -- (currently we use `NullDb` anyway but this will change)
    -- if we could modify the frontend model purely, and fork off the DB writing,
    -- and we could drop the `SetApp` action and make this a lot simpler
    runMutation mr = do
      app <- use #app
      scheduleIO do
        (logs, res) <- liftIO $ runMutationWithNullDb mr app
        logAllToConsole logs
        pure $ SetApp res
    -- TODO find a more principled way to decide when to do this, or just run it all the time
    -- this may become simpler when we make the panel a Miso component
    resetActionPanel = #components % #actionPanel % #optionsMode .= Nothing
    setEval = do
      app <- use #app
      opts <- use $ #components % #eval % #opts
      fullscreen <- use $ #components % #eval % #fullscreen
      let (tydefs, defs, maybeDef) = getDefs app
      evalModel <- case maybeDef of
        Nothing -> pure EvalModel{opts, fullscreen, history = []}
        Just def -> do
          let nextId = succ $ appIdCounter app
              nextName = succ $ appNameCounter app
              -- TODO put this in to a background thread rather than blocking whole program for expensive evaluations
              ((evalResult, evalFullLogs), s') =
                either absurd identity
                  . runTC (succ nextId, nextName)
                  . runPureLogT
                  . evalFull opts.normalOrder opts.viewRedex opts.runRedex tydefs defs opts.stepLimit opts.dir
                  $ Ann (Meta nextId Nothing Nothing) (exprTtoExpr def.expr) (typeTtoType def.sig)
              expr = case evalResult of
                Left (TimedOut e) -> e
                Right e -> e
              (rxs, redexesLogs) = getRedexes opts tydefs defs expr
          scheduleIO_ $ logAllToConsole @EvalLog evalFullLogs
          scheduleIO_ $ logAllToConsole redexesLogs
          pure EvalModel{opts, fullscreen, history = [(expr, rxs, s')]}
      #components % #eval .= evalModel
    -- TODO `findRedex` and `redexes` do a lot of the same work - we should find some way to use a single fold
    getRedexes (opts :: EvalOpts) tydefs defs expr = runIdentity $ runPureLogT @_ @(WithSeverity EvalLog) do
      normalOrderRedex <- runMaybeT $ getID <$> findRedex opts.normalOrder opts.viewRedex tydefs defs opts.dir expr
      allRedexes <- redexes opts.viewRedex.avoidShadowing' tydefs defs opts.dir expr
      pure (allRedexes, normalOrderRedex)
    -- TODO better logging, including handling different severities appropriately
    logAllToConsole :: Show a => Seq (WithSeverity a) -> JSM ()
    logAllToConsole logs =
      let issues = filter ((<= Notice) . msgSeverity) $ toList logs
       in unless (null issues) $ consoleLog $ ms $ unlines $ map show issues
    -- TODO DRY this with `viewModel`
    -- when we use Miso components it might be easier to compute this in one place then send messages around
    getDefs app =
      let
        prog = appProg app
        defs = snd <$> progAllDefs prog
        tydefs = snd <$> progAllTypeDefs prog
        def =
          fromMaybe (error "selected def is not fully typechecked")
            . maybe (error "unexpected primitive def") assumeDefHasTypeCheckInfo
            . defAST
            . fromMaybe (error "selected def not found")
            . (defs !?)
            <$> case progSelection prog of
              Just (SelectionDef s) -> Just s.def
              _ -> Nothing
       in
        (tydefs, defs, def)

viewModel :: Model -> View Action
viewModel Model{..} =
  div_
    ([id_ "miso-root"] <> mwhen components.eval.fullscreen [class_ "fullscreen-eval"])
    $ [ div_
          [id_ "def-panel"]
          let
            viewDef def editable =
              let s = SelectionDef $ DefSelection def Nothing
               in button_
                    [ class_ $ mwhen (Just s == maybeDefSel) "selected"
                    , class_ $ mwhen (editable == NonEditable) "read-only"
                    , onClick $ Select editable s
                    ]
                    [text $ ms $ globalNamePretty def]
            viewTypeDef def editable =
              let s = SelectionTypeDef $ TypeDefSelection def Nothing
               in button_
                    [ class_ $ mwhen (Just s == maybeDefSel) "selected"
                    , class_ $ mwhen (editable == NonEditable) "read-only"
                    , onClick $ Select editable s
                    , class_ "type"
                    ]
                    [text $ ms $ globalNamePretty def]
            (editableDefs, nonEditableDefs) =
              partition ((== Editable) . snd)
                . Map.toList
                . map fst
                . Map.mapMaybe (traverse defAST)
                $ progAllDefs prog
            (editableTypeDefs, nonEditableTypeDefs) =
              partition ((== Editable) . snd)
                . Map.toList
                . map fst
                . Map.mapMaybe (traverse typeDefAST)
                $ progAllTypeDefs prog
           in
            mconcat
              [ uncurry viewTypeDef <$> editableTypeDefs
              , uncurry viewDef <$> editableDefs
              , uncurry viewTypeDef <$> nonEditableTypeDefs
              , uncurry viewDef <$> nonEditableDefs
              ]
      ]
      <> case maybeDefSel of
        Nothing -> [text "no selection"]
        Just sel ->
          [ case selAndDef of
              Left (sel', ASTTypeDef{astTypeDefParameters}) ->
                div_ [id_ "params"] $
                  astTypeDefParameters <&> \(paramName, paramKind) ->
                    div_
                      []
                      [ div_
                          ( let s =
                                  SelectionTypeDef
                                    . TypeDefSelection sel'.def
                                    . Just
                                    . TypeDefParamNodeSelection
                                    $ TypeDefParamSelection paramName Nothing
                             in [ onClick $ Select editable s
                                , class_ "param-name"
                                ]
                                  <> mwhen (sel == s) [class_ "selected"]
                          )
                          [text $ ms $ unName $ unLocalName paramName]
                      , fst
                          . viewTree
                          $ viewTreeKind
                            ( mkMetaSelectable $
                                SelectionTypeDef
                                  . TypeDefSelection sel'.def
                                  . Just
                                  . TypeDefParamNodeSelection
                                  . TypeDefParamSelection paramName
                                  . Just
                                  . Right
                                  . Right
                            )
                            paramKind
                      ]
              Right (sel', def) ->
                div_
                  [ id_ "sig"
                  ]
                  [ fst
                      . viewTree
                      $ viewTreeType
                        ( mkMetaSelectable $
                            SelectionDef
                              . DefSelection sel'.def
                              . Just
                              . NodeSelection SigNode
                              . Right
                        )
                        def.sig
                  ]
          , case selAndDef of
              Left (sel', ASTTypeDef{astTypeDefConstructors}) ->
                div_ [id_ "cons"] $
                  astTypeDefConstructors <&> \ValCon{valConName, valConArgs} ->
                    div_
                      []
                      [ div_
                          ( let s =
                                  SelectionTypeDef
                                    . TypeDefSelection sel'.def
                                    . Just
                                    . TypeDefConsNodeSelection
                                    $ TypeDefConsSelection valConName Nothing
                             in [ onClick $ Select editable s
                                , class_ "con-name"
                                ]
                                  <> mwhen (sel == s) [class_ "selected"]
                          )
                          [text $ ms $ unName $ baseName valConName]
                      , div_ [class_ "con-fields"] $
                          zip [0 ..] valConArgs <&> \(valConIndex, conArgType) ->
                            fst
                              . viewTree
                              $ viewTreeType
                                ( mkMetaSelectable $
                                    SelectionTypeDef
                                      . TypeDefSelection sel'.def
                                      . Just
                                      . TypeDefConsNodeSelection
                                      . TypeDefConsSelection valConName
                                      . Just
                                      . TypeDefConsFieldSelection valConIndex
                                      . Right
                                )
                                conArgType
                      ]
              Right (sel', def) ->
                div_
                  [ id_ "body"
                  ]
                  [ fst
                      . viewTree
                      $ viewTreeExpr
                        ( mkMetaSelectable $
                            SelectionDef
                              . DefSelection sel'.def
                              . Just
                              . NodeSelection BodyNode
                        )
                        def.expr
                  ]
          , div_
              [ id_ "selection-type"
              ]
              [ let mkMeta = const (Nothing, Nothing, NoHighlight)
                    metaToTree = \case
                      Left t -> viewTreeType mkMeta t
                      Right (Left t) -> viewTreeKind mkMeta t
                      -- TODO this isn't really correct - kinds in Primer don't have kinds
                      Right (Right ()) -> viewTreeKind mkMeta $ KType ()
                 in case selAndDef of
                      Right (DefSelection{node}, def) -> fst $ viewTree case node of
                        Nothing -> viewTreeType mkMeta $ forgetTypeMetadata def.sig
                        Just s -> metaToTree $ nodeSelectionType s.meta
                      Left (TypeDefSelection{def = defName, node}, def) -> fst $ viewTree case node of
                        Nothing -> viewTreeKind mkMeta $ typeDefKind $ forgetTypeDefMetadata $ TypeDefAST def
                        Just (TypeDefParamNodeSelection (TypeDefParamSelection{kindMeta})) -> case kindMeta of
                          Nothing -> viewTreeType @() @() mkMeta $ TCon () defName
                          Just m -> metaToTree $ nodeSelectionType m
                        Just (TypeDefConsNodeSelection (TypeDefConsSelection{field})) -> case field of
                          Nothing -> viewTreeType @() @() mkMeta $ TCon () defName
                          Just m -> metaToTree $ nodeSelectionType m.meta
              ]
          , div_ [id_ "action-panel"] case components.actionPanel.optionsMode of
              Nothing ->
                availableForSelection tydefs defs level editable def sel <&> \action ->
                  button_
                    [ onClick case action of
                        Available.NoInput a -> ApplyAction $ Left a
                        Available.Input a ->
                          ShowActionOptions
                            ( a
                            , fromMaybe (error "couldn't get action options") $
                                Available.options
                                  tydefs
                                  defs
                                  (buildTypingContext tydefs defs SmartHoles)
                                  level
                                  def
                                  (getID <$> sel)
                                  a
                            )
                    ]
                    [ text case action of
                        Available.NoInput a -> showMs a
                        Available.Input a -> showMs a
                    ]
                where
                  def = bimap (astTypeDefTtoAstTypeDef . snd) (astDefTtoAstDef . snd) selAndDef
                  level = Primer.App.Expert -- TODO don't hardcode
              Just (action, opts) ->
                ( case opts.free of
                    Available.FreeNone -> []
                    _ ->
                      [ form_
                          []
                          [ input_
                              [ type_ "text"
                              , required_ True
                              , onChange $ ApplyAction . Right . (action,) . stringToOpt . fromMisoString
                              ]
                          , button_ [] [text "↩"]
                          ]
                      ]
                )
                  <> ( opts.opts
                         <&> \opt ->
                           button_
                             ( [onClick $ ApplyAction $ Right (action, opt)]
                                 <> mwhen
                                   opt.matchesType
                                   [class_ "matches-type"]
                             )
                             [ text $ ms $ either (unName . unLocalName) globalNamePretty $ optToName opt
                             ]
                     )
                  <> [ button_ [class_ "cancel", onClick CancelActionInput] [text "Cancel"]
                     ]
          , div_
              [id_ "undo-redo-panel"]
              [ button_ [onClick RunUndo] [text "Undo"]
              , button_ [onClick RunRedo] [text "Redo"]
              ]
          , div_ [id_ "eval"] $
              [ let checkBox t f =
                      div_
                        []
                        [ input_
                            [ type_ "checkbox"
                            , onChecked \(Checked b) -> SetEvalOpts $ f b
                            ]
                        , text t
                        ]
                 in div_
                      [id_ "eval-options"]
                      [ checkBox "Stop at binders" \b -> #normalOrder .~ if b then StopAtBinders else UnderBinders
                      , checkBox "Grouped lets" \b -> #viewRedex % #groupedLets .~ b
                      , checkBox "Aggressive elision" \b -> #viewRedex % #aggressiveElision .~ b
                      , checkBox "Avoid shadowing" \b -> #viewRedex % #avoidShadowing .~ b
                      , checkBox "Push and elide" \b -> #runRedex % #pushAndElide .~ b
                      , checkBox "Synthesise" \b -> #dir .~ if b then Syn else Chk
                      , div_
                          []
                          [ input_
                              [ type_ "number"
                              , onChange $
                                  maybe
                                    (NoOp "failed to read number input")
                                    (\n -> SetEvalOpts $ #stepLimit .~ n)
                                    . readMs
                              ]
                          , text "Steps"
                          ]
                      ]
              , div_
                  [id_ "eval-overlay"]
                  $ munless (compareLength components.eval.history 2 == LT) [button_ [onClick StepBackEval] ["↩"]]
                    <> [ button_ [onClick ToggleFullscreenEval] ["⛶"]
                       ]
              ]
                <> case components.eval.history of
                  [] -> [text "No definition selected for evaluation"]
                  (expr, rxs, _) : _ ->
                    [fst . viewTree $ viewTreeExpr (mkMeta' . getID) expr]
                    where
                      mkMeta' id =
                        if id `notElem` fst rxs
                          then (Just id, Nothing, NoHighlight)
                          else
                            ( Just id
                            , Just $ ChooseRedex id
                            , if snd rxs == Just id
                                then AnimatedHighlight -- this is the normal order redex
                                else SimpleHighlight
                            )
          ]
          where
            defsWithEditable = progAllDefs prog
            tydefsWithEditable = second forgetTypeDefMetadata <$> tydefsWithEditableMeta
            tydefsWithEditableMeta = progAllTypeDefsMeta prog
            defs = snd <$> defsWithEditable
            tydefs = snd <$> tydefsWithEditable
            (editable, selAndDef) = case sel of
              SelectionTypeDef sel'@TypeDefSelection{def = d} ->
                second (Left . (sel',) . getTypeDef) . fromMaybe (error "selected def not found") $
                  tydefsWithEditableMeta !? d
              SelectionDef sel'@DefSelection{def = d} ->
                second (Right . (sel',) . getDef) . fromMaybe (error "selected def not found") $
                  defsWithEditable !? d
            mkMetaSelectable mkSel m =
              let s = mkSel m
               in ( Just $ getID m
                  , Just $ Select editable s
                  , if sel == s then SimpleHighlight else NoHighlight
                  )
  where
    prog = appProg app
    maybeDefSel = readOnlySelection <|> (getSelection <$> progSelection prog)
    -- TODO better error handling
    getDef =
      fromMaybe (error "selected def is not fully typechecked")
        . maybe (error "unexpected primitive def") assumeDefHasTypeCheckInfo
        . defAST
    getTypeDef =
      fromMaybe (error "selected typedef is not fully typechecked")
        . maybe (error "unexpected primitive typedef") assumeTypeDefHasTypeCheckInfo
        . typeDefAST
    getSelection = \case
      SelectionTypeDef d -> SelectionTypeDef $ fromMaybe (error "no TC info in selection") $ assumeTypeDefSelectionHasTypeCheckInfo d
      SelectionDef d -> SelectionDef $ fromMaybe (error "no TC info in selection") $ assumeDefSelectionHasTypeCheckInfo d

-- TODO `isNothing clickAction` implies `highlight == NoHighlight`, and `isNothing id` iff `isNothing clickAction`
-- we could model this better, but in the long run, we intend to have no unselectable nodes anyway
data NodeViewData action = NodeViewData
  { id :: Maybe ID
  , clickAction :: Maybe action
  , highlight :: NodeHighlight
  , level :: Level
  , opts :: NodeViewOpts action
  }

data NodeHighlight
  = NoHighlight
  | SimpleHighlight
  | AnimatedHighlight

data NodeViewOpts action
  = SyntaxNode {wide :: Bool, flavor :: MisoString, text :: MisoString}
  | HoleNode {empty :: Bool}
  | PrimNode PrimCon
  | ConNode {name :: Name, scope :: ModuleName}
  | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
  | PatternBoxNode (Maybe (View action, V2 Double)) -- `Nothing` indicates that this is a fallback pattern.

data Level
  = Expr
  | Type
  | Kind

viewNodeData :: Bool -> P2 Double -> V2 Double -> [View action] -> NodeViewData action -> View action
viewNodeData showIDs position dimensions edges node = case node.opts of
  PrimNode (PrimAnimation animation) ->
    img_
      [ src_ ("data:img/gif;base64," <> ms animation)
      , style_ $ clayToMiso do
          Clay.width $ Clay.px $ realToClay dimensions.x
          Clay.height $ Clay.px $ realToClay dimensions.y
      ]
  _ ->
    div_
      ( [ class_
            . mconcat
            . intersperse " "
            $ [ "node"
              , case node.level of
                  Expr -> "expr"
                  Type -> "type"
                  Kind -> "kind"
              , case node.opts of
                  SyntaxNode{} -> "syntax"
                  _ -> "non-syntax"
              , case node.opts of
                  SyntaxNode{flavor} -> flavor
                  HoleNode{} -> "hole"
                  PrimNode{} -> "prim"
                  ConNode{} -> "con"
                  VarNode{} -> "var"
                  PatternBoxNode{} -> "pattern-box"
              ]
              <> case node.highlight of
                NoHighlight -> []
                _ ->
                  "highlighted" : case node.highlight of
                    SimpleHighlight -> []
                    AnimatedHighlight -> ["animated"]
              <> mwhen (isJust node.clickAction) ["selectable"]
        , style_ $ clayToMiso do
            Clay.position Clay.absolute
            Clay.transform $
              Clay.translate
                (Clay.px $ realToClay position.x)
                (Clay.px $ realToClay position.y)
        ]
          <> foldMap' (pure . onClick) node.clickAction
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
                       [ text
                           if showIDs
                             then maybe "" (ms @Text . show) node.id
                             else case node.opts of
                               SyntaxNode{text = t} -> t
                               HoleNode{empty = e} -> if e then "?" else "⚠️"
                               PrimNode pc -> case pc of
                                 PrimChar c' -> showMs c'
                                 PrimInt n -> showMs n
                               ConNode{name} -> ms $ unName name
                               VarNode{name} -> ms $ unName name
                       ]
                   ]
           ]

viewTreeExpr ::
  (Data a, Data b, Data c) =>
  (TermMeta' a b c -> (Maybe ID, Maybe action, NodeHighlight)) ->
  Expr' a b c ->
  Tree.Tree (NodeViewData action)
viewTreeExpr mkMeta e =
  Tree.Node
    (uncurry3 NodeViewData (mkMeta $ Left $ e ^. _exprMetaLens) Expr nodeView)
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
                ( NodeViewData Nothing Nothing NoHighlight Expr
                    $ PatternBoxNode
                    $ Just
                    $ viewTree
                    $ Tree.Node
                      ( NodeViewData Nothing Nothing NoHighlight Expr case p of
                          PatCon c -> ConNode{name = baseName c, scope = qualifiedModule c}
                          PatPrim c -> PrimNode c
                      )
                    $ bindings <&> \(Bind m v) ->
                      Tree.Node
                        (uncurry3 NodeViewData (mkMeta $ Left m) Expr VarNode{name = unLocalName v, mscope = Nothing})
                        []
                )
                [viewTreeExpr mkMeta r]
          , case fb of
              CaseExhaustive -> []
              CaseFallback r -> [Tree.Node (NodeViewData Nothing Nothing NoHighlight Expr $ PatternBoxNode Nothing) [viewTreeExpr mkMeta r]]
          ]
      _ ->
        mconcat
          [ map (viewTreeBinding Type) (e ^.. typeBindingsInExpr)
          , map (viewTreeBinding Expr) (e ^.. bindingsInExpr)
          , map (viewTreeType (mkMeta . Right)) (e ^.. typesInExpr)
          , map (viewTreeExpr mkMeta) (children e)
          ]
        where
          viewTreeBinding l name = Tree.Node (NodeViewData Nothing Nothing NoHighlight l VarNode{name = unLocalName name, mscope = Nothing}) []

viewTreeType ::
  (Data b, Data c) =>
  (Either b c -> (Maybe ID, Maybe action, NodeHighlight)) ->
  Type' b c ->
  Tree.Tree (NodeViewData action)
viewTreeType mkMeta t =
  Tree.Node
    (uncurry3 NodeViewData (mkMeta $ Left $ t ^. _typeMetaLens) Type nodeView)
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
        (\name -> Tree.Node (NodeViewData Nothing Nothing NoHighlight Type VarNode{name, mscope = Nothing}) [])
        (t ^.. bindingsInType % to unLocalName)
        <> map (viewTreeKind (mkMeta . Right)) (t ^.. kindsInType)
        <> map (viewTreeType mkMeta) (children t)

viewTreeKind ::
  (Data c) =>
  (c -> (Maybe ID, Maybe action, NodeHighlight)) ->
  Kind' c ->
  Tree.Tree (NodeViewData action)
viewTreeKind mkMeta k =
  Tree.Node
    (uncurry3 NodeViewData (mkMeta $ k ^. _kindMetaLens) Kind nodeView)
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
                  showIDs
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
    showIDs = False
