{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Primer.Miso (component) where

import Foreword

import Clay qualified
import Control.Monad.Except (liftEither)
import Control.Monad.Log (Severity (Notice), WithSeverity, msgSeverity)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Data (Data (..))
import Data.Generics.Uniplate.Data (children, universe)
import Data.List.Extra (compareLength, partition)
import Data.Map ((!?))
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import Data.Tuple.Extra (uncurry3)
import GHC.Base (error)
import Linear (Metric (norm), R1 (_x), V2 (V2), unangle)
import Linear.Affine ((.+^), (.-.), (.-^))
import Miso (
  Checked (Checked),
  Component (..),
  DOMRef,
  Effect,
  LogLevel (Off),
  View,
  consoleLog,
  io,
  io_,
  onCreatedWith,
  text,
 )
import Miso.CSS (style_)
import Miso.DSL ((#))
import Miso.Event (stopPropagation)
import Miso.FFI (alert)
import Miso.Html (
  button_,
  div_,
  form_,
  img_,
  input_,
  onChange,
  onChecked,
  onClick,
  onClickWithOptions,
  onInput,
  onSubmit,
  textarea_,
 )
import Miso.Html.Property (
  class_,
  disabled_,
  id_,
  max_,
  min_,
  placeholder_,
  required_,
  src_,
  step_,
  type_,
  value_,
 )
import Miso.Navigator (copyClipboard)
import Miso.String (MisoString, fromMisoString, ms)
import Numeric (readHex, showFFloat, showHex)
import Numeric.Natural (Natural)
import Optics (at, ix, lensVL, to, use, (%), (%~), (.~), (^.), (^..))
import Optics.State.Operators ((%=), (.=), (?=))
import Primer.Action (ProgAction (BodyAction, MoveToDef), toProgActionInput, toProgActionNoInput)
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
  mkSimpleModuleName,
  qualifyName,
  trivialMeta,
  typesInExpr,
  _exprMetaLens,
  _kindMetaLens,
  _type,
  _typeMetaLens,
 )
import Primer.Core qualified as Primer
import Primer.Core.Utils (forgetMetadata, forgetTypeMetadata, generateIDs)
import Primer.Def (defAST)
import Primer.Eval (Dir (..), EvalLog, NormalOrderOptions (..), findRedex, redexes, step)
import Primer.Eval.Redex (RunRedexOptions (..), ViewRedexOptions (..))
import Primer.EvalFullInterp (InterpError, Timeout (MicroSec), interp, mkGlobalEnv)
import Primer.EvalFullStep (EvalFullError (TimedOut), evalFull)
import Primer.JSON (CustomJSON (..), PrimerJSON)
import Primer.Log (runPureLogT)
import Primer.Miso.ActionName (actionName)
import Primer.Miso.Edit (Literal (LitPrim), colourLiteral, editActions, stringLiteral)
import Primer.Miso.Layout (
  Layering (NonLayered),
  Placement (Below, Beside),
  TidyOpts (..),
  tidyLayout,
 )
import Primer.Miso.PathEditor (EditPath (..), PathEditorOpts (..), pathEditorSize, pathLiteral, viewPathEditor)
import Primer.Miso.Picture (renderPicture)
import Primer.Miso.Projection (Projection (..), ProjectionView (..), availableProjection, sameKind)
import Primer.Miso.Util (
  ASTDefT (expr, sig),
  EvalExprT,
  ExprInfo (projection, typeInfo),
  P2,
  SelectionT,
  TermMeta',
  addExprInfo,
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
  defaultProjections,
  findASTTypeOrTermDef,
  forgetExprInfo,
  kindsInType,
  nodeSelectionType,
  optToName,
  readMs,
  realToClay,
  runMutationWithNullDb,
  runTC,
  setProjection,
  setSelectionAction,
  showMs,
  stringToOpt,
  syncDefs,
  typeBindingsInExpr,
  typeCacheType,
 )
import Primer.Name (Name, NameCounter, unName, unsafeMkName)
import Primer.Primitives (Colour (RGB), Path)
import Primer.TypeDef (ASTTypeDef (..), TypeDef (TypeDefAST), ValCon (..), forgetTypeDefMetadata, typeDefAST, typeDefKind)
import Primer.Typecheck (SmartHoles (SmartHoles), buildTypingContext, exprTtoExpr, typeTtoType)

component :: Component () () Model Action
component =
  Component
    { model =
        Model
          { app = initialApp
          , defsT = syncDefs (snd <$> progAllDefs (appProg initialApp)) mempty
          , replacedNodes = mempty
          , pointDrag = Nothing
          , projectionEdits = ProjectionEditsIdle
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
                            , interpret = False
                            }
                      , fullscreen = False
                      , history = []
                      }
                , load = Nothing
                }
          }
    , update = updateModel
    , view = viewModel
    , subs = []
    , styles = []
    , scripts = []
    , mountPoint = Nothing
    , mount = Nothing
    , unmount = Nothing
    , logLevel = Off
    , mailbox = const Nothing
    , hydrateModel = Nothing
    , eventPropagation = False
    , useContext = False
    , onPropsChanged = Nothing
    }

initialApp :: Primer.App.App
initialApp = fromRight (error "initial app is not well-formed") $ checkAppWellFormed newApp

data Model = Model
  { app :: Primer.App.App
  , readOnlySelection :: Maybe SelectionT
  -- ^ A non-editable def is being viewed (e.g. from an imported module), rather than the selection in `app`.
  , defsT :: Map GVarName ASTDefT
  -- ^ The frontend versions of the definitions in `app`. See `syncDefs`.
  , replacedNodes :: Map ID ID
  -- ^ Nodes which have been replaced by edits through projections, e.g. a number literal edited with a slider.
  -- This lets us apply further edits from the same projection, which may have been rendered before the replacement.
  , pointDrag :: Maybe (ID, Int)
  -- ^ The point being dragged in a path editor, if any: the path's node ID, and the index of the point.
  , projectionEdits :: ProjectionEdits
  , components :: ComponentModels
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON Model

-- | The state of edits through projections. See `ApplyProjectionEdit`.
data ProjectionEdits
  = ProjectionEditsIdle
  | -- | An edit is being applied. We also store the latest edit to apply next, if any.
    ProjectionEditInProgress (Maybe (GVarName, ID, Literal, Projection))
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ProjectionEdits

-- TODO When Miso 1.9/2.0 is released, we should take advantage of its component support
-- we can then simplify some code, removing unnecessary error handling etc.
-- this type contains the state which should be component-local
data ComponentModels = ComponentModels
  { actionPanel :: ActionPanelModel
  , eval :: EvalModel
  , load :: Maybe (Maybe Text)
  -- ^ Whether the box for loading a saved program is open, and if so, why the last attempt failed (if it did).
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ComponentModels

{- HLINT ignore "Use newtype instead of data" -}
data ActionPanelModel = ActionPanelModel
  { optionsMode :: Maybe (Either TypeOrTermDef (Available.InputAction, Available.Options))
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ActionPanelModel

data EvalModel = EvalModel
  { opts :: EvalOpts
  , fullscreen :: Bool
  , history :: [(EvalExprT, ([ID], Maybe ID), (ID, NameCounter))]
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
  , interpret :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON EvalOpts

data Action
  = NoOp MisoString -- For situations where Miso requires an action, but we don't actually want to do anything.
  | Select Editable SelectionT
  | ShowActionOptions (Available.InputAction, Available.Options)
  | ApplyAction (Either Available.NoInputAction (Available.InputAction, Available.Option))
  | SelectCreateDef TypeOrTermDef
  | CreateDef TypeOrTermDef Text
  | CancelActionInput
  | RunUndo
  | RunRedo
  | SaveApp
  | OpenLoadBox
  | CloseLoadBox
  | LoadApp MisoString
  | ResetApp
  | Focus DOMRef
  | Alert MisoString
  | SetApp Primer.App.App
  | SetEvalOpts (EvalOpts -> EvalOpts)
  | ToggleFullscreenEval
  | SetDefProjection GVarName ID (Maybe Projection)
  | SetEvalProjection ID (Maybe Projection)
  | -- | Replace the node with the given ID with a literal, through the given projection.
    ApplyProjectionEdit GVarName ID Literal Projection
  | SetAppProjecting GVarName ID (Maybe Projection) Primer.App.App
  | DragPathPoint (Maybe (ID, Int))
  | ChooseRedex ID
  | StepBackEval
  | SetInterpResult (Either InterpError (Expr' () () ()))

data TypeOrTermDef = TypeDef | TermDef
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON TypeOrTermDef

updateModel :: Action -> Effect () () Model Action
updateModel = \case
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
    #components % #actionPanel % #optionsMode ?= Right a
  ApplyAction actionAndOpts -> do
    prog <- appProg <$> use #app
    let defs = progAllDefs prog
        tydefs = progAllTypeDefsMeta prog
    -- TODO handle errors properly, not just `Either MisoString`
    actionResult <- runExceptT do
      sel <- liftEither $ maybeToEither (Left "no selection for action") $ progSelection prog
      (_editable, def) <-
        liftEither
          . first (Left . ms . ("findASTDef failure in runAction: " <>))
          $ findASTTypeOrTermDef tydefs defs sel
      liftEither $ first (Right . ActionError) case actionAndOpts of
        Left action -> toProgActionNoInput (snd <$> defs) def (getID <$> sel) action
        Right (action, opt) -> toProgActionInput def (getID <$> sel) opt action
    case actionResult of
      Right actions -> runMutation $ Edit actions
      Left e -> io_ $ consoleLog $ "running action failed: " <> either identity showMs e
    resetActionPanel
  SelectCreateDef d ->
    #components % #actionPanel % #optionsMode ?= Left d
  CreateDef d name -> do
    let activeModule = mkSimpleModuleName "Main"
    runMutation $
      Edit
        [ case d of
            TermDef -> Primer.App.CreateDef activeModule $ Just name
            TypeDef -> Primer.App.AddTypeDef (qualifyName activeModule $ unsafeMkName name) $ ASTTypeDef [] [] []
        ]
    resetActionPanel
  RunUndo -> do
    runMutation Undo
    resetActionPanel
  RunRedo -> do
    runMutation Redo
    resetActionPanel
  SaveApp -> do
    app <- use #app
    copyClipboard
      (ms $ encodeToLazyText app)
      (Alert "Program copied to clipboard")
      (const $ Alert "Failed to copy program to clipboard")
  OpenLoadBox -> #components % #load ?= Nothing
  CloseLoadBox -> #components % #load .= Nothing
  LoadApp t
    | T.null $ T.strip t' -> #components % #load ?= Nothing
    | otherwise ->
        case first toS (eitherDecodeStrictText t') >>= first show . checkAppWellFormed of
          Left e -> #components % #load ?= Just e
          Right app -> do
            #components % #load .= Nothing
            replaceApp app
    where
      t' = fromMisoString t
  ResetApp -> replaceApp initialApp
  Focus e -> io_ $ void $ e # "focus" $ ()
  Alert t -> io_ $ alert t
  CancelActionInput ->
    resetActionPanel
  SetApp a -> setApp a
  ApplyProjectionEdit def target literal projection ->
    -- Edits can arrive much faster than we can apply them and re-evaluate, e.g. while dragging a slider.
    -- So while one is in progress, we just remember the latest, and apply that once the current one is done.
    use #projectionEdits >>= \case
      ProjectionEditsIdle -> startProjectionEdit def target literal projection
      ProjectionEditInProgress _ -> #projectionEdits .= ProjectionEditInProgress (Just (def, target, literal, projection))
  DragPathPoint d -> #pointDrag .= d
  SetAppProjecting def target projection a -> do
    setApp a
    -- If we replaced the root node, the actions leave the cursor, and thus the selection, on its replacement.
    for_ projection \p -> case progSelection $ appProg a of
      Just (SelectionDef DefSelection{def = def', node = Just n}) | def' == def -> do
        #defsT % ix def % #expr %= setProjection (getID n) (Just p)
        #replacedNodes %= Map.insert target (getID n)
      _ -> pure ()
    use #projectionEdits >>= \case
      ProjectionEditInProgress (Just (def', target', literal, projection')) -> startProjectionEdit def' target' literal projection'
      _ -> #projectionEdits .= ProjectionEditsIdle
  SetEvalOpts f -> do
    #components % #eval % #opts %= f
    setEval
  ToggleFullscreenEval -> #components % #eval % #fullscreen %= not
  SetInterpResult result -> case result of
    Left e -> io_ $ consoleLog $ "interpreter failed: " <> showMs e
    Right e -> do
      app <- use #app
      let (expr, s) = either absurd identity $ runTC (succ $ appIdCounter app, succ $ appNameCounter app) $ generateIDs e
      #components % #eval % #history .= [(fromEvalResult expr, ([], Nothing), s)]
  SetDefProjection def id p -> #defsT % ix def % #expr %= setProjection id p
  SetEvalProjection id p ->
    #components % #eval % #history %= \case
      [] -> [] -- TODO warn here? this shouldn't happen since we don't display the eval panel in this state
      (expr, rxs, s) : h -> (setProjection id p expr, rxs, s) : h
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
                $ step opts.viewRedex.avoidShadowing' tydefs defs (forgetExprInfo currentEvalExpr) opts.dir id
        io_ $ logAllToConsole @EvalLog evalStepLogs
        case evalStepResult of
          Left err -> io_ $ consoleLog $ "eval error: " <> showMs err
          -- TODO do something with `_detail`, i.e. move towards actual eval mode
          -- a first step could be to label redexes with a brief explanation
          -- i.e. match on the `EvalDetail` constructor, without looking at its fields
          Right (expr, _detail) -> do
            let (rxs, redexesLogs) = getRedexes opts tydefs defs expr
            io_ $ logAllToConsole redexesLogs
            #components % #eval % #history %= ((fromEvalResult expr, rxs, s1) :)
  StepBackEval -> do
    (#components % #eval % #history) %= \case
      [] -> [] -- TODO warn here? this shouldn't happen since we don't display the button for this in this state
      _ : h -> h
  where
    -- TODO the only part of this that should really require `IO` is writing to a database
    -- (currently we use `NullDb` anyway but this will change)
    -- if we could modify the frontend model purely, and fork off the DB writing,
    -- and we could drop the `SetApp` action and make this a lot simpler
    runMutation = runMutation' SetApp
    runMutation' k mr = do
      app <- use #app
      io do
        (logs, res) <- liftIO $ runMutationWithNullDb mr app
        logAllToConsole logs
        pure $ k res
    -- We work out the actions here, rather than when rendering, since the rendered term may be out of date,
    -- if edits arrive faster than we re-render.
    startProjectionEdit def target0 literal projection = do
      #projectionEdits .= ProjectionEditsIdle
      target <- flip resolveReplaced target0 <$> use #replacedNodes
      use (#defsT % at def) >>= \case
        Nothing -> io_ $ consoleLog $ "projection edit: definition not found: " <> showMs def
        Just d -> case find ((== target) . getID) $ universe d.expr of
          Nothing -> io_ $ consoleLog $ "projection edit: node not found: " <> showMs target
          Just e -> do
            let (actions, rootReplaced) = editActions e literal
            unless (null actions) do
              #projectionEdits .= ProjectionEditInProgress Nothing
              runMutation' (SetAppProjecting def target $ guard rootReplaced $> projection) $
                Edit [MoveToDef def, BodyAction actions]
    -- Follow the chain of replacements of a node, in case it has been replaced by edits we haven't yet rendered.
    resolveReplaced replaced i = maybe i (resolveReplaced replaced) $ Map.lookup i replaced
    setApp a = do
      #app .= a
      #defsT %= syncDefs (snd <$> progAllDefs (appProg a))
      setEval
    -- TODO this replaces the whole app, including its undo history, so it can't itself be undone
    replaceApp app = do
      #readOnlySelection .= Nothing
      #defsT .= mempty
      #replacedNodes .= mempty
      resetActionPanel
      updateModel $ SetApp app
    -- TODO find a more principled way to decide when to do this, or just run it all the time
    -- this may become simpler when we make the panel a Miso component
    resetActionPanel = #components % #actionPanel % #optionsMode .= Nothing
    setEval = do
      app <- use #app
      opts <- use $ #components % #eval % #opts
      let (tydefs, defs, maybeDef) = getDefs app
      case maybeDef of
        Just def
          | opts.interpret ->
              -- PROTOTYPE: the interpreter's timeout relies on the runtime being able to interrupt a pure computation,
              -- which may not be the case on Wasm, so a non-terminating program may hang the frontend.
              io $
                SetInterpResult
                  <$> interp
                    (MicroSec 1_000_000)
                    tydefs
                    (mkGlobalEnv tydefs defs)
                    opts.dir
                    (forgetMetadata $ Ann (trivialMeta 0) (exprTtoExpr $ forgetExprInfo def.expr) (typeTtoType def.sig))
        _ -> stepEval
    -- Evaluate the selected definition by stepping, up to the step limit.
    stepEval = do
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
                  $ Ann (Meta nextId Nothing Nothing) (exprTtoExpr $ forgetExprInfo def.expr) (typeTtoType def.sig)
              expr = case evalResult of
                Left (TimedOut e) -> e
                Right e -> e
              (rxs, redexesLogs) = getRedexes opts tydefs defs expr
          io_ $ logAllToConsole @EvalLog evalFullLogs
          io_ $ logAllToConsole redexesLogs
          pure EvalModel{opts, fullscreen, history = [(fromEvalResult expr, rxs, s')]}
      #components % #eval .= evalModel
    -- Note that any projections chosen in the previous step, or in the definition being evaluated, are lost,
    -- since the core doesn't pass through our metadata (see `ExprInfo`).
    fromEvalResult = defaultProjections . addExprInfo
    -- TODO `findRedex` and `redexes` do a lot of the same work - we should find some way to use a single fold
    getRedexes (opts :: EvalOpts) tydefs defs expr = runIdentity $ runPureLogT @_ @(WithSeverity EvalLog) do
      normalOrderRedex <- runMaybeT $ getID <$> findRedex opts.normalOrder opts.viewRedex tydefs defs opts.dir expr
      allRedexes <- redexes opts.viewRedex.avoidShadowing' tydefs defs opts.dir expr
      pure (allRedexes, normalOrderRedex)
    -- TODO better logging, including handling different severities appropriately
    logAllToConsole :: Show a => Seq (WithSeverity a) -> IO ()
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

viewModel :: Model -> View () () Model Action
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
                        (Just . typeCacheType)
                        (SetDefProjection sel'.def)
                        ( guard (editable == Editable)
                            $> ProjectionEditing
                              { replace = ApplyProjectionEdit sel'.def
                              , dragging = pointDrag
                              , drag = DragPathPoint
                              }
                        )
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
                [ button_ [class_ "new", onClick $ SelectCreateDef TermDef] [text "New Def"]
                , button_ [class_ "new", onClick $ SelectCreateDef TypeDef] [text "New Type"]
                ]
                  <> ( availableForSelection tydefs defs level editable def sel <&> \action ->
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
                           [text $ ms $ actionName action]
                     )
                where
                  def = bimap (astTypeDefTtoAstTypeDef . snd) (astDefTtoAstDef . snd) selAndDef
                  level = Primer.App.Expert -- TODO don't hardcode
              Just optsMode ->
                ( case optsMode of
                    Left d -> [textEntryForm $ CreateDef d . fromMisoString]
                    Right (action, opts) ->
                      ( case opts.free of
                          Available.FreeNone -> []
                          _ -> [textEntryForm $ ApplyAction . Right . (action,) . stringToOpt . fromMisoString]
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
                )
                  <> [ button_ [class_ "cancel", onClick CancelActionInput] [text "Cancel"]
                     ]
                where
                  textEntryForm onEnter =
                    form_
                      [onSubmit $ NoOp "form submitted"] -- disables form submission, via `preventDefault`
                      [ input_
                          [ type_ "text"
                          , required_ True
                          , onChange onEnter
                          ]
                      , button_ [] [text "↩"]
                      ]
          , div_ [id_ "undo-redo-panel"] case components.load of
              Nothing ->
                [ button_ [onClick RunUndo] [text "Undo"]
                , button_ [onClick RunRedo] [text "Redo"]
                , button_ [onClick SaveApp] [text "Save"]
                , button_ [onClick OpenLoadBox] [text "Load"]
                , button_ [onClick ResetApp] [text "Reset"]
                ]
              Just err ->
                [ textarea_
                    [ id_ "load-box"
                    , placeholder_ "Paste a saved program here"
                    , onInput LoadApp
                    , onCreatedWith Focus
                    ]
                ]
                  <> maybe [] (\e -> [div_ [class_ "load-error"] [text $ ms e]]) err
                  <> [button_ [class_ "cancel", onClick CloseLoadBox] [text "Cancel"]]
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
                      , checkBox "Interpret" \b -> #interpret .~ b
                      , div_
                          []
                          [ input_
                              [ type_ "number"
                              , min_ "0"
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
                    [fst . viewTree $ viewTreeExpr (const Nothing) SetEvalProjection Nothing (mkMeta' . getID) expr]
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
                second (const $ Right (sel', getDef d)) . fromMaybe (error "selected def not found") $
                  defsWithEditable !? d
            mkMetaSelectable mkSel m =
              let s = mkSel m
               in ( Just $ getID m
                  , Just $ Select editable s
                  , -- We compare IDs, since the metadata may differ, e.g. due to projections.
                    if (getID <$> sel) == (getID <$> s) then SimpleHighlight else NoHighlight
                  )
  where
    prog = appProg app
    maybeDefSel = readOnlySelection <|> (getSelection <$> progSelection prog)
    -- TODO better error handling
    getDef d = fromMaybe (error "selected def is not fully typechecked") $ defsT !? d
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
  , projectionToggle :: Maybe (MisoString, action)
  -- ^ A button for switching between displaying the node as a tree and as a projection, with its label.
  , placement :: Placement
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
  | StringNode Text (Maybe (Text -> action)) -- See `projectString`. The function is for editing, if possible.
  | SliderNode Double Double Double (Maybe (Double -> action)) -- Bounds and value, and a function for editing, if possible.
  | ColourNode Colour (Maybe (Colour -> action)) -- The function is for editing, if possible.
  | PathNode (PathEditorOpts action) Path
  | PictureNode MisoString (V2 Double) -- An image URL, and the size at which to display it. See `renderPicture`.
  | ConNode {name :: Name, scope :: ModuleName}
  | VarNode {name :: Name, mscope :: Maybe ModuleName} -- TODO we should be able to re-use the name `scope`: https://github.com/ghc-proposals/ghc-proposals/pull/535#issuecomment-1694388075
  | PatternBoxNode (Maybe (View () () Model action, V2 Double)) -- `Nothing` indicates that this is a fallback pattern.

data Level
  = Expr
  | Type
  | Kind

viewNodeData :: Bool -> P2 Double -> V2 Double -> [View () () Model action] -> NodeViewData action -> View () () Model action
viewNodeData showIDs position dimensions edges node =
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
                StringNode{} -> "string"
                SliderNode{} -> "slider"
                ColourNode{} -> "colour"
                PathNode{} -> "path"
                PictureNode{} -> "picture"
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
             $ ( case node.opts of
                   PatternBoxNode (Just p) -> [fst p]
                   PatternBoxNode Nothing ->
                     [ div_
                         [class_ "fallback-pattern"]
                         -- "🤷🏽‍♀️" is a lexical error: https://gitlab.haskell.org/ghc/ghc/-/issues/25635
                         [text "\x1f937\x1f3fd\x200d\x2640\xfe0f"]
                     ]
                   PictureNode url _ -> [img_ [src_ url]]
                   StringNode s (Just f) ->
                     [text "\"", input_ [type_ "text", class_ "string-input", value_ $ ms s, onChange $ f . fromMisoString], text "\""]
                   SliderNode lo hi x f ->
                     [ input_ $
                         [type_ "range", min_ $ showMs lo, max_ $ showMs hi, step_ "any", value_ $ showMs x]
                           <> maybe [disabled_] (\f' -> [onInput $ f' . fromMaybe x . readMs]) f
                     , div_ [class_ "slider-value"] [text $ ms $ showFFloat (Just 2) x ""]
                     ]
                   PathNode o path -> [viewPathEditor o path]
                   ColourNode c f ->
                     [ input_ $
                         [type_ "color", value_ $ colourHex c]
                           <> maybe [disabled_] (\f' -> [onInput $ f' . fromMaybe c . colourFromHex . fromMisoString]) f
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
                                   PrimDouble x -> showMs x
                                 StringNode s _ -> showStringLiteral s
                                 ConNode{name} -> ms $ unName name
                                 VarNode{name} -> ms $ unName name
                         ]
                     ]
               )
               <> foldMap'
                 ( \(label, action) ->
                     [ button_
                         [ class_ "projection-toggle"
                         , onClickWithOptions stopPropagation action
                         ]
                         [text label]
                     ]
                 )
                 node.projectionToggle
         ]

-- | Show a string literal, with quotes, but escaping only those characters which would otherwise be confusing.
showStringLiteral :: Text -> MisoString
showStringLiteral s = ms $ "\"" <> T.concatMap escape s <> "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      c -> T.singleton c

-- | Show a colour in the form used by HTML colour inputs, e.g. @#ff8000@. Components are clamped to the range 0 to 1.
colourHex :: Colour -> MisoString
colourHex (RGB r g b) = ms $ '#' : foldMap' hexComponent ([r, g, b] :: [Double])
  where
    hexComponent x =
      let s = showHex @Int (round $ 255 * max 0 (min 1 x)) ""
       in replicate (2 - length s) '0' <> s

-- | Parse a colour from the form used by HTML colour inputs, e.g. @#ff8000@.
colourFromHex :: Text -> Maybe Colour
colourFromHex t = case T.unpack t of
  ['#', r1, r2, g1, g2, b1, b2] -> RGB <$> hexComponent [r1, r2] <*> hexComponent [g1, g2] <*> hexComponent [b1, b2]
  _ -> Nothing
  where
    hexComponent s = case readHex s of
      [(n, "")] -> Just $ fromInteger n / 255
      _ -> Nothing

-- | What we need for editing through projections.
data ProjectionEditing action = ProjectionEditing
  { replace :: ID -> Literal -> Projection -> action
  -- ^ Replace the node with the given ID with a literal, through the given projection.
  , dragging :: Maybe (ID, Int)
  -- ^ The point being dragged in a path editor, if any.
  , drag :: Maybe (ID, Int) -> action
  -- ^ Start (or with `Nothing`, stop) dragging a point in a path editor.
  }

viewTreeExpr ::
  (Data t, Data b, Data c) =>
  -- | The type of a node, if it is known reliably, i.e. not from a possibly-stale type cache.
  (t -> Maybe (Type' () ())) ->
  -- | Set the projection of the node with the given ID.
  (ID -> Maybe Projection -> action) ->
  -- | `Nothing` if editing isn't possible, e.g. for a non-editable definition.
  Maybe (ProjectionEditing action) ->
  (TermMeta' (Meta (ExprInfo t)) b c -> (Maybe ID, Maybe action, NodeHighlight)) ->
  Expr' (Meta (ExprInfo t)) b c ->
  Tree.Tree (NodeViewData action)
viewTreeExpr knownType setProjection' edit mkMeta e =
  Tree.Node
    (uncurry3 NodeViewData (mkMeta $ Left meta) projectionToggle Below Expr nodeView)
    childViews
  where
    meta = e ^. _exprMetaLens
    available = availableProjection (knownType (meta ^. _type).typeInfo) e
    -- We check that the term actually has the projection, rather than just trusting the metadata,
    -- since the latter may have been carried over from before an edit (see `syncDefs`).
    -- We use the projection from the metadata, since it may have parameters, e.g. the range of a slider.
    projected = case (available, (meta ^. _type).projection) of
      (Just (p, v), Just p') | sameKind p p' -> Just (p', v)
      _ -> Nothing
    projectionToggle =
      available <&> \(p, _) -> case projected of
        Just _ -> ("🌲", setProjection' (getID meta) Nothing)
        Nothing ->
          ( case p of
              StringProjection -> "\"…\""
              PictureProjection -> "🖼"
              SliderProjection{} -> "🎚"
              ColourProjection -> "🎨"
              PathProjection{} -> "✎"
          , setProjection' (getID meta) (Just p)
          )
    -- Replace the term with the literal produced from some input.
    editor p toLiteral = edit <&> \ed -> \x -> ed.replace (getID meta) (toLiteral x) p
    nodeView = case projected of
      Just (p, StringView s) -> StringNode s $ editor p stringLiteral
      Just (_, PictureView pic) -> uncurry PictureNode $ renderPicture pic
      Just (p@(SliderProjection lo hi), NumberView x) -> SliderNode lo hi x $ editor p $ LitPrim . PrimDouble
      Just (p, ColourView c) -> ColourNode c $ editor p colourLiteral
      Just (p@(PathProjection l b r t), PathView path) ->
        flip
          PathNode
          path
          PathEditorOpts
            { bounds = (V2 l b, V2 r t)
            , edit =
                edit <&> \ed ->
                  EditPath
                    { dragging = do
                        (i, k) <- ed.dragging
                        guard (i == getID meta) $> k
                    , drag = ed.drag . fmap (getID meta,)
                    , setPath = \path' -> ed.replace (getID meta) (pathLiteral path') p
                    }
            }
      _ -> nodeViewTree
    nodeViewTree = case e of
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
    childViews = if isJust projected then [] else childViewsTree
    childViewsTree = case e of
      -- The scrutinee goes below, and the branches form a chain off to the right.
      Case _ scrut branches fb ->
        viewTreeExpr' scrut
          : maybeToList
            ( foldr
                ( \(pat, r) next ->
                    Just
                      . Tree.Node (NodeViewData Nothing Nothing NoHighlight Nothing Beside Expr $ PatternBoxNode pat)
                      $ viewTreeExpr' r : maybeToList next
                )
                Nothing
                $ ( branches <&> \(CaseBranch p bindings r) ->
                      ( Just
                          . viewTree
                          . Tree.Node
                            ( NodeViewData Nothing Nothing NoHighlight Nothing Below Expr case p of
                                PatCon c -> ConNode{name = baseName c, scope = qualifiedModule c}
                                PatPrim c -> PrimNode c
                            )
                          $ bindings <&> \(Bind m v) ->
                            Tree.Node
                              (uncurry3 NodeViewData (mkMeta $ Left m) Nothing Below Expr VarNode{name = unLocalName v, mscope = Nothing})
                              []
                      , r
                      )
                  )
                  <> case fb of
                    CaseExhaustive -> []
                    CaseFallback r -> [(Nothing, r)]
            )
      _ ->
        mconcat
          [ map (viewTreeBinding Type) (e ^.. typeBindingsInExpr)
          , map (viewTreeBinding Expr) (e ^.. bindingsInExpr)
          , map (viewTreeType (mkMeta . Right)) (e ^.. typesInExpr)
          , map viewTreeExpr' (children e)
          ]
        where
          viewTreeBinding l name = Tree.Node (NodeViewData Nothing Nothing NoHighlight Nothing Beside l VarNode{name = unLocalName name, mscope = Nothing}) []
    viewTreeExpr' = viewTreeExpr knownType setProjection' edit mkMeta

viewTreeType ::
  (Data b, Data c) =>
  (Either b c -> (Maybe ID, Maybe action, NodeHighlight)) ->
  Type' b c ->
  Tree.Tree (NodeViewData action)
viewTreeType mkMeta t =
  Tree.Node
    (uncurry3 NodeViewData (mkMeta $ Left $ t ^. _typeMetaLens) Nothing Below Type nodeView)
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
        (\name -> Tree.Node (NodeViewData Nothing Nothing NoHighlight Nothing Beside Type VarNode{name, mscope = Nothing}) [])
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
    (uncurry3 NodeViewData (mkMeta $ k ^. _kindMetaLens) Nothing Below Kind nodeView)
    childViews
  where
    nodeView = case k of
      KHole{} -> HoleNode{empty = True}
      KType{} -> SyntaxNode False "kind-type" "*"
      KFun{} -> SyntaxNode False "kind-fun" "→"
    childViews = map (viewTreeKind mkMeta) (children k)

viewEdge :: V2 Double -> View () () Model action
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

viewTree :: Tree (NodeViewData action) -> (View () () Model action, V2 Double)
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
      tidyLayout @Double
        TidyOpts
          { horizontalGap = nodePadding
          , verticalGap = nodePadding
          , layering = NonLayered
          , symmetric = True
          , nodeSize = snd
          , placement = (.placement) . fst
          }
        $ map (\opts -> (opts, getDimensions opts.opts)) t
    getDimensions = \case
      PatternBoxNode (Just (_, v)) -> v + pure boxPadding
      PatternBoxNode Nothing -> basicDimsSquare + pure boxPadding
      SyntaxNode{wide = False} -> basicDimsSquare
      HoleNode{} -> basicDimsSquare
      StringNode s _ ->
        -- A rough estimate, assuming an average character width. Text which overflows is truncated with an ellipsis anyway.
        basicDims & lensVL _x %~ max (fromIntegral (T.length s + 2) * 10 + 30)
      PictureNode _ v -> v
      SliderNode{} -> V2 200 50
      PathNode{} -> pathEditorSize + 20
      _ -> basicDims
      where
        basicDimsSquare = basicDims & lensVL _x .~ basicDims.y
    -- TODO make these configurable
    nodePadding = 20
    boxPadding = 55
    basicDims = V2 80 35
    showIDs = False
