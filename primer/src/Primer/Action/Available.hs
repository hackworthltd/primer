{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
  OfferedAction (..),
  ActionType (..),
  FunctionFiltering (..),
  ActionInput (..),
  InputAction (..),
  NoInputAction (..),
  ActionName (..),
  Level (..),
  ActionRequest (..),
  noInputAction,
  inputAction,
  mkAction,
) where

import Foreword

import Data.Aeson.Types (ToJSON (toJSON))
import Data.Data (Data)
import Data.Kind qualified as Kind
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics (
  to,
  view,
  (%),
  (^.),
  (^?),
  _Just,
 )
import Primer.Action (
  Action (..),
  Level (..),
  Movement (..),
  ProgAction (..),
  nameString,
  uniquifyDefName,
 )
import Primer.Action.Actions (QualifiedText)
import Primer.Action.Priorities qualified as P
import Primer.App (Editable (Editable, NonEditable), NodeType (..), globalInUse)
import Primer.Core (
  Bind' (..),
  Expr,
  Expr' (..),
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind,
  LVarName,
  Meta (..),
  TmVarRef (..),
  Type,
  Type' (..),
  TypeCache (..),
  getID,
  unLocalName,
  unsafeMkLocalName,
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _id,
  _synthed,
  _type,
  _typeMetaLens,
 )
import Primer.Core.Transform (unfoldFun)
import Primer.Core.Utils (forgetTypeMetadata, freeVars)
import Primer.Def (
  ASTDef (..),
  Def,
  DefMap,
  defAST,
 )
import Primer.JSON (CustomJSON (..), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name, unName)
import Primer.Questions (Question (..))
import Primer.TypeDef (
  TypeDef (TypeDefAST),
  TypeDefMap,
 )
import Primer.Typecheck (
  TypeDefError (TDIHoleType),
  TypeDefInfo (TypeDefInfo),
  getTypeDefInfo',
 )
import Primer.Zipper (
  BindLoc' (BindCase),
  Loc' (InBind, InExpr, InType),
  caseBindZFocus,
  focusOn,
  focusOnTy,
  target,
  unfocusCaseBind,
  unfocusType,
  up,
 )

-- TODO rename?
-- TODO remove most fields, just mapping on frontend instead? perhaps use (Priority, AvailableAction) here, and sort and strip priority before passing to frontend
data OfferedAction = OfferedAction
  { name :: ActionName
  , description :: Text
  , priority :: Int
  , actionType :: ActionType
  , input :: ActionInput
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON OfferedAction

-- TODO split `InputRequired` up - `InputAction` implies the `Bool` and whether the `[Text]` is empty
-- TODO rip out questions - kind of meaningless now that this all goes on in backend
-- TODO move to `Primer.Action`? incl. child types
data ActionInput -- TODO rename to `OfferedAction`? also, rename incl. constructors and subtypes
  = NoInputRequired
  | InputRequired
      -- Text -- TODO newtype `Prompt`? or just put this on the frontend, like I probably will for `description` field etc.
      -- Bool -- TODO this is whether we provide a list to choose from - I haven't decided whether that will be a separate API call, or if we send them all upfront (see `[InputOption]` below)
      [InputOption] -- options - newtype? distinguish "this sort of action doesn't come with options" from e.g. "no variables in scope"?
      -- [InputOption]
      Bool -- TODO Bool is "allow free text?" - use newtype? also, bear in mind frontend will need to keep asking whether name is valid (or we always send it set of invalid names (clashes, rude words etc.))
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON ActionInput

type InputOption = Text -- TODO this may need to become more structured

-- data InputType = Input | NoInput
-- type NoInputAction' = OfferedAction0 NoInput
-- type InputAction' = OfferedAction0 Input
-- data OfferedAction0 (k :: InputType) where
--   C1 :: OfferedAction0 NoInput
-- deriving instance Generic (OfferedAction0 k)

-- TODO more constructors
-- TODO reorder constructors logically
-- TODO rename constructors - descriptive names, also drop the prefix and we'll always qualify
-- data SomeAction
data NoInputAction
  = AMakeCase
  | AConvertLetToLetrec
  | AConstructApp
  | AConstructAPP
  | AConstructAnn
  | ARemoveAnn
  | AFinishHole
  | AEnterHole
  | AConstructFun
  | AAddInput
  | AConstructTypeApp
  | ADuplicateDef
  | ARaise
  | ARaiseType
  | ADeleteDef
  | ADeleteExpr
  | ADeleteType
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON NoInputAction
data InputAction
  = AMakeLambda
  | AUseVar
  | AUseValueCon
  | AUseSaturatedValueCon
  | ASaturatedFunction
  | AMakeLet
  | AMakeLetRec
  | AConstructBigLambda
  | AUseTypeVar
  | AUseTypeCon
  | AConstructForall
  | ARenameDef
  | ARenamePatternVar
  | ARenameLambda
  | ARenameLAM
  | ARenameLetBinding
  | ARenameForall
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via PrimerJSON InputAction

-- TODO use a GADT for actions, so this becomes `(Action a, a)`? awkward for JSON instances - in theory I don't see any reason we can't derive Generic when GADTs are only used for phantom types, but the closest discussion I've found is this (and its linked issues): https://gitlab.haskell.org/ghc/ghc/-/issues/8560
-- TODO GADT for actions would also be nice as we could tag by `Expr`/`Sig`/`Def` (EDIT: actually this gets complex because they're not mutually exclusive e.g. all sig actions can also appear in bodies)
-- TODO ditch the type and make these separate API calls / functions
data ActionRequest
  = ActionRequestSimple
      NoInputAction
  | ActionRequestComplex
      InputAction
      Text -- TODO or number from list? would that require backend to remember some state?

-- We will probably add more constructors in future.
data ActionType
  = Primary
  | Destructive
  deriving (Show, Bounded, Enum, Generic)
  deriving (ToJSON, FromJSON) via (PrimerJSON ActionType)

-- | Filter on variables and constructors according to whether they
-- have a function type.
data FunctionFiltering
  = Everything
  | OnlyFunctions
  | NoFunctions

-- | Some actions' names are meant to be rendered as code, others as
-- prose.
data ActionName
  = Code Text
  | Prose Text
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (PrimerJSON ActionName)

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
-- This is probably useful elsewhere, but we currently just need it here
data SomeNode a b
  = ExprNode (Expr' (Meta a) (Meta b))
  | TypeNode (Type' (Meta b))
  | -- | If/when we model all bindings with 'Bind'', we will want to generalise this.
    CaseBindNode (Bind' (Meta a))

actionsForDef ::
  Level ->
  -- | All existing definitions, along with their mutability
  Map GVarName (Editable, Def) ->
  -- | The name of a definition in the map
  GVarName ->
  [Either NoInputAction InputAction]
actionsForDef l defs defName = catMaybes [rename, duplicate, delete]
  where
    rename = do
      _ <- getEditableASTDef defs defName
      pure $ Right ARenameDef

    duplicate = do
      _ <- getEditableASTDef defs defName
      pure $ Left ADuplicateDef

    delete = do
      -- Ensure it is not in use, otherwise the action will not succeed
      _ <- getEditableDef defs defName
      guard $ not $ globalInUse defName $ Map.delete defName $ fmap snd defs
      pure $ Left ADeleteDef

-- | Given the body of a Def and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefBody ::
  TypeDefMap ->
  Level ->
  GVarName ->
  Editable ->
  ID ->
  Expr ->
  [Either NoInputAction InputAction]
actionsForDefBody _ _ _ NonEditable _ _ = mempty
actionsForDefBody tydefs l defName mut@Editable id expr =
  let raiseAction' = Left ARaise
   in case findNodeWithParent id expr of
        Nothing -> mempty
        Just (ExprNode e, p) ->
          let raiseAction = case p of
                Nothing -> [] -- at root already, cannot raise
                Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
                _ -> [raiseAction']
           in basicActionsForExpr tydefs l defName e <> raiseAction
        Just (TypeNode t, p) ->
          let raiseAction = case p of
                Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
                _ -> [raiseAction']
           in (basicActionsForType l defName t <> compoundActionsForType l t)
                <> raiseAction
        Just (CaseBindNode b, _) -> actionsForBinding l defName mut b

-- | Given a the type signature of a Def and the ID of a node in it,
-- return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  GVarName ->
  Editable ->
  ID ->
  Type ->
  [Either NoInputAction InputAction]
actionsForDefSig _ _ NonEditable _ _ = mempty
actionsForDefSig l defName Editable id ty =
  let raiseAction =
        [ Left ARaiseType
        | id /= getID ty
        ]
   in case findType id ty of
        Nothing -> mempty
        Just t ->
          basicActionsForType l defName t
            <> compoundActionsForType l t
            <> raiseAction

-- | Bindings support just one action: renaming.
actionsForBinding ::
  Level ->
  GVarName ->
  Editable ->
  Bind' (Meta (Maybe TypeCache)) ->
  [Either NoInputAction InputAction]
actionsForBinding _ _ NonEditable _ = mempty
actionsForBinding l defName Editable b =
  [ Right ARenamePatternVar
  ]

-- | Find a node in the AST by its ID, and also return its parent
findNodeWithParent ::
  (Data a, Data b, Eq a) =>
  ID ->
  Expr' (Meta a) (Meta b) ->
  Maybe (SomeNode a b, Maybe (SomeNode a b))
findNodeWithParent id x = do
  z <- focusOn id x
  Just $ case z of
    InExpr ez -> (ExprNode $ target ez, ExprNode . target <$> up ez)
    InType tz ->
      ( TypeNode $ target tz
      , Just $
          maybe
            (ExprNode $ target $ unfocusType tz)
            (TypeNode . target)
            (up tz)
      )
    InBind (BindCase bz) -> (CaseBindNode $ caseBindZFocus bz, Just . ExprNode . target . unfocusCaseBind $ bz)

-- | Find a sub-type in a larger type by its ID.
findType :: forall b. Data b => ID -> Type' (Meta b) -> Maybe (Type' (Meta b))
findType id ty = target <$> focusOnTy id ty

-- | Given an expression, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForExpr :: TypeDefMap -> Level -> GVarName -> Expr -> [Either NoInputAction InputAction]
basicActionsForExpr tydefs l defName expr = case expr of
  EmptyHole{} -> universalActions <> emptyHoleActions
  Hole{} -> defaultActions <> holeActions
  Ann{} -> defaultActions <> annotationActions
  Lam{} -> defaultActions <> lambdaActions
  LAM{} -> defaultActions <> bigLambdaActions
  Let _ v e _ -> defaultActions <> letActions v e
  Letrec _ _ _ t _ -> defaultActions <> letRecActions (Just t)
  e -> let _ = e ^. _exprMetaLens in defaultActions ++ expert annotateExpression
  where
    m = expr ^. _exprMetaLens

    insertVariable =
      let filterVars = case l of
            Beginner -> NoFunctions
            _ -> Everything
       in Right AUseVar

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    insertVariableSaturatedRefined = Right ASaturatedFunction
    annotateExpression = Left AConstructAnn
    applyFunction = Left AConstructApp
    applyType = Left AConstructAPP
    patternMatch = Left AMakeCase
    makeLambda = Right AMakeLambda
    makeTypeAbstraction = Right AConstructBigLambda
    useValueConstructor = Right AUseValueCon
    useSaturatedRefinedValueConstructor = Right AUseSaturatedValueCon
    makeLetBinding = Right AMakeLet
    makeLetrec = Right AMakeLetRec
    enterHole = Left AEnterHole
    finishHole = Left AFinishHole
    removeAnnotation = Left ARemoveAnn
    renameVariable = Right ARenameLambda
    renameTypeVariable = Right ARenameLAM
    makeLetRecursive = Left AConvertLetToLetrec
    renameLet t = Right ARenameLetBinding
    deleteExpr = Left ADeleteExpr
    expert = if l == Expert then (: []) else const []
    emptyHoleActions = case l of
      Beginner ->
        [ insertVariable
        , useValueConstructor
        ]
      _ ->
        [ insertVariable
        , insertVariableSaturatedRefined
        , useValueConstructor
        , useSaturatedRefinedValueConstructor
        , makeLetBinding
        , makeLetrec
        , enterHole
        ]
          ++ expert annotateExpression
    holeActions = finishHole : expert annotateExpression
    annotationActions = expert removeAnnotation
    lambdaActions = renameVariable : expert annotateExpression
    bigLambdaActions = concatMap expert [annotateExpression, renameTypeVariable]
    letActions v e =
      renameLet (e ^? _exprMetaLens % _type % _Just % _synthed)
        : munless (unLocalName v `Set.member` freeVars e) [makeLetRecursive]
          <> expert annotateExpression
    letRecActions t = renameLet t : expert annotateExpression

    -- Actions for every expression node
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    universalActions =
      let both = case l of
            Beginner ->
              [ makeLambda
              ]
            Intermediate ->
              [ makeLambda
              , applyFunction
              ]
            Expert ->
              [ applyFunction
              , applyType
              , makeLambda
              , makeTypeAbstraction
              ]
          synthTy = m ^? _type % _Just % _synthed
          synOnly ty = case getTypeDefInfo' tydefs ty of
            Left TDIHoleType{} -> Just patternMatch
            Right (TypeDefInfo _ _ TypeDefAST{}) -> Just patternMatch
            _ -> Nothing
       in (synOnly =<< synthTy) ?: both

    -- Extract the source of the function type we were checked at
    -- i.e. the type that a lambda-bound variable would have here
    lamVarTy = \case
      TFun _ s _ -> pure s
      _ -> Nothing

    -- Extract the kind a forall-bound variable would have here
    lAMVarKind = \case
      TForall _ _ k _ -> Just k
      _ -> Nothing

    -- Actions for every expression node except holes and annotations
    defaultActions = universalActions <> [deleteExpr]

(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x : xs
Nothing ?: xs = xs
infixr 5 ?:

-- | Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> GVarName -> Type -> [Either NoInputAction InputAction]
basicActionsForType l defName ty = case ty of
  TEmptyHole _ -> universalActions <> emptyHoleActions
  TForall _ _ k _ -> defaultActions <> forAllActions k
  _ -> defaultActions
  where
    m = ty ^. _typeMetaLens
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    constructFunctionType = Left AConstructFun
    constructPolymorphicType = Right AConstructForall
    constructTypeApplication = Left AConstructAPP
    useTypeConstructor = Right AUseTypeCon
    useTypeVariable = Right AUseTypeVar
    renameTypeVariable k m' = Right ARenameForall
    deleteType = Left ADeleteType
    emptyHoleActions = [useTypeConstructor] <> expert useTypeVariable
    forAllActions k = expert $ renameTypeVariable k m
    universalActions =
      -- Actions for every type node
      [constructFunctionType]
        <> concatMap
          expert
          [ constructPolymorphicType
          , constructTypeApplication
          ]
    defaultActions = universalActions <> [deleteType] -- Actions for every type node except empty holes
    expert = if l == Expert then (: []) else const []

-- | These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: Level -> Type' (Meta a) -> [Either NoInputAction InputAction]
compoundActionsForType l ty = case ty of
  TFun _m a b -> [addFunctionArgument a b]
  _ -> []
  where
    -- This action traverses the function type and adds a function arrow to the end of it,
    -- resulting in a new argument type. The result type is unchanged.
    -- The cursor location is also unchanged.
    -- e.g. A -> B -> C ==> A -> B -> ? -> C
    addFunctionArgument a b = Left AAddInput

-- do we stull want/need this?
-- priorityNoInputAction :: NoInputAction -> Level -> Int
-- priorityNoInputAction = \case
--   AMakeCase -> P.makeCase
--   AConvertLetToLetrec -> P.makeLetRecursive
--   AConstructApp -> P.applyFunction
--   AConstructAPP -> P.applyType
--   AConstructAnn -> P.annotateExpr
--   ARemoveAnn -> P.removeAnnotation
--   AFinishHole -> P.finishHole
--   AEnterHole -> P.enterHole
--   AConstructFun -> P.constructFunction
--   AAddInput -> P.addInput
--   AConstructTypeApp -> P.constructTypeApp
--   ADuplicateDef -> P.duplicate
--   ARaise -> P.raise
--   ARaiseType -> P.raise
--   ADeleteDef -> P.delete
--   ADeleteExpr -> P.delete
--   ADeleteType -> P.delete
-- priorityInputAction :: InputAction -> Level -> Int
-- priorityInputAction = \case
--   AMakeLambda -> P.makeLambda
--   AUseVar -> P.useVar
--   AUseValueCon -> P.useValueCon
--   AUseSaturatedValueCon -> P.useSaturatedValueCon
--   ASaturatedFunction -> P.useFunction
--   AMakeLet -> P.makeLet
--   AMakeLetRec -> P.makeLetrec
--   AConstructBigLambda -> P.makeTypeAbstraction
--   AUseTypeVar -> P.useTypeVar
--   AUseTypeCon -> P.useTypeCon
--   AConstructForall -> P.constructForall
--   ARenameDef -> P.rename
--   ARenamePatternVar -> P.rename
--   ARenameLambda -> P.rename
--   ARenameLAM -> P.rename
--   ARenameLetBinding -> P.rename
--   ARenameForall -> P.rename

-- getInput :: Either NoInputAction InputAction -> (OfferedAction, [ProgAction])
-- getInput :: Level -> Either NoInputAction InputAction -> (OfferedAction, [ProgAction])
-- TODO obviously don't use `Text` for arrows
-- TODO but can we avoid errors here completely by shifting more responsibility to `ProgAction`
-- TODO fewer args?
-- inputAction ::
--   Map GVarName (a, Def) ->
--   ASTDef ->
--   GVarName ->
--   ID ->
--   Level ->
--   Either Expr Type ->
--   Either NoInputAction InputAction ->
--   (OfferedAction, Either Text [ProgAction])
-- TODO these two will become fully-frontend
noInputAction l = \case
  AMakeCase ->
    OfferedAction
      { name = Code "m"
      , description = case l of
          Beginner -> "Match a variable with its value"
          Intermediate -> "Match a variable with its value"
          Expert -> "Pattern match"
      , input = NoInputRequired
      , priority = P.makeCase l
      , actionType = Destructive
      }
  AConvertLetToLetrec ->
    OfferedAction
      { name = Prose "rec"
      , description = "Make this let recursive"
      , input = NoInputRequired
      , priority = P.makeLetRecursive l
      , actionType = Primary
      }
  AConstructApp ->
    OfferedAction
      { name = Code "$"
      , description = "Apply function"
      , input = NoInputRequired
      , priority = P.applyFunction l
      , actionType = Primary
      }
  AConstructAPP ->
    OfferedAction
      { name = Code "@"
      , description = "Apply type"
      , input = NoInputRequired
      , priority = P.applyType l
      , actionType = Destructive
      }
  AConstructAnn ->
    OfferedAction
      { name = Code ":"
      , description = "Annotate this expression with a type"
      , input = NoInputRequired
      , priority = P.annotateExpr l
      , actionType = Primary
      }
  ARemoveAnn ->
    OfferedAction
      { name = Prose "⌫:"
      , description = "Remove this annotation"
      , input = NoInputRequired
      , priority = P.removeAnnotation l
      , actionType = Destructive
      }
  AFinishHole ->
    OfferedAction
      { name = Prose "e"
      , description = "Convert this into a normal expression"
      , input = NoInputRequired
      , priority = P.finishHole l
      , actionType = Primary
      }
  AEnterHole ->
    OfferedAction
      { name = Prose "h"
      , description = "Make this hole into a non-empty hole"
      , input = NoInputRequired
      , priority = P.enterHole l
      , actionType = Primary
      }
  AConstructFun ->
    OfferedAction
      { name = Code "→"
      , description = "Construct a function type"
      , input = NoInputRequired
      , priority = P.constructFunction l
      , actionType = Primary
      }
  AAddInput ->
    OfferedAction
      { name = Code "→A→"
      , description = "Add an input to this function"
      , input = NoInputRequired
      , priority = P.addInput l
      , actionType = Primary
      }
  AConstructTypeApp ->
    OfferedAction
      { name = Code "$"
      , description = "Construct a type application"
      , input = NoInputRequired
      , priority = P.constructTypeApp l
      , actionType = Primary
      }
  ADuplicateDef ->
    OfferedAction
      { name = Prose "d"
      , description = "Duplicate this definition"
      , input = NoInputRequired
      , priority = P.duplicate l
      , actionType = Primary
      }
  ARaise ->
    OfferedAction
      { name = Prose "↑"
      , description = "Replace parent with this subtree"
      , input = NoInputRequired
      , priority = P.raise l
      , actionType = Destructive
      }
  ARaiseType ->
    OfferedAction
      { name = Prose "↑"
      , description = "Replace parent with this subtree"
      , input = NoInputRequired
      , priority = P.raise l
      , actionType = Destructive
      }
  ADeleteDef ->
    OfferedAction
      { name = Prose "⌫"
      , description = "Delete this definition"
      , input = NoInputRequired
      , priority = P.delete l
      , actionType = Destructive
      }
  ADeleteExpr ->
    OfferedAction
      { name = Prose "⌫"
      , description = "Delete this expression"
      , input = NoInputRequired
      , priority = P.delete l
      , actionType = Destructive
      }
  ADeleteType ->
    OfferedAction
      { name = Prose "⌫"
      , description = "Delete this type"
      , input = NoInputRequired
      , priority = P.delete l
      , actionType = Destructive
      }
inputAction l = \case
  action ->
    case action of
      AMakeLambda ->
        OfferedAction
          { name = Code "λx"
          , description = "Make a function with an input"
          , input =
              -- AskQuestion
              -- (GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)) $ \options ->
              --   InputRequired
              --     $ ChooseOrEnterName
              --       ("Choose a " <> nameString <> " for the input variable")
              --       options
              --     $ \n ->
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.makeLambda l
          , actionType = Primary
          }
      AUseVar ->
        OfferedAction
          { name = Code "x"
          , description = "Use a variable"
          , input =
              -- ChooseVariable filterVars $
              --   pure . ConstructVar
              InputRequired
                [] -- TODO note 1
                False
          , priority = P.useVar l
          , actionType = Primary
          }
      AUseValueCon ->
        let filterCtors = case l of
              Beginner -> NoFunctions
              _ -> Everything
         in OfferedAction
              { name = Code "V"
              , description = "Use a value constructor"
              , input =
                  -- ChooseConstructor filterCtors $
                  --   \c ->
                  InputRequired
                    [] -- TODO note 1
                    False
              , priority = P.useValueCon l
              , actionType = Primary
              }
      AUseSaturatedValueCon ->
        -- NB: Exactly one of the saturated and refined actions will be available
        -- (depending on whether we have useful type information to hand).
        -- We put the same labels on each.
        OfferedAction
          { name = Code "V $ ?"
          , description = "Apply a value constructor to arguments"
          , input =
              -- . ChooseConstructor OnlyFunctions
              --  $ \c ->
              InputRequired
                [] -- TODO note 1
                False
          , priority = P.useSaturatedValueCon l
          , actionType = Primary
          }
      ASaturatedFunction ->
        OfferedAction
          { name = Code "f $ ?"
          , description = "Apply a function to arguments"
          , input =
              InputRequired
                -- . ChooseVariable OnlyFunctions
                -- \$ \name ->
                [] -- TODO note 1
                False
          , priority = P.useFunction l
          , actionType = Primary
          }
      AMakeLet ->
        OfferedAction
          { name = Code "="
          , description = "Make a let binding"
          , input =
              -- (GenerateName defName (m ^. _id) (Left Nothing))
              --  $ \options ->
              --   InputRequired
              --     . ChooseOrEnterName
              --       ("Choose a " <> nameString <> " for the new let binding")
              --       options
              --     $ \n -> [ConstructLet $ Just $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.makeLet l
          , actionType = Primary
          }
      AMakeLetRec ->
        OfferedAction
          { name = Code "=,="
          , description = "Make a recursive let binding"
          , input =
              -- (GenerateName defName (m ^. _id) (Left Nothing)) $ \options ->
              -- InputRequired
              --   . ChooseOrEnterName
              --     ("Choose a " <> nameString <> " for the new let binding")
              --     options
              --   $ \n -> [ConstructLetrec $ Just $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.makeLetrec l
          , actionType = Primary
          }
      AConstructBigLambda ->
        OfferedAction
          { name = Code "Λx"
          , description = "Make a type abstraction"
          , input =
              -- AskQuestion
              -- (GenerateName defName (m ^. _id) (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind)) $ \options ->
              --   InputRequired
              --     $ ChooseOrEnterName
              --       ("Choose a " <> nameString <> " for the bound type variable")
              --       options
              --     $ \n -> [ConstructLAM $ Just $ unName n]
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.makeTypeAbstraction l
          , actionType = Primary
          }
      AUseTypeVar ->
        OfferedAction
          { name = Code "t"
          , description = "Use a type variable"
          , input =
              -- ChooseTypeVariable $
              --   \v -> [ConstructTVar v]
              InputRequired
                [] -- TODO note 1
                False
          , priority = P.useTypeVar l
          , actionType = Primary
          }
      AUseTypeCon ->
        OfferedAction
          { name = Code "T"
          , description = "Use a type constructor"
          , input =
              -- ChooseTypeConstructor $
              --   \t -> [ConstructTCon t]
              InputRequired
                [] -- TODO note 1
                False
          , priority = P.useTypeCon l
          , actionType = Primary
          }
      AConstructForall ->
        OfferedAction
          { name = Code "∀"
          , description = "Construct a polymorphic type"
          , input =
              --  (GenerateName defName (m ^. _id) (Right Nothing)) $ \options ->
              -- InputRequired
              --   . ChooseOrEnterName
              --     ("Choose a " <> nameString <> " for the bound type variable")
              --     options
              --   $ \n -> [ConstructTForall $ Just $ unName n, Move Child1]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.constructForall l
          , actionType = Primary
          }
      ARenameDef ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this definition"
          , input =
              -- ("Enter a new " <> nameString <> " for the definition")
              -- (\name -> [RenameDef defName (unName name)])
              InputRequired
                []
                True
          , priority = P.rename l
          , actionType = Primary
          }
      ARenamePatternVar ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this pattern variable"
          , input =
              -- AskQuestion
              -- (GenerateName defName (b ^. _bindMeta % _id) (Left $ b ^? _bindMeta % _type % _Just % _chkedAt))
              --  $ \options ->
              --   InputRequired
              --     $ ChooseOrEnterName
              --       ("Choose a new " <> nameString <> " for the pattern variable")
              --       options
              --     $ \n -> [RenameCaseBinding $ unName n]
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.rename l
          , actionType = Primary
          }
      ARenameLambda ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this input variable"
          , input =
              -- (GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy))
              --   $ \options ->
              --   InputRequired
              --     . ChooseOrEnterName
              --       ("Choose a new " <> nameString <> " for the input variable")
              --       options
              --     $ \n -> [RenameLam $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.rename l
          , actionType = Primary
          }
      ARenameLAM ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this type variable"
          , input =
              -- (GenerateName defName (m ^. _id) (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind))
              --  $ \options ->
              --   InputRequired
              --     . ChooseOrEnterName
              --       ("Choose a new " <> nameString <> " for the type variable")
              --       options
              --     $ \n -> [RenameLAM $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.rename l
          , actionType = Primary
          }
      ARenameLetBinding ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this let binding"
          , input =
              -- (GenerateName defName (m ^. _id) (Left $ forgetTypeMetadata <$> t))
              --   $ \options ->
              --   InputRequired
              --     . ChooseOrEnterName
              --       ("Choose a new " <> nameString <> " for the let binding")
              --       options
              --     $ \n -> [RenameLet $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.rename l
          , actionType = Primary
          }
      ARenameForall ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this type variable"
          , input =
              --  (GenerateName defName (m' ^. _id) (Right $ Just k)) $ \options ->
              -- InputRequired
              --   $ ChooseOrEnterName
              --     ("Choose a new " <> nameString <> " for the bound type variable")
              --     options
              --   $ \n -> [RenameForall $ unName n]
              -- AskQuestion
              InputRequired
                [] -- TODO note 2
                True
          , priority = P.rename l
          , actionType = Primary
          }

-- TODO rename
-- essentially, map a high-level (and serialisation-friendly) action to a sequence of low-level ones
-- TODO ID is unused for a def action - probably indicates we need to split the action type up
mkAction ::
  DefMap ->
  ASTDef ->
  GVarName ->
  Maybe (NodeType, ID) ->
  ActionRequest ->
  Either Text [ProgAction]
mkAction defs def defName mNodeSel = \case
  ActionRequestSimple AMakeCase ->
    toProgAction [ConstructCase]
  ActionRequestSimple AConvertLetToLetrec ->
    toProgAction [ConvertLetToLetrec]
  ActionRequestSimple AConstructApp ->
    toProgAction [ConstructApp, Move Child2]
  ActionRequestSimple AConstructAPP ->
    toProgAction [ConstructAPP, EnterType]
  ActionRequestSimple AConstructAnn ->
    toProgAction [ConstructAnn]
  ActionRequestSimple ARemoveAnn ->
    toProgAction [RemoveAnn]
  ActionRequestSimple AFinishHole ->
    toProgAction [FinishHole]
  ActionRequestSimple AEnterHole ->
    toProgAction [EnterHole]
  ActionRequestSimple AConstructFun ->
    toProgAction [ConstructArrowL, Move Child1]
  ActionRequestSimple AAddInput -> do
    -- case et of
    --   Right t ->
    --     let unfoldFun' :: Type' a -> (Type' a, [Type' a])
    --         unfoldFun' = undefined

    --         (_resultType, argTypes) = unfoldFun' t

    --         moveToLastArg = replicate (length argTypes) (Move Child2)

    --         moveBack = replicate (length argTypes) (Move Parent)
    --      in toProgAction $ moveToLastArg <> [ConstructArrowR] <> moveBack
    --   _ -> Left "expected TFun"
    -- l <- case et of
    --   Right (TFun _ a b) -> pure $ NE.length $ fst $ unfoldFun a b
    --   _ -> Left "expected TFun"
    -- TODO don't hardcode
    -- it's awkward though - there's a reason this was in its own section before this commit
    let l = 1
    let moveToLastArg = replicate l (Move Child2)
        moveBack = replicate l (Move Parent)
     in toProgAction $ moveToLastArg <> [ConstructArrowR] <> moveBack
  ActionRequestSimple AConstructTypeApp ->
    toProgAction [ConstructTApp, Move Child1]
  ActionRequestSimple ADuplicateDef ->
    let sigID = getID $ astDefType def

        bodyID = getID $ astDefExpr def

        copyName = uniquifyDefName (qualifiedModule defName) (unName (baseName defName) <> "Copy") defs
     in pure
          [ CreateDef (qualifiedModule defName) (Just copyName)
          , CopyPasteSig (defName, sigID) []
          , CopyPasteBody (defName, bodyID) []
          ]
  ActionRequestSimple ARaise -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
  ActionRequestSimple ARaiseType -> do
    id <- id'
    pure [MoveToDef defName, CopyPasteSig (defName, id) [SetCursor id, Move Parent, Delete]]
  ActionRequestSimple ADeleteDef ->
    pure [DeleteDef defName]
  ActionRequestSimple ADeleteExpr ->
    toProgAction [Delete]
  ActionRequestSimple ADeleteType ->
    toProgAction [Delete]
  -- TODO rename `tInput`
  ActionRequestComplex action tInput ->
    let -- TODO should we handle "parsing" in to trusted input here
        -- see the comment on `Action` - given that we're now not exposing that type via the API, it should probably use the rich versions
        -- I think we previously were inconsistent, or just hadn't given this much thought

        -- TODO hmm this needn't necessarily be local
        -- and obviously we shouldn't use `unsafeMkLocalName` etc.
        -- and the hardcoded string for qualified text makes no sense
        -- this needs a big rethink
        -- `ActionRequestComplex` field shouldn't be just `Text` (same as `InputOption`?)?
        tInputTmVar = LocalVarRef $ unsafeMkLocalName tInput
        tInputQT = (pure "Builtins", tInput)
     in case action of
          AMakeLambda ->
            toProgAction [ConstructLam $ Just tInput]
          AUseVar ->
            toProgAction [ConstructVar tInputTmVar]
          AUseValueCon ->
            toProgAction [ConstructCon tInputQT]
          AUseSaturatedValueCon -> do
            oR <- offerRefined
            toProgAction [if oR then ConstructRefinedCon tInputQT else ConstructSaturatedCon tInputQT]
          ASaturatedFunction -> do
            oR <- offerRefined
            toProgAction [if oR then InsertRefinedVar tInputTmVar else InsertSaturatedVar tInputTmVar]
          AMakeLet ->
            toProgAction [ConstructLet $ Just tInput]
          AMakeLetRec ->
            toProgAction [ConstructLetrec $ Just tInput]
          AConstructBigLambda ->
            toProgAction [ConstructLAM $ Just tInput]
          AUseTypeVar ->
            toProgAction [ConstructTVar tInput]
          AUseTypeCon ->
            toProgAction [ConstructTCon tInputQT]
          AConstructForall ->
            toProgAction [ConstructTForall $ Just tInput, Move Child1]
          ARenameDef ->
            pure [RenameDef defName tInput]
          ARenamePatternVar ->
            toProgAction [RenameCaseBinding tInput]
          ARenameLambda ->
            toProgAction [RenameLam tInput]
          ARenameLAM ->
            toProgAction [RenameLAM tInput]
          ARenameLetBinding ->
            toProgAction [RenameLet tInput]
          ARenameForall ->
            toProgAction [RenameForall tInput]
  where
    toProgAction actions = do
      id <- id'
      sigOrBody <- maybeToEither "no node selection" $ fst <$> mNodeSel
      let a = case sigOrBody of
            SigNode -> SigAction
            BodyNode -> BodyAction
      pure [MoveToDef defName, a $ SetCursor id : actions]
    -- If we have a useful type, offer the refine action, otherwise offer the
    -- saturate action.
    -- TODO I don't really understand the difference that `offerRefined` represents
    -- and maybe we should be moving all this to a higher level anyway - the rest of this function has little logic, and we could potentially avoid error handling
    offerRefined = do
      id <- id'
      case findNodeWithParent id $ astDefExpr def of
        Just (ExprNode e, _) -> pure $ case e ^. _exprMetaLens ^? _type % _Just % _chkedAt of
          Just (TEmptyHole _) -> False
          Just (THole _ _) -> False
          Just _ -> True
          _ -> False
        _ -> Left "expected TypeNode"
    id' = maybeToEither "no node selection" $ snd <$> mNodeSel

getEditableDef defs defName = do
  (m, d) <- Map.lookup defName defs
  case m of
    NonEditable -> Nothing
    Editable -> pure d
getEditableASTDef defs defName = defAST =<< getEditableDef defs defName

withExpr et as = case et of
  Left e -> pure $ as e
  Right _ -> Left "expected expression" -- TODO we should really be able to rule this out statically

-- data Loc = LocDef | LocSig | LocBody
-- type family LocContents loc where
--   LocContents LocDef = ()
--   LocContents LocSig = Type
--   LocContents LocBody = Either Expr Type
-- data Act (freeInputAllowed :: Bool) (location :: Loc) (inputData :: Kind.Type)

-- none, choice only (subtypes - text, var, constructor), choice with free text
