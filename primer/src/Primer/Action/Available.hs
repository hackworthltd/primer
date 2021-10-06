{-# LANGUAGE NamedFieldPuns #-}

-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
) where

import Foreword

import qualified Data.List.NonEmpty as NE
import Optics (
  set,
  to,
  (%),
  (^.),
  (^?),
  _Just,
 )
import Primer.Action (
  Action (..),
  ActionInput (..),
  ActionName (..),
  FunctionFiltering (..),
  Level (..),
  Movement (..),
  OfferedAction (..),
  ProgAction (..),
  Question (..),
  UserInput (..),
  nameString,
  uniquifyDefName,
 )
import qualified Primer.Action.Priorities as P
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Def (..),
  Expr,
  Expr' (..),
  ExprMeta,
  ID,
  Kind,
  Meta (..),
  Type,
  Type' (..),
  TypeCache (..),
  _bindMeta,
  _chkedAt,
  _exprMetaLens,
  _id,
  _synthed,
  _type,
  _typeMeta,
  _typeMetaLens,
 )
import Primer.Core.Transform (unfoldFun)
import Primer.Name (unName)

-- | An AST node tagged with its "sort" - i.e. if it's a type or expression or binding etc.
-- This is probably useful elsewhere, but we currently just need it here
data SomeNode a b
  = ExprNode (Expr' (Meta a) (Meta b))
  | TypeNode (Type' (Meta b))
  | -- If/when we model all bindings with 'Bind'', we will want to generalise this.
    CaseBindNode (Bind' (Meta a))

actionsForDef ::
  Level ->
  Map ID Def -> -- only used to generate a unique name for a duplicate definition
  Def ->
  [OfferedAction [ProgAction]]
actionsForDef l defs def =
  [ OfferedAction
      { name = Prose "r"
      , description = "Rename this definition"
      , input =
          InputRequired $
            ChooseOrEnterName
              { prompt = "Enter a new " <> nameString <> " for the definition"
              , options = []
              , choose = (\name -> [RenameDef (defID def) (unName name)])
              }
      , priority = P.rename l
      , destructive = False
      }
  , OfferedAction
      { name = Prose "d"
      , description = "Duplicate this definition"
      , input =
          let sigID = defType def ^. _typeMetaLens % _id

              bodyID = defExpr def ^. _exprMetaLens % _id

              copyName = uniquifyDefName (unName (defName def) <> "Copy") defs
           in NoInputRequired
                [ CreateDef (Just copyName)
                , CopyPasteSig (defID def, sigID) []
                , CopyPasteBody (defID def, bodyID) []
                ]
      , priority = P.duplicate l
      , destructive = False
      }
  , OfferedAction
      { name = Prose "⌫"
      , description = "Delete this definition"
      , input = NoInputRequired [DeleteDef $ defID def]
      , priority = P.delete l
      , destructive = True
      }
  ]

-- Given the body of a Def and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefBody ::
  Level ->
  Def ->
  ID ->
  Expr ->
  [OfferedAction [ProgAction]]
actionsForDefBody l def id expr =
  let toProgAction actions = [MoveToDef (defID def), BodyAction actions]

      raiseAction' =
        OfferedAction
          { name = Prose "↑"
          , description = "Replace parent with this subtree"
          , input = NoInputRequired [MoveToDef (defID def), CopyPasteBody (defID def, id) [SetCursor id, Move Parent, Delete]]
          , priority = P.raise l
          , destructive = True
          }
   in case findNodeWithParent id expr of
        Nothing -> mempty
        Just (ExprNode e, p) ->
          let raiseAction = case p of
                Nothing -> [] -- at root already, cannot raise
                Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
                _ -> [raiseAction']
           in (toProgAction <<$>> basicActionsForExpr l (defID def) e) <> raiseAction
        Just (TypeNode t, p) ->
          let raiseAction = case p of
                Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
                _ -> [raiseAction']
           in ( toProgAction
                  <<$>> (basicActionsForType l (defID def) t <> compoundActionsForType l t)
              )
                <> raiseAction
        Just (CaseBindNode b, _) -> toProgAction <<$>> actionsForBinding l (defID def) b

-- Given a Type and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  Def ->
  ID ->
  Type ->
  [OfferedAction [ProgAction]]
actionsForDefSig l def id ty =
  let toProgAction actions = [MoveToDef (defID def), SigAction actions]

      raiseAction =
        if id == defType def ^. _typeMetaLens % _id
          then [] -- at root already, cannot raise
          else
            [ OfferedAction
                { name = Prose "↑"
                , description = "Replace parent with this subtree"
                , input = NoInputRequired [MoveToDef (defID def), CopyPasteSig (defID def, id) [SetCursor id, Move Parent, Delete]]
                , priority = P.raise l
                , destructive = True
                }
            ]
   in case findType id ty of
        Nothing -> mempty
        Just t ->
          ( toProgAction
              <<$>> (basicActionsForType l (defID def) t <> compoundActionsForType l t)
          )
            <> raiseAction

-- Bindings support just one action: renaming.
actionsForBinding ::
  Level ->
  ID ->
  Bind' (Meta (Maybe TypeCache)) ->
  [OfferedAction [Action]]
actionsForBinding l defId b =
  realise
    b
    (b ^. _bindMeta)
    [ \p m' ->
        OfferedAction
          { name = Prose "r"
          , description = "Rename this pattern variable"
          , input =
              actionWithNames
                defId
                (Left $ b ^? _bindMeta % _type % _Just % _chkedAt)
                (\n -> [RenameCaseBinding n])
                m'
                ("Choose a new " <> nameString <> " for the pattern variable")
          , priority = P.rename l
          , destructive = False
          }
    ]

-- Find a node in the AST by its ID, and also return its parent
findNodeWithParent :: forall a b. ID -> Expr' (Meta a) (Meta b) -> Maybe (SomeNode a b, Maybe (SomeNode a b))
findNodeWithParent id x = go x Nothing
  where
    go expr parent
      | expr ^. _exprMetaLens % _id == id = Just (ExprNode expr, parent)
      | otherwise = case expr of
        Hole _ e -> go e (Just (ExprNode expr))
        EmptyHole _ -> Nothing
        Ann _ e t -> go e (Just (ExprNode expr)) <|> goTy t expr
        App _ a b -> go a (Just (ExprNode expr)) <|> go b (Just (ExprNode expr))
        APP _ a b -> go a (Just (ExprNode expr)) <|> goTy b expr
        Con _ _ -> Nothing
        Lam _ _ e -> go e (Just (ExprNode expr))
        LAM _ _ e -> go e (Just (ExprNode expr))
        Var _ _ -> Nothing
        GlobalVar _ _ -> Nothing
        Let _ _ a b -> go a (Just (ExprNode expr)) <|> go b (Just (ExprNode expr))
        Letrec _ _ a ta b -> go a (Just (ExprNode expr)) <|> goTy ta expr <|> go b (Just (ExprNode expr))
        LetType _ _ t e -> goTy t expr <|> go e (Just (ExprNode expr))
        Case _ e branches ->
          let (Alt inBranches) = flip foldMap branches $
                \(CaseBranch _ binds rhs) ->
                  Alt (go rhs (Just (ExprNode expr)))
                    <> foldMap (Alt . map (\b -> (CaseBindNode b, Just (ExprNode expr))) . findBind id) binds
           in go e (Just (ExprNode expr)) <|> inBranches

    goTy t p = case findTypeWithParent id t of
      Nothing -> Nothing
      Just (t', Nothing) -> Just (TypeNode t', Just (ExprNode p))
      Just (t', Just p') -> Just (TypeNode t', Just (TypeNode p'))

-- Find a sub-type in a larger type by its ID.
findType :: forall b. ID -> Type' (Meta b) -> Maybe (Type' (Meta b))
findType id ty
  | ty ^. _typeMetaLens % _id == id = Just ty
  | otherwise = case ty of
    TEmptyHole _ -> Nothing
    THole _ t -> findType id t
    TCon _ _ -> Nothing
    TVar _ _ -> Nothing
    TFun _ a b -> findType id a <|> findType id b
    TApp _ a b -> findType id a <|> findType id b
    TForall _ _ _ t -> findType id t

-- Find a sub-type in a larger type by its ID. Also returning its parent
findTypeWithParent :: forall b. ID -> Type' (Meta b) -> Maybe (Type' (Meta b), Maybe (Type' (Meta b)))
findTypeWithParent id x = go x Nothing
  where
    go ty parent
      | ty ^. _typeMetaLens % _id == id = Just (ty, parent)
      | otherwise = case ty of
        TEmptyHole _ -> Nothing
        THole _ t -> go t (Just ty)
        TCon _ _ -> Nothing
        TVar _ _ -> Nothing
        TFun _ a b -> go a (Just ty) <|> go b (Just ty)
        TApp _ a b -> go a (Just ty) <|> go b (Just ty)
        TForall _ _ _ t -> go t (Just ty)

-- | If the given binding has the given ID, return Just that binding, otherwise return nothing.
-- This is just a helper for 'findNode'.
findBind :: forall c. ID -> Bind' (Meta c) -> Maybe (Bind' (Meta c))
findBind id bind
  | bind ^. _bindMeta % _id == id = Just bind
  | otherwise = Nothing

-- An ActionSpec is an OfferedAction that needs
-- metadata in order to be used. Typically this is because it starts with
-- SetCursor, which needs an ID.
--
-- Type parameter 'p' is a ghost parameter that provides some type
-- safety, to prevent the accidental mixing of actions for different
-- types (e.g., to prevent the mixing of actions for 'Expr's and
-- 'Type's). The argument of type 'p' in the type signature is not
-- used other than to provide proof that you have a value of type 'p'.
type ActionSpec p a =
  p -> Meta a -> OfferedAction [Action]

-- From multiple actions, construct an ActionSpec which starts with SetCursor
action :: forall a p. ActionName -> Text -> Int -> Bool -> [Action] -> ActionSpec p a
action name description priority destructive as p m =
  OfferedAction
    { name
    , description
    , input = NoInputRequired $ SetCursor (m ^. _id) : as
    , priority
    , destructive
    }

-- Construct an ActionSpec which requires some input, and then starts with SetCursor
actionWithInput :: forall a p. ActionName -> Text -> Int -> Bool -> UserInput [Action] -> ActionSpec p a
actionWithInput name description priority destructive input p m =
  OfferedAction
    { name
    , description
    , input = InputRequired $ map (\as -> (SetCursor (m ^. _id) : as)) input
    , priority
    , destructive
    }

-- Construct an ActionSpec which requires the user to select from a bunch of
-- generated names for the current location (or specify their own), and starts
-- with SetCursor. Requires the definition id (not the node's id: that is
-- controlled by the ActionSpec)
actionWithNames ::
  forall a.
  ID ->
  Either (Maybe (Type' ())) (Maybe Kind) ->
  (Text -> [Action]) ->
  Meta a ->
  Text ->
  ActionInput [Action]
actionWithNames defId tk k m prompt =
  AskQuestion (GenerateName defId (m ^. _id) tk) $ \options ->
    InputRequired $
      ChooseOrEnterName
        { prompt
        , options
        , choose = \n -> SetCursor (m ^. _id) : k (unName n)
        }

-- A set of ActionSpecs can be realised by providing them with metadata.
realise :: forall a p. p -> Meta a -> [ActionSpec p a] -> [OfferedAction [Action]]
realise p m as = map (\a -> a p m) as

-- Given an expression, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForExpr :: Level -> ID -> Expr -> [OfferedAction [Action]]
basicActionsForExpr l defID expr = case expr of
  EmptyHole m -> realise expr m $ universalActions m <> emptyHoleActions m
  Hole m _ -> realise expr m $ defaultActions m <> holeActions
  Ann m _ _ -> realise expr m $ defaultActions m <> annotationActions
  Lam m _ _ -> realise expr m $ defaultActions m <> lambdaActions m
  LAM m _ _ -> realise expr m $ defaultActions m <> bigLambdaActions m
  Let m _ e _ -> realise expr m $ defaultActions m <> letActions (e ^? _exprMetaLens % _type % _Just % _synthed)
  Letrec m _ _ t _ -> realise expr m $ defaultActions m <> letRecActions (Just t)
  e -> realise expr (e ^. _exprMetaLens) $ defaultActions (e ^. _exprMetaLens)
  where
    insertVariable =
      let filterVars = case l of
            Beginner -> NoFunctions
            _ -> Everything
       in actionWithInput (Code "x") "Use a variable" (P.useVar l) False $
            ChooseVariable filterVars $ \case
              Left name -> [ConstructVar name]
              Right id_ -> [ConstructGlobalVar id_]

    -- If we have a useful type, offer the refine action, otherwise offer the
    offerRefined :: ExprMeta -> Bool
    offerRefined m = case m ^? _type % _Just % _chkedAt of
      Just (TEmptyHole _) -> False
      Just (THole _ _) -> False
      Just _ -> True
      _ -> False

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    insertVariableSaturatedRefined :: forall a. ExprMeta -> ActionSpec Expr a
    insertVariableSaturatedRefined m =
      actionWithInput (Code "f $ ?") "Apply a function to arguments" (P.useFunction l) False $
        ChooseVariable OnlyFunctions $ \case
          Left name -> [if offerRefined m then InsertRefinedVar name else InsertSaturatedVar name]
          Right id_ -> [if offerRefined m then InsertRefinedGlobalVar id_ else InsertSaturatedGlobalVar id_]

    annotateExpression :: forall a. ActionSpec Expr a
    annotateExpression = action (Code ":") "Annotate this expression with a type" (P.annotateExpr l) False [ConstructAnn]

    applyFunction :: forall a. ActionSpec Expr a
    applyFunction = action (Code "$") "Apply function" (P.applyFunction l) False [ConstructApp, Move Child2]

    applyType :: forall a. ActionSpec Expr a
    applyType = action (Code "@") "Apply type" (P.applyType l) False [ConstructAPP, EnterType]

    patternMatch :: forall a. ActionSpec Expr a
    patternMatch = action (Code "m") patternMatchProse (P.makeCase l) False [ConstructCase]

    patternMatchProse = case l of
      Beginner -> "Match a variable with its value"
      Intermediate -> "Match a variable with its value"
      Expert -> "Pattern match"

    makeLambda :: forall a. Meta (Maybe TypeCache) -> ActionSpec Expr a
    makeLambda m p m' =
      OfferedAction
        { name = Code "λx"
        , description = "Make a function with an input"
        , input =
            actionWithNames
              defID
              (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
              (\n -> [ConstructLam $ Just n])
              m'
              ("Choose a " <> nameString <> " for the input variable")
        , priority = P.makeLambda l
        , destructive = False
        }

    makeTypeAbstraction :: forall a. ExprMeta -> ActionSpec Expr a
    makeTypeAbstraction m p m' =
      OfferedAction
        { name = Code "Λx"
        , description = "Make a type abstraction"
        , input =
            actionWithNames
              defID
              (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind)
              (\n -> [ConstructLAM $ Just n])
              m'
              ("Choose a " <> nameString <> " for the bound type variable")
        , priority = P.makeTypeAbstraction l
        , destructive = False
        }

    useValueConstructor :: forall a. ActionSpec Expr a
    useValueConstructor =
      let filterCtors = case l of
            Beginner -> NoFunctions
            _ -> Everything
       in actionWithInput
            (Code "V")
            "Use a value constructor"
            (P.useValueCon l)
            False
            $ ChooseConstructor filterCtors (\c -> [ConstructCon c])

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    useSaturatedRefinedValueConstructor :: forall a. ExprMeta -> ActionSpec Expr a
    useSaturatedRefinedValueConstructor m =
      actionWithInput
        (Code "V $ ?")
        "Apply a value constructor to arguments"
        (P.useSaturatedValueCon l)
        False
        $ ChooseConstructor OnlyFunctions (\c -> [if offerRefined m then ConstructRefinedCon c else ConstructSaturatedCon c])

    makeLetBinding :: forall a. ActionSpec Expr a
    makeLetBinding p m' =
      OfferedAction
        { name = (Code "=")
        , description = "Make a let binding"
        , input =
            actionWithNames
              defID
              (Left Nothing)
              (\n -> [ConstructLet $ Just n])
              m'
              ("Choose a " <> nameString <> " for the new let binding")
        , priority = P.makeLet l
        , destructive = False
        }

    makeLetrec :: forall a. ActionSpec Expr a
    makeLetrec p m' =
      OfferedAction
        { name = (Code "=,=")
        , description = "Make a recursive let binding"
        , input =
            actionWithNames
              defID
              (Left Nothing)
              (\n -> [ConstructLetrec $ Just n])
              m'
              ("Choose a " <> nameString <> " for the new let binding")
        , priority = P.makeLetrec l
        , destructive = False
        }

    enterHole :: forall a. ActionSpec Expr a
    enterHole = action (Prose "h") "Make this hole into a non-empty hole" (P.enterHole l) False [EnterHole]

    finishHole :: forall a. ActionSpec Expr a
    finishHole = action (Prose "e") "Convert this into a normal expression" (P.finishHole l) False [FinishHole]

    removeAnnotation :: forall a. ActionSpec Expr a
    removeAnnotation = action (Prose "⌫:") "Remove this annotation" (P.removeAnnotation l) True [RemoveAnn]

    renameVariable :: forall a. ExprMeta -> ActionSpec Expr a
    renameVariable m p m' =
      OfferedAction
        { name = (Prose "r")
        , description = "Rename this input variable"
        , input =
            actionWithNames
              defID
              (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)
              (\n -> [RenameLam n])
              m'
              ("Choose a new " <> nameString <> " for the input variable")
        , priority = P.rename l
        , destructive = False
        }

    renameTypeVariable :: forall a. ExprMeta -> ActionSpec Expr a
    renameTypeVariable m p m' =
      OfferedAction
        { name = (Prose "r")
        , description = "Rename this type variable"
        , input =
            actionWithNames
              defID
              (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind)
              (\n -> [RenameLAM n])
              m'
              ("Choose a new " <> nameString <> " for the type variable")
        , priority = P.rename l
        , destructive = False
        }

    makeLetRecursive :: forall a. ActionSpec Expr a
    makeLetRecursive = action (Prose "rec") "Make this let recursive" (P.makeLetRecursive l) False [ConvertLetToLetrec]

    renameLet :: forall a b. Maybe (Type' b) -> ActionSpec Expr a
    renameLet t p m' =
      OfferedAction
        { name = Prose "r"
        , description = "Rename this let binding"
        , input =
            actionWithNames
              defID
              (Left $ set (_Just % _typeMeta) () t)
              (\n -> [RenameLet n])
              m'
              ("Choose a new " <> nameString <> " for the let binding")
        , priority = P.rename l
        , destructive = False
        }

    deleteExpr :: forall a. ActionSpec Expr a
    deleteExpr = action (Prose "⌫") "Delete this expression" (P.delete l) True [Delete]

    emptyHoleActions :: forall a. ExprMeta -> [ActionSpec Expr a]
    emptyHoleActions m = case l of
      Beginner ->
        [ insertVariable
        , useValueConstructor
        ]
      _ ->
        [ insertVariable
        , insertVariableSaturatedRefined m
        , useValueConstructor
        , useSaturatedRefinedValueConstructor m
        , makeLetBinding
        , makeLetrec
        , enterHole
        ]

    holeActions :: forall a. [ActionSpec Expr a]
    holeActions = [finishHole]

    annotationActions :: forall a. [ActionSpec Expr a]
    annotationActions = case l of
      Beginner -> []
      Intermediate -> []
      Expert -> [removeAnnotation]

    lambdaActions :: forall a. ExprMeta -> [ActionSpec Expr a]
    lambdaActions m = [renameVariable m]

    bigLambdaActions :: forall a. ExprMeta -> [ActionSpec Expr a]
    bigLambdaActions m = case l of
      Beginner -> []
      Intermediate -> []
      Expert -> [renameTypeVariable m]

    letActions :: forall a b. Maybe (Type' b) -> [ActionSpec Expr a]
    letActions t =
      [ renameLet t
      , makeLetRecursive
      ]

    letRecActions :: forall a b. Maybe (Type' b) -> [ActionSpec Expr a]
    letRecActions t = [renameLet t]

    -- Actions for every expression node
    universalActions :: forall a. ExprMeta -> [ActionSpec Expr a]
    universalActions m = case l of
      Beginner ->
        [ makeLambda m
        , patternMatch
        ]
      Intermediate ->
        [ makeLambda m
        , patternMatch
        , applyFunction
        ]
      Expert ->
        [ annotateExpression
        , applyFunction
        , applyType
        , patternMatch
        , makeLambda m
        , makeTypeAbstraction m
        ]

    -- Extract the source of the function type we were checked at
    -- i.e. the type that a lambda-bound variable would have here
    lamVarTy :: Type' () -> Maybe (Type' ())
    lamVarTy = \case
      TFun _ s _ -> pure s
      _ -> Nothing

    -- Extract the kind a forall-bound variable would have here
    lAMVarKind :: Type' () -> Maybe Kind
    lAMVarKind = \case
      TForall _ _ k _ -> Just k
      _ -> Nothing

    -- Actions for every expression node except holes and annotations
    defaultActions :: forall a. ExprMeta -> [ActionSpec Expr a]
    defaultActions m = universalActions m <> [deleteExpr]

-- Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> ID -> Type -> [OfferedAction [Action]]
basicActionsForType l defID ty = case ty of
  TEmptyHole m -> realise ty m $ universalActions <> emptyHoleActions
  TForall m _ k _ -> realise ty m $ defaultActions <> forAllActions k
  t -> realise ty (t ^. _typeMetaLens) defaultActions
  where
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    constructFunctionType :: forall a. ActionSpec Type a
    constructFunctionType = action (Code "→") "Construct a function type" (P.constructFunction l) False [ConstructArrowL, Move Child1]

    constructPolymorphicType :: forall a. ActionSpec Type a
    constructPolymorphicType p m' =
      OfferedAction
        { name = (Code "∀")
        , description = "Construct a polymorphic type"
        , input =
            actionWithNames
              defID
              (Right Nothing)
              (\n -> [ConstructTForall (Just n), Move Child1])
              m'
              ("Choose a " <> nameString <> " for the bound type variable")
        , priority = P.constructForall l
        , destructive = False
        }

    constructTypeApplication :: forall a. ActionSpec Type a
    constructTypeApplication = action (Code "$") "Construct a type application" (P.constructTypeApp l) False [ConstructTApp, Move Child1]

    useTypeConstructor :: forall a. ActionSpec Type a
    useTypeConstructor = actionWithInput (Code "T") "Use a type constructor" (P.useTypeCon l) False $ ChooseTypeConstructor (\t -> [ConstructTCon t])

    useTypeVariable :: forall a. ActionSpec Type a
    useTypeVariable = actionWithInput (Code "t") "Use a type variable" (P.useTypeVar l) False $ ChooseTypeVariable (\v -> [ConstructTVar v])

    renameTypeVariable :: forall a. Kind -> ActionSpec Type a
    renameTypeVariable k p m' =
      OfferedAction
        { name = (Prose "r")
        , description = "Rename this type variable"
        , input =
            actionWithNames
              defID
              (Right $ Just k)
              (\n -> [RenameForall n])
              m'
              ("Choose a new " <> nameString <> " for the bound type variable")
        , priority = P.rename l
        , destructive = False
        }

    deleteType :: forall a. ActionSpec Type a
    deleteType = action (Prose "⌫") "Delete this type" (P.delete l) True [Delete]

    emptyHoleActions :: forall a. [ActionSpec Type a]
    emptyHoleActions = case l of
      Beginner -> [useTypeConstructor]
      Intermediate -> [useTypeConstructor]
      Expert ->
        [ useTypeConstructor
        , useTypeVariable
        ]

    forAllActions :: forall a. Kind -> [ActionSpec Type a]
    forAllActions k = case l of
      Beginner -> mempty
      Intermediate -> mempty
      Expert -> [renameTypeVariable k]

    -- Actions for every type node
    universalActions :: forall a. [ActionSpec Type a]
    universalActions = case l of
      Beginner -> [constructFunctionType]
      Intermediate -> [constructFunctionType]
      Expert ->
        [ constructFunctionType
        , constructPolymorphicType
        , constructTypeApplication
        ]

    -- Actions for every type node except empty holes
    defaultActions :: forall a. [ActionSpec Type a]
    defaultActions = universalActions <> [deleteType]

-- These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: forall a. Level -> Type' (Meta a) -> [OfferedAction [Action]]
compoundActionsForType l ty = case ty of
  TFun m a b -> realise ty m [addFunctionArgument a b]
  _ -> []
  where
    -- This action traverses the function type and adds a function arrow to the end of it,
    -- resulting in a new argument type. The result type is unchanged.
    -- The cursor location is also unchanged.
    -- e.g. A -> B -> C ==> A -> B -> ? -> C
    addFunctionArgument a b =
      let (argTypes, _resultType) = unfoldFun a b

          moveToLastArg = replicate (NE.length argTypes) (Move Child2)

          moveBack = replicate (NE.length argTypes) (Move Parent)
       in action (Code "→A→") "Add an input to this function" (P.addInput l) False $ moveToLastArg <> [ConstructArrowR] <> moveBack
