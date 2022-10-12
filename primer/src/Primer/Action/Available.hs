-- | Compute all the possible actions which can be performed on a definition
module Primer.Action.Available (
  actionsForDef,
  actionsForDefBody,
  actionsForDefSig,
) where

import Foreword

import Data.Data (Data)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics (
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
  ActionType (..),
  FunctionFiltering (..),
  Level (..),
  Movement (..),
  OfferedAction (..),
  ProgAction (..),
  UserInput (..),
  nameString,
  uniquifyDefName,
 )
import Primer.Action.Priorities qualified as P
import Primer.App (Editable (Editable, NonEditable), globalInUse)
import Primer.Core (
  Bind' (..),
  Expr,
  Expr' (..),
  ExprMeta,
  GVarName,
  GlobalName (baseName, qualifiedModule),
  ID,
  Kind,
  LVarName,
  Meta (..),
  Type,
  Type' (..),
  TypeCache (..),
  getID,
  unLocalName,
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
  defAST,
 )
import Primer.Name (unName)
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
  [OfferedAction [ProgAction]]
actionsForDef l defs defName = catMaybes [rename, duplicate, delete]
  where
    rename = do
      _ <- getEditableASTDef
      pure $
        OfferedAction
          { name = Prose "r"
          , description = "Rename this definition"
          , input =
              InputRequired $
                ChooseOrEnterName
                  ("Enter a new " <> nameString <> " for the definition")
                  []
                  (\name -> [RenameDef defName (unName name)])
          , priority = P.rename l
          , actionType = Primary
          }
    duplicate = do
      def <- getEditableASTDef
      pure $
        OfferedAction
          { name = Prose "d"
          , description = "Duplicate this definition"
          , input =
              let sigID = getID $ astDefType def

                  bodyID = getID $ astDefExpr def

                  copyName = uniquifyDefName (qualifiedModule defName) (unName (baseName defName) <> "Copy") $ fmap snd defs
               in NoInputRequired
                    [ CreateDef (qualifiedModule defName) (Just copyName)
                    , CopyPasteSig (defName, sigID) []
                    , CopyPasteBody (defName, bodyID) []
                    ]
          , priority = P.duplicate l
          , actionType = Primary
          }
    delete = do
      -- Ensure it is not in use, otherwise the action will not succeed
      _ <- getEditableDef
      guard $ not $ globalInUse defName $ Map.delete defName $ fmap snd defs
      pure
        OfferedAction
          { name = Prose "⌫"
          , description = "Delete this definition"
          , input = NoInputRequired [DeleteDef defName]
          , priority = P.delete l
          , actionType = Destructive
          }
    getEditableDef = do
      (m, d) <- Map.lookup defName defs
      case m of
        NonEditable -> Nothing
        Editable -> pure d
    getEditableASTDef = defAST =<< getEditableDef

-- | Given the body of a Def and the ID of a node in it, return the possible actions that can be applied to it
actionsForDefBody ::
  TypeDefMap ->
  Level ->
  GVarName ->
  Editable ->
  ID ->
  Expr ->
  [OfferedAction [ProgAction]]
actionsForDefBody _ _ _ NonEditable _ _ = mempty
actionsForDefBody tydefs l defName mut@Editable id expr =
  let toProgAction actions = [MoveToDef defName, BodyAction $ SetCursor id : actions]

      raiseAction' =
        OfferedAction
          { name = Prose "↑"
          , description = "Replace parent with this subtree"
          , input = NoInputRequired [MoveToDef defName, CopyPasteBody (defName, id) [SetCursor id, Move Parent, Delete]]
          , priority = P.raise l
          , actionType = Destructive
          }
   in case findNodeWithParent id expr of
        Nothing -> mempty
        Just (ExprNode e, p) ->
          let raiseAction = case p of
                Nothing -> [] -- at root already, cannot raise
                Just (ExprNode (Hole _ _)) -> [] -- in a NE hole, don't offer raise (as hole will probably just be recreated)
                _ -> [raiseAction']
           in (toProgAction <<$>> basicActionsForExpr tydefs l defName e) <> raiseAction
        Just (TypeNode t, p) ->
          let raiseAction = case p of
                Just (ExprNode _) -> [] -- at the root of an annotation, so cannot raise
                _ -> [raiseAction']
           in ( toProgAction
                  <<$>> (basicActionsForType l defName t <> compoundActionsForType l t)
              )
                <> raiseAction
        Just (CaseBindNode b, _) -> toProgAction <<$>> actionsForBinding l defName mut b

-- | Given a the type signature of a Def and the ID of a node in it,
-- return the possible actions that can be applied to it
actionsForDefSig ::
  Level ->
  GVarName ->
  Editable ->
  ID ->
  Type ->
  [OfferedAction [ProgAction]]
actionsForDefSig _ _ NonEditable _ _ = mempty
actionsForDefSig l defName Editable id ty =
  let toProgAction actions = [MoveToDef defName, SigAction $ SetCursor id : actions]

      raiseAction =
        [ OfferedAction
          { name = Prose "↑"
          , description = "Replace parent with this subtree"
          , input = NoInputRequired [MoveToDef defName, CopyPasteSig (defName, id) [SetCursor id, Move Parent, Delete]]
          , priority = P.raise l
          , actionType = Destructive
          }
        | id /= getID ty
        ]
   in case findType id ty of
        Nothing -> mempty
        Just t ->
          ( toProgAction
              <<$>> (basicActionsForType l defName t <> compoundActionsForType l t)
          )
            <> raiseAction

-- | Bindings support just one action: renaming.
actionsForBinding ::
  Level ->
  GVarName ->
  Editable ->
  Bind' (Meta (Maybe TypeCache)) ->
  [OfferedAction [Action]]
actionsForBinding _ _ NonEditable _ = mempty
actionsForBinding l defName Editable b =
  [ OfferedAction
      { name = Prose "r"
      , description = "Rename this pattern variable"
      , input =
          AskQuestion (GenerateName defName (b ^. _bindMeta % _id) (Left $ b ^? _bindMeta % _type % _Just % _chkedAt)) $ \options ->
            InputRequired $
              ChooseOrEnterName
                ("Choose a new " <> nameString <> " for the pattern variable")
                options
                ((\n -> [RenameCaseBinding n]) . unName)
      , priority = P.rename l
      , actionType = Primary
      }
  ]

-- | Find a node in the AST by its ID, and also return its parent
findNodeWithParent ::
  (Data a, Data b, Eq a) =>
  ID ->
  Expr' (Meta a) (Meta b) ->
  Maybe (SomeNode a b, Maybe (SomeNode a b))
findNodeWithParent id x = do
  z <- focusOn id x
  Just
    ( case z of
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
    )

-- | Find a sub-type in a larger type by its ID.
findType :: forall b. Data b => ID -> Type' (Meta b) -> Maybe (Type' (Meta b))
findType id ty = target <$> focusOnTy id ty

-- | Given an expression, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForExpr :: TypeDefMap -> Level -> GVarName -> Expr -> [OfferedAction [Action]]
basicActionsForExpr tydefs l defName expr = case expr of
  EmptyHole m -> universalActions m <> emptyHoleActions m
  Hole m _ -> defaultActions m <> holeActions
  Ann m _ _ -> defaultActions m <> annotationActions
  Lam m _ _ -> defaultActions m <> lambdaActions m
  LAM m _ _ -> defaultActions m <> bigLambdaActions m
  Let m v e _ -> defaultActions m <> letActions v e m
  Letrec m _ _ t _ -> defaultActions m <> letRecActions (Just t) m
  e -> let m = e ^. _exprMetaLens in defaultActions m ++ expert annotateExpression
  where
    insertVariable =
      let filterVars = case l of
            Beginner -> NoFunctions
            _ -> Everything
       in ( \input ->
              OfferedAction
                { name = Code "x"
                , description = "Use a variable"
                , input = InputRequired input
                , priority = P.useVar l
                , actionType = Primary
                }
          )
            $ ChooseVariable filterVars
            $ pure . ConstructVar

    -- If we have a useful type, offer the refine action, otherwise offer the
    -- saturate action.
    offerRefined :: ExprMeta -> Bool
    offerRefined m = case m ^? _type % _Just % _chkedAt of
      Just (TEmptyHole _) -> False
      Just (THole _ _) -> False
      Just _ -> True
      _ -> False

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    insertVariableSaturatedRefined :: ExprMeta -> OfferedAction [Action]
    insertVariableSaturatedRefined m =
      OfferedAction
        { name = Code "f $ ?"
        , description = "Apply a function to arguments"
        , input =
            InputRequired
              ( ChooseVariable OnlyFunctions $
                  \name -> [if offerRefined m then InsertRefinedVar name else InsertSaturatedVar name]
              )
        , priority = P.useFunction l
        , actionType = Primary
        }

    annotateExpression :: OfferedAction [Action]
    annotateExpression =
      OfferedAction
        { name = Code ":"
        , description = "Annotate this expression with a type"
        , input = NoInputRequired [ConstructAnn]
        , priority = P.annotateExpr l
        , actionType = Primary
        }

    applyFunction :: OfferedAction [Action]
    applyFunction =
      OfferedAction
        { name = Code "$"
        , description = "Apply function"
        , input = NoInputRequired [ConstructApp, Move Child2]
        , priority = P.applyFunction l
        , actionType = Primary
        }

    applyType :: OfferedAction [Action]
    applyType =
      OfferedAction
        { name = Code "@"
        , description = "Apply type"
        , input = NoInputRequired [ConstructAPP, EnterType]
        , priority = P.applyType l
        , actionType = Destructive
        }

    patternMatch :: OfferedAction [Action]
    patternMatch =
      OfferedAction
        { name = Code "m"
        , description = patternMatchProse
        , input = NoInputRequired [ConstructCase]
        , priority = P.makeCase l
        , actionType = Destructive
        }

    patternMatchProse = case l of
      Beginner -> "Match a variable with its value"
      Intermediate -> "Match a variable with its value"
      Expert -> "Pattern match"

    makeLambda :: Meta (Maybe TypeCache) -> OfferedAction [Action]
    makeLambda m =
      OfferedAction
        { name = Code "λx"
        , description = "Make a function with an input"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a " <> nameString <> " for the input variable")
                  options
                  ((\n -> [ConstructLam $ Just n]) . unName)
        , priority = P.makeLambda l
        , actionType = Primary
        }

    makeTypeAbstraction :: ExprMeta -> OfferedAction [Action]
    makeTypeAbstraction m =
      OfferedAction
        { name = Code "Λx"
        , description = "Make a type abstraction"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a " <> nameString <> " for the bound type variable")
                  options
                  ((\n -> [ConstructLAM $ Just n]) . unName)
        , priority = P.makeTypeAbstraction l
        , actionType = Primary
        }

    useValueConstructor :: OfferedAction [Action]
    useValueConstructor =
      let filterCtors = case l of
            Beginner -> NoFunctions
            _ -> Everything
       in ( \input ->
              OfferedAction
                { name = Code "V"
                , description = "Use a value constructor"
                , input = InputRequired input
                , priority = P.useValueCon l
                , actionType = Primary
                }
          )
            $ ChooseConstructor filterCtors (\c -> [ConstructCon c])

    -- NB: Exactly one of the saturated and refined actions will be available
    -- (depending on whether we have useful type information to hand).
    -- We put the same labels on each.
    useSaturatedRefinedValueConstructor :: ExprMeta -> OfferedAction [Action]
    useSaturatedRefinedValueConstructor m =
      OfferedAction
        { name = Code "V $ ?"
        , description = "Apply a value constructor to arguments"
        , input =
            InputRequired
              ( ChooseConstructor
                  OnlyFunctions
                  (\c -> [if offerRefined m then ConstructRefinedCon c else ConstructSaturatedCon c])
              )
        , priority = P.useSaturatedValueCon l
        , actionType = Primary
        }

    makeLetBinding :: Meta a -> OfferedAction [Action]
    makeLetBinding m =
      OfferedAction
        { name = Code "="
        , description = "Make a let binding"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Left Nothing)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a " <> nameString <> " for the new let binding")
                  options
                  ((\n -> [ConstructLet $ Just n]) . unName)
        , priority = P.makeLet l
        , actionType = Primary
        }

    makeLetrec :: Meta a -> OfferedAction [Action]
    makeLetrec m =
      OfferedAction
        { name = Code "=,="
        , description = "Make a recursive let binding"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Left Nothing)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a " <> nameString <> " for the new let binding")
                  options
                  ((\n -> [ConstructLetrec $ Just n]) . unName)
        , priority = P.makeLetrec l
        , actionType = Primary
        }

    enterHole :: OfferedAction [Action]
    enterHole =
      OfferedAction
        { name = Prose "h"
        , description = "Make this hole into a non-empty hole"
        , input = NoInputRequired [EnterHole]
        , priority = P.enterHole l
        , actionType = Primary
        }

    finishHole :: OfferedAction [Action]
    finishHole =
      OfferedAction
        { name = Prose "e"
        , description = "Convert this into a normal expression"
        , input = NoInputRequired [FinishHole]
        , priority = P.finishHole l
        , actionType = Primary
        }

    removeAnnotation :: OfferedAction [Action]
    removeAnnotation =
      OfferedAction
        { name = Prose "⌫:"
        , description = "Remove this annotation"
        , input = NoInputRequired [RemoveAnn]
        , priority = P.removeAnnotation l
        , actionType = Destructive
        }

    renameVariable :: ExprMeta -> OfferedAction [Action]
    renameVariable m =
      OfferedAction
        { name = Prose "r"
        , description = "Rename this input variable"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Left $ join $ m ^? _type % _Just % _chkedAt % to lamVarTy)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a new " <> nameString <> " for the input variable")
                  options
                  ((\n -> [RenameLam n]) . unName)
        , priority = P.rename l
        , actionType = Primary
        }

    renameTypeVariable :: ExprMeta -> OfferedAction [Action]
    renameTypeVariable m =
      OfferedAction
        { name = Prose "r"
        , description = "Rename this type variable"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Right $ join $ m ^? _type % _Just % _chkedAt % to lAMVarKind)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a new " <> nameString <> " for the type variable")
                  options
                  ((\n -> [RenameLAM n]) . unName)
        , priority = P.rename l
        , actionType = Primary
        }

    makeLetRecursive :: OfferedAction [Action]
    makeLetRecursive =
      OfferedAction
        { name = Prose "rec"
        , description = "Make this let recursive"
        , input = NoInputRequired [ConvertLetToLetrec]
        , priority = P.makeLetRecursive l
        , actionType = Primary
        }

    renameLet :: Maybe (Type' b) -> Meta a -> OfferedAction [Action]
    renameLet t m' =
      OfferedAction
        { name = Prose "r"
        , description = "Rename this let binding"
        , input =
            AskQuestion (GenerateName defName (m' ^. _id) (Left $ forgetTypeMetadata <$> t)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a new " <> nameString <> " for the let binding")
                  options
                  ((\n -> [RenameLet n]) . unName)
        , priority = P.rename l
        , actionType = Primary
        }

    deleteExpr :: OfferedAction [Action]
    deleteExpr =
      OfferedAction
        { name = Prose "⌫"
        , description = "Delete this expression"
        , input = NoInputRequired [Delete]
        , priority = P.delete l
        , actionType = Destructive
        }

    expert :: a -> [a]
    expert = if l == Expert then (: []) else const []

    emptyHoleActions :: ExprMeta -> [OfferedAction [Action]]
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
        , makeLetBinding m
        , makeLetrec m
        , enterHole
        ]
          ++ expert annotateExpression

    holeActions :: [OfferedAction [Action]]
    holeActions = finishHole : expert annotateExpression

    annotationActions :: [OfferedAction [Action]]
    annotationActions = expert removeAnnotation

    lambdaActions :: ExprMeta -> [OfferedAction [Action]]
    lambdaActions m = renameVariable m : expert annotateExpression

    bigLambdaActions :: ExprMeta -> [OfferedAction [Action]]
    bigLambdaActions m = concatMap expert [annotateExpression, renameTypeVariable m]

    letActions :: LVarName -> Expr -> Meta a -> [OfferedAction [Action]]
    letActions v e m =
      renameLet (e ^? _exprMetaLens % _type % _Just % _synthed) m
        : munless (unLocalName v `Set.member` freeVars e) [makeLetRecursive]
          <> expert annotateExpression

    letRecActions :: Maybe (Type' b) -> Meta a -> [OfferedAction [Action]]
    letRecActions t m = renameLet t m : expert annotateExpression

    -- Actions for every expression node
    -- We assume that the input program is type-checked, in order to
    -- filter some actions by Syn/Chk
    universalActions :: ExprMeta -> [OfferedAction [Action]]
    universalActions m =
      let both = case l of
            Beginner ->
              [ makeLambda m
              ]
            Intermediate ->
              [ makeLambda m
              , applyFunction
              ]
            Expert ->
              [ applyFunction
              , applyType
              , makeLambda m
              , makeTypeAbstraction m
              ]
          synthTy = m ^? _type % _Just % _synthed
          synOnly ty = case getTypeDefInfo' tydefs ty of
            Left TDIHoleType{} -> Just patternMatch
            Right (TypeDefInfo _ _ TypeDefAST{}) -> Just patternMatch
            _ -> Nothing
       in (synOnly =<< synthTy) ?: both

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
    defaultActions :: ExprMeta -> [OfferedAction [Action]]
    defaultActions m = universalActions m <> [deleteExpr]

(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x : xs
Nothing ?: xs = xs
infixr 5 ?:

-- | Given a type, determine what basic actions it supports
-- Specific projections may provide other actions not listed here
basicActionsForType :: Level -> GVarName -> Type -> [OfferedAction [Action]]
basicActionsForType l defName ty = case ty of
  TEmptyHole m -> universalActions m <> emptyHoleActions
  TForall m _ k _ -> defaultActions m <> forAllActions k m
  t -> defaultActions $ t ^. _typeMetaLens
  where
    -- We arbitrarily choose that the "construct a function type" action places the focused expression
    -- on the domain (left) side of the arrow.
    constructFunctionType :: OfferedAction [Action]
    constructFunctionType =
      OfferedAction
        { name = Code "→"
        , description = "Construct a function type"
        , input = NoInputRequired [ConstructArrowL, Move Child1]
        , priority = P.constructFunction l
        , actionType = Primary
        }

    constructPolymorphicType :: Meta a -> OfferedAction [Action]
    constructPolymorphicType m =
      OfferedAction
        { name = Code "∀"
        , description = "Construct a polymorphic type"
        , input =
            AskQuestion (GenerateName defName (m ^. _id) (Right Nothing)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a " <> nameString <> " for the bound type variable")
                  options
                  ((\n -> [ConstructTForall (Just n), Move Child1]) . unName)
        , priority = P.constructForall l
        , actionType = Primary
        }

    constructTypeApplication :: OfferedAction [Action]
    constructTypeApplication =
      OfferedAction
        { name = Code "$"
        , description = "Construct a type application"
        , input = NoInputRequired [ConstructTApp, Move Child1]
        , priority = P.constructTypeApp l
        , actionType = Primary
        }

    useTypeConstructor :: OfferedAction [Action]
    useTypeConstructor =
      ( \input ->
          OfferedAction
            { name = Code "T"
            , description = "Use a type constructor"
            , input = InputRequired input
            , priority = P.useTypeCon l
            , actionType = Primary
            }
      )
        $ ChooseTypeConstructor (\t -> [ConstructTCon t])

    useTypeVariable :: OfferedAction [Action]
    useTypeVariable =
      ( \input ->
          OfferedAction
            { name = Code "t"
            , description = "Use a type variable"
            , input = InputRequired input
            , priority = P.useTypeVar l
            , actionType = Primary
            }
      )
        $ ChooseTypeVariable (\v -> [ConstructTVar v])

    renameTypeVariable :: Kind -> Meta a -> OfferedAction [Action]
    renameTypeVariable k m' =
      OfferedAction
        { name = Prose "r"
        , description = "Rename this type variable"
        , input =
            AskQuestion (GenerateName defName (m' ^. _id) (Right $ Just k)) $ \options ->
              InputRequired $
                ChooseOrEnterName
                  ("Choose a new " <> nameString <> " for the bound type variable")
                  options
                  ((\n -> [RenameForall n]) . unName)
        , priority = P.rename l
        , actionType = Primary
        }

    deleteType :: OfferedAction [Action]
    deleteType =
      OfferedAction
        { name = Prose "⌫"
        , description = "Delete this type"
        , input = NoInputRequired [Delete]
        , priority = P.delete l
        , actionType = Destructive
        }

    emptyHoleActions :: [OfferedAction [Action]]
    emptyHoleActions = [useTypeConstructor] <> expert useTypeVariable

    forAllActions :: Kind -> Meta a -> [OfferedAction [Action]]
    forAllActions k m = expert $ renameTypeVariable k m

    -- Actions for every type node
    universalActions :: Meta a -> [OfferedAction [Action]]
    universalActions m =
      [constructFunctionType]
        <> concatMap
          expert
          [ constructPolymorphicType m
          , constructTypeApplication
          ]

    -- Actions for every type node except empty holes
    defaultActions :: Meta a -> [OfferedAction [Action]]
    defaultActions m = universalActions m <> [deleteType]

    expert :: a -> [a]
    expert = if l == Expert then (: []) else const []

-- | These actions are more involved than the basic actions.
-- They may involve moving around the AST and performing several basic actions.
compoundActionsForType :: Level -> Type' (Meta a) -> [OfferedAction [Action]]
compoundActionsForType l ty = case ty of
  TFun _m a b -> [addFunctionArgument a b]
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
       in ( \as ->
              OfferedAction
                { name = Code "→A→"
                , description = "Add an input to this function"
                , input = NoInputRequired as
                , priority = P.addInput l
                , actionType = Primary
                }
          )
            $ moveToLastArg <> [ConstructArrowR] <> moveBack
