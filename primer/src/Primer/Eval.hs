{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Primer.Eval (
  -- The public API of this module
  step,
  redexes,
  EvalError (..),
  EvalDetail (..),
  BetaReductionDetail (..),
  LocalVarInlineDetail (..),
  CaseReductionDetail (..),
  GlobalVarInlineDetail (..),
  LetRemovalDetail (..),
  PushAppIntoLetrecDetail (..),
  ApplyPrimFunDetail (..),
  Locals,
  Globals,
  regenerateExprIDs,
  regenerateTypeIDs,
  -- Only exported for testing
  tryReduceExpr,
  tryReduceType,
  findNodeByID,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh, fresh)
import Data.Generics.Product (position)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics (mapping, set, traverseOf, view, (%), (^.))
import Primer.Core (
  Bind' (..),
  CaseBranch' (..),
  Def (..),
  Expr,
  Expr' (..),
  HasID (_id),
  ID,
  Kind,
  Meta,
  PrimFun (..),
  PrimFunError (..),
  Type,
  Type' (..),
  TypeCache,
  bindName,
  _exprMeta,
  _exprTypeMeta,
  _typeMeta,
 )
import Primer.Core.DSL (ann, create, hole, letType, let_, tEmptyHole)
import Primer.Core.Transform (removeAnn, renameVar, unfoldAPP, unfoldApp, unfoldFun)
import Primer.JSON
import Primer.Name (Name, unName, unsafeMkName)
import Primer.Primitives (globalPrims)
import Primer.Subst (freeVars, freeVarsTy)
import Primer.Zipper (
  ExprZ,
  Loc (InExpr, InType),
  TypeZ,
  current,
  focus,
  focusOn,
  foldAbove,
  prior,
  replace,
  target,
  unfocusExpr,
  unfocusType,
 )

-- | Errors that can be raised during reduction.
--
-- Note: none of these should occur in normal operation.
-- If we get an EvalError, it means either:
-- - we provided bad input, e.g. an ID not in the given expression
-- - the typechecker has a bug which has allowed a badly-typed expression
-- - a previous invocation of 'step' created a badly-typed expression
--
-- The last case is most likely, since we currently don't typecheck the results of evaluation.
-- We should keep an eye on this to see if there are any issues.
data EvalError
  = -- | The node was not reducible
    NotRedex
  | -- | The node with the given ID could not be found in the expression
    NodeNotFound ID
  | -- | A lambda expression was annotated with a non-function type.
    -- The expression is the offending annotation node
    BadLambdaAnnotation Expr
  | -- | A big lambda expression was annotated with a non-forall type.
    -- The expression is the offending annotation node
    BadBigLambdaAnnotation Expr
  | -- | The outer constructor of a case scrutinee didn't match any of the constructors in the case
    -- branches.
    NoMatchingCaseBranch
  | -- | The number of bindings in a branch pattern doesn't match the number of arguments in the
    -- scrutinee.
    CaseBranchBindingLengthMismatch
  | -- | An error occurred while evaluating a primitive function.
    PrimFunError PrimFunError
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalError

-- | Detailed information about a reduction step
data EvalDetail
  = -- | Reduction of (λx. a) b
    BetaReduction (BetaReductionDetail Type Type)
  | -- | Reduction of (Λx. a) b
    BETAReduction (BetaReductionDetail Kind Type)
  | -- | Inlining of a local variable
    LocalVarInline LocalVarInlineDetail
  | -- | Inlining of a local type variable
    LocalTypeVarInline LocalVarInlineDetail
  | -- | ID of definition, name of variable
    GlobalVarInline GlobalVarInlineDetail
  | -- | ID of let(rec)
    LetRemoval LetRemovalDetail
  | -- | TODO: some details here
    CaseReduction CaseReductionDetail
  | -- | Push the argument of an application inside a letrec
    PushAppIntoLetrec PushAppIntoLetrecDetail
  | -- | Apply a primitive function
    ApplyPrimFun ApplyPrimFunDetail
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSON EvalDetail

-- | Detailed information about a beta reduction (of a λ or Λ).
-- If λ:
-- - 'betaLambdaID' is the ID of the λ
-- - 'betaLetID' is the ID of the let
-- - 'betaTypes' is optionally the domain type and codomain type of the λ
-- - i.e. domain ~ Type, codomain ~ Type
-- If Λ:
-- - 'betaLambdaID' is the ID of the Λ
-- - 'betaLetID' is the ID of the "let type"
-- - 'betaTypes' is optionally the domain kind and codomain type of the λ
-- - i.e. domain ~ Kind, codomain ~ Type
data BetaReductionDetail domain codomain = BetaReductionDetail
  { betaBefore :: Expr
  , betaAfter :: Expr
  , betaBindingName :: Name
  , betaLambdaID :: ID
  , betaLetID :: ID
  , betaArgID :: ID
  , betaBodyID :: ID
  , betaTypes :: Maybe (domain, codomain)
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "beta" (BetaReductionDetail domain codomain)

data LocalVarInlineDetail = LocalVarInlineDetail
  { -- | ID of the let expression that binds this variable
    localVarInlineLetID :: ID
  , -- | ID of the variable being replaced
    localVarInlineVarID :: ID
  , -- | Name of the variable
    localVarInlineBindingName :: Name
  , -- | ID of the expression or type that the variable is bound to
    localVarInlineValueID :: ID
  , -- | ID of the expression or type that has replaced the variable in the result
    localVarInlineReplacementID :: ID
  , -- | If 'True', the variable being inlined is a type variable.
    -- Otherwise it is a term variable.
    localVarInlineIsTypeVar :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "localVarInline" LocalVarInlineDetail

data GlobalVarInlineDetail = GlobalVarInlineDetail
  { -- | The definition that the variable refers to
    globalVarInlineDef :: Def
  , -- | The variable being replaced
    globalVarInlineVar :: Expr
  , -- | The result of the reduction
    globalVarInlineAfter :: Expr
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "globalVarInline" GlobalVarInlineDetail

data CaseReductionDetail = CaseReductionDetail
  { -- | the case expression before reduction
    caseBefore :: Expr
  , -- | the resulting expression after reduction
    caseAfter :: Expr
  , -- | the ID of the target (scrutinee)
    caseTargetID :: ID
  , -- | the ID of the constructor node in the target
    caseTargetCtorID :: ID
  , -- | the name of the matching constructor
    caseCtorName :: Name
  , -- | the arguments to the constructor in the target
    caseTargetArgIDs :: [ID]
  , -- | the bindings in the case branch (one for each arg above)
    caseBranchBindingIDs :: [ID]
  , -- | the right hand side of the selected case branch
    caseBranchRhsID :: ID
  , -- | the let expressions binding each argument in the result
    caseLetIDs :: [ID]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "case" CaseReductionDetail

data LetRemovalDetail = LetRemovalDetail
  { -- | the let expression before reduction
    letRemovalBefore :: Expr
  , -- | the resulting expression after reduction
    letRemovalAfter :: Expr
  , -- | the name of the unused bound variable
    letRemovalBindingName :: Name
  , -- | the full let expression
    letRemovalLetID :: ID
  , -- | the right hand side of the let
    letRemovalBodyID :: ID
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "letRemoval" LetRemovalDetail

data PushAppIntoLetrecDetail = PushAppIntoLetrecDetail
  { -- | the expression before reduction
    pushAppIntoLetrecBefore :: Expr
  , -- | the expression after reduction
    pushAppIntoLetrecAfter :: Expr
  , -- | the ID of the argument to the application
    pushAppIntoLetrecArgID :: ID
  , -- | the ID of the letrec
    pushAppIntoLetrecLetrecID :: ID
  , -- | the ID of the lambda
    pushAppIntoLetrecLamID :: ID
  , -- | The name of the variable bound by the letrec
    pushAppIntoLetrecLetBindingName :: Name
  , -- | If 'True', the application is of a big lambda to a type.
    -- Otherwise it is of a small lambda to a term.
    pushAppIntoLetrecIsTypeApplication :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "pushAppIntoLetrec" PushAppIntoLetrecDetail

data ApplyPrimFunDetail = ApplyPrimFunDetail
  { -- | the expression before reduction
    applyPrimFunBefore :: Expr
  , -- | the expression after reduction
    applyPrimFunAfter :: Expr
  , -- | the name of the primitive function
    applyPrimFunName :: Name
  , -- | the IDs of the arguments to the application
    applyPrimFunArgIDs :: [ID]
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "applyPrimFun" ApplyPrimFunDetail

-- | A map from definition IDs to definitions themselves
type Globals = Map ID Def

-- | A map from local variable names to the ID of their binding and their bound value.
-- Since each entry must have a value, this only includes let(rec) bindings.
-- Lambda bindings must be reduced to a let before their variables can appear here.
type Locals = Map Name (ID, Either Expr Type)

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  MonadFresh ID m =>
  Globals ->
  Expr ->
  ID ->
  m (Either EvalError (Expr, EvalDetail))
step globals expr i = runExceptT $ do
  (locals, nodeZ) <- maybe (throwError (NodeNotFound i)) pure (findNodeByID i expr)
  case nodeZ of
    Left z -> do
      (node', detail) <- tryReduceExpr globals locals (target z)
      let expr' = unfocusExpr $ replace node' z
      pure (expr', detail)
    Right z -> do
      (node', detail) <- tryReduceType globals locals (target z)
      let expr' = unfocusExpr $ unfocusType $ replace node' z
      pure (expr', detail)

-- | Search for the given node by its ID.
-- Collect all local bindings in scope and return them along with the focused node.
-- Returns Nothing if the node is a type, because types can't be evaluated in Primer.
findNodeByID :: ID -> Expr -> Maybe (Locals, Either ExprZ TypeZ)
findNodeByID i expr = do
  loc <- focusOn i (focus expr)
  case loc of
    InExpr z ->
      let locals = foldAbove collectBinding z
       in pure (locals, Left z)
    InType z ->
      let locals = foldAbove collectBinding (unfocusType z)
       in pure (locals, Right z)
    _ -> Nothing
  where
    collectBinding a = case (current a, prior a) of
      (Let m x e1 e2, e2') | e2 == e2' -> Map.singleton x (view _id m, Left e1)
      -- Note that because @x@ is in scope in @e1@, we will allow @e1@ to be reduced even if this
      -- reduction may never terminate.
      -- See https://github.com/hackworthltd/primer/issues/4
      -- @x@ is not in scope in @t@.
      (Letrec m x e1 _t e2, e')
        | e2 == e' -> Map.singleton x (view _id m, Left e1)
        | e1 == e' -> Map.singleton x (view _id m, Left e1)
      (LetType m x t _, _) -> Map.singleton x (view _id m, Right t)
      _ -> mempty

-- | Return the IDs of nodes which are reducible
redexes :: Expr -> Set ID
redexes = go mempty
  where
    go locals expr =
      -- A set containing just the ID of this expression
      let self = Set.singleton (expr ^. _id)
       in case expr of
            -- Application nodes are reducible only if their left child is a λ node or an annotation
            -- wrapping a λ node.
            -- (λ ...) x
            App _ e1@Lam{} e2 -> self <> go locals e1 <> go locals e2
            -- (λ ... : T) x
            App _ e1@(Ann _ Lam{} _) e2 -> self <> go locals e1 <> go locals e2
            -- (letrec x : T = λ ...) e
            -- We can reduce an application across a letrec as long as x isn't a free variable in e.
            -- If it was, it would be a different x and we'd cause variable capture if we
            -- substituted e into the λ body.
            App _ e1@(Letrec _ x _ _ Lam{}) e4 ->
              (self `munless` Set.member x (freeVars e4)) <> go locals e1 <> go locals e4
            -- Application of a primitive (fully-applied, with all arguments in normal form).
            App{}
              | (Var _ fName, args) <- unfoldApp expr
                , Just PrimFun{primFunType} <- Map.lookup fName globalPrims
                , TFun _ lhs rhs <- fst $ create primFunType
                , length args == length (fst $ unfoldFun lhs rhs)
                , all isNormalForm args ->
                self
              where
                isNormalForm = \case
                  PrimCon _ _ -> True
                  Con _ _ -> True
                  App _ f x -> isNormalForm f && isNormalForm x
                  _ -> False
            -- f x
            App _ e1 e2 -> go locals e1 <> go locals e2
            APP _ e@LAM{} t -> self <> go locals e <> goType locals t
            APP _ e@(Ann _ LAM{} _) t -> self <> go locals e <> goType locals t
            -- (letrec x : T = Λ ...) e
            -- This is the same as the letrec case above, but for Λ
            APP _ e1@(Letrec _ x _ _ LAM{}) e4 ->
              (self `munless` Set.member x (freeVarsTy e4)) <> go locals e1 <> goType locals e4
            APP _ e t -> go locals e <> goType locals t
            Var _ x
              | Set.member x locals -> self
              | otherwise -> mempty
            GlobalVar _ _ -> self
            -- Note that x is in scope in e2 but not e1.
            Let _ x e1 e2 ->
              let locals' = Set.insert x locals
               in go locals e1 <> go locals' e2 <> (self `munless` Set.member x (freeVars e2))
            -- Whereas here, x is in scope in both e1 and e2.
            Letrec _ x e1 t e2 ->
              let locals' = Set.insert x locals
               in go locals' e1 <> go locals' e2 <> goType locals t <> (self `munless` Set.member x (freeVars e2))
            -- As with Let, x is in scope in e but not in t
            LetType _ x _t e ->
              let locals' = Set.insert x locals
               in go locals' e <> self `munless` Set.member x (freeVars e)
            Lam _ x e -> go (Set.delete x locals) e
            LAM _ x e -> go (Set.delete x locals) e
            EmptyHole{} -> mempty
            Hole _ e -> go locals e
            Ann _ e t -> go locals e <> goType locals t
            Con{} -> mempty
            Case _ e branches ->
              let branchRedexes (CaseBranch _ binds rhs) =
                    let locals' = Set.difference locals (Set.fromList (map bindName binds))
                     in go locals' rhs
                  scrutRedex = case unfoldAPP $ fst $ unfoldApp $ removeAnn e of
                    (Con{}, _) -> self
                    _ -> mempty
               in scrutRedex <> go locals e <> mconcat (map branchRedexes branches)
            PrimCon{} -> mempty
    goType locals ty =
      -- A set containing just the ID of this type
      let self = Set.singleton (ty ^. _id)
       in case ty of
            TEmptyHole _ -> mempty
            THole _ t -> goType locals t
            TVar _ x
              | Set.member x locals -> self
              | otherwise -> mempty
            TCon _ _ -> mempty
            TFun _ a b -> goType locals a <> goType locals b
            TApp _ a b -> goType locals a <> goType locals b
            TForall _ x _ t -> goType (Set.delete x locals) t

-- | Extract the cached type information from the metadata of an AST node.
annOf :: Meta a -> a
annOf = view (position @2)

-- | Set the cached type information of the root node of the given expression to the given value.
annotate :: Maybe TypeCache -> Expr -> Expr
annotate = set (position @1 % position @2)

-- | This function helps us to convert a λ or Λ application into a let binding without causing
-- variable capture. It takes as arguments:
-- - the name of the lambda binding
-- - the free variables in the application argument
-- - the body of the lambda
-- It will then modify the original name until it finds one that:
-- - doesn't clash with any free variables in the argument
-- - can be safely used instead of the original name in the lambda body
--
-- We assume that the original name is safe to use in the lambda body (as you'd expect), so we
-- return it unchanged if it doesn't clash with a free variable in the argument.
--
-- See 'Tests.Eval.unit_tryReduce_beta_name_clash' for an example of where this is useful.
makeSafeLetBinding :: Name -> Set Name -> Expr -> (Name, Expr)
makeSafeLetBinding name others body | Set.notMember name others = (name, body)
makeSafeLetBinding name others body = go 0
  where
    go :: Int -> (Name, Expr)
    go n =
      let newName = unsafeMkName $ unName name <> show n
       in if Set.member newName others
            then go (n + 1)
            else case renameVar name newName body of
              Just body' -> (newName, body')
              Nothing -> go (n + 1)

-- | Given a context of local and global variables and an expression, try to reduce that expression.
-- Expects that the expression is redex and will throw an error if not.
-- TODO: consider using view patterns for these cases
tryReduceExpr ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Globals ->
  Locals ->
  Expr ->
  m (Expr, EvalDetail)
tryReduceExpr globals locals = \case
  -- Beta reduction (no annotation)
  -- (\x. e1) e2 ==> let x = e2 in e1
  App mApp lam@(Lam _ x body) arg -> do
    let (x', body') = makeSafeLetBinding x (freeVars arg) body
    expr <- annotate (annOf mApp) <$> let_ x' (pure arg) (pure body')
    pure
      ( expr
      , BetaReduction
          BetaReductionDetail
            { betaBefore = App mApp lam arg
            , betaAfter = expr
            , betaBindingName = x
            , betaLambdaID = lam ^. _id
            , betaLetID = expr ^. _id
            , betaArgID = arg ^. _id
            , betaBodyID = body ^. _id
            , betaTypes = Nothing
            }
      )
  -- Beta reduction (with annotation)
  -- (\x. e1 : A -> B) e2 ==> let x = e2 : A in e1 : B
  App mApp annotation@(Ann _ lam@(Lam _ x body) ty) arg -> do
    let (x', body') = makeSafeLetBinding x (freeVars arg) body
        -- The annotation is a hole. This means we can't trust that the lambda has the right type for
        -- this context. Specifically:
        -- - the argument may not have the right type for the lambda body
        -- - the lambda body may not have the right type for the application
        -- To deal with this, we put both the argument and the body in holes.
        -- TODO: explain this in the detail view.
        holeAnn = do
          lty <- tEmptyHole
          rty <- tEmptyHole
          l <- let_ x' (hole (pure arg)) (pure body')
          e <- annotate (annOf mApp) <$> hole (pure l)
          pure (e, l, Just (lty, rty))
    (expr, letexpr, types) <- case ty of
      -- The annotation is a function type, as expected
      (TFun _ lty rty) -> do
        l <- let_ x' (ann (pure arg) (pure lty)) (pure body')
        e <- annotate (annOf mApp) <$> ann (pure l) (pure rty)
        pure (e, l, Just (lty, rty))
      TEmptyHole _ -> holeAnn
      THole _ _ -> holeAnn
      -- The annotation is of some other form, which we can't handle
      _ -> throwError $ BadLambdaAnnotation annotation
    pure
      ( expr
      , BetaReduction
          BetaReductionDetail
            { betaBefore = App mApp annotation arg
            , betaAfter = expr
            , betaBindingName = x
            , betaLambdaID = lam ^. _id
            , betaLetID = letexpr ^. _id
            , betaArgID = arg ^. _id
            , betaBodyID = body ^. _id
            , betaTypes = types
            }
      )
  -- (letrec x : T = λ ...) e
  before@(App mApp (Letrec mLet x e1 t lam@Lam{}) e2) | Set.notMember x (freeVars e2) -> do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step.
    let expr = annotate (annOf mApp) $ Letrec mLet x e1 t (App mApp lam e2)
    pure
      ( expr
      , PushAppIntoLetrec
          PushAppIntoLetrecDetail
            { pushAppIntoLetrecBefore = before
            , pushAppIntoLetrecAfter = expr
            , pushAppIntoLetrecArgID = e2 ^. _id
            , pushAppIntoLetrecLetrecID = mLet ^. _id
            , pushAppIntoLetrecLamID = lam ^. _id
            , pushAppIntoLetrecLetBindingName = x
            , pushAppIntoLetrecIsTypeApplication = False
            }
      )

  -- apply primitive function
  before@App{}
    | (Var _ fName, args) <- unfoldApp before
      , Just PrimFun{primFunDef, primFunType} <- Map.lookup fName globalPrims
      , TFun _ lhs rhs <- fst $ create primFunType
      , length args == length (fst $ unfoldFun lhs rhs)
      , all isNormalForm args ->
      case primFunDef args of
        Left err -> throwError $ PrimFunError err
        Right e -> do
          expr <- e
          pure
            ( expr
            , ApplyPrimFun
                ApplyPrimFunDetail
                  { applyPrimFunBefore = before
                  , applyPrimFunAfter = expr
                  , applyPrimFunName = fName
                  , applyPrimFunArgIDs = args ^. mapping _id
                  }
            )
    where
      isNormalForm = \case
        PrimCon _ _ -> True
        Con _ _ -> True
        App _ f x -> isNormalForm f && isNormalForm x
        _ -> False

  -- Beta reduction of an inner application
  -- This rule is theoretically redundant but because we render nested applications with just one
  -- 'App' node in the tupled style, the user can only select the top-most application even if the
  -- redex is in an inner application. So if we're given an application we must try to reduce its
  -- left hand side before giving up.
  --   a ==> b
  -- -----------
  -- a c ==> b c
  App mApp e1 e2 -> do
    -- Try to reduce e1
    (e1', detail) <- tryReduceExpr globals locals e1
    pure (App mApp e1' e2, detail)
  -- Beta reduction of big lambda (no annotation)
  -- (Λx. e) t ==> let type x = t in e
  APP mAPP lam@(LAM _ x body) arg -> do
    let (x', body') = makeSafeLetBinding x (freeVarsTy arg) body
    expr <- annotate (annOf mAPP) <$> letType x' (pure arg) (pure body')
    pure
      ( expr
      , BETAReduction
          BetaReductionDetail
            { betaBefore = APP mAPP lam arg
            , betaAfter = expr
            , betaBindingName = x
            , betaLambdaID = lam ^. _id
            , betaLetID = expr ^. _id
            , betaArgID = arg ^. _id
            , betaBodyID = body ^. _id
            , betaTypes = Nothing
            }
      )
  -- Beta reduction of big lambda (with annotation)
  -- With the current editor K is always KType, so the annotation is a
  -- bit pointless, but we include this rule for completeness.
  -- This is what we technically should do:
  --   (Λx. e : ∀a : K. B) t ==> let type x = t in e : [t/a]B
  -- But performing the substitution [t/a]B is a bit of a pain when you have to ensure ID uniqueness
  -- and worry about other metadata, so for simplicity we just drop the annotation.
  -- This might change in future if we decide we want to keep it.
  -- So this is what we actually do:
  --   (Λx. e : ∀a : K. B) t ==> let type x = t in e
  APP mAPP annotation@(Ann _ lam@(LAM _ x body) ty) t -> do
    let (x', body') = makeSafeLetBinding x (freeVarsTy t) body
    case ty of
      (TForall _ _ k b) -> do
        expr <- annotate (annOf mAPP) <$> letType x' (pure t) (pure body')
        pure
          ( expr
          , BETAReduction
              BetaReductionDetail
                { betaBefore = APP mAPP annotation t
                , betaAfter = expr
                , betaBindingName = x
                , betaLambdaID = lam ^. _id
                , betaLetID = expr ^. _id
                , betaArgID = t ^. _id
                , betaBodyID = body ^. _id
                , betaTypes = Just (k, b)
                }
          )
      _ -> throwError $ BadBigLambdaAnnotation annotation
  -- (letrec x : T = Λ ...) e
  before@(APP mApp (Letrec mLet x e1 t lam@LAM{}) e2) | Set.notMember x (freeVarsTy e2) -> do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step.
    let expr = annotate (annOf mApp) $ Letrec mLet x e1 t (APP mApp lam e2)
    pure
      ( expr
      , PushAppIntoLetrec
          PushAppIntoLetrecDetail
            { pushAppIntoLetrecBefore = before
            , pushAppIntoLetrecAfter = expr
            , pushAppIntoLetrecArgID = e2 ^. _id
            , pushAppIntoLetrecLetrecID = mLet ^. _id
            , pushAppIntoLetrecLamID = lam ^. _id
            , pushAppIntoLetrecLetBindingName = x
            , pushAppIntoLetrecIsTypeApplication = True
            }
      )
  -- Beta reduction of an inner big lambda application
  -- This rule is theoretically redundant but because we render nested applications with just one
  -- 'APP' node in the tupled style, the user can only select the top-most application even if the
  -- redex is in an inner application. So if we're given an application we must try to reduce its
  -- left hand side before giving up.
  --   a ==> b
  -- -----------
  -- a c ==> b c
  APP mAPP e t -> do
    -- Try to reduce e
    (e', detail) <- tryReduceExpr globals locals e
    pure (APP mAPP e' t, detail)
  -- Inline local variable
  -- x=e |- x ==> e
  -- If the variable is not in the local set, that's fine - it just means it is bound by a lambda
  -- that hasn't yet been reduced.
  Var mVar x
    | Just (i, Left e) <- Map.lookup x locals -> do
      -- Since we're duplicating @e@, we must regenerate all its IDs.
      e' <- regenerateExprIDs e
      pure
        ( e'
        , LocalVarInline
            LocalVarInlineDetail
              { localVarInlineLetID = i
              , localVarInlineVarID = mVar ^. _id
              , localVarInlineValueID = e ^. _id
              , localVarInlineBindingName = x
              , localVarInlineReplacementID = e' ^. _id
              , localVarInlineIsTypeVar = False
              }
        )
  -- Inline global variable
  -- (f = e : t) |- f ==> e : t
  GlobalVar mVar i | Just def <- Map.lookup i globals -> do
    -- Since we're duplicating the definition, we must regenerate all its IDs.
    e <- regenerateExprIDs (defExpr def)
    t <- regenerateTypeIDs (defType def)
    expr <- ann (pure e) (pure t)
    pure
      ( expr
      , GlobalVarInline
          GlobalVarInlineDetail
            { globalVarInlineVar = GlobalVar mVar i
            , globalVarInlineDef = def
            , globalVarInlineAfter = expr
            }
      )
  -- Redundant let removal
  -- let x = e1 in e2 ==> e2    if x not free in e2
  expr@(Let meta x _ body)
    | Set.notMember x (freeVars body) -> mkLetRemovalDetail expr body x meta
  -- Redundant letrec removal
  -- letrec x = e in e2 ==> e2  if x not free in e2
  expr@(Letrec meta x _ _ body)
    | Set.notMember x (freeVars body) -> mkLetRemovalDetail expr body x meta
  -- Redundant letType removal
  -- let type x = t in e ==> e  if x not free in e
  expr@(LetType meta x _ body)
    | Set.notMember x (freeVars body) -> mkLetRemovalDetail expr body x meta
  -- Case reduction
  -- If the scrutinee starts with a constructor, we can reduce the case expression.
  -- We do that by picking the branch with the same constructor.
  -- For each variable bound by the branch pattern, we create a let with the same name, binding the
  -- corresponding argument in the scrutinee.
  Case m scrut branches
    | (expr, termArgs) <- unfoldApp (removeAnn scrut)
      , (Con mCon c, _typeArgs) <- unfoldAPP expr ->
      do
        -- Find the branch with the same constructor
        case find (\(CaseBranch n _ _) -> n == c) branches of
          Just (CaseBranch _ binds rhs) -> do
            -- Check that we have as many term args as bindings
            when (length binds /= length termArgs) $ throwError CaseBranchBindingLengthMismatch
            -- Construct a let for each bind
            let makeLet (Bind _ x, e) rest = let_ x (pure e) (pure rest)
            (expr', letIDs) <-
              foldrM
                ( \a (e, lets) -> do
                    l <- makeLet a e
                    pure (l, l ^. _id : lets)
                )
                (rhs, [])
                (zip binds termArgs)
            pure
              ( expr'
              , CaseReduction
                  CaseReductionDetail
                    { caseBefore = Case m scrut branches
                    , caseAfter = expr'
                    , caseTargetID = scrut ^. _id
                    , caseTargetCtorID = mCon ^. _id
                    , caseCtorName = c
                    , caseTargetArgIDs = map (^. _id) termArgs
                    , caseBranchBindingIDs = map (^. _id) binds
                    , caseBranchRhsID = rhs ^. _id
                    , caseLetIDs = letIDs
                    }
              )
          Nothing -> throwError NoMatchingCaseBranch
  _ -> throwError NotRedex
  where
    mkLetRemovalDetail expr body x meta =
      pure
        ( body
        , LetRemoval
            LetRemovalDetail
              { letRemovalBefore = expr
              , letRemovalAfter = body
              , letRemovalBindingName = x
              , letRemovalLetID = meta ^. _id
              , letRemovalBodyID = body ^. _id
              }
        )

tryReduceType ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Globals ->
  Locals ->
  Type ->
  m (Type, EvalDetail)
tryReduceType _globals locals = \case
  -- Inline local type variable
  -- x=t |- x ==> t
  -- If the variable is not in the local set, that's fine - it just means it is bound by a big
  -- lambda that hasn't yet been reduced.
  TVar mTVar x
    | Just (i, Right t) <- Map.lookup x locals -> do
      -- Since we're duplicating @t@, we must regenerate all its IDs.
      t' <- regenerateTypeIDs t
      pure
        ( t'
        , LocalTypeVarInline
            LocalVarInlineDetail
              { localVarInlineLetID = i
              , localVarInlineVarID = mTVar ^. _id
              , localVarInlineValueID = t ^. _id
              , localVarInlineBindingName = x
              , localVarInlineReplacementID = t' ^. _id
              , localVarInlineIsTypeVar = True
              }
        )
  _ -> throwError NotRedex

-- Traverse a type, regenerating all its IDs
regenerateTypeIDs :: (HasID a, MonadFresh ID m) => Type' a -> m (Type' a)
regenerateTypeIDs = traverseOf (_typeMeta % _id) (const fresh)

-- Traverse an expression, regenerating all its IDs
regenerateExprIDs :: (HasID a, HasID b, MonadFresh ID m) => Expr' a b -> m (Expr' a b)
regenerateExprIDs =
  traverseOf (_exprMeta % _id) (const fresh)
    >=> traverseOf (_exprTypeMeta % _id) (const fresh)

-- | @x `munless` b@ is `x` if `b` is 'False', otherwise it is 'mempty'.
-- It's like 'Control.Monad.unless' but for Monoids rather than Applicatives.
munless :: Monoid a => a -> Bool -> a
munless x b = if b then mempty else x
