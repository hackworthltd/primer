{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
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
  LetRenameDetail (..),
  PushAppIntoLetrecDetail (..),
  ApplyPrimFunDetail (..),
  Locals,
  tryPrimFun,
  -- Only exported for testing
  tryReduceExpr,
  tryReduceType,
  findNodeByID,
) where

import Foreword

import Control.Arrow ((***))
import Control.Monad.Fresh (MonadFresh)
import Data.Generics.Product (position)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Optics (
  Field1 (_1),
  elemOf,
  filtered,
  getting,
  mapping,
  notElemOf,
  set,
  to,
  view,
  (%),
  (^.),
  (^..),
  _2,
 )
import Primer.Core (
  ASTDef (..),
  Bind' (..),
  CaseBranch' (..),
  Def (..),
  DefMap,
  Expr,
  Expr' (..),
  ExprAnyFresh (..),
  GVarName,
  HasID (_id),
  ID,
  Kind,
  LVarName,
  LocalName (LocalName, unLocalName),
  LocalNameKind (..),
  Meta,
  PrimDef (..),
  PrimFun (..),
  PrimFunError (..),
  TmVarRef (..),
  TyVarName,
  Type,
  Type' (..),
  TypeCache,
  ValConName,
  bindName,
  defPrim,
  getID,
 )
import Primer.Core.DSL (ann, hole, letType, let_, tEmptyHole)
import Primer.Core.Transform (removeAnn, renameLocalVar, renameTyVarExpr, unfoldAPP, unfoldApp)
import Primer.Core.Utils (
  concreteTy,
  forgetIDs,
  freeVars,
  freeVarsTy,
  regenerateExprIDs,
  regenerateTypeIDs,
  _freeTmVars,
  _freeTyVars,
  _freeVars,
  _freeVarsTy,
 )
import Primer.JSON
import Primer.Name (Name, unName, unsafeMkName)
import Primer.Primitives (allPrimDefs)
import Primer.Zipper (
  ExprZ,
  Loc' (InBind, InExpr, InType),
  TypeZ,
  current,
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
    BetaReduction (BetaReductionDetail 'ATmVar Type Type)
  | -- | Reduction of (Λx. a) b
    BETAReduction (BetaReductionDetail 'ATyVar Kind Type)
  | -- | Inlining of a local variable
    LocalVarInline (LocalVarInlineDetail 'ATmVar)
  | -- | Inlining of a local type variable
    LocalTypeVarInline (LocalVarInlineDetail 'ATyVar)
  | -- | ID of definition, name of variable
    GlobalVarInline GlobalVarInlineDetail
  | -- | ID of let(rec)
    LetRemoval LetRemovalDetail
  | -- | Renaming of binding in let x = ...x... in ...x...x...
    LetRename LetRenameDetail
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
-- - i.e. k ~ ATmVar, domain ~ Type, codomain ~ Type
-- If Λ:
-- - 'betaLambdaID' is the ID of the Λ
-- - 'betaLetID' is the ID of the "let type"
-- - 'betaTypes' is optionally the domain kind and codomain type of the λ
-- - i.e. k ~ ATyVar, domain ~ Kind, codomain ~ Type
data BetaReductionDetail k domain codomain = BetaReductionDetail
  { betaBefore :: Expr
  , betaAfter :: Expr
  , betaBindingName :: LocalName k
  , betaLambdaID :: ID
  , betaLetID :: ID
  , betaArgID :: ID
  , betaBodyID :: ID
  , betaTypes :: Maybe (domain, codomain)
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "beta" (BetaReductionDetail k domain codomain)

data LocalVarInlineDetail k = LocalVarInlineDetail
  { localVarInlineLetID :: ID
  -- ^ ID of the let expression that binds this variable
  , localVarInlineVarID :: ID
  -- ^ ID of the variable being replaced
  , localVarInlineBindingName :: LocalName k
  -- ^ Name of the variable
  , localVarInlineValueID :: ID
  -- ^ ID of the expression or type that the variable is bound to
  , localVarInlineReplacementID :: ID
  -- ^ ID of the expression or type that has replaced the variable in the result
  , localVarInlineIsTypeVar :: Bool
  -- ^ If 'True', the variable being inlined is a type variable.
  -- Otherwise it is a term variable.
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "localVarInline" (LocalVarInlineDetail k)

data GlobalVarInlineDetail = GlobalVarInlineDetail
  { globalVarInlineDef :: ASTDef
  -- ^ The definition that the variable refers to
  , globalVarInlineVar :: Expr
  -- ^ The variable being replaced
  , globalVarInlineAfter :: Expr
  -- ^ The result of the reduction
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "globalVarInline" GlobalVarInlineDetail

data CaseReductionDetail = CaseReductionDetail
  { caseBefore :: Expr
  -- ^ the case expression before reduction
  , caseAfter :: Expr
  -- ^ the resulting expression after reduction
  , caseTargetID :: ID
  -- ^ the ID of the target (scrutinee)
  , caseTargetCtorID :: ID
  -- ^ the ID of the constructor node in the target
  , caseCtorName :: ValConName
  -- ^ the name of the matching constructor
  , caseTargetArgIDs :: [ID]
  -- ^ the arguments to the constructor in the target
  , caseBranchBindingIDs :: [ID]
  -- ^ the bindings in the case branch (one for each arg above)
  , caseBranchRhsID :: ID
  -- ^ the right hand side of the selected case branch
  , caseLetIDs :: [ID]
  -- ^ the let expressions binding each argument in the result
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "case" CaseReductionDetail

data LetRemovalDetail = LetRemovalDetail
  { letRemovalBefore :: Expr
  -- ^ the let expression before reduction
  , letRemovalAfter :: Expr
  -- ^ the resulting expression after reduction
  , letRemovalBindingName :: Name
  -- ^ the name of the unused bound variable (either term or type variable)
  , letRemovalLetID :: ID
  -- ^ the full let expression
  , letRemovalBodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "letRemoval" LetRemovalDetail

data LetRenameDetail = LetRenameDetail
  { letRenameBefore :: Expr
  -- ^ the let expression before reduction
  , letRenameAfter :: Expr
  -- ^ the resulting expression after reduction
  , letRenameBindingNameOld :: Name
  -- ^ the old name of the let-bound variable
  , letRenameBindingNameNew :: Name
  -- ^ the new name of the let-bound variable
  , letRenameLetID :: ID
  -- ^ the full let expression
  , letRenameBindingOccurrences :: [ID]
  -- ^ where the old name occurred inside the bound expression
  , letRenameBodyID :: ID
  -- ^ the right hand side of the let
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "letRename" LetRenameDetail

data PushAppIntoLetrecDetail = PushAppIntoLetrecDetail
  { pushAppIntoLetrecBefore :: Expr
  -- ^ the expression before reduction
  , pushAppIntoLetrecAfter :: Expr
  -- ^ the expression after reduction
  , pushAppIntoLetrecArgID :: ID
  -- ^ the ID of the argument to the application
  , pushAppIntoLetrecLetrecID :: ID
  -- ^ the ID of the letrec
  , pushAppIntoLetrecLamID :: ID
  -- ^ the ID of the lambda
  , pushAppIntoLetrecLetBindingName :: LVarName
  -- ^ The name of the variable bound by the letrec
  , pushAppIntoLetrecIsTypeApplication :: Bool
  -- ^ If 'True', the application is of a big lambda to a type.
  -- Otherwise it is of a small lambda to a term.
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "pushAppIntoLetrec" PushAppIntoLetrecDetail

data ApplyPrimFunDetail = ApplyPrimFunDetail
  { applyPrimFunBefore :: Expr
  -- ^ the expression before reduction
  , applyPrimFunAfter :: Expr
  -- ^ the expression after reduction
  , applyPrimFunName :: GVarName
  -- ^ the name of the primitive function
  , applyPrimFunArgIDs :: [ID]
  -- ^ the IDs of the arguments to the application
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via VJSONPrefix "applyPrimFun" ApplyPrimFunDetail

-- | A map from local variable names to the ID of their binding and their bound value.
-- Since each entry must have a value, this only includes let(rec) bindings.
-- Lambda bindings must be reduced to a let before their variables can appear here.
type Locals = Map Name (ID, Either Expr Type)

-- | Perform one step of reduction on the node with the given ID
-- Returns the new expression and its redexes.
step ::
  MonadFresh ID m =>
  DefMap ->
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
-- Returns Nothing if the node is a binding, because no reduction rules can apply there.
findNodeByID :: ID -> Expr -> Maybe (Locals, Either ExprZ TypeZ)
findNodeByID i expr = do
  loc <- focusOn i expr
  case loc of
    InExpr z ->
      let locals = foldAbove collectBinding z
       in pure (locals, Left z)
    InType z ->
      let -- Since we are only collecting various sorts of let bindings,
          -- we don't need to look in types, as they cannot contain let bindings
          locals = foldAbove collectBinding (unfocusType z)
       in pure (locals, Right z)
    InBind{} -> Nothing
  where
    collectBinding a = case (current a, prior a) of
      (Let m x e1 e2, e2') | e2 == e2' -> Map.singleton (unLocalName x) (view _id m, Left e1)
      -- Note that because @x@ is in scope in @e1@, we will allow @e1@ to be reduced even if this
      -- reduction may never terminate.
      -- See https://github.com/hackworthltd/primer/issues/4
      -- @x@ is not in scope in @t@.
      (Letrec m x e1 _t e2, e')
        | e2 == e' -> Map.singleton (unLocalName x) (view _id m, Left e1)
        | e1 == e' -> Map.singleton (unLocalName x) (view _id m, Left e1)
      (LetType m x t _, _) -> Map.singleton (unLocalName x) (view _id m, Right t)
      _ -> mempty

-- | Return the IDs of nodes which are reducible.
-- We assume the expression is well scoped, and do not e.g. check whether
-- @e@ refers to a type variable @x@ when deciding if we can reduce a
-- @let x = _ in e@ (we of course check whether @e@ refers to a term variable
-- @x@)
redexes :: Map GVarName PrimDef -> Expr -> Set ID
redexes primDefs = go mempty
  where
    -- letTm and letTy track the set of local variables we have a definition for
    go locals@(letTm, letTy) expr =
      -- A set containing just the ID of this expression
      let self = Set.singleton (expr ^. _id)
          member = Set.member . unLocalName
          member' x s = member x $ Set.map unLocalName s
          freeTmVar = elemOf $ getting _freeTmVars % _2
          freeTyVar = elemOf $ getting _freeTyVars % _2
       in case expr of
            -- Application nodes are reducible only if their left child is a λ node or an annotation
            -- wrapping a λ node.
            -- (λ ...) x
            App _ e1@Lam{} e2 -> self <> go locals e1 <> go locals e2
            -- (λ ... : T) x
            App _ e1@(Ann _ Lam{} _) e2 -> self <> go locals e1 <> go locals e2
            -- (letrec x : T = t in λ ...) e
            -- We can reduce an application across a letrec as long as x isn't a free variable in e.
            -- If it was, it would be a different x and we'd cause variable capture if we
            -- substituted e into the λ body.
            App _ e1@(Letrec _ x _ _ Lam{}) e4 ->
              (self `munless` member x (freeVars e4)) <> go locals e1 <> go locals e4
            -- Application of a primitive (fully-applied, with all arguments in normal form).
            App{} | Just _ <- tryPrimFun primDefs expr -> self
            -- f x
            App _ e1 e2 -> go locals e1 <> go locals e2
            APP _ e@LAM{} t -> self <> go locals e <> goType letTy t
            APP _ e@(Ann _ LAM{} _) t -> self <> go locals e <> goType letTy t
            -- (letrec x : T = t in Λ ...) e
            -- This is the same as the letrec case above, but for Λ
            APP _ e1@(Letrec _ x _ _ LAM{}) e4 ->
              (self `munless` member' x (freeVarsTy e4)) <> go locals e1 <> goType letTy e4
            APP _ e t -> go locals e <> goType letTy t
            Var _ (LocalVarRef x)
              | Set.member x letTm -> self
              | otherwise -> mempty
            Var _ (GlobalVarRef x)
              | Map.member x primDefs -> mempty
              | otherwise -> self
            -- Note that x is in scope in e2 but not e1.
            Let _ x e1 e2 ->
              -- If we have something like let x = f x in (x,x), then we cannot
              -- substitute each occurrence individually, since the let would
              -- capture the new 'x' in 'f x'. We first rename to
              -- let y = f x in (y,y)
              let selfCapture = Set.member (unLocalName x) $ freeVars e1
                  locals' =
                    ( if selfCapture then letTm else Set.insert x letTm
                    , letTy
                    )
               in go locals e1 <> go locals' e2 <> (self `munless` (not selfCapture && freeTmVar x e2))
            -- Whereas here, x is in scope in both e1 and e2.
            Letrec _ x e1 t e2 ->
              let locals' = (Set.insert x letTm, letTy)
               in go locals' e1 <> go locals' e2 <> goType letTy t <> (self `munless` freeTmVar x e2)
            -- As with Let, x is in scope in e but not in t
            LetType _ x t e ->
              -- We need to be careful that the LetType will not capture a
              -- variable occurrence arising from any potential substitution
              -- of itself. See the comment on 'Let' above for an example.
              let selfCapture = Set.member x $ freeVarsTy t
                  locals' = (letTm, if selfCapture then letTy else Set.insert x letTy)
               in goType (snd locals) t <> go locals' e <> self `munless` (not selfCapture && freeTyVar x e)
            Lam _ x e -> go (Set.delete x letTm, letTy) e
            LAM _ x e -> go (letTm, Set.delete x letTy) e
            EmptyHole{} -> mempty
            Hole _ e -> go locals e
            Ann _ e t -> go locals e <> goType letTy t
            Con{} -> mempty
            Case _ e branches ->
              let branchRedexes (CaseBranch _ binds rhs) =
                    let locals' = (Set.difference letTm (Set.fromList (map bindName binds)), letTy)
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

-- | This function helps us to convert a λ application into a let binding without causing
-- variable capture. It takes as arguments:
-- - the name of the lambda binding
-- - the free (type and term) variables in the application argument
-- - the body of the lambda
-- It will then modify the original name until it finds one that:
-- - doesn't clash with any free variables in the argument
-- - can be safely used instead of the original name in the lambda body
--
-- We assume that the original name is safe to use in the lambda body (as you'd expect), so we
-- return it unchanged if it doesn't clash with a free variable in the argument.
--
-- See 'Tests.Eval.unit_tryReduce_beta_name_clash' for an example of where this is useful.
makeSafeLetBinding :: LVarName -> Set Name -> Expr -> (LVarName, Expr)
makeSafeLetBinding = makeSafeLetBinding' renameLocalVar

-- | As 'makeSafeLetBinding', but for Λ applications
makeSafeLetTypeBinding :: TyVarName -> Set Name -> Expr -> (TyVarName, Expr)
makeSafeLetTypeBinding = makeSafeLetBinding' renameTyVarExpr

-- Helper for makeSafeLet{,Type}Binding
makeSafeLetBinding' ::
  (LocalName k -> LocalName k -> Expr -> Maybe Expr) ->
  LocalName k ->
  Set Name ->
  Expr ->
  (LocalName k, Expr)
makeSafeLetBinding' _ name others body | Set.notMember (unLocalName name) others = (name, body)
makeSafeLetBinding' rename name others body = go 0
  where
    go :: Int -> (LocalName k, Expr)
    go n =
      let newName' = unsafeMkName $ unName (unLocalName name) <> show n
          newName = LocalName newName'
       in if Set.member newName' others
            then go (n + 1)
            else case rename name newName body of
              Just body' -> (newName, body')
              Nothing -> go (n + 1)

-- | Given a context of local and global variables and an expression, try to reduce that expression.
-- Expects that the expression is redex and will throw an error if not.
-- TODO: consider using view patterns for these cases
tryReduceExpr ::
  (MonadFresh ID m, MonadError EvalError m) =>
  DefMap ->
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
  -- (letrec x : T = t in λ ...) e
  before@(App mApp (Letrec mLet x e1 t lam@Lam{}) e2) | notMember x (freeVars e2) -> do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step. This does not cause capture, as we have checked that x is not free in e2.
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
    | Just (name, args, ExprAnyFresh e) <- tryPrimFun (Map.mapMaybe defPrim globals) before -> do
        expr <- e
        pure
          ( expr
          , ApplyPrimFun
              ApplyPrimFunDetail
                { applyPrimFunBefore = before
                , applyPrimFunAfter = expr
                , applyPrimFunName = name
                , applyPrimFunArgIDs = args ^. mapping _id
                }
          )

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
    let (x', body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy arg) body
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
    let (x', body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy t) body
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
  -- (letrec x : T = t in Λ ...) e
  before@(APP mApp (Letrec mLet x e1 t lam@LAM{}) e2) | notMember' x (freeVarsTy e2) -> do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step. This does not cause capture, as we have checked that x is not free in e2.
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
  Var mVar (LocalVarRef x)
    | Just (i, Left e) <- Map.lookup (unLocalName x) locals -> do
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
  Var mVar (GlobalVarRef x) | Just (DefAST def) <- Map.lookup x globals -> do
    -- Since we're duplicating the definition, we must regenerate all its IDs.
    e <- regenerateExprIDs (astDefExpr def)
    t <- regenerateTypeIDs (astDefType def)
    expr <- ann (pure e) (pure t)
    pure
      ( expr
      , GlobalVarInline
          GlobalVarInlineDetail
            { globalVarInlineVar = Var mVar (GlobalVarRef x)
            , globalVarInlineDef = def
            , globalVarInlineAfter = expr
            }
      )
  expr@(Let meta x e body)
    -- Redundant let removal
    -- let x = e1 in e2 ==> e2    if x not free in e2
    | notElemOf (getting _freeTmVars % _2) x body -> mkLetRemovalDetail expr body x meta
    -- Renaming a potentially self-capturing let
    -- let x = f[x] in g[x] ==> let y = f[x] in g[y]
    | otherwise -> mkLetRenameDetail expr body (Left (x, e)) meta
  expr@(Letrec meta x _ _ body)
    -- Redundant letrec removal
    -- letrec x = e in e2 ==> e2  if x not free in e2
    | notElemOf (getting _freeTmVars % _2) x body -> mkLetRemovalDetail expr body x meta
  expr@(LetType meta x t body)
    -- Redundant letType removal
    -- let type x = t in e ==> e  if x not free in e
    | notElemOf (getting _freeTyVars % _2) x body -> mkLetRemovalDetail expr body x meta
    -- Renaming a potentially self-capturing letType
    -- let type x = f[x] in g[x] ==> let type y = f[x] in g[y]
    | otherwise -> mkLetRenameDetail expr body (Right (x, t)) meta
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
              -- We need to rename bindings to avoid variable capture.
              -- See Note [Case reduction and variable capture]
              -- in EvalFull
              let (rhs', binds') = mapAccumR (\r (Bind _ x) -> swap $ makeSafeLetBinding x (foldMap freeVars termArgs) r) rhs binds
              -- Construct a let for each bind
              let makeLet (x, e) rest = let_ x (pure e) (pure rest)
              (expr', letIDs) <-
                foldrM
                  ( \a (e, lets) -> do
                      l <- makeLet a e
                      pure (l, l ^. _id : lets)
                  )
                  (rhs', [])
                  (zip binds' termArgs)
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
    notMember = Set.notMember . unLocalName
    notMember' n = notMember n . Set.map unLocalName
    mkLetRemovalDetail expr body x meta =
      pure
        ( body
        , LetRemoval
            LetRemovalDetail
              { letRemovalBefore = expr
              , letRemovalAfter = body
              , letRemovalBindingName = unLocalName x
              , letRemovalLetID = meta ^. _id
              , letRemovalBodyID = body ^. _id
              }
        )
    mkLetRenameDetail expr body binding meta = do
      (x, y, occ, expr') <- case binding of
        Left (x, e) -> do
          let (y, body') = makeSafeLetBinding x (freeVars e) body
          let idName = either (getID *** unLocalName) (getID *** unLocalName)
          let occ = e ^.. _freeVars % to idName % filtered ((== unLocalName x) . snd) % _1
          (unLocalName x,unLocalName y,occ,) <$> let_ y (pure e) (pure body')
        Right (x, ty) -> do
          let (y, body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy ty) body
          let occ = ty ^.. getting _freeVarsTy % to (first getID) % filtered ((== x) . snd) % _1
          (unLocalName x,unLocalName y,occ,) <$> letType y (pure ty) (pure body')
      pure
        ( expr'
        , LetRename
            LetRenameDetail
              { letRenameBefore = expr
              , letRenameAfter = expr'
              , letRenameBindingNameOld = x
              , letRenameBindingNameNew = y
              , letRenameLetID = meta ^. _id
              , letRenameBindingOccurrences = occ
              , letRenameBodyID = body ^. _id
              }
        )

tryReduceType ::
  (MonadFresh ID m, MonadError EvalError m) =>
  DefMap ->
  Locals ->
  Type ->
  m (Type, EvalDetail)
tryReduceType _globals locals = \case
  -- Inline local type variable
  -- x=t |- x ==> t
  -- If the variable is not in the local set, that's fine - it just means it is bound by a big
  -- lambda that hasn't yet been reduced.
  TVar mTVar x
    | Just (i, Right t) <- Map.lookup (unLocalName x) locals -> do
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

-- | @x `munless` b@ is `x` if `b` is 'False', otherwise it is 'mempty'.
-- It's like 'Control.Monad.unless' but for Monoids rather than Applicatives.
munless :: Monoid a => a -> Bool -> a
munless x b = if b then mempty else x

-- | If this node is a reducible application of a primitive, return the name of the primitive, the arguments, and
-- (a computation for building) the result.
tryPrimFun :: Map GVarName PrimDef -> Expr -> Maybe (GVarName, [Expr], ExprAnyFresh)
tryPrimFun primDefs expr
  | -- Since no primitive functions are polymorphic, there is no need to unfoldAPP
    (Var _ (GlobalVarRef name), args) <- bimap stripAnns (map stripAnns) $ unfoldApp expr
  , Map.member name primDefs
  , Just PrimFun{primFunDef} <- Map.lookup name allPrimDefs
  , Right e <- primFunDef $ forgetIDs <$> args =
      Just (name, args, e)
  | otherwise = Nothing
  where
    -- We have to be able to apply a primitive in the presence of type annotations.
    -- This is important because other evaluation steps may introduce unnecessary annotations,
    -- so we need to be able to ignore them (as we also do in the case of beta reduction).
    -- During evaluation, we may choose to hide annotations anyway, so they really shouldn't make a difference to
    -- what can be evaluated.
    -- Note that it's only safe to remove concrete annotations, since holes can act as type-changing casts.
    stripAnns = \case
      Ann _ e t | concreteTy t -> stripAnns e
      e -> e
