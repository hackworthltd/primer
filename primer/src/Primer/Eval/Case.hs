{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Case (CaseReductionDetail (..), tryCaseReduction) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Optics ((^.))
import Primer.Core (
  Bind' (Bind),
  CaseBranch' (CaseBranch),
  Expr,
  Expr' (Case, Con),
  ID,
  ValConName,
  _id,
 )
import Primer.Core.DSL (let_)
import Primer.Core.Transform (removeAnn, unfoldAPP, unfoldApp)
import Primer.Core.Utils (freeVars)
import Primer.Eval.EvalError (EvalError (CaseBranchBindingLengthMismatch, NoMatchingCaseBranch))
import Primer.Eval.Utils (makeSafeLetBinding)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

data CaseReductionDetail = CaseReductionDetail
  { before :: Expr
  -- ^ the case expression before reduction
  , after :: Expr
  -- ^ the resulting expression after reduction
  , targetID :: ID
  -- ^ the ID of the target (scrutinee)
  , targetCtorID :: ID
  -- ^ the ID of the constructor node in the target
  , ctorName :: ValConName
  -- ^ the name of the matching constructor
  , targetArgIDs :: [ID]
  -- ^ the arguments to the constructor in the target
  , branchBindingIDs :: [ID]
  -- ^ the bindings in the case branch (one for each arg above)
  , branchRhsID :: ID
  -- ^ the right hand side of the selected case branch
  , letIDs :: [ID]
  -- ^ the let expressions binding each argument in the result
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON CaseReductionDetail

tryCaseReduction ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Expr ->
  Maybe (m (Expr, CaseReductionDetail))
tryCaseReduction = \case
  -- Case reduction
  -- If the scrutinee starts with a constructor, we can reduce the case expression.
  -- We do that by picking the branch with the same constructor.
  -- For each variable bound by the branch pattern, we create a let with the same name, binding the
  -- corresponding argument in the scrutinee.
  Case m scrut branches
    | (expr, termArgs) <- unfoldApp (removeAnn scrut)
    , (Con mCon c, _typeArgs) <- unfoldAPP expr ->
        Just $ do
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
                , CaseReductionDetail
                    { before = Case m scrut branches
                    , after = expr'
                    , targetID = scrut ^. _id
                    , targetCtorID = mCon ^. _id
                    , ctorName = c
                    , targetArgIDs = map (^. _id) termArgs
                    , branchBindingIDs = map (^. _id) binds
                    , branchRhsID = rhs ^. _id
                    , letIDs = letIDs
                    }
                )
            Nothing -> throwError NoMatchingCaseBranch
  _ -> Nothing
