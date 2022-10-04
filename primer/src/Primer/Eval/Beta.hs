module Primer.Eval.Beta (
  BetaReductionDetail (..),
  tryReduceBeta,
  tryReduceBETA,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Set qualified as Set
import Optics ((^.))
import Primer.Core (
  Expr,
  Expr' (APP, Ann, App, LAM, Lam),
  ID,
  Kind,
  LocalName,
  LocalNameKind (ATmVar, ATyVar),
  Type,
  Type' (TEmptyHole, TForall, TFun, THole),
  unLocalName,
  _id,
 )
import Primer.Core.DSL (ann, hole, letType, let_, tEmptyHole)
import Primer.Core.Utils (freeVars, freeVarsTy)
import Primer.Eval.EvalError (EvalError (BadBigLambdaAnnotation, BadLambdaAnnotation))
import Primer.Eval.Utils (annOf, annotate, makeSafeLetBinding, makeSafeLetTypeBinding)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

-- | Detailed information about a beta reduction (of a λ or Λ).
-- If λ:
-- - 'lambdaID' is the ID of the λ
-- - 'letID' is the ID of the let
-- - 'types' is optionally the domain type and codomain type of the λ
-- - i.e. k ~ ATmVar, domain ~ Type, codomain ~ Type
-- If Λ:
-- - 'lambdaID' is the ID of the Λ
-- - 'letID' is the ID of the "let type"
-- - 'types' is optionally the domain kind and codomain type of the λ
-- - i.e. k ~ ATyVar, domain ~ Kind, codomain ~ Type
data BetaReductionDetail k domain codomain = BetaReductionDetail
  { before :: Expr
  , after :: Expr
  , bindingName :: LocalName k
  , lambdaID :: ID
  , letID :: ID
  , argID :: ID
  , bodyID :: ID
  , types :: Maybe (domain, codomain) -- TODO: this is always Just now ?
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (BetaReductionDetail k domain codomain)

tryReduceBeta ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Expr ->
  Maybe (m (Expr, BetaReductionDetail 'ATmVar Type Type))
tryReduceBeta = \case
  -- Beta reduction (no annotation)
  -- (\x. e1) e2 ==> let x = e2 in e1
  App mApp lam@(Lam _ x body) arg -> Just $ do
    let (x', body') = makeSafeLetBinding x (freeVars arg) body
    expr <- annotate (annOf mApp) <$> let_ x' (pure arg) (pure body')
    pure
      ( expr
      , BetaReductionDetail
          { before = App mApp lam arg
          , after = expr
          , bindingName = x
          , lambdaID = lam ^. _id
          , letID = expr ^. _id
          , argID = arg ^. _id
          , bodyID = body ^. _id
          , types = Nothing
          }
      )

  -- Beta reduction (with annotation)
  -- (\x. e1 : A -> B) e2 ==> let x = e2 : A in e1 : B
  App mApp annotation@(Ann _ lam@(Lam _ x body) ty) arg -> Just $ do
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
      , BetaReductionDetail
          { before = App mApp annotation arg
          , after = expr
          , bindingName = x
          , lambdaID = lam ^. _id
          , letID = letexpr ^. _id
          , argID = arg ^. _id
          , bodyID = body ^. _id
          , types = types
          }
      )
  _ -> Nothing

tryReduceBETA ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Expr ->
  Maybe (m (Expr, BetaReductionDetail 'ATyVar Kind Type))
tryReduceBETA = \case
  -- Beta reduction of big lambda (no annotation)
  -- (Λx. e) t ==> let type x = t in e
  APP mAPP lam@(LAM _ x body) arg -> Just $ do
    let (x', body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy arg) body
    expr <- annotate (annOf mAPP) <$> letType x' (pure arg) (pure body')
    pure
      ( expr
      , BetaReductionDetail
          { before = APP mAPP lam arg
          , after = expr
          , bindingName = x
          , lambdaID = lam ^. _id
          , letID = expr ^. _id
          , argID = arg ^. _id
          , bodyID = body ^. _id
          , types = Nothing
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
  APP mAPP annotation@(Ann _ lam@(LAM _ x body) ty) t -> Just $ do
    let (x', body') = makeSafeLetTypeBinding x (Set.map unLocalName $ freeVarsTy t) body
    case ty of
      (TForall _ _ k b) -> do
        expr <- annotate (annOf mAPP) <$> letType x' (pure t) (pure body')
        pure
          ( expr
          , BetaReductionDetail
              { before = APP mAPP annotation t
              , after = expr
              , bindingName = x
              , lambdaID = lam ^. _id
              , letID = expr ^. _id
              , argID = t ^. _id
              , bodyID = body ^. _id
              , types = Just (k, b)
              }
          )
      _ -> throwError $ BadBigLambdaAnnotation annotation
  _ -> Nothing
