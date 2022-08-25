{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Push (PushAppIntoLetrecDetail (..), tryReducePush) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Optics ((^.))
import Primer.Core (
  Expr,
  Expr' (APP, App, LAM, Lam, Letrec),
  ID,
  LVarName,
  _id,
 )
import Primer.Eval.Utils (annOf, annotate)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name.Fresh (isFresh, isFreshTy)

data PushAppIntoLetrecDetail = PushAppIntoLetrecDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the expression after reduction
  , argID :: ID
  -- ^ the ID of the argument to the application
  , letrecID :: ID
  -- ^ the ID of the letrec
  , lamID :: ID
  -- ^ the ID of the lambda
  , letBindingName :: LVarName
  -- ^ The name of the variable bound by the letrec
  , isTypeApplication :: Bool
  -- ^ If 'True', the application is of a big lambda to a type.
  -- Otherwise it is of a small lambda to a term.
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON PushAppIntoLetrecDetail

tryReducePush ::
  MonadFresh ID m =>
  Expr ->
  Maybe (m (Expr, PushAppIntoLetrecDetail))
tryReducePush = \case
  -- (letrec x : T = t in λ ...) e  ~> letrec x : T = t in ((λ...) e)
  before@(App mApp (Letrec mLet x e1 t lam@Lam{}) e2) | isFresh x e2 -> Just $ do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step. This does not cause capture, as we have checked that x is not free in e2.
    let expr = annotate (annOf mApp) $ Letrec mLet x e1 t (App mApp lam e2)
    pure
      ( expr
      , PushAppIntoLetrecDetail
          { before = before
          , after = expr
          , argID = e2 ^. _id
          , letrecID = mLet ^. _id
          , lamID = lam ^. _id
          , letBindingName = x
          , isTypeApplication = False
          }
      )

  -- (letrec x : T = t in Λ ...) e  ~>  letrec x : T = t in ((Λ ...) e)
  before@(APP mApp (Letrec mLet x e1 t lam@LAM{}) e2) | isFreshTy x e2 -> Just $ do
    -- We push the application into the letrec, in order to enable it to reduce in a subsequent
    -- step. This does not cause capture, as we have checked that x is not free in e2.
    let expr = annotate (annOf mApp) $ Letrec mLet x e1 t (APP mApp lam e2)
    pure
      ( expr
      , PushAppIntoLetrecDetail
          { before = before
          , after = expr
          , argID = e2 ^. _id
          , letrecID = mLet ^. _id
          , lamID = lam ^. _id
          , letBindingName = x
          , isTypeApplication = True
          }
      )
  _ -> Nothing
