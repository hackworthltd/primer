{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Primer.Eval.Prim (ApplyPrimFunDetail (..), tryPrimFun) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Primer.Core (
  Expr,
  Expr' (Ann, Var),
  GVarName,
  ID,
  TmVarRef (GlobalVarRef),
 )
import Primer.Core.Transform (unfoldApp)
import Primer.Core.Utils (concreteTy, forgetMetadata)
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Primitives (PrimDef, primFunDef)

data ApplyPrimFunDetail = ApplyPrimFunDetail
  { before :: Expr
  -- ^ the expression before reduction
  , after :: Expr
  -- ^ the expression after reduction
  , name :: GVarName
  -- ^ the name of the primitive function
  , argIDs :: [ID]
  -- ^ the IDs of the arguments to the application
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON ApplyPrimFunDetail

-- | If this node is a reducible application of a primitive, return the name of the primitive, the arguments, and
-- (a computation for building) the result.
tryPrimFun :: Map GVarName PrimDef -> Expr -> Maybe (GVarName, [Expr], forall m. MonadFresh ID m => m Expr)
tryPrimFun primDefs expr
  | -- Since no primitive functions are polymorphic, there is no need to unfoldAPP
    (Var _ (GlobalVarRef name), args) <- bimap stripAnns (map stripAnns) $ unfoldApp expr
  , Just x <- Map.lookup name primDefs
  , Right e <- primFunDef x $ forgetMetadata <$> args =
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
