module Primer.Refine (refine, Inst (..)) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Primer.Core.Fresh (freshLocalName)
import Primer.Core.Meta (ID, TyVarName)
import Primer.Core.Type (Kind, Type' (TForall, TFun, TVar))
import Primer.Name (NameCounter)
import Primer.Subst (substTy, substTys)
import Primer.Typecheck.Cxt (Cxt)
import Primer.Typecheck.Kindcheck qualified as TC
import Primer.Unification (InternalUnifyError, unify)
import Primer.Zipper.Type (bindersBelowTy, focus)

data Inst
  = InstApp TC.Type
  | InstAPP TC.Type
  | InstUnconstrainedAPP TyVarName Kind
  deriving stock (Show, Eq)

-- | Given a target type @T@ and a source type @S@, find an instantiation @I@
-- so that if @e ∈ S@, then @e I ∈ T' ~ T@
-- Here,
--
--  * @e (InstApp ty)@ represents "apply to any @t@ st @ty ∋ t@"
--  * @e (InstAPP ty)@ represents "apply to the type @ty"@: @e \@ty@
--  * @e (InstUnconstrainedAPP _ k)@ represents "apply to some type of kind @k@, but we don't care what"
--
-- The names in @InstUnconstrainedAPP@s scope over all the @Inst@s to the right, as well as the returned @Type@.
refine ::
  forall m.
  (MonadFresh ID m, MonadFresh NameCounter m, MonadError InternalUnifyError m) =>
  -- | only care about local type vars and typedefs
  Cxt ->
  TC.Type ->
  TC.Type ->
  m (Maybe ([Inst], TC.Type))
refine cxt tgtTy srcTy = go [] srcTy
  where
    boundNames = bindersBelowTy (focus tgtTy) <> bindersBelowTy (focus srcTy)
    avoidNames = Map.keysSet (TC.localTyVars cxt) <> boundNames
    go :: [Either TC.Type (TyVarName, Kind)] -> TC.Type -> m (Maybe ([Inst], TC.Type))
    go instantiation tmTy =
      let cxt' = extendCxtTys (rights instantiation) cxt
          uvs = Set.fromList $ map fst $ rights instantiation
       in unify cxt' uvs tgtTy tmTy >>= \case
            Just sub ->
              let f = \case
                    Left t -> InstApp <$> substTys (Map.toList sub) t -- need to instantiate unif vars
                    Right (v, k) -> pure $ case Map.lookup v sub of
                      Nothing -> InstUnconstrainedAPP v k
                      Just t -> InstAPP t
               in -- 'instantiation' is built up so the head corresponds to the
                  -- outermost application. Reverse it so the head of the result
                  -- corresponds to the innermost (first) application.
                  curry Just <$> traverse f (reverse instantiation) <*> substTys (Map.toList sub) tmTy
            Nothing -> case tmTy of
              TFun _ s t -> go (Left s : instantiation) t
              TForall _ a k t -> do
                uv <- freshLocalName avoidNames
                t' <- substTy a (TVar () uv) t
                go (Right (uv, k) : instantiation) t'
              _ -> pure Nothing

-- NB: this assumes the list is ordered st the /last/ element is most global
extendCxtTys :: [(TyVarName, Kind)] -> Cxt -> Cxt
extendCxtTys = TC.extendLocalCxtTys . reverse
