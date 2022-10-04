{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Inline (
  LocalVarInlineDetail (..),
  Locals,
  LocalLet (..),
  RHSCaptured (..),
  GlobalVarInlineDetail (..),
  tryInlineLocal,
  tryInlineGlobal,
) where

import Foreword

import Control.Monad.Fresh (MonadFresh)
import Data.Map qualified as Map
import Optics ((^.))
import Primer.Core (
  Expr,
  Expr' (Var),
  ID,
  LocalName,
  LocalNameKind (ATmVar),
  TmVarRef (GlobalVarRef, LocalVarRef),
  Type,
  unLocalName,
  _id,
 )
import Primer.Core.DSL (ann)
import Primer.Core.Utils (regenerateExprIDs, regenerateTypeIDs)
import Primer.Def (ASTDef (astDefExpr, astDefType), Def (DefAST), DefMap)
import Primer.Eval.EvalError (EvalError (NotRedex))
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Name (Name)

data LocalVarInlineDetail k = LocalVarInlineDetail
  { letID :: ID
  -- ^ ID of the let expression that binds this variable
  , varID :: ID
  -- ^ ID of the variable being replaced
  , bindingName :: LocalName k
  -- ^ Name of the variable
  , valueID :: ID
  -- ^ ID of the expression or type that the variable is bound to
  , replacementID :: ID
  -- ^ ID of the expression or type that has replaced the variable in the result
  , isTypeVar :: Bool
  -- ^ If 'True', the variable being inlined is a type variable.
  -- Otherwise it is a term variable.
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (LocalVarInlineDetail k)

data GlobalVarInlineDetail = GlobalVarInlineDetail
  { def :: ASTDef
  -- ^ The definition that the variable refers to
  , var :: Expr
  -- ^ The variable being replaced
  , after :: Expr
  -- ^ The result of the reduction
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON GlobalVarInlineDetail

-- | A map from local variable names to the ID of their binding, their bound
-- value and whether anything in their value would be captured by an intervening
-- binder. (NB: a non-recursive let can "capture itself" and this is also
-- detected here.)
-- Since each entry must have a value, this only includes let(rec) bindings.
-- Lambda bindings must be reduced to a let before their variables can appear here.
--
-- Values of this type are constructed by 'findNodeByID'.
type Locals = Map Name (ID, LocalLet, RHSCaptured)

-- TODO: delete lots of code. here and elsewhere
-- weeder reports a bunch of stuff, but note have typeclass roots on (not sure if relevent)
--  and I'm not sure if it reports unused constructors etc

data LocalLet = LLet Expr | LLetRec Expr | LLetType Type
  deriving (Eq, Show)

data RHSCaptured
  = NoCapture
  | Capture
  deriving (Eq, Show)

tryInlineLocal ::
  (MonadFresh ID m, MonadError EvalError m) =>
  Locals ->
  Expr ->
  Maybe (m (Expr, LocalVarInlineDetail 'ATmVar))
tryInlineLocal locals = \case
  -- Inline local variable
  -- x=e |- x ==> e
  -- If the variable is not in the local set, that's fine - it just means it is bound by a lambda
  -- that hasn't yet been reduced.
  Var mVar (LocalVarRef x)
    | Just (i, l, NoCapture) <- Map.lookup (unLocalName x) locals -> Just $ do
        e <- case l of
          LLet e' -> pure e'
          LLetRec e' -> pure e'
          LLetType _ -> throwError NotRedex
        -- Since we're duplicating @e@, we must regenerate all its IDs.
        e' <- regenerateExprIDs e
        pure
          ( e'
          , LocalVarInlineDetail
              { letID = i
              , varID = mVar ^. _id
              , valueID = e ^. _id
              , bindingName = x
              , replacementID = e' ^. _id
              , isTypeVar = False
              }
          )
  _ -> Nothing

tryInlineGlobal ::
  MonadFresh ID m =>
  DefMap ->
  Expr ->
  Maybe (m (Expr, GlobalVarInlineDetail))
tryInlineGlobal globals = \case
  -- Inline global variable
  -- (f = e : t) |- f ==> e : t
  Var mVar (GlobalVarRef x) | Just (DefAST def) <- Map.lookup x globals -> Just $ do
    -- Since we're duplicating the definition, we must regenerate all its IDs.
    e <- regenerateExprIDs (astDefExpr def)
    t <- regenerateTypeIDs (astDefType def)
    expr <- ann (pure e) (pure t)
    pure
      ( expr
      , GlobalVarInlineDetail
          { var = Var mVar (GlobalVarRef x)
          , def = def
          , after = expr
          }
      )
  _ -> Nothing
