{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Detail (
  EvalDetail (..),
  module Beta,
  module Inline,
  module Case,
  module Let,
  module Push,
  module Prim,
) where

import Foreword

import Primer.Core.Meta (LocalNameKind (..))
import Primer.Core.Type (Kind, Type)
import Primer.Eval.Beta as Beta
import Primer.Eval.Case as Case
import Primer.Eval.Inline as Inline
import Primer.Eval.Let as Let
import Primer.Eval.Prim as Prim
import Primer.Eval.Push as Push
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

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
  deriving (FromJSON, ToJSON) via PrimerJSON EvalDetail
