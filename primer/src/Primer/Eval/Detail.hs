{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Detail (
  EvalDetail (..),
  module Ann,
  module Beta,
  module Bind,
  module Inline,
  module Case,
  module Let,
  module Prim,
  module Push,
) where

import Foreword

import Primer.Core (Expr)
import Primer.Core.Meta (LocalNameKind (..))
import Primer.Core.Type (Kind, Type)
import Primer.Eval.Ann as Ann
import Primer.Eval.Beta as Beta
import Primer.Eval.Bind as Bind
import Primer.Eval.Case as Case
import Primer.Eval.Inline as Inline
import Primer.Eval.Let as Let
import Primer.Eval.Prim as Prim
import Primer.Eval.Push as Push
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)

-- | Detailed information about a reduction step
data EvalDetail
  = -- | Reduction of (λx. a : S -> T) b
    BetaReduction (BetaReductionDetail 'ATmVar Type Type)
  | -- | Reduction of (Λx. a : ∀y:k. T) S
    BETAReduction (BetaReductionDetail 'ATyVar Kind Type)
  | -- | Inlining of a local (let-bound) variable
    LocalVarInline (LocalVarInlineDetail 'ATmVar)
  | -- | Inlining of a local (let-bound) type variable
    LocalTypeVarInline (LocalVarInlineDetail 'ATyVar)
  | -- | Inlining of a global variable (top-level definition)
    GlobalVarInline GlobalVarInlineDetail
  | -- | Removing a term-level @let@ whose bound variable is unused
    LetRemoval (LetRemovalDetail Expr)
  | -- | Removing a type-level @let@ whose bound variable is unused
    TLetRemoval (LetRemovalDetail Type)
  | -- | Explicit-substitution style pushing a 'let' down the tree
    PushLetDown (PushLetDetail Expr)
  | PushLetDownTy (PushLetDetail Type)
  | -- | Renaming of binding in an expression
    BindRename (BindRenameDetail Expr)
  | -- | Renaming of binding in a type
    TBindRename (BindRenameDetail Type)
  | -- | Reduction of case-with-only-a-wildcard-branch
    CaseReductionTrivial CaseReductionTrivialDetail
  | -- | Reduction of case-of-known-constructor
    CaseReduction CaseReductionDetail
  | -- | Elide annotation
    RemoveAnn RemoveAnnDetail
  | -- | Apply a primitive function
    ApplyPrimFun ApplyPrimFunDetail
  deriving stock (Eq, Show, Read, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalDetail
