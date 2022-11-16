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
    LetRemoval (LetRemovalDetail Expr)
  | TLetRemoval (LetRemovalDetail Type)
  | -- | Renaming of binding
    BindRename (BindRenameDetail Expr)
  | -- | Renaming of binding in a type
    TBindRename (BindRenameDetail Type)
  | -- | TODO: some details here
    CaseReduction CaseReductionDetail
  | -- | Elide annotation
    RemoveAnn RemoveAnnDetail
  | -- | Apply a primitive function
    ApplyPrimFun ApplyPrimFunDetail
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalDetail
