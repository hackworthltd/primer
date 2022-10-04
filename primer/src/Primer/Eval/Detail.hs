{-# LANGUAGE DuplicateRecordFields #-}

module Primer.Eval.Detail (
  EvalDetail (..),
  module Ann,
  module Beta,
  module Bind,
  module Inline,
  module Case,
  module Forall,
  module Let,
  module Push,
  module Prim,
) where

import Foreword

import Primer.Core (Expr)
import Primer.Core.Meta (LocalNameKind (..))
import Primer.Core.Type (Kind, Type)
import Primer.Eval.Beta as Beta
import Primer.Eval.Bind as Bind
import Primer.Eval.Case as Case
import Primer.Eval.Forall as Forall
import Primer.Eval.Inline as Inline
import Primer.Eval.Let as Let
import Primer.Eval.Prim as Prim
import Primer.Eval.Push as Push
import Primer.JSON (CustomJSON (CustomJSON), FromJSON, PrimerJSON, ToJSON)
import Primer.Eval.Ann as Ann

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
  | -- | Renaming of binding in let x = ...x... in ...x...x...
    BindRename (BindRenameDetail Expr) -- TODO: use this more, eg for TForall
--  | LetRename (LetRenameDetail Expr)
  | TLetRename (LetRenameDetail Type)
  | TForallRename ForallRenameDetail
  | -- | TODO: some details here
    CaseReduction CaseReductionDetail
  | -- | Elide annotation
    RemoveAnn RemoveAnnDetail
  | -- | Push the argument of an application inside a letrec
    PushAppIntoLetrec PushAppIntoLetrecDetail
  | -- | Apply a primitive function
    ApplyPrimFun ApplyPrimFunDetail
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON EvalDetail

-- TODO: are all the above actually used?
