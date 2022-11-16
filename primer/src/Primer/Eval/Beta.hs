module Primer.Eval.Beta (
  BetaReductionDetail (..),
) where

import Foreword

import Primer.Core (
  Expr,
  ID,
  LocalName,
 )
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
  , types :: (domain, codomain)
  }
  deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via PrimerJSON (BetaReductionDetail k domain codomain)
