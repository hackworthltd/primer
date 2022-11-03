module Primer.Editable (Editable (..)) where

import Protolude

data Editable = Editable | NonEditable
  deriving (Bounded, Enum, Show)
