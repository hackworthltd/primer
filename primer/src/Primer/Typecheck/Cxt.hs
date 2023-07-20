module Primer.Typecheck.Cxt (
  Type,
  KindOrType (..),
  Cxt (..),
) where

import Foreword

import Primer.Core.Meta (GVarName)
import Primer.Core.Type (Kind', Type')
import Primer.Name (Name)
import Primer.TypeDef (TypeDefMap)
import Primer.Typecheck.SmartHoles (SmartHoles)

type Type = Type' ()

data KindOrType = K (Kind' ()) | T Type
  deriving stock (Show, Eq)

data Cxt = Cxt
  { smartHoles :: SmartHoles
  , typeDefs :: TypeDefMap
  , localCxt :: Map Name KindOrType
  -- ^ local variables. invariant: the Name comes from a @LocalName k@, and
  -- the tag @k@ should say whether the value is a kind or a type.
  -- We detect violations of this in 'lookupLocal' (thus we key this map
  -- by the underlying 'Name', rather than use a dependent map)
  , globalCxt :: Map GVarName Type
  -- ^ global variables (i.e. IDs of top-level definitions)
  }
  deriving stock (Show, Generic)
