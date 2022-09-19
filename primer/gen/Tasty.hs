module Tasty (Property, property, withTests, withDiscards) where

import Foreword

import Data.Coerce (coerce)
import Data.String (fromString)
import Hedgehog qualified as H
import Test.Tasty.Discover qualified as TD
import Test.Tasty.Hedgehog qualified as TH

-- | Work around tasty changes which give deprecation warnings for tasty-discover generated code
newtype Property = Property
  { unProperty :: H.Property
  }

instance TD.Tasty Property where
  tasty info =
    pure
      . TH.testPropertyNamed (TD.descriptionOf info) (fromString (TD.descriptionOf info))
      . unProperty

property :: HasCallStack => H.PropertyT IO () -> Property
property = Property . H.property

withTests :: H.TestLimit -> Property -> Property
withTests = coerce H.withTests

withDiscards :: H.DiscardLimit -> Property -> Property
withDiscards = coerce H.withDiscards
