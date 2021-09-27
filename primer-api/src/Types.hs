module Types where

import Protolude
import Data.Aeson
import Data.OpenApi
import Servant.OpenApi

data Species = Dog | Cat
 deriving (Generic,ToJSON)

instance ToSchema Species where

-- | Some documentation for "Pet" in the backend
-- This is output from the server
data Pet = Pet {name :: Text, species :: Species}
 deriving (Generic,ToJSON)

instance ToSchema Pet where

-- | Some documentation for "Person" in the backend
-- This is not directly output by the server
data Person = Person {
  title :: Text,
  pets :: [Pet],
  siblings :: [Person]
}
