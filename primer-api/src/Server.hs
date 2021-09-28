module Server where

import Types

import Data.Aeson
import Data.OpenApi
import Optics
import Protolude hiding ((%))
import Servant.API
import Servant.OpenApi

type HATEOAS a = Text
type ID = Int

-- | Docs for API media type for Person
-- - has ID
-- - has title
-- - has HATEOAS links to find pets
-- - has non-HATEOAS ids of siblings (so then can look up via the /persons/:id endpoint)
data PersonResp = PersonResp
  { prID :: ID
  , prTitle :: Text
  , prPets :: [HATEOAS Pet]
  , prSiblings :: [ID]
  }
  deriving (Generic, ToJSON)

instance ToSchema PersonResp where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped % #schema % #description ?~ "A person. TODO: how do I link this to the haddock docs?"
      & mapped % #schema % #example ?~ toJSON (PersonResp 0 "Mr" ["/pet/3"] [1, 2])

type API =
  "persons" :> Capture "id" ID :> Get '[JSON] PersonResp -- "public"
    :<|> "pet" :> Capture "id" ID :> Get '[JSON] Pet -- "internal, only via HATEOAS"
