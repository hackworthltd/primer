module Primer.Servant.API (
  API,
  api,
  PrimerAPI,
  primerApi,
) where

import Foreword

import Data.OpenApi (OpenApi)
import Primer.Servant.API.Root (PrimerLegacyAPI)
import Primer.Servant.OpenAPI (PrimerOpenAPI)
import Servant (Get, JSON, (:<|>), (:>))

-- | The Primer API, including its OpenAPI specification.
--
-- Note that the OpenAPI spec currently only covers the part of the
-- API that we've converted to OpenAPI.
type API =
  "openapi.json" :> Get '[JSON] OpenApi
    :<|> PrimerAPI

-- | The Primer API.
--
-- 'PrimerOpenAPI' is the portion of our API that is documented with
-- an exported OpenAPI3 spec. 'PrimerLegacyAPI' is everything else.
--
-- Over time, the 'PrimerLegacyAPI' should shrink as we improve our
-- documentation.
type PrimerAPI = PrimerOpenAPI :<|> PrimerLegacyAPI

primerApi :: Proxy PrimerAPI
primerApi = Proxy

api :: Proxy API
api = Proxy
