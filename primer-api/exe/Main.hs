import Types
import Server

import Optics
import Data.OpenApi hiding (Server, title, server)
import Network.Wai.Handler.Warp
import Servant
import Servant.OpenApi

-- | API for serving @swagger.json@.
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

-- | Swagger spec for API.
swaggerInfo :: OpenApi
swaggerInfo = toOpenApi (Proxy @API)
  & #info % #title   .~ "Test API"
  & #info % #version .~ "1.0"
  & #info % #description ?~ "This is an API that tests swagger integration"
  -- & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: Server (SwaggerAPI :<|> API)
server = pure swaggerInfo :<|> person :<|> pet
 where
  person id = pure $ PersonResp 0 "Mr" ["/pet/3"] [1,2] -- dummy
  pet id = pure $ Pet "Clifford" Dog -- dummy

main :: IO ()
main = do
  putStrLn "Running on 8081"
  run 8081 $ serve (Proxy @(SwaggerAPI :<|> API)) server
