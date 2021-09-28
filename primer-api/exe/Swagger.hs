import Types
import Server

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy
import Data.OpenApi hiding (Server, title, server)
import Optics
import Servant.OpenApi

-- | Swagger spec for API.
swaggerInfo :: OpenApi
swaggerInfo = toOpenApi (Proxy @API)
  & #info % #title   .~ "Test API"
  & #info % #version .~ "1.0"
  & #info % #description ?~ "This is an API that tests swagger integration"
  -- & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

main :: IO ()
main = BSL.putStrLn $ encodePretty swaggerInfo
