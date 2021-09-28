-- TODO: seem to need this in the .hs rather than .cabal for fourmolu to not complain that the GHC parser failed on '@'
{-# LANGUAGE TypeApplications #-}

import Server
import Types

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.OpenApi hiding (Server, server, title)
import Data.Proxy
import Optics
import Servant.OpenApi

-- | Swagger spec for API.
swaggerInfo :: OpenApi
swaggerInfo =
  toOpenApi (Proxy @API)
    & #info % #title .~ "Test API"
    & #info % #version .~ "1.0"
    & #info % #description ?~ "This is an API that tests swagger integration"

-- & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

main :: IO ()
main = BSL.putStrLn $ encodePretty swaggerInfo
