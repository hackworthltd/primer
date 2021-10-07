module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 as BSL

import Primer.Server (
  openAPIInfo,
 )

main :: IO ()
main = BSL.putStrLn $ encodePretty openAPIInfo
