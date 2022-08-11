{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Control.Arrow (left)
import Data.String (String)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (
  Parser,
  command,
  eitherReader,
  execParser,
  fullDesc,
  header,
  helper,
  hsubparser,
  info,
  long,
  option,
  progDesc,
 )
import Primer.Client (
  defaultAPIPath,
  getVersion,
 )
import Servant.Client (
  BaseUrl (..),
  Scheme (Http),
  mkClientEnv,
  parseBaseUrl,
  runClientM,
 )
import System.Environment (lookupEnv)

-- | The default URL assumes you're running the service on localhost.
defaultBaseUrl :: BaseUrl
defaultBaseUrl =
  BaseUrl
    { baseUrlScheme = Http
    , baseUrlHost = "localhost"
    , baseUrlPort = 8081
    , baseUrlPath = defaultAPIPath
    }

data GlobalOptions = GlobalOptions (Maybe BaseUrl) Command

data Command = GetVersion

getVersionCommand :: Parser Command
getVersionCommand = pure GetVersion

getOptions :: Parser GlobalOptions
getOptions =
  GlobalOptions
    <$> optional (option (eitherReader (left show . parseBaseUrl)) (long "base-url"))
    <*> hsubparser
      ( command
          "get-version"
          (info getVersionCommand (progDesc "Get the server version"))
      )

baseUrlEnvVar :: String
baseUrlEnvVar = "PRIMER_URL"

-- | When no base URL is provided on the command line, we try first to
-- lookup and parse the @PRIMER_URL@ environment variable. If that's
-- not present, we fall back to a default.
noBaseUrlOption :: IO BaseUrl
noBaseUrlOption = do
  envVar <- lookupEnv baseUrlEnvVar
  case envVar of
    Nothing -> pure defaultBaseUrl
    Just val -> parseBaseUrl val

run :: GlobalOptions -> IO ()
run (GlobalOptions svc c) = do
  baseUrl <- maybe noBaseUrlOption pure svc
  manager <- newTlsManager
  let env = mkClientEnv manager baseUrl
  case c of
    GetVersion -> do
      version <- runClientM getVersion env
      case version of
        Left e -> putStrLn $ "Error: " ++ show e
        Right v -> putStrLn $ "Primer version: " ++ show v

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (helper <*> getOptions)
        ( fullDesc
            <> progDesc "Command-line access to the Primer API."
            <> header
              "primer-client - A client for the Primer API."
        )
