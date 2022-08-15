{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Control.Arrow (left)
import Data.String (String)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative (
  Parser,
  argument,
  command,
  eitherReader,
  execParser,
  fullDesc,
  header,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  str,
 )
import Primer.App (
  App,
 )
import Primer.Client (
  addSession,
  defaultAPIPath,
  getVersion,
 )
import Primer.Examples (
  even3App,
  mapOddApp,
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

data AppName
  = Even3
  | MapOdd
  deriving stock (Eq, Show, Read, Enum, Bounded)

appNameToApp :: AppName -> App
appNameToApp Even3 = even3App
appNameToApp MapOdd = mapOddApp

showAppChoices :: String
showAppChoices = toS $ unwords (map show allApps)
  where
    allApps :: [AppName]
    allApps = [minBound .. maxBound]

parseAppName :: String -> Either String AppName
parseAppName arg = case reads arg of
  [(appName, "")] -> Right appName
  _ -> Left $ "Unknown app: " <> arg <> "\nRun with --help for a list of available apps."

data Command
  = GetVersion
  | AddSession Text AppName

getVersionCommand :: Parser Command
getVersionCommand = pure GetVersion

addSessionCommand :: Parser Command
addSessionCommand =
  AddSession
    <$> argument str (metavar "NAME")
    <*> argument (eitherReader parseAppName) (metavar "APP")

getOptions :: Parser GlobalOptions
getOptions =
  GlobalOptions
    <$> optional (option (eitherReader (left show . parseBaseUrl)) (long "base-url"))
    <*> hsubparser
      ( command
          "get-version"
          (info getVersionCommand (progDesc "Get the server version"))
          <> command
            "add-session"
            (info addSessionCommand (progDesc $ "Add app APP to the database with the name NAME. The following apps are available: " <> showAppChoices))
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
    AddSession name app -> do
      result <- runClientM (addSession name $ appNameToApp app) env
      case result of
        Left e -> putStrLn $ "Error: " ++ show e
        Right sid -> putStrLn $ "Added app with session ID: " ++ show sid

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
