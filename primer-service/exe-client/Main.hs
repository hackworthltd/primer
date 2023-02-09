{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Control.Arrow (left)
import Data.Map qualified as Map
import Data.String (String)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import Primer.App (
  App,
  Prog (..),
  defaultProg,
  mkApp,
 )
import Primer.Client (
  addSession,
  defaultAPIPath,
  getVersion,
 )
import Primer.Core (mkSimpleModuleName, unsafeMkLocalName)
import Primer.Core.DSL hiding (app)
import Primer.Def (
  ASTDef (ASTDef, astDefExpr, astDefType),
  Def (DefAST),
 )
import Primer.Examples (
  even3App,
  mapOddApp,
 )
import Primer.Module (
  Module (..),
 )
import Primer.Name (unsafeMkName)
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
  | Large {defs :: Word, height :: Word}
  deriving stock (Eq, Show)

appNameToApp :: AppName -> App
appNameToApp Even3 = even3App
appNameToApp MapOdd = mapOddApp
appNameToApp (Large{defs, height}) = mkApp nextId (toEnum 0) prog
  where
    (prog, nextId) =
      create do
        moduleDefs <-
          Map.fromList <$> for [1 .. defs] \i -> do
            b <- lvar "x1"
            astDefExpr <-
              foldrM
                (\f e -> f (pure e))
                b
                $ map (lam . unsafeMkLocalName . ("x" <>) . show) [1 .. height]
            astDefType <- tEmptyHole
            pure (unsafeMkName $ show i, DefAST ASTDef{astDefExpr, astDefType})
        pure
          defaultProg
            { progModules =
                [ Module
                    { moduleName = mkSimpleModuleName "Large"
                    , moduleTypes = mempty
                    , moduleDefs
                    }
                ]
            }

data Command
  = GetVersion
  | AddSession Text AppName

getVersionCommand :: Parser Command
getVersionCommand = pure GetVersion

addSessionCommand :: Parser Command
addSessionCommand =
  AddSession
    <$> argument str (metavar "NAME")
    <*> parseApp

parseApp :: Parser AppName
parseApp =
  hsubparser $
    mconcat
      [ command "Even3" $ info (pure Even3) mempty
      , command "MapOdd" $ info (pure MapOdd) mempty
      , command "Large" $ flip info mempty do
          defs <- option auto $ long "defs" <> metavar "INT"
          height <- option auto $ long "height" <> metavar "INT"
          pure Large{defs, height}
      ]
      <> metavar "APP"
  where
    -- Ensures the compiler warns us to update this parser to add a case for any new apps.
    _ = \case
      Even3{} -> ()
      MapOdd{} -> ()
      Large{} -> ()

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
            (info addSessionCommand (progDesc "Add app APP to the database with the name NAME."))
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
