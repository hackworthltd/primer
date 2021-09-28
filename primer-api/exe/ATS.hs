-- needed for the TH-generated code
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- TODO: seem to need this in the .hs rather than .cabal for fourmolu to not complain that the GHC parser failed on '@'
{-# LANGUAGE TypeApplications #-}

import Server
import Types

import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.Proxy

-- NB: important to keep these options in sync with the ones
-- from the JSON instances
$(deriveTypeScript defaultOptions ''PersonResp)

-- Or, in a different style:
$(mconcat <$> traverse (deriveTypeScript defaultOptions) [''Pet, ''Species])

main :: IO ()
main =
  putStrLn $
    formatTSDeclarations $
      mconcat
        [ getTypeScriptDeclarations (Proxy @PersonResp)
        , getTypeScriptDeclarations (Proxy @Pet)
        , getTypeScriptDeclarations (Proxy @Species)
        ]
