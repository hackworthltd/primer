{-# LANGUAGE ScopedTypeVariables #-} -- needed for the TH-generated code
{-# LANGUAGE TemplateHaskell #-}
import Types
import Server

import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.Proxy

-- NB: important to keep these options in sync with the ones
-- from the JSON instances
$(deriveTypeScript defaultOptions ''PersonResp)
-- Or, in a different style:
$(mconcat <$> traverse (deriveTypeScript defaultOptions) [''Pet,''Species])

main :: IO ()
main = putStrLn $ formatTSDeclarations $ mconcat
  [getTypeScriptDeclarations (Proxy @PersonResp)
  ,getTypeScriptDeclarations (Proxy @Pet)
  ,getTypeScriptDeclarations (Proxy @Species)
  ]
