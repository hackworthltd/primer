{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Foreword

import Data.Data (Data)
import Miso (Binding (..), MisoString, component, key_, noop, run, startComponent, (+>))
import Miso.Html qualified as H
import Optics (Iso', Lens', set, simple, view)
import Primer.Miso.UI qualified as UI

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
main :: IO ()
main = run $ startComponent app
#else
import qualified Miso as M

main :: IO ()
main = run $ startComponent app
  { M.styles =
    [ M.Href "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.2/dist/basecoat.cdn.min.css"
    ]
  , M.scripts =
      [ M.Src "https://cdn.jsdelivr.net/npm/basecoat-css@0.3.2/dist/js/all.min.js"
      , M.Src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"
      ]
  }
#endif

data Model
  = Model {testButton :: UI.Button}
  deriving stock (Eq, Show, Read, Data, Generic)

defaultModel :: Model
defaultModel = Model (UI.Button "Test button")

_id :: Iso' a a
_id = simple

(-->) :: Lens' parent a -> Iso' model a -> Binding parent model
parent --> child = ParentToChild (view parent) (set child)

app :: M.Component parent Model action
app = component defaultModel noop $ \_ ->
  H.div_
    []
    [ H.div_ [key_ @MisoString "test-button"]
        +> UI.button_ (#testButton --> _id)
    ]
