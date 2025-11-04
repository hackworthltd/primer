{-# LANGUAGE CPP #-}

module Main (main) where

import Foreword

import Miso (App, component, noop, run, startComponent)
import Miso.Html qualified as H
import Miso.Html.Property qualified as P

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
  = Model
  deriving stock (Eq)

emptyModel :: Model
emptyModel = Model

app :: App Model ()
app = component emptyModel noop $ \_ ->
  H.div_
    []
    [ H.button_
        [ P.className "btn"
        ]
        [ "UI button"
        ]
    ]
