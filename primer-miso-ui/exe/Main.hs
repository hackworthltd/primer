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
    [ M.Href "static/deps/basecoat-css@0.3.3/basecoat.cdn.min.css"
    ]
  , M.scripts =
      [ M.Src "static/deps/basecoat-css@0.3.3/js/all.min.js"
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
