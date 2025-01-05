{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import Foreword

import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm
import Primer.Miso (start)

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = JSaddle.Wasm.run start

#else

{-# LANGUAGE NoOverloadedStrings #-}

module Main (main) where

import Foreword

import JSDOM (currentDocument)
import JSDOM.Generated.Document (createElement, getHead)
import JSDOM.Generated.HTMLLinkElement (setHref, setRel)
import JSDOM.Generated.Node (appendChild_)
import JSDOM.Types (Element (Element), HTMLLinkElement (HTMLLinkElement), MonadDOM, ToJSString, unsafeCastTo)
import Language.Javascript.JSaddle.Warp (debugOr)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Primer.Miso (start)

-- Note that `debug` works with `cabal repl` but not `cabal run`.
-- The best workflow is to run `ghcid -c "cabal repl primer-miso" -W -T ':main'`.
main :: IO ()
main =
  debugOr
    8000
    (insertStylesheet "style.css" >> start)
    (staticApp $ defaultWebAppSettings "frontend")

-- https://github.com/ghcjs/jsaddle/pull/149#issuecomment-2525187769
insertStylesheet :: (MonadDOM m, MonadFail m, ToJSString val) => val -> m ()
insertStylesheet stylesheetUrl = do
  Just doc <- currentDocument
  Just headEl <- getHead doc
  linkEl <- unsafeCastTo HTMLLinkElement =<< createElement doc "link"
  setRel linkEl "stylesheet"
  setHref linkEl stylesheetUrl
  appendChild_ headEl linkEl

#endif
