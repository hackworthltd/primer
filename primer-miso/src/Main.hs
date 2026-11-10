{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import Foreword

import Data.Map qualified as Map
import Miso qualified
import Miso.Event qualified
import Primer.Miso (component)

main :: IO ()
#ifdef INTERACTIVE
main = Miso.live events component{Miso.styles = [Miso.Href "http://127.0.0.1:8002/style.css" True]}
#else
main = Miso.startApp events component
foreign export javascript "hs_start" main :: IO ()
#endif

-- | We also need pointer events, for dragging points in path editors.
-- Miso's `pointerEvents` doesn't include `pointermove`, so we add that ourselves.
events :: Miso.Events
events = Miso.defaultEvents <> Miso.Event.pointerEvents <> Map.singleton "pointermove" Miso.Event.BUBBLE

#else

module Main (main) where

import Foreword

main :: IO ()
main = do
  putStrLn @Text "Native compilation is only supported for HLS usage. Nothing to run."
  exitFailure

#endif
