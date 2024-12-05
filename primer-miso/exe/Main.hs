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

module Main (main) where

import Foreword

import Language.Javascript.JSaddle.Warp
import Primer.Miso (start)

-- Note that `debug` works with `cabal repl` but not `cabal run`.
-- The best workflow is to run `ghcid -c "cabal repl primer-miso" -W -T ':main'`.
main :: IO ()
main = debug 8000 start

#endif
