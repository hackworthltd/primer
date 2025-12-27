{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import Foreword

import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm
import Miso qualified
import Primer.Miso (start)

main :: IO ()
#ifdef INTERACTIVE
main = Miso.reload start
#else
main = JSaddle.Wasm.run start
foreign export javascript "hs_start" main :: IO ()
#endif

#else

module Main (main) where

import Foreword

main :: IO ()
main = do
  putStrLn @Text "Native compilation is only supported for HLS usage. Nothing to run."
  exitFailure

#endif
