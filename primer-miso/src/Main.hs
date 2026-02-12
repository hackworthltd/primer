{-# LANGUAGE CPP #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import Foreword

import Miso qualified
import Primer.Miso (component)

main :: IO ()
#ifdef INTERACTIVE
main = Miso.reload Miso.defaultEvents component
#else
main = Miso.startApp Miso.defaultEvents component
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
