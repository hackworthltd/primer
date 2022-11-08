{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Foreword

import Criterion.Main (
  bench,
  bgroup,
  defaultMain,
  whnf,
 )

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Foreword"
        [ bench "print" $ whnf (print :: Text -> IO ()) "Hello, World!"
        ]
    ]
