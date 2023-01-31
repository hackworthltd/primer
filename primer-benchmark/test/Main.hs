module Main (main) where

import Foreword

import Benchmarks (benchmarks, runTests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain $ runTests benchmarks
