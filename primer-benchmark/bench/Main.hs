module Main (main) where

import Foreword

import Benchmarks (benchmarks, runBenchmarks)
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain $ runBenchmarks benchmarks
