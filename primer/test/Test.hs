{-TODO We should be able to pass `-optF --modules=Tests/*` to stop the preprocessor from wasting time searching elsewhere:
https://github.com/haskell-works/tasty-discover/issues/12#issuecomment-947689298
-}
--{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

import Foreword
import Tests.EvalFull

main :: IO ()
main = unit_tmp
