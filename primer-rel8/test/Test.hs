{-TODO We should be able to pass `-optF --modules=Tests/*` to stop the preprocessor from wasting time searching elsewhere:
https://github.com/haskell-works/tasty-discover/issues/12#issuecomment-947689298
-}
-- Run with num-threads=1 to avoid parallelism issues.
-- See https://github.com/hackworthltd/primer/issues/266.
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --num-threads=1 -optF --tree-display #-}
