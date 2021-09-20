{-TODO We should be able to pass `--modules="Tests/*"` to stop the preprocessor from wasting time searching elsewhere:
https://github.com/haskell-works/tasty-discover/issues/12
-}
{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}
