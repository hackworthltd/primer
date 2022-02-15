module Primer.Utils (distinct, distinct') where

import qualified Data.Set as S
import Foreword

distinct :: Ord a => [a] -> Bool
distinct = isRight . distinct'

-- | Returns either the duplicate items, or the input as a set (if no duplicates)
distinct' :: Ord a => [a] -> Either [a] (S.Set a)
distinct' l = case go mempty l of
  ([], s) -> Right s
  (dups, _) -> Left dups
  where
    go seen [] = ([], seen)
    go seen (x : xs) =
      let r = go (S.insert x seen) xs
       in if x `S.member` seen
            then first (x :) r
            else r
