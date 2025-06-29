module Main where

import qualified Data.SparseSet as S

test :: Int

main :: IO ()
main = print $ S.intersection as bs
  where
    as = S.insert (10 :: Int) "B" $ S.insert 0 "A" S.empty
    bs = S.insert (10 :: Int) "B" S.empty
