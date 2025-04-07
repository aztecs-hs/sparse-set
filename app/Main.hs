module Main where

import qualified Data.SparseSet as S

main :: IO ()
main = print $ S.insert (10 :: Int) "B" $ S.insert 0 "A" S.empty
