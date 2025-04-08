module Main where

import Control.DeepSeq
import Criterion.Main
import Data.SparseSet
import qualified Data.SparseSet as S

run :: SparseSet Int a -> SparseSet Int b -> SparseSet Int a
run = S.intersection
{-# NOINLINE run #-}

main :: IO ()
main = rnf as `seq` rnf bs `seq` defaultMain [bench "intersection" $ nf (run as) bs]
  where
    (as, bs) = foldl go (S.empty, S.empty) [0 .. 10000 :: Int]
    go (s1, s2) x =
      let s1' = S.insert x (1 :: Int) s1
          s2' = S.insert x (2 :: Int) s2
       in (s1', s2')
