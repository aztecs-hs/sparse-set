module Main where

import Control.DeepSeq
import Criterion.Main
import qualified Data.SparseSet.Strict as S

main :: IO ()
main = rnf as `seq` rnf bs `seq` defaultMain [bench "intersection" $ nf (S.intersectionVec as) bs]
  where
    (as, bs) = foldl go (S.empty, S.empty) [0 .. 10000 :: Int]
    go (s1, s2) x =
      let s1' = S.insert x (1 :: Int) s1
          s2' = S.insert x (2 :: Int) s2
       in (s1', s2')
