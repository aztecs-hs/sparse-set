# sparse-set

[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aztecs-hs/sparse-set/blob/main/LICENSE)
[![Package](https://img.shields.io/hackage/v/sparse-set.svg)](https://hackage.haskell.org/package/sparse-set)
[![CI status](https://github.com/aztecs-hs/sparse-set/actions/workflows/ci.yml/badge.svg)](https://github.com/aztecs-hs/sparse-set/actions)

Efficient sparse-set data structures for Haskell

```hs
import qualified Data.SparseSet as S

main :: IO ()
main = print $ S.intersection as bs
  where
    as = S.insert (10 :: Int) "B" $ S.insert 0 "A" S.empty
    bs = S.insert (10 :: Int) "B" S.empty
```
