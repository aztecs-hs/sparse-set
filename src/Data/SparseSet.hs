module Data.SparseSet where

import Data.SparseVector (SparseVector)
import qualified Data.SparseVector as SV
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data SparseSet i a = SparseSet
  { dense :: Vector a,
    sparse :: SparseVector i
  }
  deriving (Show, Eq)

empty :: SparseSet i a
empty = SparseSet V.empty SV.empty

insert :: (Integral i) => i -> a -> SparseSet i a -> SparseSet i a
insert i a s =
  case SV.lookup (fromIntegral i) $ sparse s of
    Just denseIndex ->
      s {dense = V.modify (\v -> MV.write v (fromIntegral denseIndex) a) (dense s)}
    Nothing ->
      SparseSet
        { dense = V.snoc (dense s) a,
          sparse = SV.insert (fromIntegral i) (fromIntegral $ V.length $ dense s) (sparse s)
        }

lookup :: (Integral i) => SparseSet i a -> i -> Maybe a
lookup s i =
  case SV.lookup (fromIntegral i) $ sparse s of
    Just denseIndex -> Just $ dense s V.! fromIntegral denseIndex
    Nothing -> Nothing
