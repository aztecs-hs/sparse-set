{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Data.SparseSet
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseSet
  ( -- * Sparse sets
    SparseSet (..),

    -- ** Construction
    empty,

    -- ** Operations
    insert,
    lookup,
    delete,

    -- ** Intersection
    intersection,
    intersectionWith,
  )
where

import Control.DeepSeq
import Data.SparseVector (SparseVector)
import qualified Data.SparseVector as SV
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)
import Prelude hiding (lookup)

data SparseSet i a = SparseSet
  { dense :: Vector a,
    sparse :: SparseVector i
  }
  deriving (Show, Eq, Generic, NFData)

empty :: SparseSet i a
empty = SparseSet V.empty SV.empty
{-# INLINE empty #-}

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
{-# INLINE insert #-}

lookup :: (Integral i) => SparseSet i a -> i -> Maybe a
lookup s i =
  case SV.lookup (fromIntegral i) $ sparse s of
    Just denseIndex -> Just $ dense s V.! fromIntegral denseIndex
    Nothing -> Nothing
{-# INLINE lookup #-}

delete :: (Integral i) => i -> SparseSet i a -> SparseSet i a
delete i s =
  case SV.lookup (fromIntegral i) $ sparse s of
    Just denseIndex ->
      SparseSet
        { dense =
            V.take (fromIntegral denseIndex) (dense s)
              V.++ V.drop (fromIntegral denseIndex + 1) (dense s),
          sparse = SV.delete (fromIntegral i) (sparse s)
        }
    Nothing -> s
{-# INLINE delete #-}

intersection ::
  (Integral i) =>
  SparseSet i a ->
  SparseSet i b ->
  SparseSet i a
intersection as bs =
  let x = SV.intersection (sparse as) (sparse bs)
      (_, x') = SV.mapAccum (\i _ -> (i + 1, i)) 0 x
      as' = V.map (\i -> dense as V.! fromIntegral i) (SV.toVector x)
   in SparseSet {dense = as', sparse = x'}
{-# INLINE intersection #-}

intersectionWith ::
  (Integral i) =>
  (a -> b -> c) ->
  SparseSet i a ->
  SparseSet i b ->
  SparseSet i c
intersectionWith f as bs =
  let x = SV.intersection (sparse as) (sparse bs)
      (_, x') = SV.mapAccum (\i _ -> (i + 1, i)) 0 x
      as' = V.map (\i -> dense as V.! fromIntegral i) (SV.toVector x)
      bs' = V.map (\i -> dense bs V.! fromIntegral i) (SV.toVector x)
      cs = V.zipWith f as' bs'
   in SparseSet {dense = cs, sparse = x'}
{-# INLINE intersectionWith #-}
