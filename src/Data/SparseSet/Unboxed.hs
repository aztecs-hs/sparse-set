{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Data.SparseSet.Unboxed
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseSet.Unboxed
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
    intersectionVec,

    -- ** Conversion
    toList,
    freeze,
    unsafeFreeze,
    thaw,
    unsafeThaw,
  )
where

import Control.DeepSeq
import Data.SparseSet.Unboxed.Mutable (MSparseSet (MSparseSet))
import Data.SparseVector.Unboxed (SparseVector)
import qualified Data.SparseVector.Unboxed as SV
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (PrimMonad (..))
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.Generics (Generic)
import Prelude hiding (lookup)

data SparseSet i a = SparseSet
  { dense :: !(Vector a),
    sparse :: !(SparseVector i)
  }
  deriving (Show, Eq, Generic, NFData)

empty :: (Unbox a, Unbox i) => SparseSet i a
empty = SparseSet V.empty SV.empty
{-# INLINE empty #-}

insert :: (Integral i, Unbox a, Unbox i) => i -> a -> SparseSet i a -> SparseSet i a
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

lookup :: (Integral i, Unbox a, Unbox i) => SparseSet i a -> i -> Maybe a
lookup s i =
  case SV.lookup (fromIntegral i) $ sparse s of
    Just denseIndex -> Just . V.unsafeIndex (dense s) $ fromIntegral denseIndex
    Nothing -> Nothing
{-# INLINE lookup #-}

delete :: (Integral i, Unbox a, Unbox i) => i -> SparseSet i a -> SparseSet i a
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
  (Integral i, Unbox a, Unbox i) =>
  SparseSet i a ->
  SparseSet i b ->
  SparseSet i a
intersection as bs =
  let x = SV.intersection (sparse as) (sparse bs)
      (_, x') = SV.mapAccum (\i _ -> (i + 1, i)) 0 x
      as' = V.map (V.unsafeIndex (dense as) . fromIntegral) (SV.toVector x)
   in SparseSet {dense = as', sparse = x'}

intersectionVec ::
  (Integral i, Unbox a, Unbox i) =>
  SparseSet i a ->
  SparseSet i b ->
  Vector a
intersectionVec as bs = SV.intersectionVecWith go (sparse as) $ sparse bs
  where
    go a _ = V.unsafeIndex (dense as) $ fromIntegral a
{-# INLINE intersectionVec #-}

intersectionWith ::
  (Integral i, Unbox a, Unbox b, Unbox c, Unbox i) =>
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

toList :: (Integral i, Unbox a, Unbox i) => SparseSet i a -> [Maybe a]
toList s = fmap go $ SV.toList $ sparse s
  where
    go (Just i) = Just $ V.unsafeIndex (dense s) (fromIntegral i)
    go Nothing = Nothing
{-# INLINE toList #-}

-- | Freeze a `MSparseSet` into a `SparseSet`.
freeze :: (PrimMonad m, Unbox a, Unbox i) => MSparseSet (PrimState m) i a -> m (SparseSet i a)
freeze (MSparseSet d s) = do
  d' <- V.freeze d
  s' <- SV.freeze s
  return $ SparseSet d' s'
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, Unbox a, Unbox i) => MSparseSet (PrimState m) i a -> m (SparseSet i a)
unsafeFreeze (MSparseSet d s) = do
  d' <- V.unsafeFreeze d
  s' <- SV.unsafeFreeze s
  return $ SparseSet d' s'
{-# INLINE unsafeFreeze #-}

-- | Unfreeze a `SparseSet` into a `MSparseSet`.
thaw :: (PrimMonad m, Unbox a, Unbox i) => SparseSet i a -> m (MSparseSet (PrimState m) i a)
thaw (SparseSet d s) = do
  !d' <- V.thaw d
  !s' <- SV.thaw s
  return $ MSparseSet d' s'
{-# INLINE thaw #-}

unsafeThaw :: (PrimMonad m, Unbox a, Unbox i) => SparseSet i a -> m (MSparseSet (PrimState m) i a)
unsafeThaw (SparseSet d s) = do
  !d' <- V.unsafeThaw d
  !s' <- SV.unsafeThaw s
  return $ MSparseSet d' s'
{-# INLINE unsafeThaw #-}
