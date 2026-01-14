{-# LANGUAGE BangPatterns #-}
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
import Data.SparseSet.Mutable (MSparseSet (MSparseSet))
import Data.SparseVector (SparseVector)
import qualified Data.SparseVector as SV
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad (..))
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)
import Prelude hiding (lookup)

data SparseSet i a = SparseSet
  { dense :: Vector a,
    sparse :: SparseVector i
  }
  deriving (Show, Eq, Generic, NFData)

instance (Integral i) => Functor (SparseSet i) where
  fmap f s = s {dense = fmap f $ dense s}
  {-# INLINE fmap #-}

instance (Integral i) => Foldable (SparseSet i) where
  foldMap f s = V.foldMap f $ V.catMaybes $ toVec s
  {-# INLINE foldMap #-}

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
    Just denseIndex -> Just . V.unsafeIndex (dense s) $ fromIntegral denseIndex
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
  let keys = SV.intersectionVecWithKey (\i _ _ -> i) (sparse as) (sparse bs)
      as' = SV.intersectionVecWith (\a _ -> V.unsafeIndex (dense as) $ fromIntegral a) (sparse as) (sparse bs)
      x' = SV.fromList $ V.toList $ V.imap (\i k -> (k, fromIntegral i)) keys
   in SparseSet {dense = as', sparse = x'}
{-# INLINE intersection #-}

intersectionVec ::
  (Integral i) =>
  SparseSet i a ->
  SparseSet i b ->
  Vector a
intersectionVec as bs = SV.intersectionVecWith go (sparse as) $ sparse bs
  where
    go a _ = V.unsafeIndex (dense as) $ fromIntegral a
{-# INLINE intersectionVec #-}

intersectionWith ::
  (Integral i) =>
  (a -> b -> c) ->
  SparseSet i a ->
  SparseSet i b ->
  SparseSet i c
intersectionWith f as bs =
  let keys = SV.intersectionVecWithKey (\i _ _ -> i) (sparse as) (sparse bs)
      cs = SV.intersectionVecWith go (sparse as) (sparse bs)
      x' = SV.fromList $ V.toList $ V.imap (\i k -> (k, fromIntegral i)) keys
   in SparseSet {dense = cs, sparse = x'}
  where
    go a b = f (V.unsafeIndex (dense as) $ fromIntegral a) (V.unsafeIndex (dense bs) $ fromIntegral b)
{-# INLINE intersectionWith #-}

toList :: (Integral i) => SparseSet i a -> [Maybe a]
toList s = fmap go $ SV.toList $ sparse s
  where
    go (Just i) = Just $ V.unsafeIndex (dense s) (fromIntegral i)
    go Nothing = Nothing
{-# INLINE toList #-}

toVec :: (Integral i) => SparseSet i a -> Vector (Maybe a)
toVec s = fmap go $ SV.toVec $ sparse s
  where
    go i = Just $ V.unsafeIndex (dense s) (fromIntegral i)
{-# INLINE toVec #-}

-- | Freeze a `MSparseSet` into a `SparseSet`.
freeze :: (PrimMonad m) => MSparseSet (PrimState m) i a -> m (SparseSet i a)
freeze (MSparseSet d s) = do
  d' <- V.freeze d
  s' <- SV.freeze s
  return $ SparseSet d' s'
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m) => MSparseSet (PrimState m) i a -> m (SparseSet i a)
unsafeFreeze (MSparseSet d s) = do
  d' <- V.unsafeFreeze d
  s' <- SV.unsafeFreeze s
  return $ SparseSet d' s'
{-# INLINE unsafeFreeze #-}

-- | Unfreeze a `SparseSet` into a `MSparseSet`.
thaw :: (PrimMonad m) => SparseSet i a -> m (MSparseSet (PrimState m) i a)
thaw (SparseSet d s) = do
  !d' <- V.thaw d
  !s' <- SV.thaw s
  return $ MSparseSet d' s'
{-# INLINE thaw #-}

unsafeThaw :: (PrimMonad m) => SparseSet i a -> m (MSparseSet (PrimState m) i a)
unsafeThaw (SparseSet d s) = do
  !d' <- V.unsafeThaw d
  !s' <- SV.unsafeThaw s
  return $ MSparseSet d' s'
{-# INLINE unsafeThaw #-}
