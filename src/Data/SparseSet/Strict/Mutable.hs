-- |
-- Module      : Data.SparseSet.Strict.Mutable
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseSet.Strict.Mutable
  ( -- * Mutable sparse sets
    MSparseSet (..),

    -- ** Construction
    empty,

    -- ** Operations
    read,
    unsafeRead,
    write,
    unsafeWrite,
    unsafeModify,

    -- ** Conversion
    toList,

    -- ** Re-exports
    PrimMonad (..),
  )
where

import Data.SparseVector.Strict.Mutable (MSparseVector)
import qualified Data.SparseVector.Strict.Mutable as SV
import Data.Vector.Strict.Mutable (MVector, PrimMonad (..))
import qualified Data.Vector.Strict.Mutable as V
import Prelude hiding (lookup, read)

data MSparseSet s i a = MSparseSet
  { dense :: {-# UNPACK #-} !(MVector s a),
    sparse :: !(MSparseVector s i)
  }

empty :: (PrimMonad m) => m (MSparseSet (PrimState m) i a)
empty = do
  d <- V.new 0
  MSparseSet d <$> SV.empty
{-# INLINE empty #-}

read :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> m (Maybe a)
read (MSparseSet d s) i = do
  m <- SV.read s i
  case m of
    Nothing -> return Nothing
    Just i' -> Just <$> V.read d (fromIntegral i')
{-# INLINE read #-}

unsafeRead :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> m a
unsafeRead (MSparseSet d s) i = do
  i' <- SV.unsafeRead s i
  V.unsafeRead d (fromIntegral i')
{-# INLINE unsafeRead #-}

write :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> a -> m ()
write (MSparseSet d s) i a = do
  m <- SV.read s i
  case m of
    Just i' -> V.write d (fromIntegral i') a
    Nothing -> return ()
{-# INLINE write #-}

unsafeWrite :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> a -> m ()
unsafeWrite (MSparseSet d s) i a = do
  i' <- SV.unsafeRead s i
  V.unsafeWrite d (fromIntegral i') a
{-# INLINE unsafeWrite #-}

unsafeModify :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> (a -> a) -> m ()
unsafeModify (MSparseSet d s) i f = do
  i' <- SV.unsafeRead s i
  V.unsafeModify d f (fromIntegral i')
{-# INLINE unsafeModify #-}

toList :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> m [Maybe (i, a)]
toList s = do
  as <- SV.toList $ sparse s
  mapM go as
  where
    go (Just i) = (\a -> Just (i, a)) <$> V.unsafeRead (dense s) (fromIntegral i)
    go Nothing = return Nothing
{-# INLINE toList #-}
