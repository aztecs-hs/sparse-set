-- |
-- Module      : Data.SparseSet.Mutable
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseSet.Mutable
  ( -- * Mutable sparse sets
    MSparseSet (..),

    -- ** Construction
    empty,

    -- ** Operations
    read,
    write,
  )
where

import Data.SparseVector.Mutable (MSparseVector)
import qualified Data.SparseVector.Mutable as SV
import Data.Vector.Mutable (MVector, PrimMonad (..))
import qualified Data.Vector.Mutable as V
import Prelude hiding (lookup, read)

data MSparseSet s i a = MSparseSet
  { dense :: MVector s a,
    sparse :: MSparseVector s i
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

write :: (PrimMonad m, Integral i) => MSparseSet (PrimState m) i a -> Int -> a -> m ()
write (MSparseSet d s) i a = do
  m <- SV.read s i
  case m of
    Just i' -> V.write d (fromIntegral i') a
    Nothing -> return ()
{-# INLINE write #-}
