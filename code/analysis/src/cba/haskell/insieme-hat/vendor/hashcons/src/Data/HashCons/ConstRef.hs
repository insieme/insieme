-- |
-- Module:      Data.HashCons.ConstRef
-- Description: Read-only references
-- Copyright:   © 2018 Andy Morris
-- Licence:     BSD3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals
--
-- Read-only references with pointer equality.

module Data.HashCons.ConstRef (ConstRef, newConstRef, readConstRef) where

import Data.HashCons.MkWeak

import Data.IORef       (IORef, newIORef, readIORef)
import System.IO.Unsafe (unsafeDupablePerformIO)


-- | A read-only reference.
--
-- A 'ConstRef' is similar to an 'IORef' in that it has its own identity.
--
-- @
-- do a <- 'newConstRef' ()
--    b <- 'newConstRef' ()
--    'return' '$' a '==' b
--      -- 'False'
-- @
--
-- However, unlike most types of reference, it is immutable and its value is set
-- at construction time. This is the reason why 'readConstRef' does not need to
-- return an 'IO' value.
newtype ConstRef a = CR (IORef a)
  deriving Eq -- ^ Pointer equality

-- | Make a new 'ConstRef'.
newConstRef :: a -> IO (ConstRef a)
newConstRef x = CR <$> newIORef x

-- | Read the value of a 'ConstRef'.
readConstRef :: ConstRef a -> a
readConstRef (CR r) = unsafeDupablePerformIO $ readIORef r
{-# INLINE readConstRef #-}

instance MkWeak (ConstRef a) where mkWeak (CR r) = mkWeak r
