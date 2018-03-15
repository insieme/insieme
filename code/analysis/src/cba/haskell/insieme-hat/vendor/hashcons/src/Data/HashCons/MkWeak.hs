-- |
-- Module:      Data.HashCons.MkWeak
-- Description: The @MkWeak@ class
-- Copyright:   © 2018 Andy Morris
-- Licence:     BSD3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals
--
-- A class for types which can have weak pointers/finalizers usefully attached
-- to them.

{-# LANGUAGE
    BangPatterns, DataKinds, KindSignatures, MagicHash, UnboxedTuples,
    ScopedTypeVariables
  #-}

module Data.HashCons.MkWeak (MkWeak (..), Weak, Finalizer, deRefWeak) where

import GHC.Base  (IO (..), mkWeak#, mkWeakNoFinalizer#)
import GHC.MVar  (MVar (..))
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.Weak  (Weak (..), deRefWeak)
import GHC.Exts  (TYPE, RuntimeRep (..))


-- | A finalizer is a procedure attached to a weak pointer, which is run soon
-- after the weak pointer's key is garbage collected.
type Finalizer = IO ()

-- | A class for weak pointer keys.
--
-- Creating weak pointers is only really safe for certain types of key. Ordinary
-- Haskell values do not have a well defined lifetime. an example is with
-- @UNPACK@ pragmas, as in
-- <https://mail.haskell.org/pipermail/haskell-cafe/2018-January/128513.html>;
-- in principle the optimiser may also share or duplicate identical values.
--
-- This means that a value with an attached finaliser might go out of scope at
-- an unexpected time, at which point the finalizer will also be run. This might
-- lead to, e.g., file handles being closed while they are still in use.
--
-- In short, this class should only be implemented by values with specific
-- identity, like references, and the key should be the underlying primitive.
-- The instance for 'IORef' attaches the pointer to the 'GHC.Prim.MutVar#'
-- inside, for example.
class MkWeak k where
  mkWeak :: k -> v -> Maybe Finalizer -> IO (Weak v)

  -- | Make a weak pointer where the value is the key.
  mkWeakPtr :: k -> Maybe Finalizer -> IO (Weak k)
  mkWeakPtr x = mkWeak x x

  -- | Add a finalizer to a value.
  addFinalizer :: k -> Finalizer -> IO ()
  addFinalizer x fin = () <$ mkWeak x () (Just fin)


mkWeakUnlifted :: forall (k :: TYPE 'UnliftedRep) v.
                  k -> v -> Maybe Finalizer -> IO (Weak v)
mkWeakUnlifted k# v fin' = IO $ \s1 ->
  case fin' of
    Nothing ->
      let !(# s2, w #) = mkWeakNoFinalizer# k# v s1 in (# s2, Weak w #)
    Just (IO fin#) ->
      let !(# s2, w #) = mkWeak# k# v fin# s1 in (# s2, Weak w #)
{-# INLINE mkWeakUnlifted #-}

instance MkWeak (IORef a) where
  mkWeak (IORef (STRef r#)) = mkWeakUnlifted r#

instance MkWeak (MVar a) where
  mkWeak (MVar m#) = mkWeakUnlifted m#
