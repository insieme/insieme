-- |
-- Module:      Data.HashCons
-- Description: Hash-consing support
-- Copyright:   © 2018 Andy Morris
-- Licence:     BSD3
-- Maintainer:  hello@andy-morris.xyz
-- Stability:   experimental
-- Portability: GHC internals; weak pointers & finalizers; stable names
--
-- Hash-consing, or interning, is a way to gain constant-time equality testing
-- (and hashing) for potentially-large data types such as strings or abstract
-- syntax trees. Internally a table of live values is kept, and
-- newly-constructed values are looked up in the table to check if they already
-- exist. If they do, then the existing one is reused (along with a tag). The
-- table is pruned using finalisers when these tagged values are garbage
-- collected.
--
-- This library should be thread- and exception-safe.

{-# LANGUAGE FlexibleInstances #-}

module Data.HashCons
  (HashCons, hc,
   HC, getVal, getTag, Tag,
   Hashable (..))
where

import Data.HashCons.ConstRef
import Data.HashCons.MkWeak

import Data.Hashable
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HashTable

import Control.DeepSeq

import Control.Concurrent.MVar
import System.Mem.StableName

import System.IO.Unsafe
import Foreign

-- for HashCons instances {{{
import Numeric.Natural
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
-- }}}


-- | A tag for a value. Tags are unique among values which are simultaneously
-- alive.
newtype Tag a = Tag {fromTag :: StableName a} deriving Eq

instance Hashable (Tag a) where
  hash         = hash . fromTag
  hashWithSalt = hashUsing fromTag

makeTag :: a -> IO (Tag a)
makeTag x = fmap Tag . makeStableName $! x


-- | A value which has been given a unique tag.
data HC a = HC {-# UNPACK #-} !(Tag a) !(ConstRef a)

-- | Make an @HC@ value.
makeHC :: a -> IO (HC a)
makeHC x = HC <$> makeTag x <*> newConstRef x

-- | Retrieves the unique tag for the value.
getTag :: HC a -> Tag a
getTag (HC t _) = t

-- | Retrieves the underlying value.
getVal :: HC a -> a
getVal (HC _ x) = readConstRef x


-- | \(\mathcal{O}(1)\) using the tag
instance Eq (HC a) where
  x == y = getTag x == getTag y

-- | Checks the tag for equality first, and otherwise falls back to the
-- underlying type's ordering
instance Ord a => Ord (HC a) where
  compare x y = if x == y then EQ else compare (getVal x) (getVal y)

-- | Shows the underlying value
instance Show a => Show (HC a) where
  showsPrec d = showsPrec d . getVal

-- | \(\mathcal{O}(1)\) using the tag
instance Hashable (HC a) where
  hash         = hash . getTag
  hashWithSalt = hashUsing getTag

-- | Also evaluates the underlying value
instance NFData a => NFData (HC a) where
  rnf = rnf . getVal

instance MkWeak (HC a) where
  mkWeak (HC _ x) = mkWeak x

-- | Reads an underlying value and caches it
instance (Read a, HashCons a) => Read (HC a) where
  readsPrec d = map (\(x, s) -> (hc x, s)) . readsPrec d

-- | Stores the underlying value, and re-caches it on retrieval
instance (Storable a, HashCons a) => Storable (HC a) where
  sizeOf    = sizeOf . getVal
  alignment = alignment . getVal
  peek      = fmap hc . peek . castPtr
  poke p    = poke (castPtr p) . getVal


type HashTable k v = BasicHashTable k v

newtype Cache a = C (MVar (HashTable (Hashed a) (CacheEntry a)))

type CacheEntry a = Weak (HC a)


newCache :: IO (Cache a)
newCache =
  fmap C $ newMVar =<< HashTable.new

remove :: (Eq a, Hashable a) => a -> Cache a -> IO ()
remove x (C var) =
  let !hx = hashed x in
  withMVar var $ \cache ->
    HashTable.delete cache hx

-- TODO: switch to this when/if mutateIO is added to hashtables
-- newHC :: (Eq a, Hashable a) => a -> Cache a -> IO (Maybe (CacheEntry a), HC a)
-- newHC x c = do
--   y   <- makeHC x
--   ptr <- mkWeakPtr y (Just $ remove x c)
--   pure (Just ptr, y)
--
-- lookupOrAdd :: (Eq a, Hashable a) => a -> Cache a -> IO (HC a)
-- lookupOrAdd x c@(C var) =
--   let !hx = hashed x in
--   withMVar var $ \cache ->
--     mutateIO cache hx $ \ent -> case ent of
--       Nothing  -> newHC x c
--       Just ptr -> deRefWeak ptr >>= \y' -> case y' of
--         Nothing -> newHC x c
--         Just y  -> pure (Just ptr, y)


lookupOrAdd :: (Eq a, Hashable a) => a -> Cache a -> IO (HC a)
lookupOrAdd x c@(C var) =
    withMVar var $ \cache ->
      HashTable.lookup cache hx >>= \ent -> case ent of
        Nothing  -> newHC cache
        Just ptr -> deRefWeak ptr >>= \y' -> case y' of
          Nothing -> newHC cache
          Just y  -> pure y
  where
    !hx = hashed x
    newHC cache = do
      y   <- makeHC x
      ptr <- mkWeakPtr y (Just $ remove x c)
      y <$ HashTable.insert cache hx ptr


-- | Types which support hash-consing.
--
-- There are some restrictions on types for which this class makes sense:
--
-- 1. The type must have no type variables: an instance for @T Int@ would be
--    fine, but not for @T a@. (There must also be no constraints, but that is
--    unlikely to be a problem if all instances are ground.)
-- 2. Equality and hashing must consider all data in a value. It need not
--    necessarily be structural equality, but a subterm should not simply be
--    ignored. (An example of why someone might want to ave equality ignore
--    parts of a type is annotations in an abstract syntax tree.)
class (Eq a, Hashable a) => HashCons a where
  hcCache :: Cache a
  hcCache = unsafePerformIO newCache
  {-# NOINLINE hcCache #-}

-- | Make a hash-consed value.
hc :: HashCons a => a -> HC a
hc x = unsafePerformIO $ lookupOrAdd x hcCache


instance HashCons Integer
instance HashCons Natural
instance HashCons [Char]
-- others?


instance HashCons S.Text
instance HashCons L.Text

instance HashCons S.ByteString
instance HashCons L.ByteString
