{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Insieme.Inspire.IR.HashCons where

import Control.DeepSeq
import Control.Concurrent.MVar
import Data.Hashable
import Data.HashTable.IO (BasicHashTable)
import qualified Data.HashTable.IO as HashTable
import System.IO
import System.Mem.Weak

import Data.IORef

import System.IO.Unsafe

data HC a = HC { hcHash :: !Int, hcVal :: a, hcId :: !HCId, hcDeRef :: IORef () }

instance Hashable (HC a) where
    hash = hcHash
    hashWithSalt s hc = hashWithSalt s (hcHash hc)

instance Eq (HC a) where
    HC _ _ a _ == HC _ _ b _ = a == b

-- | Checks the tag for equality first, and otherwise falls back to the
-- underlying type's ordering
instance Ord a => Ord (HC a) where
  compare x y = if x == y then EQ else compare (hcVal x) (hcVal y)

instance NFData a => NFData (HC a) where
    rnf HC {hcVal}  = hcVal `seq` () -- since we calculate the hash strictly
                                     -- when inserting HCs are already in NF
instance Show a => Show (HC a) where
    show = show . hcVal

newtype HCId = HCId { unHCId :: Int }
    deriving (Eq, Ord, Show)

inc :: HCId -> HCId
inc (HCId !i) = HCId (i + 1)

type HashTable k v = BasicHashTable k v
type Cache a = (HashTable (Hashed a) (Weak (HC a)), IORef Integer)

newCache :: IO (Cache a)
newCache = (,) <$> HashTable.newSized 1000000 <*> newIORef 0

class (Eq a, Hashable a) => HashCons a where
  hcCache :: Cache a
  hcCache = unsafePerformIO newCache
  {-# NOINLINE hcCache #-}

class HashConsed a where
    hcdId :: a -> HCId

hcGlobalId :: MVar HCId
hcGlobalId = unsafePerformIO $ newMVar (HCId 0)
{-# NOINLINE hcGlobalId #-}

lookupOrAdd :: (Eq a, Hashable a) => a -> Cache a -> IO (HC a)
lookupOrAdd x (c, sr) = do
   mhc <- HashTable.lookup c hx
   case mhc of
     Nothing ->
       newHC
     Just whc -> do
       mhc <- deRefWeak whc
       case mhc of
         Just hc -> return hc
         Nothing -> newHC

  where
    !hx = hashed x

    newHC = do
       ref <- newIORef ()
       _ <- mkWeakIORef ref (HashTable.delete c hx >> modifyIORef' sr (subtract 1))

       !i <- takeMVar hcGlobalId
       modifyIORef' sr (+1)
       putMVar hcGlobalId (inc i)

       let hc = HC (hash hx) x i ref

       HashTable.insert c hx =<< mkWeakPtr hc Nothing
       return hc


hc :: HashCons a => a -> HC a
hc x = unsafePerformIO $ lookupOrAdd x hcCache



