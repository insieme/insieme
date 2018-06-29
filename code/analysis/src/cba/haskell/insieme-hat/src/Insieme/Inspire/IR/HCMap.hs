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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Insieme.Inspire.IR.HCMap where

import Control.DeepSeq
import Data.Maybe
import Data.Bifunctor
import GHC.Generics

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Insieme.Inspire.IR.HashCons.Types

newtype HCMap a b = HCMap (IntMap b)
    deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

empty :: HCMap a b
empty = HCMap IntMap.empty
{-# INLINE empty #-}

singleton :: HashConsed a => a -> b -> HCMap a b
singleton k v = HCMap $ IntMap.singleton (unHCId $ hcdId k) v
{-# INLINE singleton #-}

fromList :: HashConsed a => [(a,b)] -> HCMap a b
fromList xs = HCMap $ IntMap.fromList $ map (first (unHCId . hcdId)) xs
{-# INLINE fromList #-}

fromListWith :: HashConsed a => (b -> b -> b) -> [(a,b)] -> HCMap a b
fromListWith f xs = HCMap $ IntMap.fromListWith f $ map (first (unHCId . hcdId)) xs
{-# INLINE fromListWith #-}

insert :: HashConsed a => a -> b -> HCMap a b -> HCMap a b
insert k v (HCMap m) = HCMap $ IntMap.insert (unHCId $ hcdId k) v m
{-# INLINE insert #-}

insertWith :: HashConsed a => (b -> b -> b) -> a -> b -> HCMap a b -> HCMap a b
insertWith f k v (HCMap m) = HCMap $ IntMap.insertWith f (unHCId $ hcdId k) v m
{-# INLINE insertWith #-}

delete :: HashConsed a => a -> HCMap a b -> HCMap a b
delete k (HCMap m) = HCMap $ IntMap.delete (unHCId $ hcdId k) m
{-# INLINE delete #-}

lookup :: HashConsed a => a -> HCMap a b -> Maybe b
lookup k (HCMap m) = IntMap.lookup (unHCId $ hcdId k) m
{-# INLINE lookup #-}

member :: HashConsed a => a -> HCMap a b -> Bool
member k (HCMap m) = isJust $ IntMap.lookup (unHCId $ hcdId k) m
{-# INLINE member #-}
