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

module Insieme.Solver.VarMap 
    ( VarMap
    , empty
    , singleton
    , fromList
    , lookup
    , insert
    , insertAll
    , keys
    , keysSet
    , union
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.AbstractSet (Set)
import qualified Data.AbstractSet as Set
import Prelude hiding (lookup)

import {-# SOURCE #-} Insieme.Solver.Var

-- Analysis Variable Maps -----------------------------------

newtype VarMap a = VarMap (HashMap Var a)

empty :: VarMap a
empty = VarMap HashMap.empty

singleton :: Var -> a -> VarMap a
singleton k v = VarMap $ HashMap.singleton k v

lookup :: Var -> VarMap a -> Maybe a
lookup k (VarMap m) = HashMap.lookup k m

insert :: Var -> a -> VarMap a -> VarMap a
insert k v (VarMap m) = VarMap $ HashMap.insert k v m

fromList :: [(Var, a)] -> VarMap a
fromList kvs = foldr go empty kvs
  where
    go (k,v) m = insert k v m

insertAll :: [(Var, a)] -> VarMap a -> VarMap a
insertAll kvs (VarMap m) = VarMap $ foldr go m kvs
  where
    go (k,v) m = HashMap.insert k v m

keys :: VarMap a -> [Var]
keys (VarMap m) = HashMap.keys m

keysSet :: VarMap a -> Set Var
keysSet (VarMap m) = Set.fromList $ HashMap.keys m


-- | Remember: left biased
union :: VarMap a -> VarMap a -> VarMap a
union (VarMap a) (VarMap b) = VarMap $ HashMap.union a b
