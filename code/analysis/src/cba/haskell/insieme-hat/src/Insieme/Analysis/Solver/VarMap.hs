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

module Insieme.Analysis.Solver.VarMap 
    ( VarMap
    , empty
    , lookup
    , insert
    , insertAll
    , keys
    , keysSet
    ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (lookup)

import {-# SOURCE #-} Insieme.Analysis.Solver.Var

-- Analysis Variable Maps -----------------------------------

newtype VarMap a = VarMap (HashMap Var a)

empty :: VarMap a
empty = VarMap HashMap.empty

lookup :: Var -> VarMap a -> Maybe a
lookup k (VarMap m) = HashMap.lookup k m

insert :: Var -> a -> VarMap a -> VarMap a
insert k v (VarMap m) = VarMap $ HashMap.insert k v m

insertAll :: [(Var,a)] -> VarMap a -> VarMap a
insertAll kvs (VarMap m) = VarMap $ foldr go m kvs
  where
    go (k,v) m = HashMap.insert k v m

keys :: VarMap a -> [Var]
keys (VarMap m) = HashMap.keys m

keysSet :: VarMap a -> Set Var
keysSet (VarMap m) = Set.fromList $ HashMap.keys m
