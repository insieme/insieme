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
{-# LANGUAGE ConstraintKinds #-}

module Data.AbstractLut where

import Control.DeepSeq
import Data.Maybe
import GHC.Generics

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


newtype Lut a b = Lut (Map a b)
    deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

type LutKey a = (Ord a)

empty :: Lut a b
empty = Lut Map.empty
{-# INLINE empty #-}

singleton :: LutKey a => a -> b -> Lut a b
singleton k v = Lut $ Map.singleton k v
{-# INLINE singleton #-}

fromList :: LutKey a => [(a,b)] -> Lut a b
fromList xs = Lut $ Map.fromList xs
{-# INLINE fromList #-}

fromListWith :: LutKey a => (b -> b -> b) -> [(a,b)] -> Lut a b
fromListWith f xs = Lut $ Map.fromListWith f xs
{-# INLINE fromListWith #-}

insert :: LutKey a => a -> b -> Lut a b -> Lut a b
insert k v (Lut m) = Lut $ Map.insert k v m
{-# INLINE insert #-}

insertWith :: LutKey a => (b -> b -> b) -> a -> b -> Lut a b -> Lut a b
insertWith f k v (Lut m) = Lut $ Map.insertWith f k v m
{-# INLINE insertWith #-}

delete :: LutKey a => a -> Lut a b -> Lut a b
delete k (Lut m) = Lut $ Map.delete k m
{-# INLINE delete #-}

lookup :: LutKey a => a -> Lut a b -> Maybe b
lookup k (Lut m) = Map.lookup k m
{-# INLINE lookup #-}

member :: LutKey a => a -> Lut a b -> Bool
member k (Lut m) = isJust $ Map.lookup k m
{-# INLINE member #-}
