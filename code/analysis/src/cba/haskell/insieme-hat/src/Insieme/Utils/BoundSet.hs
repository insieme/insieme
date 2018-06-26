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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

{- | This module defines a data structure for bounded set.

The idea is to have a set turn into 'Universe' if it exceeds a certain number
(bound) of elements. A bound is part of a 'BoundSet' 's type. It is also
possible to change a 'BoundSet' 's bound by using 'changeBound'.

A special bound 'Unbound' has been defined which does not turn a set
automatically into 'Universe'.

 -}

module Insieme.Utils.BoundSet (
    -- * Bounds
    IsBound,
    Bound10(..),
    Bound100(..),
    Unbound(..),
    getBound,
    changeBound,

    -- * Bound Set
    BoundSet(Universe),

    -- ** Basic Constructors
    empty,
    singleton,

    -- ** Queries
    size,
    null,
    isUniverse,
    member,

    -- ** Importers
    fromSet,
    fromList,

    -- ** Exporters
    toSet,
    toList,

    -- ** Modifiers
    insert,
    delete,
    applyOrDefault,
    filter,
    map,
    lift,
    lift2,

    -- ** Set Operations
    union,
    difference,
    intersection,
    cartProduct,
    cartProductL,

    -- * Unbound Set
    UnboundSet,
    toUnboundSet,
) where

import GHC.Stack

import Control.DeepSeq
import Data.Typeable
import Data.Hashable
import Data.AbstractSet (Set, SetKey)
import qualified Data.AbstractSet as Set
import GHC.Generics (Generic)

import Prelude hiding (filter,map,null)

class Typeable b => IsBound b where
    bound :: p b a -> Int

-- | Returns the number of elements a 'BoundSet' can hold before automatically
-- turning into 'Universe'.
getBound :: IsBound bb => BoundSet bb a -> Int
getBound bs = bound bs

-- | Converts one 'BoundSet' into one with a different bound. 'Universe' stays
-- 'Universe'.
changeBound :: BoundSet bb a -> BoundSet bb' a
changeBound  Universe    = Universe
changeBound (BoundSet s) = BoundSet s

data Bound10 = Bound10
instance IsBound Bound10 where
    bound _ = 10

data Bound100 = Bound100
instance IsBound Bound100 where
    bound _ = 100

data BoundSet bb a = Universe | BoundSet (Set a)
  deriving (Eq, Ord, Show, Generic, NFData, Hashable, Foldable)

empty :: BoundSet bb a
empty = BoundSet Set.empty

singleton :: SetKey a => a -> BoundSet bb a
singleton = BoundSet . Set.singleton

-- | Returns the number of elements in the set, throws an error when used with
-- 'Universe'.
size :: BoundSet bb a -> Int
size Universe     = error "Size of Universe does not fit into Int"
size (BoundSet x) = Set.size x

null :: BoundSet bb a -> Bool
null  Universe    = False
null (BoundSet s) = Set.null s

isUniverse :: BoundSet bb a -> Bool
isUniverse Universe = True
isUniverse _        = False

member :: SetKey a => a -> BoundSet bb a -> Bool
member _ Universe      = True
member x (BoundSet xs) = Set.member x xs

enforceBound :: (IsBound bb, SetKey a) => BoundSet bb a -> BoundSet bb a
enforceBound bs@(BoundSet s) = if bound bs > 0 && Set.size s > bound bs then Universe else bs

fromSet :: (IsBound bb, SetKey a) => Set a -> BoundSet bb a
fromSet s = enforceBound $ BoundSet s

fromList :: (IsBound bb, SetKey a) => [a] -> BoundSet bb a
fromList as = fromSet $ Set.fromList as


-- | Convert a 'BoundSet' to a regular 'HashSet', throws an error when used with
-- 'Universe'.
toSet :: (IsBound bb, Ord a) => BoundSet bb a -> Set a
toSet  Universe    = error "Cannot convert Universe :: UnboundSet to set"
toSet (BoundSet s) = Set.fromList $ Set.toList s

-- Convert a 'BoundSet' to a list, throws an error when used with 'Universe'.
toList :: (HasCallStack) => BoundSet bb a -> [a]
toList Universe     = error "Cannot convet Universe to list"
toList (BoundSet x) = Set.toList x

insert :: (SetKey a) => a -> BoundSet bb a -> BoundSet bb a
insert _  Universe    = Universe
insert e (BoundSet s) = BoundSet $ Set.insert e s

delete :: (SetKey a) => a -> BoundSet bb a -> BoundSet bb a
delete _  Universe    = Universe
delete e (BoundSet s) = BoundSet $ Set.delete e s

applyOrDefault :: b -> (BoundSet bb a -> b) -> BoundSet bb a -> b
applyOrDefault d _ Universe = d
applyOrDefault _ f s        = f s

map :: (SetKey b) => (a -> b) -> BoundSet bb a -> BoundSet bb b
map _ Universe     = Universe
map f (BoundSet x) = BoundSet (Set.map f x)

filter :: (a -> Bool) -> BoundSet bb a -> BoundSet bb a
filter _ Universe     = Universe
filter f (BoundSet x) = BoundSet (Set.filter f x)


lift :: (IsBound bb, SetKey b)
     => (a -> b) -> (BoundSet bb a -> BoundSet bb b)
lift _ Universe     = Universe
lift f (BoundSet x) = fromList $ f <$> Set.toList x


lift2 :: (IsBound bb, SetKey c)
      => (a -> b -> c) -> (BoundSet bb a -> BoundSet bb b -> BoundSet bb c)
lift2 _ Universe     _            = Universe
lift2 _ _            Universe     = Universe
lift2 f (BoundSet x) (BoundSet y) = fromList prod
  where
      prod = [f u v | u <- Set.toList x, v <- Set.toList y]

union :: (IsBound bb, SetKey a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
union    Universe     _            = Universe
union    _            Universe     = Universe
union    a            b            | null a = b
union    a            b            | null b = a
union bs@(BoundSet x) (BoundSet y) = fromSet $ Set.union x y


difference :: (SetKey a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
difference Universe     Universe     = empty
difference _            Universe     = empty
difference Universe     _            = Universe
difference a            _            | null a = a
difference a            b            | null b = a
difference (BoundSet x) (BoundSet y) = BoundSet $ Set.difference x y

intersection :: (SetKey a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
intersection Universe     x            = x
intersection x            Universe     = x
intersection a            _            | null a = a
intersection _            b            | null b = b
intersection (BoundSet x) (BoundSet y) = BoundSet $ Set.intersection x y

cartProduct :: (IsBound bb, SetKey a, SetKey b)
            => BoundSet bb a -> BoundSet bb b -> BoundSet bb (a, b)
cartProduct Universe     _            = Universe
cartProduct _            Universe     = Universe
cartProduct (BoundSet x) (BoundSet y) = fromList prod
  where
     prod = [(u, v) | u <- Set.toList x, v <- Set.toList y]

cartProductL :: (IsBound bb, SetKey a) => [BoundSet bb a] -> BoundSet bb [a]
cartProductL []              = singleton []
cartProductL (Universe : _ ) = Universe
cartProductL (h        : bs) = if isUniverse base then Universe else fromList prod
  where
    base = cartProductL bs
    prod = [ u : vs | u <- toList h, vs <- toList base ]


-- | Special bound which does not turn a 'BoundSet' automatically into
-- 'Universe'.
--
-- >>> getBound $ (empty :: UnboundSet Int)
-- -1
data Unbound = Unbound

instance IsBound Unbound where
    bound _ = -1

type UnboundSet a = BoundSet Unbound a

-- | Converts a 'BoundSet' to an 'UnboundSet', 'Universe' stays 'Universe'.
toUnboundSet :: BoundSet bb a -> UnboundSet a
toUnboundSet = changeBound
