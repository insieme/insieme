{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

module Insieme.Utils.BoundSet (
    IsBound,
    Bound10,
    Bound100,
    BoundSet(Universe),
    empty,
    singleton,
    member,
    size,
    isUniverse,
    fromList,
    toList,
    toUnboundSet,
    union,
    intersection,
    cartProduct,
    map,
    lift2,
    getBound,
) where

import Data.Typeable
import qualified Data.Set as Set
import qualified Insieme.Utils.UnboundSet as USet

import Prelude hiding (map)

--
-- * Bounds
--

class Typeable b => IsBound b where
    bound :: p b a -> Int

data Bound10 = Bound10
instance IsBound Bound10 where
    bound _ = 10

data Bound100 = Bound100
instance IsBound Bound100 where
    bound _ = 100

--
-- * Bound Set
--

data BoundSet bb a = Universe | BoundSet (Set.Set a)
  deriving (Eq, Show)

empty :: BoundSet bb a
empty = BoundSet Set.empty

singleton :: a -> BoundSet bb a
singleton = BoundSet . Set.singleton

member :: Ord a => a -> BoundSet bb a -> Bool
member _ Universe      = True
member x (BoundSet xs) = Set.member x xs

size :: BoundSet bb a -> Int
size Universe     = error "Size of Universe does not fit into Int"
size (BoundSet x) = Set.size x

isUniverse :: BoundSet bb a -> Bool
isUniverse Universe = True
isUniverse _        = False

fromList :: (IsBound bb, Ord a) => [a] -> BoundSet bb a
fromList as = union set empty
  where
    set = BoundSet $ Set.fromList as

toList :: BoundSet bb a -> [a]
toList Universe     = error "Cannot convet Universe to list"
toList (BoundSet x) = Set.toList x

toUnboundSet :: BoundSet bb a -> USet.UnboundSet a
toUnboundSet Universe     = USet.Universe
toUnboundSet (BoundSet x) = USet.fromSet x

union :: (IsBound bb, Ord a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
union    Universe     _            = Universe
union    _            Universe     = Universe
union bs@(BoundSet x) (BoundSet y) = if Set.size u > bound bs then Universe else BoundSet u
  where
    u = Set.union x y

intersection :: (IsBound bb, Ord a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
intersection Universe     x            = x
intersection x            Universe     = x
intersection (BoundSet x) (BoundSet y) = BoundSet $ Set.intersection x y

cartProduct :: (IsBound bb, Ord a, Ord b)
            => BoundSet bb a -> BoundSet bb b -> BoundSet bb (a, b)
cartProduct Universe     _            = Universe
cartProduct _            Universe     = Universe
cartProduct (BoundSet x) (BoundSet y) = fromList prod
  where
     prod = [(u, v) | u <- Set.toList x, v <- Set.toList y]

map :: (IsBound bb, Ord b) => (a -> b) -> BoundSet bb a -> BoundSet bb b
map _ Universe     = Universe
map f (BoundSet x) = BoundSet (Set.map f x)

lift2 :: (IsBound bb, Ord a, Ord b, Ord c)
      => (a -> b -> c) -> (BoundSet bb a -> BoundSet bb b -> BoundSet bb c)
lift2 _ Universe     _            = Universe
lift2 _ _            Universe     = Universe
lift2 f (BoundSet x) (BoundSet y) = fromList prod
  where
      prod = [f u v | u <- Set.toList x, v <- Set.toList y]

getBound :: IsBound b => BoundSet b a -> Int
getBound bs = bound bs
