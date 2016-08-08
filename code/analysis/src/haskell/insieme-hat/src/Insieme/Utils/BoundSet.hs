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
    union,
    cartProduct,
    map,
    getBound,
) where

import Data.Typeable
import qualified Data.Set as Set

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

union :: (IsBound bb, Ord a) => BoundSet bb a -> BoundSet bb a -> BoundSet bb a
union    Universe     _            = Universe
union    _            Universe     = Universe
union bs@(BoundSet x) (BoundSet y) = if Set.size u > bound bs then Universe else BoundSet u
  where
    u = Set.union x y

cartProduct :: (IsBound bb, Ord a, Ord b)
            => BoundSet bb a -> BoundSet bb b -> BoundSet bb (a, b)
cartProduct    Universe     _            = Universe
cartProduct    _            Universe     = Universe
cartProduct bs@(BoundSet x) (BoundSet y) = fromList prod
  where
     prod = [(u, v) | u <- Set.toList x, v <- Set.toList y]

map :: (IsBound bb, Ord b) => (a -> b) -> BoundSet bb a -> BoundSet bb b
map _ Universe     = Universe
map f (BoundSet x) = BoundSet (Set.map f x)

getBound :: IsBound b => BoundSet b a -> Int
getBound bs = bound bs
