module Insieme.Utils.UnboundSet (
    UnboundSet(Universe),
    empty,
    singleton,
    member,
    size,
    isUniverse,
    fromList,
    toSet,
    toList,
    insert,
    union,
    map,
    lift2
) where

import Data.Typeable
import qualified Data.Set as Set

import Prelude hiding (map)


--
-- * (Unlimited) Set
--

data UnboundSet a = Universe | UnboundSet (Set.Set a)
  deriving (Eq,Show)

empty :: UnboundSet a
empty = UnboundSet Set.empty

singleton :: a -> UnboundSet a
singleton = UnboundSet . Set.singleton

member :: Ord a => a -> UnboundSet a -> Bool
member _ Universe        = True
member x (UnboundSet xs) = Set.member x xs

size :: UnboundSet a -> Int
size Universe       = error "Size of Universe does not fit into Int"
size (UnboundSet x) = Set.size x

isUniverse :: UnboundSet a -> Bool
isUniverse Universe = True
isUniverse _        = False

fromList :: (Ord a) => [a] -> UnboundSet a
fromList = UnboundSet . Set.fromList

toSet :: UnboundSet a -> Set.Set a
toSet  Universe      = error "Cannot convert Uiverse to set"
toSet (UnboundSet s) = s

toList :: UnboundSet a -> [a]
toList Universe       = error "Cannot convet Universe to list"
toList (UnboundSet x) = Set.toList x

insert :: (Ord a) => a -> UnboundSet a -> UnboundSet a
insert _  Universe      = Universe
insert e (UnboundSet s) = UnboundSet $ Set.insert e s

union :: (Ord a) => UnboundSet a -> UnboundSet a -> UnboundSet a
union    Universe     _            = Universe
union    _            Universe     = Universe
union (UnboundSet x) (UnboundSet y) = UnboundSet $ Set.union x y


map :: (Ord b) => (a -> b) -> UnboundSet a -> UnboundSet b
map _ Universe     = Universe
map f (UnboundSet x) = UnboundSet (Set.map f x)

lift2 :: (Ord a, Ord b, Ord c)
      => (a -> b -> c) -> (UnboundSet a -> UnboundSet b -> UnboundSet c)
lift2 _    Universe     _            = Universe
lift2 _    _            Universe     = Universe
lift2 f (UnboundSet x) (UnboundSet y) = fromList prod
  where
      prod = [f u v | u <- Set.toList x, v <- Set.toList y]
