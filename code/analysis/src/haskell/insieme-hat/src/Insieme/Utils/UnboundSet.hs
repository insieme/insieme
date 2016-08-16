module Insieme.Utils.UnboundSet (
    UnboundSet(Universe),
    empty,
    singleton,
    member,
    size,
    null,
    isUniverse,
    fromSet,
    fromList,
    toSet,
    toList,
    insert,
    union,
    map,
    lift,
    lift2,
    fromUnboundSet
) where

import Data.Typeable
import qualified Data.Set as Set

import Prelude hiding (map,null)


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

null :: UnboundSet a -> Bool
null  Universe      = False
null (UnboundSet s) = Set.null s

isUniverse :: UnboundSet a -> Bool
isUniverse Universe = True
isUniverse _        = False

fromSet :: Set.Set a -> UnboundSet a
fromSet s = UnboundSet s

fromList :: (Ord a) => [a] -> UnboundSet a
fromList = UnboundSet . Set.fromList

toSet :: UnboundSet a -> Set.Set a
toSet  Universe      = error "Cannot convert Universe :: UnboundSet to set"
toSet (UnboundSet s) = s

toList :: UnboundSet a -> [a]
toList Universe       = error "Cannot convert Universe :: UnboundSet to list"
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

lift :: (Ord a, Ord b)
      => (a -> b) -> (UnboundSet a -> UnboundSet b)
lift _  Universe      = Universe
lift f (UnboundSet x) = UnboundSet (Set.map f x)


lift2 :: (Ord a, Ord b, Ord c)
      => (a -> b -> c) -> (UnboundSet a -> UnboundSet b -> UnboundSet c)
lift2 _    Universe     _            = Universe
lift2 _    _            Universe     = Universe
lift2 f (UnboundSet x) (UnboundSet y) = fromList prod
  where
      prod = [f u v | u <- Set.toList x, v <- Set.toList y]
      
      
fromUnboundSet :: b -> (UnboundSet a -> b) -> UnboundSet a -> b
fromUnboundSet d _ Universe = d
fromUnboundSet _ f s        = f s
      
