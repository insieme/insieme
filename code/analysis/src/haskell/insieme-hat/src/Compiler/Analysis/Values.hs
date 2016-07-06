-- | Basic data types for an analysis: On the one hand there are
-- lattice values which are the base type for constraints, on the
-- other hand we have result values which are collected and
-- represented by a list of assignments.

module Compiler.Analysis.Values (
  -- * Basic data types
  Lattice(..), Assignments,
  -- * Describing updates
  Update(..), update,
  -- * Gathering analysis results
  inspect, inspectDefault, inspectDyn, inspectDyns,
  -- * Functions for modification of assignments
  empty, insert, insertWith, amap
  ) where

import Control.Monad (ap)
import Data.Dynamic (Dynamic, fromDynamic, toDyn, Typeable)
import Data.List as List (union)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- | A join-semilattice has a bottom element and can be compared and
-- combined with. A 'Monoid' is not necessarily a lattice, so we don't
-- derive from it.
class Eq v => Lattice v where
  -- | bottom element
  bot  :: v
  -- | combine elements (think: 'Data.List.union')
  join :: v -> v -> v
  -- | top element
  -- top  :: v   -- TODO

-- | Commonly, lists are lattices through list union.
instance Eq v => Lattice [v] where
  join = List.union
  bot  = []

-- | Data type for collecting values of the analysis.
newtype Assignments k = Assignments (Map.Map k Dynamic) deriving Show

-- | Create an empty table of assignments.
empty :: Assignments k
empty = Assignments Map.empty

-- | Update an assignment value if we don't get 'Nothing'.
insert :: Ord k => k -> Maybe Dynamic -> Assignments k -> Assignments k
insert k v as@(Assignments a) = maybe as (Assignments . flip (Map.insert k) a) v

-- | Update an assignment value if we don't get 'Nothing'. The update
-- will combine the old and the new value based on the evaluation of
-- the first argument.
insertWith :: Ord k => (Dynamic -> Dynamic -> Maybe Dynamic)
           -> k -> Maybe Dynamic -> Assignments k -> Assignments k
insertWith f k v as@(Assignments a) =   -- use old assigns if new value==Nothing
  maybe as (Assignments . flip (Map.insertWith combine k) a) v
  where                                 -- 'combine' used iff old value!=Nothing
    combine n o = fromMaybe o (f n o)   -- (f n o) should always return Just

-- | Create a map of assignments based upon a preset map of values.
-- This function is useful for debugging, and should not used
-- otherwise.
amap :: Typeable v => Map.Map k v -> Assignments k
amap = Assignments . Map.map toDyn

-- | Read a single assignment.
inspect :: (Ord k, Typeable a) => Assignments k -> k -> Maybe a
inspect (Assignments a) k = Map.lookup k a >>= fromDynamic

-- | Read a single assignment with default value. The default will be
-- returned when 1) no value is associated with the given label, or 2)
-- the stored value has a different type than the default value.
inspectDefault :: (Ord k, Typeable a) => Assignments k -> k -> a -> a
inspectDefault = (flip fromMaybe .) . inspect

-- | Read a single value from the list of assignments without
-- unpacking. Because this function returns a 'Dynamic' value, it is
-- generic in types and hence exposes internals, so it should not be
-- exported from user-facing modules.
inspectDyn :: Ord k => Assignments k -> k -> Maybe Dynamic
inspectDyn (Assignments a) = flip Map.lookup a

-- | Read several values from the list of assignments without
-- unpacking. Because this function returns 'Dynamic' values, it is
-- generic in types and hence exposes internals, so it should not be
-- exported from user-facing modules.
inspectDyns :: Ord k => Assignments k -> [k] -> [(k, Maybe Dynamic)]
inspectDyns = map . ap (,) . inspectDyn

-- | Describe value updates: An update mode of 'Same' indicates that
-- no update was done, the value is the same as before. 'Steady' means
-- that the value is one that is expected, 'Erratic' means that the
-- updated value an irregular one.
data Update a = Same a | Steady a | Erratic a

-- | Convert an update value to a plain value by giving three
-- functions which process the value.
update :: (a -> b) -> (a -> b) -> (a -> b) -> Update a -> b
update same stdy ertc u = case u of
  Same    a -> same a
  Steady  a -> stdy a
  Erratic a -> ertc a
