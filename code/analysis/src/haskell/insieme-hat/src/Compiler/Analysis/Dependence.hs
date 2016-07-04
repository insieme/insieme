-- | Functions to deal with dependences between elements.

module Compiler.Analysis.Dependence (
  invert, inflate
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Based on an final set of nodes that span a dependence tree return
-- a map with inverted dependence relations and a set of nodes that
-- span up the inverse dependence tree (initial nodes).
invert :: Ord a => [a] -> (a -> [a]) -> (Map.Map a (Set.Set a), Set.Set a)
invert i f = reach i Set.empty Map.empty Set.empty
  where
    combine a = (flip (Map.insertWith Set.union)) (Set.singleton a)
    -- nodes to process, already visited nodes, resulting map, external nodes
    reach [] c m x = (m, x)
    reach (e:es) c m x =
      if Set.member e c then reach es c m (Set.insert e x)
      else let d = f e
           in reach (d ++ es) (Set.insert e c)
              (foldr (combine e) m (f e))
              (if null d then Set.insert e x else x)

-- | Based on a dependence function and an initial set of nodes return
-- all nodes that can be recursively reached.
inflate :: (Eq a, Ord a) => (a -> [a]) -> [a] -> [a]
inflate dep = Set.toList . flip rec' Set.empty . Set.fromList
  where
    dep' = Set.fromList . dep
    rec' os' acc =
      let
        new  = Set.unions $ map dep' (Set.toList os')
        seen = Set.union acc os'
      in
       if Set.null os' then acc else rec' (Set.difference new seen) seen
