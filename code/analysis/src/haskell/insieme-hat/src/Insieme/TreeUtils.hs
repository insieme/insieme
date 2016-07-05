module Insieme.TreeUtils where

import Data.Tree
import Insieme.Callable as Callable
import Insieme.Inspire as IR
import Insieme.Inspire.NodeAddress
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tree

collectCallable :: NodeAddress -> Tree IR.Inspire -> Callable.CallableSet
                -> Callable.CallableSet
collectCallable addr (Node n _) s =
  let ins e s = Set.insert (e addr) s in
   case n of
    IR.Lambda -> ins Callable.Lambda s
    IR.Literal -> ins Callable.Literal s
    IR.BindExpr -> ins Callable.Closure s
    _ -> s

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> Tree t -> a -> a) -> Tree t -> a
foldTree = flip foldTreePrune noPrune

-- | Disables pruning for 'foldTreePrune'.
noPrune :: a -> b -> Bool
noPrune _ _ = True

-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> Tree t -> a -> a)   -- ^ aggregation function
                -> (NodeAddress -> Tree t -> Bool)     -- ^ prune subtrees?
                -> Tree t                            -- ^ initial tree
                -> a                                 -- ^ accumulated result
foldTreePrune collect keep tree = visit (Seq.singleton 0) tree mempty
  where
    visit base tree acc = if keep base tree
                          then collect base tree $ visitsub base tree acc
                          else acc
    visitsub base tree acc = foldr (uncurry visit) acc (subtrees base tree)
    subtrees addr tree = zip [goDown i addr | i <- [0..]] (subForest tree)

-- some examples
excoll a (Node n _) = Set.insert (a, n)
extree = unfoldTree
         (\i -> (i, if i>2 then [i `div` 2, i `div` 2 -1] else [])) 8
exprune s t = Seq.length s <= 2
