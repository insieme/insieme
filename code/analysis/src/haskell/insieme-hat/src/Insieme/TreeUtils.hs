module Insieme.TreeUtils where

import Data.Tree
import Insieme.Callable as Callable
import Insieme.Inspire.NodeAddress
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

collectCallable :: NodeAddress -> Callable.CallableSet -> Callable.CallableSet
collectCallable addr s =
  let ins e s = Set.insert (e addr) s in
   case rootLabel (getNode addr) of
    IR.Lambda -> ins Callable.Lambda s
    IR.Literal -> ins Callable.Literal s
    IR.BindExpr -> ins Callable.Closure s
    _ -> s

-- | Fold the given 'Tree'. The accumulator function takes the subtree
-- and the address of this subtree in the base tree.
foldTree :: Monoid a => (NodeAddress -> a -> a) -> Tree IR.Inspire -> a
foldTree = flip foldTreePrune noPrune

-- | Disables pruning for 'foldTreePrune'.
noPrune :: NodeAddress -> Bool
noPrune _ = True

-- | Like 'foldTree' but is able to not follow entire subtrees when
-- the pruning function returns 'False'.
foldTreePrune :: Monoid a
                => (NodeAddress -> a -> a)      -- ^ aggregation function
                -> (NodeAddress -> Bool)        -- ^ prune subtrees?
                -> Tree IR.Inspire              -- ^ initial tree
                -> a                            -- ^ accumulated result
foldTreePrune collect keep tree = visit (mkNodeAddress [] tree) mempty
  where
    visit base acc = if keep base
                     then collect base $ visitsub base acc
                     else acc
    visitsub base acc = foldr visit acc (subtrees base)
    subtrees addr = [goDown i addr | i <- [0..((-1) . length . subForest . getNode $ addr)]]

-- some examples
excoll a (Node n _) = Set.insert (a, n)
extree = unfoldTree
         (\i -> (i, if i>2 then [i `div` 2, i `div` 2 -1] else [])) 8
exprune s t = Seq.length s <= 2
