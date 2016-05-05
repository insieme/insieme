{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Insieme.Inspire.NodeAddress (
    NodeAddress,
    goUp,
    goDown,
    goLeft,
    goRight,
    resolve,
    addressTree,
    pattern (:<:),
    pattern (:>:),
) where

import Data.Maybe
import Data.Sequence
import Data.Tree
import Insieme.Inspire

-- | Represents a path along a tree to a specific node.
type NodeAddress = Seq Int

pattern Empty     <- (viewl -> EmptyL)
pattern x  :<: xs <- (viewl -> x :< xs)
pattern xs :>: x  <- (viewr -> xs :> x)

infixr 5 :<:
infixl 5 :>:

goUp :: NodeAddress -> NodeAddress
goUp (xs :>: _) = xs
goUp  xs        = xs

goDown :: Int -> NodeAddress -> NodeAddress
goDown x xs = xs |> x

goLeft :: NodeAddress -> NodeAddress
goLeft (xs :>: 0) = xs |> 0
goLeft (xs :>: x) = xs |> x - 1
goLeft  xs        = xs

goRight :: NodeAddress -> NodeAddress
goRight (xs :>: x) = xs |> x + 1
goRight  xs        = xs

-- | Traverse the tree @t@ along a given 'NodeAddress'.
resolve :: NodeAddress -> Tree Inspire -> Maybe (Tree Inspire)
resolve Empty      t           = Just t
resolve (x :<: xs) (Node _ ns) = if x < Prelude.length ns
                                 then resolve xs (ns !! x)
                                 else Nothing

-- | Pair each node of the given tree with its corresponding 'NodeAddress'.
addressTree :: Tree a -> Tree (a, NodeAddress)
addressTree = addressTree empty
  where
    addressTree :: NodeAddress -> Tree a -> Tree (a, NodeAddress)
    addressTree as (Node t ts) = Node (t, as) (mergeAddr as <$> Prelude.zip ts [0..])

    mergeAddr :: NodeAddress -> (Tree a, Int) -> Tree (a, NodeAddress)
    mergeAddr as (t, a) = addressTree (as |> a) t
