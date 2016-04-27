{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Insieme.Inspire.NodeAddress (
    NodeAddress,
    goUp,
    goDown,
    goLeft,
    goRight,
    resolve,
    pattern (:<:),
    pattern (:>:),
) where

import Data.Maybe
import Data.Sequence as Seq
import Data.Tree
import Insieme.Inspire

-- | Represents a path along a tree to a spcific node.
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
resolve (x :<: xs) (Node _ ns) = if x <= Prelude.length ns
                                 then resolve xs (ns !! x)
                                 else Nothing
