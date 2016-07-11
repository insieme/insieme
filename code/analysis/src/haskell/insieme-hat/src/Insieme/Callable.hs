{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Insieme.Callable where

import Solver
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set

data Callable =
      Lambda NodeAddress
    | Literal NodeAddress
    | Closure NodeAddress
 deriving (Eq, Ord)

instance Show Callable where
    show (Lambda na) = prettyShow na
    show (Literal na) = prettyShow na
    show (Closure na) = prettyShow na

type CallableSet = Set.Set Callable

instance Lattice CallableSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
