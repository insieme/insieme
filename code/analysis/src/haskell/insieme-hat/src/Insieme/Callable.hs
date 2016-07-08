{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Insieme.Callable where

import Solver
import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Data.Set as Set
import qualified Insieme.Inspire as IR

data Callable =
      Lambda NodeAddress
    | Literal NodeAddress
    | Closure NodeAddress
 deriving (Eq, Ord, Show)

type CallableSet = Set.Set Callable

instance Lattice CallableSet where
    join [] = Set.empty
    join xs = foldr1 Set.union xs
