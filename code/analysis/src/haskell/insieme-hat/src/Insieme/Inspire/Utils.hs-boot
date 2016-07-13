module Insieme.Inspire.Utils where

import Data.Tree
import Insieme.Inspire.NodeAddress
import qualified Insieme.Inspire as IR

foldTree :: Monoid a => (NodeAddress -> a -> a) -> Tree IR.Inspire -> a
