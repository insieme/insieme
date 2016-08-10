{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Insieme.Analysis.Framework.PropertySpace.ValueTree where

import Debug.Trace
import Data.Maybe
import Data.Typeable
import Insieme.Analysis.Framework.PropertySpace.FieldIndex
import qualified Data.Map as Map
import qualified Insieme.Analysis.Solver as Solver
import Insieme.Analysis.Framework.PropertySpace.ComposedValue

import Insieme.Arithmetic



data Tree i a = Leaf a
              | Node (Map.Map i (Tree i a))
              | Empty
              | Inconsistent
    deriving (Eq,Ord)
    
    
instance (Show i, Show a) => Show (Tree i a) where
    show (Leaf a)     = show a
    show (Node m)     = show m
    show Empty        = "-empty-"
    show Inconsistent = "-inconsistent-"
              


instance (FieldIndex i, Solver.Lattice a) => ComposedValue (Tree i a) a where

    toComposed = treeFromValue
    toValue    = treeToValue
    
--    composeFields :: [(String,c v)] -> c v
--    accessField   :: String -> c v -> c v
--    
--    setIndex :: SymbolicFormula -> c v -> c v -> c v
--    getIndex :: SymbolicFormula -> c v -> c v 


treeFromValue :: a -> Tree i a
treeFromValue a = Leaf a


treeToValue :: (Solver.Lattice a) => Tree i a -> a
treeToValue Empty    = Solver.bot
treeToValue (Leaf a) = a
treeToValue t        = trace ("Invalid access to composed tree!") $ Solver.bot    -- TODO: this should return Solver.top, but this is not defined yet

              
-- | looks up the value of a certain index field in a tree
get :: (FieldIndex i, Solver.Lattice a) => Tree i a -> i -> Tree i a
get (Node m) i = r
    where
        k = project (Map.keys m) i
        r = Solver.join $ map extract k
            where
                extract k = fromMaybe Solver.bot $ Map.lookup k m 


-- | merges two value trees
mergeTree :: (FieldIndex i, Solver.Lattice a) => Tree i a -> Tree i a -> Tree i a 

mergeTree Empty a = a
mergeTree a Empty = a

mergeTree Inconsistent _ = Inconsistent
mergeTree _ Inconsistent = Inconsistent

mergeTree (Leaf a) (Leaf b)  = Leaf $ Solver.merge a b 

mergeTree a@(Node m) b@(Node n)  = r
    where
        -- the set of keys in the resulting node
        k = join (Map.keys m) (Map.keys n)
    
        -- compute the values of those keys 
        
        r = case k of
                Just k -> Node $ Map.fromList $ map fuse k
                    where
                        fuse k = ( k, mergeTree (get a k) (get b k) ) 
                Nothing -> Inconsistent


-- | make every tree instance a lattice
instance (FieldIndex i, Solver.Lattice a) => Solver.Lattice (Tree i a) where
    bot   = Empty
    merge = mergeTree
