{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Insieme.Analysis.Framework.PropertySpace.ValueTree (
    Tree
) where

import Debug.Trace
import Data.Maybe
import Data.Typeable
import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import qualified Data.Map as Map
import qualified Insieme.Analysis.Solver as Solver
import Insieme.Analysis.Framework.PropertySpace.ComposedValue



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
              


instance (FieldIndex i, Solver.Lattice a) => ComposedValue (Tree i a) i a where

    -- build a tree with a leaf-level value
    toComposed = Leaf

    
    -- extract a value from a tree
    toValue Empty    = Solver.bot
    toValue (Leaf a) = a
    toValue t        = trace ("Invalid access to composed tree!") $ Solver.bot    -- TODO: this should return Solver.top, but this is not defined yet
    

    -- build a tree with nested elements
    composeElements l = Node $ Map.fromList l
            

    -- obtain an addressed value within a tree
    getElement dp t | isRoot dp   = t
    getElement dp t | isNarrow dp = get i (getElement (narrow is) t)
        where
            (i:is) = getPath dp
    getElement _ _ = Inconsistent

    
    -- update a field in the tree
    setElement dp v t | isNarrow dp = setElement' (reverse p) v t
        where
            p = getPath dp
            
            setElement' [] v t = v
            setElement' (x:xs) v t = set x (setElement' xs v (get x t)) t
    
    setElement _ _ _ = Inconsistent
        
    
        
  

-- | make every tree instance a lattice
instance (FieldIndex i, Solver.Lattice a) => Solver.Lattice (Tree i a) where
    bot   = Empty
    merge = mergeTree


              
-- | looks up the value of a sub tree
get :: (FieldIndex i, Solver.Lattice a) => i -> Tree i a -> Tree i a
get _ Empty        = Empty
get _ Inconsistent = Inconsistent 
get _ (Leaf _)     = Empty
get i (Node m)     = r
    where
        k = project (Map.keys m) i
        r = Solver.join $ map extract k
            where
                extract k = fromMaybe Solver.bot $ Map.lookup k m 

-- | updates the value of a sub tree
set :: (Ord i) => i -> Tree i a -> Tree i a -> Tree i a
set i v Empty        = Node $ Map.singleton i v
set i v Inconsistent = Inconsistent 
set _ _ (Leaf _)     = Inconsistent
set i v (Node m)     = Node $ Map.insert i v m


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
                        fuse k = ( k, mergeTree (get k a) (get k b) ) 
                Nothing -> Inconsistent


-- instance Solver.Lattice String where 
--     bot = ""
--     merge a b = a ++ b
-- 
-- 
-- p1 = (DataPath [field "B",field "A"]) :: DataPath SimpleFieldIndex
-- p2 = (DataPath [field "D",field "C"]) :: DataPath SimpleFieldIndex
-- p = concatPath p1 p2
