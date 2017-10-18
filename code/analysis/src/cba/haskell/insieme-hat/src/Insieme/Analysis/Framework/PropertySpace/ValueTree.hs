{-
 - Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 -
 - If you require different license terms for your intended use of the
 - software, e.g. for proprietary commercial or industrial use, please
 - contact us at:
 -                   insieme@dps.uibk.ac.at
 -
 - We kindly ask you to acknowledge the use of this software in any
 - publication or other disclosure of results by referring to the
 - following citation:
 -
 - H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 - T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 - for Parallel Codes, in Proc. of the Intl. Conference for High
 - Performance Computing, Networking, Storage and Analysis (SC 2012),
 - IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 -}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Insieme.Analysis.Framework.PropertySpace.ValueTree (
    Tree
) where

import Control.DeepSeq
import Data.List (intercalate)
import Data.Maybe
import GHC.Generics (Generic)
import Insieme.Analysis.Entities.DataPath
import Insieme.Analysis.Entities.FieldIndex
import Insieme.Analysis.Framework.PropertySpace.ComposedValue
import qualified Data.Map as Map
import qualified Insieme.Analysis.Solver as Solver



data Tree i a = Leaf a
              | Node (Map.Map i (Tree i a))
              | Empty
              | Inconsistent
    deriving (Eq,Ord,Generic,NFData)


instance (Show i, Show a) => Show (Tree i a) where
    show (Leaf a)     = show a
    show (Node m)     = show m
    show Empty        = "-empty-"
    show Inconsistent = "-unknown or inconsistent-"



instance (FieldIndex i, Solver.ExtLattice a) => ComposedValue (Tree i a) i a where

    -- build a tree with a leaf-level value
    toComposed = Leaf


    -- extract a value from a tree
    toValue Empty    = Solver.bot
    toValue (Leaf a) = a
    toValue _        = Solver.top


    -- test whether something is a value or something composed
    isValue (Leaf _) = True
    isValue _        = False

    -- build a tree with nested elements
    composeElements l = Node $ Map.fromList l


    -- obtain an addressed value within a tree
    getElement  Root t = t
    getElement (DataPath p Down i) t = get i $ getElement p t
    getElement _ _ = Inconsistent


    -- update a field in the tree
    setElement Root v _ = v
    setElement (DataPath p Down i) v t = setElement p new t
      where
        old = getElement p t
        new = set i v old
    setElement _ _ _ = Inconsistent

    -- implement element map
    mapElements _ l@(Leaf _) = l
    mapElements _ Empty = Empty
    mapElements _ Inconsistent = Inconsistent
    mapElements f (Node m) = Node $ Map.fromList $ f <$> Map.toList m

    top = Inconsistent


-- | make every tree instance a lattice
instance (FieldIndex i, Solver.Lattice a) => Solver.Lattice (Tree i a) where
    bot   = Empty
    merge = mergeTree
    
    -- add pretty print support for trees
    print (Leaf a)     = Solver.print a
    print (Node m)     = "{" ++ (intercalate "," ((\(k,v) -> (show k) ++ "=" ++ (Solver.print v)) <$> Map.toList m)) ++ "}"
    print Empty        = "-empty-"
    print Inconsistent = "-unknown or inconsistent-"



-- | make every tree instance an extended lattice
instance (FieldIndex i, Solver.ExtLattice a) => Solver.ExtLattice (Tree i a) where
    top   = Inconsistent



-- | looks up the value of a sub tree
get :: (FieldIndex i, Solver.Lattice a) => i -> Tree i a -> Tree i a
get i (Node m)     = r
    where
        k = project (Map.keys m) i
        r = Solver.join $ map extract k
            where
                extract k' = fromMaybe Empty $ Map.lookup k' m

get _ Empty        = Empty
get _ _            = Inconsistent

-- | updates the value of a sub tree
set :: (Ord i) => i -> Tree i a -> Tree i a -> Tree i a
set i v Empty        = Node $ Map.singleton i v
set i v Inconsistent = Node $ Map.singleton i v
set _ _ (Leaf _)     = Inconsistent
set i v (Node m)     = Node $ Map.insert i v m


-- | merges two value trees
mergeTree :: (FieldIndex i, Solver.Lattice a) => Tree i a -> Tree i a -> Tree i a

mergeTree Empty a = a
mergeTree a Empty = a

mergeTree (Leaf a) (Leaf b)  = Leaf $ Solver.merge a b

mergeTree a@(Node m) b@(Node n)  = r
    where
        -- the set of keys in the resulting node
        keys = join (Map.keys m) (Map.keys n)

        -- compute the values of those keys
        r = case keys of
                Just k -> Node $ Map.fromList $ map fuse k
                    where
                        fuse x = (x, mergeTree (get x a) (get x b))
                Nothing -> Inconsistent

-- mergeTree a b = trace ("Unsupported merge of " ++ (show a) ++ " and " ++ (show b)) Inconsistent
mergeTree _ _ = Inconsistent
