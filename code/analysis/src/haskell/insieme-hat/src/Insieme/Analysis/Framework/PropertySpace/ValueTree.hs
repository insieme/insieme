{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Insieme.Analysis.Framework.PropertySpace.ValueTree (
    Tree
) where

import Debug.Trace
import Data.Maybe
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



instance (FieldIndex i, Solver.ExtLattice a) => ComposedValue (Tree i a) i a where

    -- build a tree with a leaf-level value
    toComposed = Leaf


    -- extract a value from a tree
    toValue Empty    = Solver.bot
    toValue (Leaf a) = a
    toValue t        = Solver.top


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

    top = Inconsistent


-- | make every tree instance a lattice
instance (FieldIndex i, Solver.Lattice a) => Solver.Lattice (Tree i a) where
    bot   = Empty
    merge = mergeTree


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
                extract k = fromMaybe Inconsistent $ Map.lookup k m

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
        k = join (Map.keys m) (Map.keys n)

        -- compute the values of those keys

        r = case k of
                Just k -> Node $ Map.fromList $ map fuse k
                    where
                        fuse k = ( k, mergeTree (get k a) (get k b) )
                Nothing -> Inconsistent


-- mergeTree a b = trace ("Unsupported merge of " ++ (show a) ++ " and " ++ (show b)) Inconsistent
mergeTree a b = Inconsistent
