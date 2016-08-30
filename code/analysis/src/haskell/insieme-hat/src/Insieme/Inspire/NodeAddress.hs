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

module Insieme.Inspire.NodeAddress (
    NodeAddress,
    mkNodeAddress,
    getPathReversed,
    getNodePair,
    getInspire,
    getParent,
    getPath,
    getIndex,
    getNode,
    isRoot,
    getRoot,
    prettyShow,
    goUp,
    goDown,
    goLeft,
    goRight,
    isBuiltin
) where

import Data.Maybe
import Data.List (foldl')
import Data.Tree
import Data.Function (on)
import qualified Data.Map as Map
import qualified Insieme.Inspire as IR

type NodePath = [Int]

data NodeAddress = NodeAddress { getPathReversed :: NodePath,
                                 getNodePair     :: Tree (Int, IR.NodeType),
                                 getInspire      :: IR.Inspire,
                                 getParent       :: Maybe NodeAddress }

instance Eq NodeAddress where
    x == y = (fst $ rootLabel $ getNodePair x) == (fst $ rootLabel $ getNodePair y)
                && getPathReversed x == getPathReversed y
                && (fst $ rootLabel $ getRoot x) == (fst $ rootLabel $ getRoot y)

instance Ord NodeAddress where
    compare x y = if r1 == EQ then if r2 == EQ then r3 else r2 else r1 
        where
            r1 = compare (fst $ rootLabel $ getNodePair x) (fst $ rootLabel $ getNodePair y)
            r2 = compare (getPathReversed x) (getPathReversed y)
            r3 = compare (fst $ rootLabel $ getRoot x) (fst $ rootLabel $ getRoot y)

instance Show NodeAddress where
    show = prettyShow

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> IR.Inspire -> NodeAddress
mkNodeAddress xs ir = foldl' (flip goDown) (NodeAddress [] root ir Nothing) xs
  where
    root = IR.getTree ir

-- | Slow, use 'getPathReversed' where possible
getPath :: NodeAddress -> NodePath
getPath = reverse . getPathReversed

getIndex :: NodeAddress -> Int
getIndex = head . getPathReversed

getNode :: NodeAddress -> Tree IR.NodeType
getNode addr = snd <$> getNodePair addr

isRoot :: NodeAddress -> Bool
isRoot = isNothing . getParent

getRoot :: NodeAddress -> Tree (Int, IR.NodeType)
getRoot = IR.getTree . getInspire

prettyShow :: NodeAddress -> String
prettyShow na = '0' : concat ['-' : show x | x <- getPath na]

goUp :: NodeAddress -> NodeAddress
goUp na = fromMaybe na (getParent na)

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs n ir _) = NodeAddress (x : xs) n' ir (Just parent)
  where
    n' = subForest n !! x

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _ _            ) | head xs == 0 = na
goLeft na@(NodeAddress _  _ _ Nothing      ) = na
goLeft    (NodeAddress xs _ _ (Just parent)) = goDown (head xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ _ Nothing      ) = na
goRight    (NodeAddress xs _ _ (Just parent)) = goDown (head xs + 1) parent

lookupBuiltin :: NodeAddress -> String -> Maybe (Tree (Int, IR.NodeType))
lookupBuiltin addr needle = Map.lookup needle (IR.getBuiltins $ getInspire addr)

isBuiltin :: NodeAddress -> String -> Bool
isBuiltin addr needle = fromMaybe False $ (== getNodePair addr) <$> lookupBuiltin addr needle
