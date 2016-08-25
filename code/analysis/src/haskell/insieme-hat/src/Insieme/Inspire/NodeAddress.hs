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
    getNode,
    getContext,
    getParent,
    getPath,
    getIndex,
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
import qualified Insieme.Context as Ctx
import qualified Insieme.Inspire as IR

type NodePath = [Int]

data NodeAddress = NodeAddress { getPathReversed :: NodePath,
                                 getNode         :: Tree IR.NodeType,
                                 getContext      :: Ctx.Context,
                                 getParent       :: Maybe NodeAddress }

instance Eq NodeAddress where
    (==) = (==) `on` getPathReversed

instance Ord NodeAddress where
    compare = compare `on` getPathReversed

instance Show NodeAddress where
    show = prettyShow

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> Ctx.Context -> NodeAddress
mkNodeAddress xs ctx = foldl' (flip goDown) (NodeAddress [] root ctx Nothing) xs
  where
    root = Ctx.getTree ctx

-- | Slow, use 'getPathReversed' where possible
getPath :: NodeAddress -> NodePath
getPath = reverse . getPathReversed

getIndex :: NodeAddress -> Int
getIndex = head . getPathReversed

isRoot :: NodeAddress -> Bool
isRoot = isNothing . getParent

getRoot :: NodeAddress -> Tree IR.NodeType
getRoot = Ctx.getTree . getContext

prettyShow :: NodeAddress -> String
prettyShow na = '0' : concat ['-' : show x | x <- getPath na]

goUp :: NodeAddress -> NodeAddress
goUp na = fromMaybe na (getParent na)

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs n ctx _) = NodeAddress (x : xs) n' ctx (Just parent)
  where
    n' = subForest n !! x

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _ _            ) | head xs == 0 = na
goLeft na@(NodeAddress xs _ _ Nothing      ) = na
goLeft    (NodeAddress xs _ _ (Just parent)) = goDown (head xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ _ Nothing      ) = na
goRight    (NodeAddress xs _ _ (Just parent)) = goDown (head xs + 1) parent

lookupBuiltin :: NodeAddress -> String -> Maybe (Tree IR.NodeType)
lookupBuiltin addr needle = Map.lookup needle (Ctx.getBuiltins $ getContext addr)

isBuiltin :: NodeAddress -> String -> Bool
isBuiltin addr needle = fromMaybe False $ (== getNode addr) <$> lookupBuiltin addr needle
