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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Insieme.Inspire.NodeAddress (
    NodeAddress,
    mkNodeAddress,
    prettyShow,

    -- queries
    getPathReversed,
    getNode,
    getInspire,
    getParent,
    getPath,
    getAbsolutePath,
    depth,
    numChildren,
    getChildren,
    getIndex,
    getNodeType,
    isRoot,
    getRoot,
    getRootAddress,
    isChildOf,

    -- navigation
    goRel,
    goUp,
    goDown,
    goLeft,
    goRight,

    -- subtrees
    append,
    crop,
) where

import Control.DeepSeq
import Data.Function (on)
import Data.List (foldl',isSuffixOf)
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)
import Insieme.Utils
import qualified Data.Hashable as Hash
import qualified Insieme.Inspire as IR

--
-- * Node Address
--

type NodePath = [Int]

data NodeAddress = NodeAddress { getPathReversed     :: NodePath,
                                 getNode             :: IR.Tree,
                                 getInspire          :: IR.Inspire,
                                 getParent           :: Maybe NodeAddress,
                                 getAbsoluteRootPath :: NodePath
                               }
  deriving (Generic, NFData)

instance Eq NodeAddress where
    x == y = nodeID x == nodeID y
             && getPathReversed x == getPathReversed y
             && rootID x == rootID y
      where
        nodeID = IR.getID . getNode
        rootID = IR.getID . getRoot

instance Ord NodeAddress where
    compare x y = r1 `thenCompare` r2 `thenCompare` r3
        where
            r1 = compare (nodeID x) (nodeID y)
            r2 = compare (getPathReversed x) (getPathReversed y)
            r3 = compare (rootID x) (rootID y)

            nodeID = IR.getID . getNode
            rootID = IR.getID . getRoot

instance Show NodeAddress where
    show = prettyShow

instance Hash.Hashable NodeAddress where
    hashWithSalt s n = Hash.hashWithSalt s $ getPathReversed n
    hash n = Hash.hash $ getPathReversed n

prettyShow :: NodeAddress -> String
prettyShow na = '0' : concat ['-' : show x | x <- getPath na]

-- ** Constructors

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> IR.Inspire -> NodeAddress
mkNodeAddress xs ir = foldl' (flip goDown) (NodeAddress [] root ir Nothing []) xs
  where
    root = IR.getTree ir

-- ** Queries

-- | Slow, use 'getPathReversed' where possible
getPath :: NodeAddress -> NodePath
getPath = reverse . getPathReversed

getAbsolutePath :: NodeAddress -> NodePath
getAbsolutePath a =  reverse $ getPathReversed a ++ getAbsoluteRootPath a

depth :: NodeAddress -> Int
depth = length . getPathReversed

-- | Get the number of children of a given node.
numChildren :: NodeAddress -> Int
numChildren = length . IR.getChildren . getNode

getChildren :: NodeAddress -> [NodeAddress]
getChildren a = (flip goDown) a <$> [0..numChildren a - 1]

getIndex :: NodeAddress -> Int
getIndex a | isRoot a = error "Can't obtain index of a root address!"
getIndex a = head $ getPathReversed a

getNodeType :: NodeAddress -> IR.NodeType
getNodeType = IR.getNodeType . getNode

isRoot :: NodeAddress -> Bool
isRoot = isNothing . getParent

getRoot :: NodeAddress -> IR.Tree
getRoot = IR.getTree . getInspire

getRootAddress :: NodeAddress -> NodeAddress
getRootAddress a = mkNodeAddress [] (getInspire a)

isChildOf :: NodeAddress -> NodeAddress -> Bool
a `isChildOf` b = getRoot a == getRoot b && (getPathReversed b `isSuffixOf` getPathReversed a)

-- ** Navigation

-- | Return a node address relative to the given one; a negative integer means
-- going up so many levels; zero or a positive integer means going to the
-- respective child.
goRel :: [Int] -> NodeAddress -> NodeAddress
goRel []     = id
goRel (i:is) | i <  0 = goRel is . last . take (1-i) . iterate goUp
             | i >= 0 = goRel is . goDown i

goUp :: NodeAddress -> NodeAddress
goUp na | isRoot na = trace "No parent for Root" undefined
goUp na = fromJust (getParent na)

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs n ir _ r) = NodeAddress (x : xs) n' ir (Just parent) r
  where
    n' = IR.getChildren n !! x

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _ _             _) | head xs == 0 = na
goLeft na@(NodeAddress _  _ _ Nothing       _) = na
goLeft    (NodeAddress xs _ _ (Just parent) _) = goDown (head xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ _ Nothing       _) = na
goRight    (NodeAddress xs _ _ (Just parent) _) = goDown (head xs + 1) parent

-- ** Subtree construction / destruction

append :: NodeAddress -> NodeAddress -> NodeAddress
append a b | isRoot a = b
append a b | isRoot b = a
append a b = NodeAddress {
                getPathReversed     = (getPathReversed b) ++ (getPathReversed a),
                getNode             = getNode b,
                getInspire          = getInspire a,
                getParent           = Just $ append a $ fromJust $ getParent b,
                getAbsoluteRootPath = getAbsoluteRootPath a
             }

crop :: NodeAddress -> NodeAddress
crop a = NodeAddress [] (getNode a) ( (getInspire a){IR.getTree=getNode a} ) Nothing ((getPathReversed a) ++ (getAbsoluteRootPath a))
