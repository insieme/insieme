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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module Insieme.Inspire.NodeAddress (
    -- * Node Address
    NodeAddress,
    prettyShow,

    -- ** Constructor
    mkNodeAddress,
    mkNodeAddresses,
    mkNodeAddresses',

    -- ** Queries
    getPathReversed,
    getNode,
    getParent,
    getPath,
    getAbsolutePath,
    depth,
    depthAbsolute,
    numChildren,
--    getChildren,
    getIndex,
    isRoot,
    getRoot,
    getRootAddress,
    isChildOf,

    -- ** Navigation
    goRel,
    goUp,
    goUpX,
    goDown,
    goLeft,
    goRight,

    -- ** Address Clipping
    append,
    crop,
) where

import Control.DeepSeq
import Data.List (foldl',isSuffixOf,sort)
import Data.Bits
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import GHC.Generics (Generic)

import Insieme.Utils
import Insieme.Inspire.NodeReference
import Insieme.Inspire.NodePath (NodePath, ppNodePathStr)
import qualified Data.Hashable as Hash
import qualified Insieme.Inspire.IR as IR
 
data NodeAddress = NodeAddress { getPathReversed     :: NodePath,
                                 getNode             :: IR.Tree,
                                 getParent           :: Maybe NodeAddress,
                                 getRoot             :: IR.Tree,
                                 getAbsoluteRootPath :: NodePath,
                                 getPathBNP          :: BinNodePath,
                                 getHash             :: Int
                               }
  deriving (Generic, NFData)

instance IR.NodeLike NodeAddress where
    node     = getNode

instance NodeReference NodeAddress where
    child    = goDown
    children = getChildren

instance Eq NodeAddress where
    x == y = getNode x == getNode y
             && getPathBNP x == getPathBNP y
             && getRoot x == getRoot y

instance Ord NodeAddress where
    compare x y = r1 `thenCompare` r2 `thenCompare` r3
        where
            r1 = compare (getNode x) (getNode y)
            r2 = compare (getPathBNP x) (getPathBNP y)
            r3 = compare (getRoot x) (getRoot y)

instance Show NodeAddress where
    show = prettyShow

instance Hash.Hashable NodeAddress where
    hashWithSalt s n = Hash.hashWithSalt s $ getHash n


newtype BinNodePath = BinNodePath Integer 
    deriving (Eq, Ord, Show, Read, Generic, NFData)

emptyBNP :: BinNodePath
emptyBNP = BinNodePath 1

appendBNP :: Int -> BinNodePath -> BinNodePath
appendBNP 0 (BinNodePath xs) = BinNodePath $ shift xs 1
appendBNP 1 (BinNodePath xs) = BinNodePath $ 0b10      .|. shift xs 2
appendBNP 2 (BinNodePath xs) = BinNodePath $ 0b110     .|. shift xs 3
appendBNP 3 (BinNodePath xs) = BinNodePath $ 0b1110    .|. shift xs 4
appendBNP 4 (BinNodePath xs) = BinNodePath $ 0b11110   .|. shift xs 5
appendBNP 5 (BinNodePath xs) = BinNodePath $ 0b111110  .|. shift xs 6
appendBNP 6 (BinNodePath xs) = BinNodePath $ 0b1111110 .|. shift xs 7
appendBNP x (BinNodePath xs) | x < 2^8  
                             = BinNodePath $ (shift 0b11111110 8 ) .|. ((fromIntegral x) .&. 0xff)     .|. shift xs 16
appendBNP x (BinNodePath xs) = BinNodePath $ (shift 0b11111111 24) .|. ((fromIntegral x) .&. 0xffffff) .|. shift xs 32

prettyShow :: NodeAddress -> String
prettyShow na = ppNodePathStr $ getPath na

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> IR.Tree -> NodeAddress
mkNodeAddress xs root = foldl' (flip goDown) (NodeAddress [] root Nothing root [] emptyBNP 0) xs

-- | Create multiple 'NodeAddress'es from a list of 'NodePath's, originating
-- from the given root node.
mkNodeAddresses :: IR.Tree -> [NodePath] -> [NodeAddress]
mkNodeAddresses ir paths = mkNodeAddresses' ir $ sort paths

-- | Create multiple 'NodeAddress'es from a sorted list of 'NodePath's,
-- originating from the given root node.
mkNodeAddresses' :: IR.Tree -> [NodePath] -> [NodeAddress]
mkNodeAddresses' ir []     = []
mkNodeAddresses' ir (p:pp) = (\(_,_,x) -> first : x) $ foldr go (first, p, []) pp
  where
    first = mkNodeAddress p ir

    go :: NodePath -> (NodeAddress, NodePath, [NodeAddress]) -> (NodeAddress, NodePath, [NodeAddress])
    go path (prevAddr, prevPath, acc) = (addr, path, addr : acc)
      where
        addr = goRel (mkRelPath prevPath path) prevAddr

    mkRelPath :: NodePath -> NodePath -> NodePath
    mkRelPath x y = case distinctSuffixes x y of
        ([], ys) -> ys
        (xs, ys) -> negate (length xs) : ys

-- | Slow, use 'getPathReversed' where possible
getPath :: NodeAddress -> NodePath
getPath = reverse . getPathReversed

getAbsolutePath :: NodeAddress -> NodePath
getAbsolutePath a =  reverse $ getPathReversed a ++ getAbsoluteRootPath a

depth :: NodeAddress -> Int
depth = length . getPathReversed

depthAbsolute :: NodeAddress -> Int
depthAbsolute a = length $ getPathReversed a ++ getAbsoluteRootPath a

-- | Get the number of children of a given node.
numChildren :: NodeAddress -> Int
numChildren = length . IR.getChildren . getNode

getChildren :: NodeAddress -> [NodeAddress]
getChildren a = (flip goDown) a <$> [0..numChildren a - 1]

-- | Returns the position, of the given node, in its parent's list of children.
getIndex :: NodeAddress -> Int
getIndex a | isRoot a = error "Can't obtain index of a root address!"
getIndex a = head $ getPathReversed a

isRoot :: NodeAddress -> Bool
isRoot = isNothing . getParent

getRootAddress :: NodeAddress -> NodeAddress
getRootAddress a = mkNodeAddress [] (getRoot a)

isChildOf :: NodeAddress -> NodeAddress -> Bool
a `isChildOf` b = getRoot a == getRoot b && (getPathReversed b `isSuffixOf` getPathReversed a)

-- | Return a node address relative to the given one; a negative integer means
-- going up so many levels; zero or a positive integer means going to the
-- respective child.
goRel :: [Int] -> NodeAddress -> NodeAddress
goRel []     = id
goRel (i:is) | i < 0     = goRel is . goUpX (negate i)
             | otherwise = goRel is . goDown i

goUp :: NodeAddress -> NodeAddress
goUp NodeAddress { getParent = Just p }  = p
goUp NodeAddress { getParent = Nothing } = error "goUp: No parent for Root"

goUpX :: Int -> NodeAddress -> NodeAddress
goUpX 0 a = a
goUpX n a = goUpX (n-1) $ goUp a

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs n _ ir r bnp h) = 
    NodeAddress (x : xs) n' (Just parent) ir r (appendBNP x bnp) (Hash.hashWithSalt h x)
  where
    n' = IR.getChildren n !! x

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _             _ _ _ _) | head xs == 0 = na
goLeft na@(NodeAddress _  _ Nothing       _ _ _ _) = na
goLeft    (NodeAddress xs _ (Just parent) _ _ _ _) = goDown (head xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ Nothing       _ _ _ _) = na
goRight    (NodeAddress xs _ (Just parent) _ _ _ _) = goDown (head xs + 1) parent

append :: NodeAddress -> NodeAddress -> NodeAddress
append a b | isRoot a = b
append a b | isRoot b = a
append a b = NodeAddress {
                getPathReversed     = (getIndex b) : (getPathReversed newParent),
                getNode             = getNode b,
                getRoot             = getRoot a,
                getParent           = Just $ newParent,
                getAbsoluteRootPath = getAbsoluteRootPath a,
                getPathBNP          = appendBNP (getIndex b) (getPathBNP newParent),
                getHash             = Hash.hashWithSalt (getHash newParent) (getIndex b)
             }
  where
    newParent = append a $ fromJust $ getParent b

-- | Creates a 'NodeAddress' relative from the give node.
crop :: NodeAddress -> NodeAddress
crop a = NodeAddress [] (getNode a) Nothing (getNode a) ((getPathReversed a) ++ (getAbsoluteRootPath a)) emptyBNP 0
