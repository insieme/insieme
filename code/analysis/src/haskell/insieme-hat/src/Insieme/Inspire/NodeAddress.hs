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
    getAddress,
    getIndex,
    getIR,
    getNode,
    getParent,
    isRoot,
    getRoot,
    getRootIR,
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
import qualified Data.Map as Map
import qualified Insieme.Inspire as IR

data NodeAddress = NodeAddress [Int] IR.Inspire (Maybe NodeAddress)

instance Eq NodeAddress where
    -- TODO implement NodePath
    na@(NodeAddress a _ _) == nb@(NodeAddress b _ _) = (a == b) -- && (getRoot na == getRoot nb)

instance Ord NodeAddress where
    (NodeAddress a _ _) <= (NodeAddress b _ _) = a <= b
    
instance Show NodeAddress where
    show = prettyShow

-- | Create a 'NodeAddress' from a list of indizes and a root node.
mkNodeAddress :: [Int] -> IR.Inspire -> NodeAddress
mkNodeAddress xs ir = foldl' (flip goDown) (NodeAddress [] ir Nothing) xs

getAddress :: NodeAddress -> [Int]
getAddress (NodeAddress na _ _) = na

getIndex :: NodeAddress -> Int
getIndex (NodeAddress na _ _) = head na

getIR :: NodeAddress -> IR.Inspire
getIR (NodeAddress _ ir _) = ir

getNode :: NodeAddress -> Tree IR.NodeType
getNode = IR.getTree . getIR

getParent :: NodeAddress -> Maybe NodeAddress
getParent (NodeAddress _ _ parent) = parent

isRoot :: NodeAddress -> Bool
isRoot (NodeAddress _ _ Nothing ) = True
isRoot (NodeAddress _ _ _       ) = False

getRoot :: NodeAddress -> Tree IR.NodeType
getRoot = IR.getTree . getRootIR

getRootIR :: NodeAddress -> IR.Inspire
getRootIR (NodeAddress _ ir Nothing      ) = ir
getRootIR (NodeAddress _ _  (Just parent)) = getRootIR parent

prettyShow :: NodeAddress -> String
prettyShow (NodeAddress na _ _ ) = '0' : concat ['-' : show x | x <- reverse na]

goUp :: NodeAddress -> NodeAddress
goUp na@(NodeAddress _ _ Nothing  ) = na
goUp    (NodeAddress _ _ (Just na)) = na

goDown :: Int -> NodeAddress -> NodeAddress
goDown x parent@(NodeAddress xs ir _) = NodeAddress (x : xs) ir' (Just parent)
  where
    ir' = ir{IR.getTree = (subForest $ IR.getTree ir) !! x}

goLeft :: NodeAddress -> NodeAddress
goLeft na@(NodeAddress xs _ _            ) | head xs == 0 = na
goLeft na@(NodeAddress xs _ Nothing      ) = na
goLeft    (NodeAddress xs _ (Just parent)) = goDown (head xs - 1) parent

goRight :: NodeAddress -> NodeAddress
goRight na@(NodeAddress _  _ Nothing      ) = na
goRight    (NodeAddress xs _ (Just parent)) = goDown (head xs + 1) parent

lookupBuiltin :: NodeAddress -> String -> Maybe (Tree IR.NodeType)
lookupBuiltin addr needle = Map.lookup needle (IR.getBuiltins $ getIR addr)

isBuiltin :: NodeAddress -> String -> Bool
isBuiltin addr needle = fromMaybe False $ (== getNode addr) <$> lookupBuiltin addr needle
