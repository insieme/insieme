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

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

module Insieme.Inspire.IR.Tree where

import Data.Maybe
import Data.List
import Control.DeepSeq
import GHC.Generics (Generic)

import Insieme.Inspire.IR.NodeType
import {-# UP #-} Insieme.Inspire.NodeReference

-- TODO: Rename to 'Node'
data Tree = MkTree {
      mtId        :: Maybe Int,
      mtInnerTree :: InnerTree
    } deriving (Show, Generic, NFData)

instance Eq Tree where
    MkTree { mtId = Just ida } == MkTree { mtId = Just idb } =
        ida == idb
    a == b =
        mtInnerTree a == mtInnerTree b

instance Ord Tree where
    compare Tree{ getID = Just ida } Tree{ getID = Just idb } | ida == idb = EQ
    compare a b = compare (mtInnerTree a) (mtInnerTree b)

instance NodeReference Tree where
    child i  = (!!i) . children 
    children = getChildren

treeExactEq :: Tree -> Tree -> Bool
treeExactEq a b = mtId a == mtId b && mtInnerTree a == mtInnerTree b

data InnerTree = InnerTree {
      itNodeType    :: NodeType,
      itChildren    :: [Tree],
      itBuiltinTags :: [String]
    } deriving (Eq, Ord, Show, Generic, NFData)

pattern Node :: NodeType -> [Tree] -> Tree
pattern Node x y <- MkTree _ (InnerTree x y _)

pattern Tree :: Maybe Int -> NodeType -> [Tree] -> [String] -> Tree
pattern Tree { getID, getNodeType, getChildren, builtinTags }
      = MkTree getID (InnerTree getNodeType getChildren builtinTags)

unsafeMkNode :: Int -> NodeType -> [Tree] -> [String] -> Tree
unsafeMkNode i nt ch bt = MkTree (Just i) (InnerTree nt ch bt)

mkNode :: NodeType -> [Tree] -> [String] -> Tree
mkNode nt ch bt = MkTree Nothing (InnerTree nt ch bt)

getBuiltinTarget :: Tree -> Tree
getBuiltinTarget n@(Node LambdaExpr [_,tag,(Node LambdaDefinition bindings)]) = res
  where
    Just res = (child 1) <$> find p bindings
    p (Node LambdaBinding [t,_]) = tag == t
getBuiltinTarget n = n

isBuiltin :: Tree -> String -> Bool
isBuiltin t s = elem s $ builtinTags $ getBuiltinTarget t

isaBuiltin :: Tree -> Bool
isaBuiltin t = not $ null $ builtinTags $ getBuiltinTarget t

hasMaterializingTag :: Tree -> Bool
hasMaterializingTag n = isBuiltin n "tag_materializing"
