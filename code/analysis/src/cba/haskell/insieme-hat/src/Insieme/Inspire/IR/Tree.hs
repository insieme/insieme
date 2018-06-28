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
{-# LANGUAGE ViewPatterns      #-}

module Insieme.Inspire.IR.Tree where

import Data.List
import Data.Hashable
import Data.Set (Set)
import Control.DeepSeq
import GHC.Generics (Generic)

import qualified Data.Set as Set

import Insieme.Inspire.IR.NodeType
import Insieme.Inspire.IR.HashCons
import {-# UP #-} Insieme.Inspire.NodeReference

-- TODO: Rename to 'Node'
data Tree = MkTree {
      mtId        :: !(Maybe Int),
      mtInnerTree :: {-# UNPACK#-} !(HC InnerTree)
    } deriving (Show, Eq, Ord, Generic, NFData, Hashable)

instance HashConsed Tree where
    hcdId (MkTree _ hc) = hcdId hc
    {-# INLINE hcdId #-}

    hcdAttachFinalizer (MkTree _ hc) act =
        hcdAttachFinalizer hc act
    {-# INLINE hcdAttachFinalizer #-}

    hcdHVal (MkTree _ hc) = hcdHVal hc


instance NodeReference Tree where
    child i  = (!!i) . children
    children = getChildren

data InnerTree = InnerTree {
      itNodeType    :: !NodeType,
      itChildren    :: ![Tree],
      itBuiltinTags :: ![String]
    } deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance HashCons InnerTree where

pattern Node :: NodeType -> [Tree] -> Tree
pattern Node x y <- MkTree _ (hcVal -> InnerTree x y _)

innerTree :: Tree -> InnerTree
innerTree = hcVal . mtInnerTree

getID :: Tree -> Maybe Int
getID = mtId

getNodeType :: Tree -> NodeType
getNodeType = itNodeType . innerTree

getChildren :: Tree -> [Tree]
getChildren = itChildren . innerTree

builtinTags :: Tree -> [String]
builtinTags = itBuiltinTags . innerTree

unsafeMkNode :: Int -> NodeType -> [Tree] -> [String] -> Tree
unsafeMkNode i nt ch bt = MkTree (Just i) (hc $ InnerTree nt ch bt)

unsafeMkNode' :: Maybe Int -> NodeType -> [Tree] -> [String] -> Tree
unsafeMkNode' i nt ch bt = MkTree i (hc $ InnerTree nt ch bt)

mkNode :: NodeType -> [Tree] -> [String] -> Tree
mkNode nt ch bt = MkTree Nothing (hc $ InnerTree nt ch bt)

getBuiltinTarget :: Tree -> Tree
getBuiltinTarget (Node LambdaExpr [_,tag,(Node LambdaDefinition bindings)]) = res
  where
    Just res = (child 1) <$> find p bindings
    p (Node LambdaBinding [t,_]) = tag == t
    p _ = error "Invalid node composition!"
getBuiltinTarget n = n

isBuiltin :: Tree -> String -> Bool
isBuiltin t s = elem s $ builtinTags $ getBuiltinTarget t

isAnyOfBuiltin :: Set String -> Tree -> Bool
isAnyOfBuiltin s t = any (flip Set.member s) $ builtinTags $ getBuiltinTarget t

isaBuiltin :: Tree -> Bool
isaBuiltin t = not $ null $ builtinTags $ getBuiltinTarget t

hasMaterializingTag :: Tree -> Bool
hasMaterializingTag n = isBuiltin n "tag_materializing"
