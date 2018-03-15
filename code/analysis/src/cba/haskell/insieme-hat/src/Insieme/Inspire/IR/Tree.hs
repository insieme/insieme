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

module Insieme.Inspire.IR.Tree
    ( Tree
    , unTree
    , Tree' (..)
    , pattern Tree
    , pattern Node
    , getChildren
    , getNodeType
    , builtinTags
    , getBuiltinTarget
    , mkNode
    , isBuiltin
    , isaBuiltin
    , hasMaterializingTag
    ) where

import Data.List
import Data.HashCons
import Control.DeepSeq
import GHC.Generics (Generic)

import Insieme.Inspire.IR.NodeType
import {-# UP #-} Insieme.Inspire.NodeReference


newtype Tree = MkTree (HC Tree')
    deriving (Eq, Hashable, Generic, NFData)

instance Ord Tree where
    compare (MkTree a) (MkTree b) = hash a `compare` hash b

instance Show Tree where
    show (MkTree a) = show $ getVal a

instance NodeReference Tree where
    child i (MkTree a)  = (!!i) $ tChildren $ getVal a
    children (MkTree a) = tChildren $ getVal a

data Tree' = Tree' {
      tNodeType    :: !NodeType,
      tChildren    :: ![Tree],
      tBuiltinTags :: ![String] -- dont compare
    } deriving (Eq, Show, Generic, NFData, Hashable)

instance HashCons Tree' where

unTree :: Tree -> Tree'
unTree (MkTree hc) = getVal hc

getChildren :: Tree -> [Tree]
getChildren = tChildren . unTree

getNodeType :: Tree -> NodeType
getNodeType = tNodeType . unTree

builtinTags :: Tree -> [String]
builtinTags = tBuiltinTags . unTree

mkNode :: NodeType -> [Tree] -> [String] -> Tree
mkNode nt ch bt = MkTree $ hc $ Tree' nt ch bt

pattern Node :: NodeType -> [Tree] -> Tree
pattern Node x y <- (unTree -> Tree' x y _)

pattern Tree :: NodeType -> [Tree] -> [String] -> Tree
pattern Tree x y z <- (unTree -> Tree' x y z)

getBuiltinTarget :: Tree -> Tree
getBuiltinTarget (Node  LambdaExpr [_, tag, (Node LambdaDefinition binds)]) = res
  where
    Just res = ((!!1) . tChildren) <$> find p (map unTree binds)
    p (Tree' LambdaBinding [t,_] _) = tag == t
    p _ = error "Invalid node composition!"
getBuiltinTarget n = n

isBuiltin :: Tree -> String -> Bool
isBuiltin t s = elem s $ tBuiltinTags $ unTree $ getBuiltinTarget t

isaBuiltin :: Tree -> Bool
isaBuiltin t = not $ null $ tBuiltinTags $ unTree $ getBuiltinTarget t

hasMaterializingTag :: Tree -> Bool
hasMaterializingTag n = isBuiltin n "tag_materializing"

