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

module Insieme.Inspire.IR.Transform
    ( substitute
    , substitute'
    , substituteInLocalScope
    , substituteInLocalScope'
    , removeIds
    , removeIds'
    ) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Insieme.Inspire.IR.NodeType as NT
import Insieme.Inspire.IR.Tree as IR

type TreeSubst = Map IR.Tree IR.Tree

substitute :: TreeSubst -> IR.Tree -> IR.Tree
substitute s t = evalState (substitute' t) s

substitute' :: IR.Tree -> State TreeSubst IR.Tree
substitute' t = do
  s <- get
  case Map.lookup t s of
    Just t' -> return t'
    Nothing -> do
        let ch = getChildren t
        ch' <- mapM substitute' ch
        let t' | ch' == ch = t
               | otherwise = t { getID = Nothing
                               , getChildren = ch'
                               , builtinTags = []
                               }
        modify (Map.insert t t')
        return t'

substituteInLocalScope :: TreeSubst -> IR.Tree -> IR.Tree
substituteInLocalScope s t = evalState (substituteInLocalScope' t) s

substituteInLocalScope' :: IR.Tree -> State TreeSubst IR.Tree
substituteInLocalScope' t = do
  s <- get
  case getNodeType t of
    NT.Lambda -> return t
    _ -> case Map.lookup t s of
        Just t' -> return t'
        Nothing -> do
            let ch = getChildren t
            ch' <- mapM substitute' ch
            let t' | ch' == ch = t
                   | otherwise = t { getID = Nothing
                                   , getChildren = ch'
                                   , builtinTags = []
                                   }
            modify (Map.insert t t')
            return t'



removeIds :: IR.Tree -> IR.Tree
removeIds t = evalState (removeIds' t) Map.empty

removeIds' :: IR.Tree -> State TreeSubst IR.Tree
removeIds' t = do
  s <- get
  case Map.lookup t s of
    Just t' -> return t'
    Nothing -> do
        let ch = getChildren t
        ch' <- mapM removeIds' ch
        let t' = t { getID = Nothing, getChildren = ch' }
        modify (Map.insert t t')
        return t'
