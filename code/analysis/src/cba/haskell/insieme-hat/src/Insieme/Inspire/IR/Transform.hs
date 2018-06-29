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
    ( substitutePrunable
    , substitute
    , substituteInLocalScope
    , removeIds
    , removeIds'
    ) where


import Control.Monad.State
import Data.Maybe

import Insieme.Inspire.IR.HCMap (HCMap)
import qualified Insieme.Inspire.IR.HCMap as HCMap

import Insieme.Inspire.IR.NodeType as NT
import Insieme.Inspire.IR.Tree as IR


import qualified Insieme.Inspire.Visit.NodeMap as NodeMap

substitutePrunable :: (IR.Tree -> Bool) -> (IR.Tree -> IR.Tree) -> IR.Tree -> IR.Tree
substitutePrunable prune subst t = evalState (substitutePrunable' prune subst t) HCMap.empty

substitutePrunable' :: (IR.Tree -> Bool) -> (IR.Tree -> IR.Tree) -> IR.Tree -> State (HCMap IR.Tree IR.Tree) IR.Tree
substitutePrunable' prune subst t0 = do
  s <- get
  case HCMap.lookup t0 s of
    Just t1 -> return t1
    Nothing | prune t0 -> return t0
    Nothing -> do
       let ch = getChildren t0
       ch' <- mapM (substitutePrunable' prune subst) ch
       let t1 = subst t0
       let t2 | ch' == ch = t1
              | otherwise = mkNode (getNodeType t1) ch' []
       modify (HCMap.insert t0 t2)
       return t2


substitute :: (IR.Tree -> IR.Tree) -> IR.Tree -> IR.Tree
substitute = substitutePrunable $ const False

substituteInLocalScope :: (IR.Tree -> IR.Tree) -> IR.Tree -> IR.Tree
substituteInLocalScope = substitutePrunable $ (NT.Lambda==) . getNodeType


removeIds :: IR.Tree -> IR.Tree
removeIds t = evalState (removeIds' t) HCMap.empty

removeIds' :: IR.Tree -> State (HCMap IR.Tree IR.Tree) IR.Tree
removeIds' t = do
  s <- get
  case HCMap.lookup t s of
    Just t' -> return t'
    Nothing -> do
        let ch = getChildren t
        ch' <- mapM removeIds' ch
        let t' = mkNode (getNodeType t) ch' (builtinTags t)
        modify (HCMap.insert t t')
        return t'
