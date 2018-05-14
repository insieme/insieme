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
import Data.HashMap.Lazy (HashMap)
import Data.Maybe
import qualified Data.HashMap.Lazy as HashMap -- TODO: Strict?

import Insieme.Inspire.IR.NodeType as NT
import Insieme.Inspire.IR.Tree as IR

import qualified Insieme.Inspire.Visit.NodeMap as NodeMap



type TreeSubst = HashMap IR.Tree IR.Tree

substitutePrunable :: (IR.Tree -> Bool) -> TreeSubst -> IR.Tree -> IR.Tree
substitutePrunable prune s t = evalState (substitutePrunable' prune t) s

substitutePrunable' :: (IR.Tree -> Bool) -> IR.Tree -> State TreeSubst IR.Tree
substitutePrunable' prune t = do
  s <- get
  case HashMap.lookup t s of
    Just t' -> return t'
    Nothing | prune t -> return t
    Nothing -> do
       let ch = getChildren t
       ch' <- mapM (substitutePrunable' prune) ch
       let t' | ch' == ch = t
              | otherwise = mkNode (getNodeType t) ch' []
       modify (HashMap.insert t t')
       return t'


substitute :: TreeSubst -> IR.Tree -> IR.Tree
substitute = substitutePrunable $ const False

substituteInLocalScope :: TreeSubst -> IR.Tree -> IR.Tree
substituteInLocalScope = substitutePrunable $ (NT.Lambda==) . getNodeType



removeIds :: IR.Tree -> IR.Tree
removeIds t = evalState (removeIds' t) HashMap.empty

removeIds' :: IR.Tree -> State TreeSubst IR.Tree
removeIds' t = do
  s <- get
  case HashMap.lookup t s of
    Just t' -> return t'
    Nothing -> do
        let ch = getChildren t
        ch' <- mapM removeIds' ch
        let t' = mkNode (getNodeType t) ch' (builtinTags t)
        modify (HashMap.insert t t')
        return t'
