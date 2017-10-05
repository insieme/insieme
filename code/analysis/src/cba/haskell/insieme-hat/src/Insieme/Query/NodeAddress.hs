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

module Insieme.Query.NodeAddress where

import Data.Maybe

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as IR
import Insieme.Query.NodeReference

-- * Semantic Queries

-- | Returns 'True' if given variable (in declaration) is a loop iterator.
isLoopIterator :: NodeAddress -> Bool
isLoopIterator a = (IR.depth a >= 2) && ((==IR.ForStmt) $ getNodeType $ IR.goUp $ IR.goUp a)

hasEnclosingStatement :: NodeAddress -> Bool
hasEnclosingStatement a = case IR.getNode a of
    IR.Node n _ | (IR.toNodeKind n) == IR.Statement -> True
    IR.Node n _ | n /= IR.LambdaExpr && n /= IR.Variable && (IR.toNodeKind n) == IR.Expression -> True
    _ -> not (IR.isRoot a) && hasEnclosingStatement (fromJust $ IR.getParent a)

isEntryPoint :: NodeAddress -> Bool
isEntryPoint a = IR.isRoot a || not (hasEnclosingStatement $ fromJust $ IR.getParent a)

isEntryPointParameter :: NodeAddress -> Bool
isEntryPointParameter v | (not . isVariable) v = False
isEntryPointParameter v | IR.isRoot v = False
isEntryPointParameter v = case IR.getNode $ fromJust $ IR.getParent v of
            IR.Node IR.Parameters _ -> not $ hasEnclosingStatement v
            _                     -> False
