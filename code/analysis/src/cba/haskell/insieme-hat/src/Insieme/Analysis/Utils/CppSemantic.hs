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
{-# LANGUAGE FlexibleInstances #-}

module Insieme.Analysis.Utils.CppSemantic (
    
    -- some tools to identify implicit contexts
    isImplicitCtorOrDtor,
    isImplicitConstructor,
    isImplicitDestructor,
    
    isImplicitCtorOrDtorParameter,
    getEnclosingDeclaration,
    
    getEnclosingScope,
    getImplicitDestructorBodies
) where

import Debug.Trace

import Data.List
import Data.Maybe
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import qualified Insieme.Inspire as IR



--- Identification of implicit constructor and destructor calls ---

isImplicitCtorOrDtor :: NodeAddress -> Bool
isImplicitCtorOrDtor a = (depth a > 6) && isCorrectLambda && (nodeType == IR.Struct || nodeType == IR.Union)
  where
    lambdaExpr = goUpX 3 a
    isCorrectLambda = fromMaybe False $ (==a) <$> getLambda lambdaExpr
    
    record = goUpX 2 lambdaExpr
    nodeType = getNodeType record


isImplicitConstructor :: NodeAddress -> Bool
isImplicitConstructor a = isImplicitCtorOrDtor a && getIndex ( goUpX 4 a ) == 2

isImplicitDestructor :: NodeAddress -> Bool
isImplicitDestructor a = isImplicitCtorOrDtor a && getIndex ( goUpX 4 a ) == 3


--- Navigation to 'call-sites' of implicit constructor and destructor calls ---

isImplicitCtorOrDtorParameter :: NodeAddress -> Bool
isImplicitCtorOrDtorParameter a = 
    (depth a > 8) && (getNodeType a == IR.Variable) && (getNodeType params == IR.Parameters) && 
                     (getNodeType lambda == IR.Lambda) && isImplicitCtorOrDtor lambda 
  where
    params = goUp a    
    lambda = goUp params


getEnclosingDeclaration :: NodeAddress -> NodeAddress
getEnclosingDeclaration a = case getNodeType a of
    IR.Declaration -> a
    _ | isRoot a   -> error "No enclosing declaration found!"
    _              -> getEnclosingDeclaration $ fromJust $ getParent a 

  
--- Destructor Utilities ---

getEnclosingScope :: NodeAddress -> NodeAddress
getEnclosingScope s = case () of 
    _ | getNodeType s == IR.CompoundStmt -> s
      | isRoot s                         -> error "No enclosing compound statement found!"
      | otherwise                        -> getEnclosingScope $ fromJust $ getParent s


getImplicitDestructorBodies :: NodeAddress -> [NodeAddress]
getImplicitDestructorBodies scope = case getNodeType scope of 
    
    IR.CompoundStmt -> reverse bodies 
    
  where
    
    declarations = goDown 0 <$> (filter isDeclStmt $ getChildren scope) 
      where
        isDeclStmt t = getNodeType t == IR.DeclarationStmt

    classes = catMaybes $ go <$> declarations
      where
        go d = getReferencedType d >>= getTagType

    bodies = (goDown 2 . fromJust . getLambda) <$> dtors
        where
            dtors =  catMaybes $ getDestructor <$> classes

