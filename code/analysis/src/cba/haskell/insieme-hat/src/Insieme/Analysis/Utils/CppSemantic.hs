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

    -- some tools to deduce construct properties
    isMaterializingDeclaration,
    isMaterializingCall,

    -- some tools to identify calls to implicit constructs
    callsImplicitConstructor,
    getImplicitConstructor,

    -- some tools to identify implicit contexts
    isImplicitCtorOrDtor,
    isImplicitConstructor,
    isImplicitDestructor,

    isImplicitCtorOrDtorParameter,
    getEnclosingDeclaration,

    getEnclosingScope,
    getImplicitDestructorBodies
) where

import Data.Maybe
import Insieme.Inspire.NodeAddress
import Insieme.Inspire.Query
import qualified Insieme.Inspire as IR



hasMoreReferences :: IR.Tree -> IR.Tree -> Bool
hasMoreReferences a b | isReference a && (not . isReference $ b) = True
hasMoreReferences a b | isReference a && isReference b = hasMoreReferences (getTypeParameter 0 a) (getTypeParameter 0 b)
hasMoreReferences _ _ = False

isCtorBasedConversion :: IR.Tree -> IR.Tree -> Bool
isCtorBasedConversion srcType trgType | isReference srcType && isReference trgType =
    -- if the source reference is a plain, but the target not, an implicit constructor is called
    (srcRefKind == RK_CppRef || srcRefKind == RK_CppRRef) && (trgRefKind == RK_Plain) && (not $ isReference srcObjType) && (not $ isReference trgObjType)
  where
    srcRefKind = getReferenceKind srcType
    trgRefKind = getReferenceKind trgType

    srcObjType = fromJust $ getReferencedType srcType
    trgObjType = fromJust $ getReferencedType trgType


isCtorBasedConversion _ _ = False

-- tests whether a given combination of declaration and init type causes a materializiation
isMaterializing :: IR.Tree -> IR.Tree -> Bool
isMaterializing declType initType | hasMoreReferences declType initType = True
isMaterializing declType initType | isCtorBasedConversion initType declType = True
isMaterializing _ _ = False

-- tests whether the given node is a materializing declaration
isMaterializingDeclaration :: IR.Tree -> Bool
isMaterializingDeclaration (IR.Node IR.Declaration [declType,IR.Node _ (initType:_)]) = isMaterializing declType initType
isMaterializingDeclaration _ = False


-- tests whether the given node is a materializing call
isMaterializingCall :: IR.Tree -> Bool
isMaterializingCall _ = False        -- the default, for now - TODO: implement real check
-- isMaterializingCall (IR.Node IR.CallExpr ( resType : (IR.Node _ ((IR.Node IR.FunctionType (_:retType:_)):_)) : _)) = hasMoreReferences resType retType
-- isMaterializingCall _ = False



--- Tests whether a declaration is involving an implicit constructor call

callsImplicitConstructor :: NodeAddress -> Bool
callsImplicitConstructor addr = case getNode addr of
    (IR.Node IR.Declaration [declType,IR.Node _ (initType:_)]) -> isCtorBasedConversion initType declType
    _ -> False


-- locates a matching implicit constructor in the given declaration
getImplicitConstructor :: NodeAddress -> Maybe NodeAddress
getImplicitConstructor addr | not (callsImplicitConstructor addr) = 
    error "Can not obtain implicit constructor from expression without implicit constructor."

getImplicitConstructor addr = case getNode addr of
    (IR.Node IR.Declaration [declType,IR.Node _ (initType:_)]) -> case nodeType of

        IR.TagType -> getConstructorAccepting [initType] declObjType
          where
            declTypeAddr = goDown 0 $ addr
            declObjType = fromJust $ getReferencedType declTypeAddr

        -- for intercepted and variable cases, we point to the declared type
        IR.GenericType  -> genericCtor
        IR.TypeVariable -> genericCtor

        _ -> error $ "Unsupported type with constructor encountered: " ++ (show nodeType) ++ " @ " ++ (show addr)

      where

        nodeType = getNodeType $ fromJust $ getReferencedType declType

        genericCtor = getReferencedType $ goDown 0 addr

    _ -> error "Should not be reachable!" -- due to filter above!




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

    _ -> error "unhandled getImplicitDestructorBodies case"

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

