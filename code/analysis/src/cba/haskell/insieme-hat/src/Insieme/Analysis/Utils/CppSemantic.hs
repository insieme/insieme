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

    -- a utility to spot constructor calls
    callsConstructor,

    -- some tools to identify calls to explicit constructs
    isExplicitConstructorCall,
    callsExplicitConstructor,

    -- some tools to identify calls to implicit constructs
    callsImplicitConstructor,
    getImplicitConstructor,

    callsImplicitCopyConstructor,
    callsImplicitMoveConstructor,

    -- some tools to identify implicit contexts
    isImplicitCtorOrDtor,
    isImplicitConstructor,
    isImplicitDestructor,

    isImplicitCtorOrDtorParameter,
    getEnclosingDeclaration,

    getEnclosingScope,
    getImplicitDestructorBodies,

    -- a utility to identify side-effect free builtin operators
    isSideEffectFreeBuiltin

) where

import Data.Maybe

import Insieme.Inspire (NodeAddress)
import qualified Insieme.Inspire as I
import Insieme.Query as Q




hasMoreReferences :: I.Tree -> I.Tree -> Bool
hasMoreReferences a b | isReference a && (not . isReference $ b) = True
hasMoreReferences a b | isReference a && isReference b = hasMoreReferences (getTypeParameter 0 a) (getTypeParameter 0 b)
hasMoreReferences _ _ = False


isCtorBasedConversion' :: ReferenceKind -> I.Tree -> I.Tree -> Bool
isCtorBasedConversion' refKind srcType trgType | isReference srcType && isReference trgType =
    -- if the source reference is a plain, but the target not, an implicit constructor is called
    (srcRefKind == refKind) && (trgRefKind == RK_Plain) && (not $ isReference srcObjType) && (not $ isReference trgObjType)
  where
    srcRefKind = getReferenceKind srcType
    trgRefKind = getReferenceKind trgType

    srcObjType = fromJust $ getReferencedType srcType
    trgObjType = fromJust $ getReferencedType trgType

isCtorBasedConversion' _ _ _ = False


isCopyCtorConversion :: I.Tree -> I.Tree -> Bool
isCopyCtorConversion = isCtorBasedConversion' RK_CppRef

isMoveCtorConversion :: I.Tree -> I.Tree -> Bool
isMoveCtorConversion = isCtorBasedConversion' RK_CppRRef

isCtorBasedConversion :: I.Tree -> I.Tree -> Bool
isCtorBasedConversion a b = isCopyCtorConversion a b || isMoveCtorConversion a b


-- tests whether a given combination of declaration and init type causes a materializiation
isMaterializationOf :: I.Tree -> I.Tree -> Bool
isMaterializationOf declType initType | hasMoreReferences declType initType = True
isMaterializationOf declType initType | isCtorBasedConversion initType declType = True
isMaterializationOf _ _ = False

-- tests whether the given node is a materializing declaration
isMaterializingDeclaration :: I.Tree -> Bool
isMaterializingDeclaration n@(I.Node I.Declaration _) = hasMaterializingTag n
isMaterializingDeclaration _ = False


-- tests whether the given node is a materializing call
isMaterializingCall :: I.Tree -> Bool
isMaterializingCall n@(I.Node I.CallExpr _) = hasMaterializingTag n
isMaterializingCall _ = False


--- Tests whether a call is a constructor call

isExplicitConstructorCall :: NodeAddress -> Bool
isExplicitConstructorCall addr = case I.getNode addr of
    (I.Node I.CallExpr ( _ : fun : _ )) -> isConstructor fun
    _ -> False


--- Tests whether a declaration is involving an explicit constructor call

callsConstructor :: NodeAddress -> Bool
callsConstructor addr = callsExplicitConstructor addr || callsImplicitConstructor addr


--- Tests whether a declaration is involving an explicit constructor call

callsExplicitConstructor :: NodeAddress -> Bool
callsExplicitConstructor addr = case Q.getNodeType addr of
    I.Declaration -> isExplicitConstructorCall $ I.goDown 1 addr
    _ -> False


--- Tests whether a declaration is involving an implicit constructor call

callsImplicitConstructor :: NodeLike a => a -> Bool
callsImplicitConstructor addr = case I.node addr of
    (I.Node I.Declaration [declType,I.Node _ (initType:_)]) -> isCtorBasedConversion initType declType
    _ -> False

callsImplicitCopyConstructor :: NodeAddress -> Bool
callsImplicitCopyConstructor addr = case I.getNode addr of
    (I.Node I.Declaration [declType,I.Node _ (initType:_)]) -> isCopyCtorConversion initType declType
    _ -> False

callsImplicitMoveConstructor :: NodeAddress -> Bool
callsImplicitMoveConstructor addr = case I.getNode addr of
    (I.Node I.Declaration [declType,I.Node _ (initType:_)]) -> isMoveCtorConversion initType declType
    _ -> False


-- locates a matching implicit constructor in the given declaration
getImplicitConstructor :: NodeAddress -> Maybe NodeAddress
getImplicitConstructor addr | not (callsImplicitConstructor addr) =
    error "Can not obtain implicit constructor from expression without implicit constructor."

getImplicitConstructor addr = case I.getNode addr of
    (I.Node I.Declaration [declType,I.Node _ (initType:_)]) -> case nodeType of

        I.TagType -> getConstructorAccepting [initType] declObjType
          where
            declTypeAddr = I.goDown 0 $ addr
            declObjType = fromJust $ getReferencedType declTypeAddr

        -- for intercepted and variable cases, we point to the declared type
        I.GenericType  -> genericCtor
        I.TypeVariable -> genericCtor

        _ -> error $ "Unsupported type with constructor encountered: " ++ (show nodeType) ++ " @ " ++ (show addr)

      where

        nodeType = getNodeType $ fromJust $ getReferencedType declType

        genericCtor = getReferencedType $ I.goDown 0 addr

    _ -> error "Should not be reachable!" -- due to filter above!




--- Identification of implicit constructor and destructor calls ---

isImplicitCtorOrDtor :: NodeAddress -> Bool
isImplicitCtorOrDtor a = (I.depth a > 6) && isCorrectLambda && (nodeType == I.Struct || nodeType == I.Union)
  where
    lambdaExpr = I.goUpX 3 a
    isCorrectLambda = fromMaybe False $ (==a) <$> getLambda lambdaExpr

    record = I.goUpX 2 lambdaExpr
    nodeType = getNodeType record


isImplicitConstructor :: NodeAddress -> Bool
isImplicitConstructor a = isImplicitCtorOrDtor a && I.getIndex ( I.goUpX 4 a ) == 2

isImplicitDestructor :: NodeAddress -> Bool
isImplicitDestructor a = isImplicitCtorOrDtor a && I.getIndex ( I.goUpX 4 a ) == 3


--- Navigation to 'call-sites' of implicit constructor and destructor calls ---

isImplicitCtorOrDtorParameter :: NodeAddress -> Bool
isImplicitCtorOrDtorParameter a =
    (I.depth a > 8) && (getNodeType a == I.Variable) && (getNodeType params == I.Parameters) &&
                     (getNodeType lambda == I.Lambda) && isImplicitCtorOrDtor lambda
  where
    params = I.goUp a
    lambda = I.goUp params


getEnclosingDeclaration :: NodeAddress -> NodeAddress
getEnclosingDeclaration a = case getNodeType a of
    I.Declaration -> a
    _ | I.isRoot a   -> error "No enclosing declaration found!"
    _              -> getEnclosingDeclaration $ fromJust $ I.getParent a


--- Destructor Utilities ---

getEnclosingScope :: NodeAddress -> NodeAddress
getEnclosingScope s = case () of
    _ | getNodeType s == I.CompoundStmt -> s
      | I.isRoot s                         -> error "No enclosing compound statement found!"
      | otherwise                        -> getEnclosingScope $ fromJust $ I.getParent s


getImplicitDestructorBodies :: NodeAddress -> [NodeAddress]
getImplicitDestructorBodies scope = case getNodeType scope of

    I.CompoundStmt -> reverse bodies

    _ -> error "unhandled getImplicitDestructorBodies case"

  where

    declarations = I.goDown 0 <$> (filter isDeclStmt $ I.children scope)
      where
        isDeclStmt t = getNodeType t == I.DeclarationStmt

    classes = catMaybes $ go <$> declarations
      where
        go d = getReferencedType d >>= getTagType

    bodies = (I.goDown 2 . fromJust . getLambda) <$> dtors
        where
            dtors =  catMaybes $ getDestructor <$> classes


--- Side-effect free operators ---

isSideEffectFreeBuiltin :: NodeAddress -> Bool
isSideEffectFreeBuiltin addr = any (Q.isBuiltin addr)
        [
          "ref_kind_cast", "ref_const_cast",
          "ref_scalar_to_ref_array",
          "ref_array_element", "ref_member_access", "ref_component_access",
          "ptr_from_array", "ptr_to_array", "ptr_to_ref", "ptr_null", "ptr_const_cast",
          "ptr_cast", "ptr_add", "ptr_sub",
          "bool_not",
          "num_cast"
        ]
