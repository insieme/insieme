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

{-# LANGUAGE ConstraintKinds #-}

module Insieme.Query.NodeReference where

import Control.Monad
import Data.List
import Data.Maybe

import Insieme.Inspire (NodeReference(..), node)
import qualified Insieme.Inspire.IR as IR

type NodeLike a = (IR.NodeLike a, NodeReference a)

-- * Queries for 'NodeReference'

getNodeType :: NodeLike a => a -> IR.NodeType
getNodeType = IR.getNodeType . node

isVariable :: NodeLike a => a -> Bool
isVariable = (==IR.Variable) . IR.getNodeType . node

-- ** Value

getBoolValue :: NodeLike a => a -> Bool
getBoolValue a = case getNodeType a of
    IR.BoolValue b -> b
    _ -> error "not of type BoolValue"

getCharValue :: NodeLike a => a -> Char
getCharValue a = case getNodeType a of
    IR.CharValue b -> b
    _ -> error "not of type CharValue"

getIntValue :: NodeLike a => a -> Int
getIntValue a = case getNodeType a of
    IR.IntValue b -> b
    _ -> error "not of type IntValue"

getUIntValue :: NodeLike a => a -> Int
getUIntValue a = case getNodeType a of
    IR.UIntValue b -> b
    _ -> error "not of type UIntValue"

getStringValue :: NodeLike a => a -> String
getStringValue a = case getNodeType a of
    IR.StringValue b -> b
    _ -> error "not of type StringValue"

-- ** Type

-- | Return 'True' if the given node is a type.
isType :: NodeLike a => a -> Bool
isType = (==IR.Type) . IR.toNodeKind . IR.getNodeType . node

-- | Returns the Type of an expression (or, if it is a type already, the given
-- type). This does not return 'IR.NodeType', see 'getNodeType'.
getType :: NodeLike a => a -> Maybe a
getType n | kind == IR.Type        = Just $ n
          | kind == IR.Expression  = Just $ child 0 n
          | ntyp == IR.Lambda      = Just $ child 0 n
          | ntyp == IR.Declaration = Just $ child 0 n
          | otherwise              = Nothing
  where
    ntyp = getNodeType n
    kind = IR.toNodeKind ntyp

-- *** Unit Type

isUnitType :: NodeLike a => a -> Bool
isUnitType n = case node n of
    IR.Node IR.GenericType (IR.Node (IR.StringValue "unit") _ : _) -> True
    _ -> False

isUnit :: NodeLike a => a -> Bool
isUnit n = fromMaybe False $ isUnitType <$> getType n


-- *** Generic Types

getTypeParameter :: Int -> IR.Tree -> IR.Tree
getTypeParameter i (IR.Node IR.GenericType ( _ : _ : (IR.Node IR.Types params) : [] )) = params !! i
getTypeParameter _ _ = error "unexpected NodeType"


-- *** Reference Type

isReferenceType :: NodeLike a => a -> Bool
isReferenceType n = case node n of
    IR.Node IR.GenericType (IR.Node (IR.StringValue "ref") _ : _) -> True
    _ -> False

isReference :: NodeLike a => a  -> Bool
isReference n = fromMaybe False $ isReferenceType <$> getType n

getReferenceType :: NodeLike a => a -> Maybe a
getReferenceType a = mfilter isReference $ getType a

getReferencedType :: NodeLike a => a -> Maybe a
getReferencedType n = (child 0) <$> (child 2) <$> getReferenceType n

data ReferenceKind =
      RK_Plain
    | RK_CppRef
    | RK_CppRRef
    | RK_Unknown
  deriving (Eq, Ord, Show)

getReferenceKind :: NodeLike a => a -> ReferenceKind
getReferenceKind r | isReference r = case kind of
    (IR.Node IR.GenericType ((IR.Node (IR.StringValue    "plain") _) : _)) -> RK_Plain
    (IR.Node IR.GenericType ((IR.Node (IR.StringValue  "cpp_ref") _) : _)) -> RK_CppRef
    (IR.Node IR.GenericType ((IR.Node (IR.StringValue "cpp_rref") _) : _)) -> RK_CppRRef
    _ -> RK_Unknown
  where
    kind = child 3 $ child 2 $ fromJust $ getReferenceType $ node r

getReferenceKind _ = RK_Unknown


-- *** Array Type

isArrayType :: NodeLike a => a -> Bool
isArrayType n = case node n of
    IR.Node IR.GenericType (IR.Node (IR.StringValue "array") _ : _) -> True
    _ -> False

isArray :: NodeLike a => a -> Bool
isArray n = fromMaybe False $ isArrayType <$> getType n

-- TODO check if Int really is a good choice here
getArraySize :: NodeLike a => a -> Int
getArraySize n | not (isArray n) = error "node reference is not an array"
getArraySize n = read $ getStringValue $ gotoValue $ fromJust $ getType n
  where
    gotoValue = child 1 . child 0 . child 1 . child 2

-- *** std::array type

isStdArrayType :: NodeLike a => a -> Bool
isStdArrayType n = case node n of
    IR.Node IR.GenericType (IR.Node (IR.StringValue "IMP_std_colon__colon_array") _ : _) -> True
    _ -> False

isStdArray :: NodeLike a => a -> Bool
isStdArray n = fromMaybe False $ isStdArrayType <$> getType n

getStdArraySize :: NodeLike a => a -> Int
getStdArraySize n | not (isStdArray n) = error "node reference is not an array"
getStdArraySize n = read $ getStringValue $ gotoValue $ fromJust $ getType n
  where
    gotoValue = child 1 . child 0 . child 1 . child 2


-- *** Tag Type

isTagType :: NodeLike a => a -> Bool
isTagType a | not (isType a) = fromMaybe False $ isTagType <$> getType (node a)

isTagType n | IR.getNodeType (node n) == IR.TagType = True
            | otherwise                             = False


getTagType :: NodeLike a => a -> Maybe a
getTagType a = mfilter isTagType $ getType a


-- *** Record


getRecord :: NodeLike a => a -> Maybe a
getRecord a = (child 1) <$> binding
  where
    typ = getTagType a
    tag = fromJust $ (child 0) <$> typ
    bindings = children <$> (child 1) <$> typ
    binding = bindings >>= find go
      where
        go x = (node tag) == ((child 0) $ node x)

getFieldNames :: NodeLike a => a -> Maybe [String]
getFieldNames a = map (getStringValue . node . child 0) <$> fields
  where
    fields = (children . child 1) <$> getRecord a


getConstructorAccepting :: (NodeLike a, NodeLike b) => [b] -> a -> Maybe a
getConstructorAccepting paramTypes tagType = ctor
  where
    ctors = child 2 <$> getRecord tagType
    ctor = (children <$> ctors) >>= (find filter)

    requiredParamTypes = node <$> paramTypes
    filter lambda = eqList requiredParamTypes curParamTypes 
      where
        curParamTypes = (tail $ children $ child 0 $ fromJust $ getType $ node lambda)

    -- a type-list comparison interpreting tag types and tag type references equivalent

    eqList [] [] = True
    eqList []  _ = False
    eqList _  [] = False
    eqList (x:xs) (y:ys) = eq x y && eqList xs ys

    eq a b | a == b = True
    eq (IR.Node t1 c1) (IR.Node t2 c2) | t1 == t2 = eqList c1 c2
    eq (IR.Node IR.TagType (a : _) ) b@(IR.Node IR.TagTypeReference _ ) | a == b = True
    eq a@(IR.Node IR.TagTypeReference _ ) (IR.Node IR.TagType (b : _) ) | a == b = True
    eq _ _ = False




getDestructor :: NodeLike a => a -> Maybe a
getDestructor a = if noDtor then Nothing else (child 0) <$> optDtor
  where
    optDtor = (child 3) <$> getRecord a
    noDtor = if isJust optDtor then null $ children (node $ fromJust optDtor) else False



-- ** Kind

-- *** Function Kinds

checkFunctionKind :: NodeLike a
                  => (IR.FunctionKind -> Bool) -> a -> Bool
checkFunctionKind test n = fromMaybe False $ test <$> (IR.toFunctionKind =<< getType (node n))

isConstructor :: NodeLike a => a -> Bool
isConstructor = checkFunctionKind (==IR.FK_Constructor)

isDestructor :: NodeLike a => a -> Bool
isDestructor = checkFunctionKind (==IR.FK_Destructor)

isConstructorOrDestructor :: NodeLike a => a -> Bool
isConstructorOrDestructor n = isConstructor n || isDestructor n

-- ** Expressions

-- *** Lambda Expressions

getLambda :: NodeLike a => a -> Maybe a
getLambda a = case () of

    _ | isLambda     -> Just a
      | isLambdaExpr -> lambda
      | otherwise    -> Nothing

  where
    isLambda     = getNodeType a == IR.Lambda
    isLambdaExpr = getNodeType a == IR.LambdaExpr

    ref = child 1 $ node a
    defs = children $ child 2 a

    lambda = (child 1) <$> find go defs
      where
        go b = ref == ((child 0) $ node b)

-- *** Literal Expressions

isLiteral :: NodeLike a => a -> Bool
isLiteral a = getNodeType a == IR.Literal

getLiteralValue :: NodeLike a => a -> Maybe String
getLiteralValue a | isLiteral a = case node a of
    IR.Node IR.Literal (_ : (IR.Node (IR.StringValue name) _) : _) -> Just name
    _ -> Nothing

getLiteralValue _ = Nothing


-- *** Call Expressions

isCallExpr :: NodeLike a => a -> Bool
isCallExpr a = getNodeType a == IR.CallExpr

-- ** Builtins

isBuiltin :: (NodeLike a) => a -> String -> Bool
isBuiltin r n = IR.isBuiltin (node r) n

-- ** Operator

isOperator :: (NodeLike a) => a -> String -> Bool
isOperator a n = fromMaybe False $ (==n) <$> getLiteralValue a

