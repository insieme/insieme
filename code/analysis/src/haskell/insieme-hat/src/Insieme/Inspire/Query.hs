{-
 - Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 -                Institute of Computer Science,
 -               University of Innsbruck, Austria
 -
 - This file is part of the INSIEME Compiler and Runtime System.
 -
 - We provide the software of this file (below described as "INSIEME")
 - under GPL Version 3.0 on an AS IS basis, and do not warrant its
 - validity or performance.  We reserve the right to update, modify,
 - or discontinue this software at any time.  We shall have no
 - obligation to supply such updates or modifications or any other
 - form of support to you.
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
 -
 - All copyright notices must be kept intact.
 -
 - INSIEME depends on several third party software packages. Please
 - refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 - regarding third party software licenses.
 -}

module Insieme.Inspire.Query where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Insieme.Inspire as IR

--
-- * Node Reference
--

class NodeReference a where
    node  :: a -> IR.Tree
    child :: Int -> a -> a
    
    children :: a -> [a]
    children a = [ child i a | i <- [0 .. IR.numChildren (node a) - 1]]
    

instance NodeReference IR.Tree where
    node  = id
    child = IR.goDown



getNodeType :: NodeReference a => a -> IR.NodeType
getNodeType = IR.getNodeType . node

--
-- * Queries for Node Reference
--

isVariable :: NodeReference a => a -> Bool
isVariable = (==IR.Variable) . IR.getNodeType . node

-- ** Type

-- | Return 'True' if the given node is a type.
isType :: NodeReference a => a -> Bool
isType = (==IR.Type) . IR.toNodeKind . IR.getNodeType . node

-- | Returns the Type of an expression (or, if it is a type already, the given type)
getType :: NodeReference a => a -> Maybe a
getType n | kind == IR.Type        = Just $ n
          | kind == IR.Expression  = Just $ child 0 n
          | ntyp == IR.Lambda      = Just $ child 0 n
          | ntyp == IR.Declaration = Just $ child 0 n
          | otherwise              = Nothing
  where
    ntyp = getNodeType n
    kind = IR.toNodeKind ntyp 

-- *** Unit Type

isUnitType :: NodeReference a => a -> Bool
isUnitType n = case node n of
    IR.NT IR.GenericType (IR.NT (IR.StringValue "unit") _ : _) -> True
    _ -> False

isUnit :: NodeReference a => a -> Bool
isUnit n = fromMaybe False $ isUnitType <$> getType n

-- *** Reference Type

isReferenceType :: NodeReference a => a -> Bool
isReferenceType n = case node n of
    IR.NT IR.GenericType (IR.NT (IR.StringValue "ref") _ : _) -> True
    _ -> False

isReference :: NodeReference a => a  -> Bool
isReference n = fromMaybe False $ isReferenceType <$> getType n

getReferenceType :: NodeReference a => a -> Maybe a
getReferenceType a = mfilter isReference $ getType a

getReferencedType :: NodeReference a => a -> Maybe a
getReferencedType n = (child 0) <$> (child 2) <$> getReferenceType n
                      

-- *** Array Type

isArrayType :: NodeReference a => a -> Bool
isArrayType n = case node n of
    IR.NT IR.GenericType (IR.NT (IR.StringValue "array") _ : _) -> True
    _ -> False

isArray :: NodeReference a => a -> Bool
isArray n = fromMaybe False $ isArrayType <$> getType n


-- *** Tag Type

isTagType :: NodeReference a => a -> Bool
isTagType a | not (isType a) = fromMaybe False $ isTagType <$> getType (node a)

isTagType n | IR.getNodeType (node n) == IR.TagType = True
            | otherwise                             = False


getTagType :: NodeReference a => a -> Maybe a
getTagType a = mfilter isTagType $ getType a


-- *** Record 


getRecord :: NodeReference a => a -> Maybe a
getRecord a = (child 1) <$> binding 
  where
    typ = getTagType a
    tag = fromJust $ (child 0) <$> typ
    bindings = children <$> (child 1) <$> typ
    binding = bindings >>= find go
      where
        go a = (node tag) == ((child 0) $ node a)        
     


getDestructor :: NodeReference a => a -> Maybe a
getDestructor a = if noDtor then Nothing else (child 0) <$> optDtor 
  where
    optDtor = (child 3) <$> getRecord a 
    noDtor = if isJust optDtor then IR.numChildren (node $ fromJust optDtor) == 0 else False 



-- ** Kind

-- *** Function Kinds
checkFunctionKind :: NodeReference a
                  => (IR.FunctionKind -> Bool) -> a -> Bool
checkFunctionKind test n = fromMaybe False $ test <$> (IR.toFunctionKind =<< getType (node n))

isConstructor :: NodeReference a => a -> Bool
isConstructor = checkFunctionKind (==IR.FK_Constructor)

isDestructor :: NodeReference a => a -> Bool
isDestructor = checkFunctionKind (==IR.FK_Destructor)

isConstructorOrDestructor :: NodeReference a => a -> Bool
isConstructorOrDestructor n = isConstructor n || isDestructor n



-- ** Expressions

-- *** Lambda Expressions

getLambda :: NodeReference a => a -> Maybe a
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
    

-- ** Builtins

isBuiltin :: (NodeReference a) => a -> String -> Bool
isBuiltin r n = IR.isBuiltin (node r) n
