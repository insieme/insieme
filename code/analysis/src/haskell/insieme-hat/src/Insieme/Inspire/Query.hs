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
import Data.Maybe
import Insieme.Inspire.NodeAddress
import qualified Data.Map.Strict as Map
import qualified Insieme.Inspire as IR

--
-- * Node Reference
--

class NodeReference a where
    node :: a -> IR.Tree

instance NodeReference IR.Tree where
    node = id

instance NodeReference NodeAddress where
    node = getNode

--
-- * Queries for Node Reference
--

isVariable :: NodeReference a => a -> Bool
isVariable = (==IR.Variable) . IR.getNodeType . node

-- ** Type

-- | Return 'True' if the given node is a type.
isType :: IR.NodeType -> Bool
isType = (==IR.Type) . IR.toNodeKind

-- | Returns the Type of an expression (or, if it is a type already, the given type)
getType :: NodeReference a => a -> Maybe IR.Tree
getType n | kind == IR.Type                      = Just $ node n
          | kind == IR.Expression                = Just $ head $ IR.getChildren $ node n
          | IR.getNodeType (node n) == IR.Lambda = Just $ head $ IR.getChildren $ node n
          | otherwise                            = Nothing
  where
    kind = IR.toNodeKind $ IR.getNodeType $ node n

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

getReferencedType :: NodeReference a => a -> Maybe IR.Tree
getReferencedType n = if isReference n
                         then Just $ head $ IR.getChildren $ fromJust $ getType n
                         else Nothing

-- *** Array Type

isArrayType :: NodeReference a => a -> Bool
isArrayType n = case node n of
    IR.NT IR.GenericType (IR.NT (IR.StringValue "array") _ : _) -> True
    _ -> False

isArray :: NodeReference a => a -> Bool
isArray n = fromMaybe False $ isArrayType <$> getType n

-- *** Record Type

isRecordType :: NodeReference a => a -> Bool
isRecordType n | IR.getNodeType (node n) == IR.TagType = True
               | otherwise                             = False

isRecord :: NodeReference a=> a -> Bool
isRecord n = fromMaybe False $ isRecordType <$> getType n

-- ** Kind

-- *** Function Kinds
checkFunctionKind :: NodeReference a
                  => (IR.FunctionKind -> Bool) -> a -> Bool
checkFunctionKind test n = fromMaybe False $ test <$> (IR.toFunctionKind =<< getType n)

isConstructor :: NodeReference a => a -> Bool
isConstructor = checkFunctionKind (==IR.FK_Constructor)

isDestructor :: NodeReference a => a -> Bool
isDestructor = checkFunctionKind (==IR.FK_Destructor)

isConstructorOrDestructor :: NodeReference a => a -> Bool
isConstructorOrDestructor n = isConstructor n || isDestructor n

--
-- * Queries for Node Address
--

-- | Returns 'True' if given variable (in declaration) is a loop iterator.
isLoopIterator :: NodeAddress -> Bool
isLoopIterator a = (depth a >= 2) && ((==IR.ForStmt) $ getNodeType $ goUp $ goUp a)

hasEnclosingStatement :: NodeAddress -> Bool
hasEnclosingStatement a = case getNode a of
    IR.NT n _ | (IR.toNodeKind n) == IR.Statement -> True
    IR.NT n _ | n /= IR.LambdaExpr && n /= IR.Variable && (IR.toNodeKind n) == IR.Expression -> True
    _ -> not (isRoot a) && hasEnclosingStatement (fromJust $ getParent a)

isEntryPoint :: NodeAddress -> Bool
isEntryPoint a = case getNode a of
    IR.NT IR.CompoundStmt _ -> isRoot a || not (hasEnclosingStatement $ fromJust $ getParent a)
    _                       -> isRoot a

isEntryPointParameter :: NodeAddress -> Bool
isEntryPointParameter v | (not . isVariable) v = False
isEntryPointParameter v | isRoot v = False
isEntryPointParameter v = case getNode $ fromJust $ getParent v of
            IR.NT IR.Parameters _ -> not $ hasEnclosingStatement v
            _                     -> False

-- ** Builtins

getBuiltin :: NodeAddress -> String -> IR.Builtin
getBuiltin addr name = join $ Map.lookup name (IR.getBuiltins $ getInspire addr)

isBuiltin :: NodeAddress -> IR.Builtin -> Bool
isBuiltin _ Nothing    = False
isBuiltin a b@(Just t) = case getNodeType a of
    IR.Lambda | depth a >= 3 -> isBuiltin (goUp $ goUp $ goUp $ a) b
    _                        -> getNode a == t

isBuiltinByName :: NodeAddress -> String -> Bool
isBuiltinByName a n = isBuiltin a $ getBuiltin a n
