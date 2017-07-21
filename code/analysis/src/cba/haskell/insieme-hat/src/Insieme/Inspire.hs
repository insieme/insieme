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
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

module Insieme.Inspire (
    module Insieme.Inspire,
    module Insieme.Inspire.NodeType,
) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Inspire.NodeType

-- * Tree

data Tree = MkTree {
      mtId        :: Maybe Int,
      mtInnerTree :: InnerTree
    } deriving (Show, Generic, NFData)

instance Eq Tree where
    MkTree { mtId = Just ida } == MkTree { mtId = Just idb } =
        ida == idb
    a == b =
        mtInnerTree a == mtInnerTree b

instance Ord Tree where
    compare Tree{ getID = Just ida } Tree{ getID = Just idb } | ida == idb = EQ
    compare a b = compare (mtInnerTree a) (mtInnerTree b)

treeExactEq :: Tree -> Tree -> Bool
treeExactEq a b = mtId a == mtId b && mtInnerTree a == mtInnerTree b

data InnerTree = InnerTree {
      itNodeType    :: NodeType,
      itChildren    :: [Tree],
      itBuiltinTags :: [String]
    } deriving (Eq, Ord, Show, Generic, NFData)

pattern Node :: NodeType -> [Tree] -> Tree
pattern Node x y <- MkTree _ (InnerTree x y _)

pattern Tree :: Maybe Int -> NodeType -> [Tree] -> [String] -> Tree
pattern Tree { getID, getNodeType, getChildren, builtinTags }
      = MkTree getID (InnerTree getNodeType getChildren builtinTags)

unsafeMkNode :: Int -> NodeType -> [Tree] -> [String] -> Tree
unsafeMkNode i nt ch bt = MkTree (Just i) (InnerTree nt ch bt)

mkNode :: NodeType -> [Tree] -> [String] -> Tree
mkNode nt ch bt = MkTree Nothing (InnerTree nt ch bt)

numChildren :: Tree -> Int
numChildren = length . getChildren

goDown :: Int -> Tree -> Tree
goDown i t = (!!i) $ getChildren t

isBuiltin :: Tree -> String -> Bool
isBuiltin t s = elem s $ builtinTags t

-- * Node Kind

data NodeKind = Value
              | Type
              | Expression
              | Statement
              | TranslationUnit
              | Support
  deriving (Eq, Ord, Show)

toNodeKind :: NodeType -> NodeKind

toNodeKind (BoolValue _)   = Value
toNodeKind (CharValue _)   = Value
toNodeKind (IntValue _)    = Value
toNodeKind (UIntValue _)   = Value
toNodeKind (StringValue _) = Value

toNodeKind FunctionType                = Type
toNodeKind TupleType                   = Type
toNodeKind GenericType                 = Type
toNodeKind NumericType                 = Type
toNodeKind TagType                     = Type
toNodeKind TagTypeReference            = Type
toNodeKind TypeVariable                = Type
toNodeKind VariadicTypeVariable        = Type
toNodeKind GenericTypeVariable         = Type
toNodeKind VariadicGenericTypeVariable = Type

toNodeKind Literal         = Expression
toNodeKind Variable        = Expression
toNodeKind CallExpr        = Expression
toNodeKind LambdaExpr      = Expression
toNodeKind LambdaReference = Expression
toNodeKind BindExpr        = Expression
toNodeKind CastExpr        = Expression
toNodeKind TupleExpr       = Expression
toNodeKind InitExpr        = Expression
toNodeKind JobExpr         = Expression
toNodeKind MarkerExpr      = Expression

toNodeKind BreakStmt       = Statement
toNodeKind ContinueStmt    = Statement
toNodeKind ReturnStmt      = Statement
toNodeKind DeclarationStmt = Statement
toNodeKind CompoundStmt    = Statement
toNodeKind WhileStmt       = Statement
toNodeKind ForStmt         = Statement
toNodeKind IfStmt          = Statement
toNodeKind SwitchStmt      = Statement
toNodeKind ThrowStmt       = Statement
toNodeKind TryCatchStmt    = Statement
toNodeKind LabelStmt       = Statement
toNodeKind GotoStmt        = Statement
toNodeKind MarkerStmt      = Statement

toNodeKind Program = TranslationUnit

toNodeKind Types                        = Support
toNodeKind TagTypeBinding               = Support
toNodeKind TagTypeDefinition            = Support
toNodeKind Declaration                  = Support
toNodeKind Struct                       = Support
toNodeKind Union                        = Support
toNodeKind Field                        = Support
toNodeKind Fields                       = Support
toNodeKind MemberFunction               = Support
toNodeKind MemberFunctions              = Support
toNodeKind PureVirtualMemberFunction    = Support
toNodeKind PureVirtualMemberFunctions   = Support
toNodeKind Parent                       = Support
toNodeKind Parents                      = Support
toNodeKind Lambda                       = Support
toNodeKind LambdaBinding                = Support
toNodeKind LambdaDefinition             = Support
toNodeKind SwitchCase                   = Support
toNodeKind SwitchCases                  = Support
toNodeKind CatchClause                  = Support
toNodeKind Parameters                   = Support
toNodeKind Expressions                  = Support

-- * Function Kind

data FunctionKind = FK_Plain
                  | FK_Closure
                  | FK_Constructor
                  | FK_Destructor
                  | FK_MemberFunction
                  | FK_VirtualMemberFunction
  deriving (Eq, Ord, Show)

toFunctionKind :: Tree -> Maybe FunctionKind
toFunctionKind (Node FunctionType (_:_:k:_)) = case getNodeType k of
    UIntValue 1 -> Just $ FK_Plain
    UIntValue 2 -> Just $ FK_Closure
    UIntValue 3 -> Just $ FK_Constructor
    UIntValue 4 -> Just $ FK_Destructor
    UIntValue 5 -> Just $ FK_MemberFunction
    UIntValue 6 -> Just $ FK_VirtualMemberFunction
    _ -> Nothing

toFunctionKind _ = Nothing
