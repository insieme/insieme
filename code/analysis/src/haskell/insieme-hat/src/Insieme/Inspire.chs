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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Insieme.Inspire where

import Data.Function (on)
import Data.Map (Map)
import Insieme.Inspire.ThHelpers
import Language.Haskell.TH

#c
#define CONCRETE(name) NT_##name,
enum C_NodeType {
#include "insieme/core/ir_nodes.def"
};
#undef CONCRETE
#endc

-- | NodeType taken from Insieme header files.
{#enum C_NodeType {} deriving (Eq, Show)#}

-- | Generates Inspire data structure.
$(let

    base :: Q [Dec]
    base =
      [d|
        data NodeType =
              BoolValue   Bool
            | CharValue   Char
            | IntValue    Int
            | UIntValue   Int
            | StringValue String
          deriving (Eq, Ord, Show, Read)
      |]

    extend :: Q [Dec] -> Q [Dec]
    extend base = do
#if defined(MIN_VERSION_template_haskell)
#if MIN_VERSION_template_haskell(2,11,0)
        [DataD [] n [] Nothing cons ds] <- base
        TyConI (DataD [] _ [] Nothing nt_cons _) <- reify ''C_NodeType
        let cons' = genCons <$> filter (not . isLeaf) nt_cons
        return $ [DataD [] n [] Nothing (cons ++ cons') ds]
#endif
#else
        [DataD [] n [] cons ds] <- base
        TyConI (DataD [] _ [] nt_cons _) <- reify ''C_NodeType
        let cons' = genCons <$> filter (not . isLeaf) nt_cons
        return $ [DataD [] n [] (cons ++ cons') ds]
#endif

    genCons :: Con -> Con
    genCons (NormalC n _) = NormalC (removePrefix "NT_" n) []
    genCons _             = error "unexpected Con"

  in
    extend base
 )

-- | Generates toNodeType and fromNodeType functions.
$(let

    baseFromNodeType :: Q [Dec]
    baseFromNodeType =
      [d|
        fromNodeType :: C_NodeType -> NodeType
        fromNodeType NT_BoolValue   = BoolValue False
        fromNodeType NT_CharValue   = CharValue ' '
        fromNodeType NT_IntValue    = IntValue 0
        fromNodeType NT_UIntValue   = UIntValue 0
        fromNodeType NT_StringValue = StringValue ""
      |]

    baseToNodeType :: Q [Dec]
    baseToNodeType =
      [d|
        toNodeType :: NodeType -> C_NodeType
        toNodeType (BoolValue   _) = NT_BoolValue
        toNodeType (CharValue   _) = NT_CharValue
        toNodeType (IntValue    _) = NT_IntValue
        toNodeType (UIntValue   _) = NT_UIntValue
        toNodeType (StringValue _) = NT_StringValue
      |]

    extend :: Q [Dec] -> (Con -> Clause) -> Q [Dec]
    extend base gen = do
        [d, FunD n cls]               <- base
#if defined(MIN_VERSION_template_haskell)
#if MIN_VERSION_template_haskell(2,11,0)
        TyConI (DataD [] _ [] Nothing cons _) <- reify ''C_NodeType
#endif
#else
        TyConI (DataD [] _ [] cons _) <- reify ''C_NodeType
#endif
        let cls' = gen <$> filter (not . isLeaf) cons
        return $ [d,FunD n (cls ++ cls')]

    genClauseFromNodeType :: Con -> Clause
    genClauseFromNodeType (NormalC n _) = Clause [ConP n []] (NormalB (ConE (removePrefix "NT_" n))) []
    genClauseFromNodeType _             = error "unexpected Con"

    genClauseToNodeType :: Con -> Clause
    genClauseToNodeType (NormalC n _) = Clause [ConP (removePrefix "NT_" n) []] (NormalB (ConE n)) []
    genClauseToNodeType _             = error "unexpected Con"

  in
    (++) <$> extend baseFromNodeType genClauseFromNodeType
         <*> extend baseToNodeType   genClauseToNodeType
 )

--
-- * Tree
--

data Tree = Tree { getNodePair :: (Int, NodeType),
                   getChildren :: [Tree]           }
  deriving (Show)

instance Eq Tree where
    (==) = (==) `on` getID

instance Ord Tree where
    compare = compare `on` getID

pattern NT x y <- Tree (_, x) y

mkNode :: (Int, NodeType) -> [Tree] -> Tree
mkNode = Tree

getID :: Tree -> Int
getID = fst . getNodePair

getNodeType :: Tree -> NodeType
getNodeType = snd . getNodePair

--
-- * Inspire
--

type Builtins = Map String Tree

data Inspire = Inspire { getTree     :: Tree,
                         getBuiltins :: Builtins }

--
-- * Node Kinds
-- 

data NodeKind =
        Value | Type | Expression | Statement | TranslationUnit | Support
    deriving (Eq,Ord,Show)
    
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
                         
