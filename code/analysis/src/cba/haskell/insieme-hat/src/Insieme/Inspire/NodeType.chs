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
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module defines INSPIRE NodeTypes in Haskell.

NodeTypes are imported from the original INSIEME header files resulting in
'C_NodeType'. The actual data structure 'NodeType' is generated from it using
Template Haskell. The difference between them is that values of value nodes
have been attached, making access more convenient.

Additional the functions 'toNodeType' and 'fromNodeType' are provided, but
since values cannot be known only from a 'C_NodeType' they are set to a default
value.

 -}

module Insieme.Inspire.NodeType where

import Control.DeepSeq
import GHC.Generics (Generic)
import Insieme.Inspire.ThHelpers
import Language.Haskell.TH

#c
#define CONCRETE(name) NT_##name,
enum C_NodeType {
#include "insieme/core/ir_nodes.def"
};
#undef CONCRETE
#endc

-- | NodeTypes taken from Insieme header files. These should be used only
-- inside "Insieme.Inspire.BinaryParser", use 'NodeType' instead.
{#enum C_NodeType {} deriving (Eq, Ord, Show)#}

-- | Represent INSPIRE NodeTypes, /ValueNodes/ have their values attached via
-- these NodeTypes.
$(let

    base :: Q [Dec]
    base =
      [d|
        data NodeType = BoolValue   Bool
                      | CharValue   Char
                      | IntValue    Int
                      | UIntValue   Int
                      | StringValue String
          deriving (Eq, Ord, Show, Read, Generic, NFData)
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

  in extend base
 )

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

  in (++) <$> extend baseFromNodeType genClauseFromNodeType
          <*> extend baseToNodeType   genClauseToNodeType
 )
