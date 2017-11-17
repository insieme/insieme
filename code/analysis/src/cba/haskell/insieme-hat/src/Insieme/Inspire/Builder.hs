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

-- | This moduel defines function to build peaces of code.
-- The workings of these functions are specific to INSPIRE.

module Insieme.Inspire.Builder (

    mkCall,
    mkCallWithArgs,

    mkTypeLiteralType,
    mkTypeLiteral,

    plus,
    plusOne,

    minus,
    minusOne,

    deref,
    refMember,
    refComponent,
    refTemporary,
    refTemporaryInit,
    refArrayElement,
    refStdArrayElement
) where

--import Debug.Trace

import Control.Exception.Base
import Data.Maybe

import Insieme.Inspire.NodeReference
import qualified Insieme.Inspire.IR as IR
import qualified Insieme.Inspire.SourceParser as SP
import {-# UP #-} qualified Insieme.Query as Q

-- basic node types --

mkStringValue :: String -> IR.Tree
mkStringValue s = IR.mkNode (IR.StringValue s) [] []

mkLiteral :: String -> IR.Tree -> IR.Tree
mkLiteral s t = assert (Q.isType t) $ IR.mkNode IR.Literal [t,mkStringValue s] []

mkDeclaration :: IR.Tree -> IR.Tree -> IR.Tree
mkDeclaration t v = assert (Q.isType t) $ IR.mkNode IR.Declaration [t,v] []

mkCall :: IR.Tree -> IR.Tree -> [IR.Tree] -> IR.Tree
mkCall t f argDecls = IR.mkNode IR.CallExpr (t:f:argDecls) []

mkCallWithArgs :: IR.Tree -> IR.Tree -> [IR.Tree] -> IR.Tree
mkCallWithArgs t f args = mkCall t f $ toDecl <$> args
  where
    toDecl a = mkDeclaration t a
      where 
        Just t = Q.getType a

mkIdentifier :: String -> IR.Tree
mkIdentifier s = mkLiteral s SP.identifierType

mkTypeLiteralType :: IR.Tree -> IR.Tree
mkTypeLiteralType t = IR.mkNode IR.GenericType [name, parents, params] []
  where
    name = mkStringValue "type"
    parents = IR.mkNode IR.Parents [] []
    params = IR.mkNode IR.Types [t] []

mkTypeLiteral :: IR.Tree -> IR.Tree
mkTypeLiteral t = mkLiteral "type_literal" $ mkTypeLiteralType t


-- arithmetic operator calls --


plus :: IR.Tree -> IR.Tree -> IR.Tree
plus a b = mkCallWithArgs some_type SP.arithAdd [a,b]

plusOne :: IR.Tree -> IR.Tree
plusOne a = plus a $ mkLiteral "1" SP.uint1


minus :: IR.Tree -> IR.Tree -> IR.Tree
minus a b = mkCallWithArgs some_type SP.arithSub [a,b]

minusOne :: IR.Tree -> IR.Tree
minusOne a = minus a $ mkLiteral "1" SP.uint1


-- ref-operator calls --

deref :: IR.Tree -> IR.Tree
deref t = mkCall resType SP.refDeref [decl]
  where
    resType = fromJust $ Q.getReferencedType refType

    refType = child 0 t

    decl = mkDeclaration refType t

refMember :: IR.Tree -> String -> IR.Tree
refMember t f = mkCall some_ref_type SP.hsRefMemberAccess $ wrapSomeDecl <$> [t,mkIdentifier f]

refComponent :: IR.Tree -> Int -> IR.Tree
refComponent t i = mkCall some_ref_type SP.hsRefComponentAccess $ wrapSomeDecl <$> [t,mkLiteral (show i) SP.uint8]

refTemporary :: IR.Tree -> IR.Tree
refTemporary t = mkCall some_ref_type SP.refTemp [mkDeclaration some_type (mkTypeLiteral t)]

refTemporaryInit :: IR.Tree -> IR.Tree
refTemporaryInit e = mkCall some_ref_type SP.refTempInit [mkDeclaration some_type e]

refArrayElement :: IR.Tree -> Int -> IR.Tree
refArrayElement t i = mkCall some_ref_type SP.hsRefArrayElementAccess $ wrapSomeDecl <$> [t,mkLiteral (show i) SP.uint8]

refStdArrayElement :: IR.Tree -> Int -> IR.Tree
refStdArrayElement t i = mkCall some_ref_type SP.hsRefStdArrayElementAccess $ wrapSomeDecl <$> [t,mkLiteral (show i) SP.uint8]

-- utilities --

some_type :: IR.Tree
some_type     = SP.parseType "some_type"

some_ref_type :: IR.Tree
some_ref_type = SP.parseType "ref<some_type>"

wrapSomeDecl :: IR.Tree -> IR.Tree
wrapSomeDecl = mkDeclaration some_type
