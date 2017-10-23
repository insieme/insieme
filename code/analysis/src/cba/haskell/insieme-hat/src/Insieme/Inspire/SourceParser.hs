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

module Insieme.Inspire.SourceParser where

import System.IO.Unsafe (unsafePerformIO)

import Insieme.Inspire.SourceParser.FFI (parseInspireSource)
import Insieme.Inspire.NodeReference
import qualified Insieme.Inspire.IR as IR

-- | Parse a given IR expression.

parseExpr :: String -> IR.Tree
parseExpr = IR.removeIds . unsafePerformIO . parseInspireSource

-- | Parse a given IR type.
parseType :: String -> IR.Tree
parseType txt = child 0 $ parseExpr $ "lit(\"x\":" ++ txt ++ ")"

-- inspire constructs --

int1 :: IR.Tree
int1 = parseType "int<1>"

int2 :: IR.Tree
int2 = parseType "int<2>"

int4 :: IR.Tree
int4 = parseType "int<4>"

int8 :: IR.Tree
int8 = parseType "int<8>"


uint1 :: IR.Tree
uint1 = parseType "uint<1>"

uint2 :: IR.Tree
uint2 = parseType "uint<2>"

uint4 :: IR.Tree
uint4 = parseType "uint<4>"

uint8 :: IR.Tree
uint8 = parseType "uint<8>"


identifierType :: IR.Tree
identifierType = parseType "identifier"

-- arithmetic operations --


arithAdd :: IR.Tree
arithAdd = parseExpr "hs_arith_add"

arithSub :: IR.Tree
arithSub = parseExpr "hs_arith_sub"


-- reference operations --

refDeref :: IR.Tree
refDeref = parseExpr "ref_deref"

refAssign :: IR.Tree
refAssign = parseExpr "ref_assign"

refTempInit :: IR.Tree
refTempInit = parseExpr "ref_temp_init"

-- Haskell extension constructs --

hsRefMemberAccess :: IR.Tree
hsRefMemberAccess = parseExpr "hs_ref_member_access"

hsRefComponentAccess :: IR.Tree
hsRefComponentAccess = parseExpr "hs_ref_component_access"

hsRefArrayElementAccess :: IR.Tree
hsRefArrayElementAccess = parseExpr "hs_ref_array_element_access"

hsRefStdArrayElementAccess :: IR.Tree
hsRefStdArrayElementAccess = parseExpr "hs_ref_std_array_element_access"