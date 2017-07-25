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
    deref,
    refMember,
    refTemporaryInit
) where

import Control.Exception.Base
import Data.Maybe
import Debug.Trace
import Insieme.Inspire.Query

import qualified Insieme.Inspire.Query as Q
import qualified Insieme.Utils.ParseIR as Lang
import qualified Insieme.Inspire as IR


-- basic node types --

mkStringValue :: String -> IR.Tree
mkStringValue s = IR.mkNode (IR.StringValue s) [] []

mkLiteral :: String -> IR.Tree -> IR.Tree
mkLiteral s t = assert (Q.isType t) $ IR.mkNode IR.Literal [t,mkStringValue s] []

mkDeclaration :: IR.Tree -> IR.Tree -> IR.Tree
mkDeclaration t v = assert (Q.isType t) $ IR.mkNode IR.Declaration [t,v] []

mkCall :: IR.Tree -> IR.Tree -> [IR.Tree] -> IR.Tree
mkCall t f argDecls = IR.mkNode IR.CallExpr (t:f:argDecls) []

mkIdentifier :: String -> IR.Tree
mkIdentifier s = mkLiteral s Lang.identifierType


-- invocations of built-ins --

deref :: IR.Tree -> IR.Tree
deref t = mkCall resType Lang.refDeref [decl]
  where
    resType = fromJust $ getReferencedType refType
    
    refType = IR.goDown 0 t
    
    decl = mkDeclaration refType t


refMember :: IR.Tree -> String -> IR.Tree
refMember t f = mkCall some_ref_type Lang.hsRefMemberAccess $ wrapSomeDecl <$> [t,mkIdentifier f]
  where
    wrapSomeDecl e = mkDeclaration some_type e


refTemporaryInit :: IR.Tree -> IR.Tree
refTemporaryInit e = mkCall some_ref_type Lang.refTempInit [mkDeclaration some_type e]


-- utilities --

some_type     = Lang.parseType "some_type"
some_ref_type = Lang.parseType "ref<some_type>"


