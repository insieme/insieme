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

{- | Use this module instead of the original Insieme.Adapter for profiling.
     No FFI imports are allowed here!
 -}

module Insieme.Adapter.Utils where

import Insieme.Inspire.BinaryParser
import qualified Data.ByteString.Char8 as BS8
import System.Process (proc, CreateProcess(..))
import System.Process.ByteString
import System.Environment (getEnvironment)

import qualified Insieme.Inspire as IR

pprintTree :: IR.Tree -> String
pprintTree = const "-omitted-for-profiling-"

parseIR :: String -> IO IR.Tree
parseIR input = do
    envvars <- getEnvironment
    let cp = (proc "inspire" ["-s", "-i", "-", "-k", "-"]) {
                    env = Just $ envvars ++ [("INSIEME_NO_SEMA","1")]
                }
    irb <- readCreateProcess cp (BS8.pack input)
    let Right ir = parseBinaryDump irb
    return ir
