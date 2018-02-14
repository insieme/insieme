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

{- ONLY USE THIS MODULE IF YOU ARE LINKING WITH libinsieme_analysis.so -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Insieme.Adapter.Utils (pprintTree) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Char8 as BS8

import Data.List

import qualified Insieme.Inspire as I

foreign import ccall "hat_c_pretty_print_tree"
  prettyPrintTree :: CString -> CSize -> IO CString

pprintTree :: I.Tree -> String

--pprintTree ir = unsafePerformIO $ do
--    let dump = I.dumpBinaryDump ir
--    pretty_c <- BS8.useAsCStringLen dump $ \(sz,l) -> prettyPrintTree sz (fromIntegral l)
--    pretty   <- peekCString pretty_c
--    free pretty_c
--    return pretty


-- support node types
pprintTree (I.Node (I.StringValue s) _) = s
pprintTree (I.Node (I.BoolValue   v) _) = show v
pprintTree (I.Node (I.CharValue   v) _) = show v
pprintTree (I.Node (I.IntValue    v) _) = show v
pprintTree (I.Node (I.UIntValue   v) _) = show v

-- support expressions
pprintTree (I.Node I.Literal [_,v]) = pprintTree v
pprintTree (I.Node I.Variable [_,i]) = "v" ++ pprintTree i
pprintTree (I.Node I.CallExpr (_:f:args) ) = (pprintTree f) ++ "(" ++ (intercalate "," $ pprintTree <$> args) ++ ")"

-- handle derived built-ins
--pprintTree n@(I.Node (I.LambdaExpr) _) | I.isaBuiltin n = head $ I.builtinTags n


-- support support nodes
pprintTree (I.Node I.Declaration [_,v]) = pprintTree v


-- everything else
pprintTree (I.Node n _) = "<some " ++ show n ++ " node>"