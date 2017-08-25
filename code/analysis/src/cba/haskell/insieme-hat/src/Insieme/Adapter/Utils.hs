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

module Insieme.Adapter.Utils (
    parseIR
  , pprintTree
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Insieme.Inspire.BinaryDumper
import Insieme.Inspire.BinaryParser
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Char8 as BS8
import qualified Insieme.Inspire as IR

foreign import ccall "hat_c_parse_ir_statement"
  cParseIrStatement :: CString -> CSize -> Ptr CString -> Ptr CSize -> IO ()

parseIR :: String -> IO IR.Tree
parseIR stmt = do
    alloca $ \data_ptr_c ->
        alloca $ \size_ptr_c -> do
            withCStringLen stmt $ \(sz,l) ->cParseIrStatement sz (fromIntegral l) data_ptr_c size_ptr_c
            data_c <- peek data_ptr_c
            size_c <- peek size_ptr_c
            dump   <- BS8.packCStringLen (data_c, fromIntegral size_c)
            free data_c
            let Right ir = parseBinaryDump dump
            return ir

foreign import ccall "hat_c_pretty_print_tree"
  prettyPrintTree :: CString -> CSize -> IO CString

pprintTree :: IR.Tree -> String
pprintTree ir = unsafePerformIO $ do
    let dump = dumpBinaryDump ir
    pretty_c <- BS8.useAsCStringLen dump $ \(sz,l) -> prettyPrintTree sz (fromIntegral l)
    pretty   <- peekCString pretty_c
    free pretty_c
    return pretty
